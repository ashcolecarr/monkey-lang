use super::ast::*;
use super::environment::Environment;
use super::object::*;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub fn eval(node: &Node, environment: Rc<RefCell<Environment>>) -> Object {
    match node {
        Node::Program(p) => eval_program(&p, environment),
        Node::Statement(stmt) => {
            match stmt {
                Statement::ExpressionStatement(es) => {
                    match &es.expression {
                        Some(exp) => eval(&Node::Expression(exp.clone()), environment),
                        None => new_error("expression could not be evaluated"),
                    }
                },
                Statement::BlockStatement(bs) => eval_block_statement(&bs, environment),
                Statement::ReturnStatement(rs) => {
                    if let Some(rv) = &rs.return_value {
                        let value = eval(&Node::Expression(rv.clone()), environment);
                        if is_error(&value) {
                            return value;
                        }

                        return Object::ReturnValue(ReturnValue::new(Box::new(value)));
                    }
                    
                    Object::Null(Null::new())
                },
                Statement::LetStatement(ls) => {
                    match &ls.value {
                        Some(v) => {
                            let val = eval(&Node::Expression(v.clone()), Rc::clone(&environment));
                            if is_error(&val) {
                                return val;
                            }

                            environment.borrow_mut().set(ls.name.value.clone(), val);
                            Object::NonPrint
                        },
                        None => Object::Null(Null::new()),
                    }
                },
            }
        },
        Node::Expression(exp) => {
            match exp {
                Expression::IntegerLiteral(il) => Object::Integer(Integer::new(il.value)),
                Expression::BooleanLiteral(bl) => Object::Boolean(Boolean::new(bl.value)),
                Expression::PrefixExpression(pe) => {
                    let right = eval(&Node::Expression(*pe.right.clone()), environment); 
                    if is_error(&right) {
                        return right;
                    }
                        
                    eval_prefix_expression(pe.operator.as_str(), &right)
                },
                Expression::InfixExpression(ie) => {
                    let left = eval(&Node::Expression(*ie.left.clone()), Rc::clone(&environment)); 
                    if is_error(&left) {
                        return left;
                    }
                    
                    let right = eval(&Node::Expression(*ie.right.clone()), Rc::clone(&environment)); 
                    if is_error(&right) {
                        return right;
                    }

                    eval_infix_expression(ie.operator.as_str(), &left, &right)
                },
                Expression::IfExpression(ie) => eval_if_expression(ie, environment),
                Expression::Identifier(id) => eval_identifier(id, environment),
                Expression::FunctionLiteral(fl) => {
                    let parameters = &fl.parameters;
                    let body = &fl.body;
                    Object::Function(Function::new(parameters.clone(), body.clone(), Rc::clone(&environment)))
                },
                Expression::CallExpression(ce) => {
                    let function = eval(&Node::Expression(*ce.function.clone()), Rc::clone(&environment));
                    if is_error(&function) {
                        return function;
                    }

                    let arguments = eval_expressions(&ce.arguments, environment);
                    if arguments.len() == 1 && is_error(&arguments[0]) {
                        return arguments[0].clone();
                    }

                    apply_function(&function, &arguments)
                },
                Expression::StringLiteral(sl) => Object::String(StringObject::new(sl.value.as_str())),
                Expression::ArrayLiteral(al) => {
                    let elements = eval_expressions(&al.elements, environment);
                    if elements.len() == 1 && is_error(&elements[0]) {
                        return elements[0].clone();
                    }

                    Object::Array(Array::new(elements))
                },
                Expression::IndexExpression(ie) => {
                    let left = eval(&Node::Expression(*ie.left.clone()), Rc::clone(&environment));
                    if is_error(&left) {
                        return left;
                    }

                    let index = eval(&Node::Expression(*ie.index.clone()), environment);
                    if is_error(&index) {
                        return index;
                    }

                    eval_index_expression(&left, &index)
                },
                Expression::HashLiteral(hl) => eval_hash_literal(&hl, environment),
            }
        },
    }
}

fn eval_program(program: &Program, environment: Rc<RefCell<Environment>>) -> Object {
    let mut result = Object::Null(Null::new());

    for statement in &program.statements {
        result = eval(&Node::Statement(statement.clone()), Rc::clone(&environment));

        if let Object::ReturnValue(rv) = result {
            return *rv.value.clone();
        } else if let Object::Error(_) = result {
            return result;
        }
    }

    result
}

fn eval_block_statement(block: &BlockStatement, environment: Rc<RefCell<Environment>>) -> Object {
    let mut result = Object::Null(Null::new());

    for statement in &block.statements {
        result = eval(&Node::Statement(statement.clone()), Rc::clone(&environment));

        if let Object::ReturnValue(_) = &result {
            return result;
        } else if let Object::Error(_) = result {
            return result;
        }
    }

    result
}

fn eval_prefix_expression(operator: &str, right: &Object) -> Object {
    match operator {
        "!" => eval_bang_operator_expression(right),
        "-" => eval_minus_prefix_operator_expression(right),
        _ => new_error(format!("unknown operator: {}{}", operator, right.type_of()).as_str()),
    }
}

fn eval_bang_operator_expression(right: &Object) -> Object {
    match right {
        Object::Boolean(bl) => {
            if bl.value {
                Object::Boolean(Boolean::new(false))
            } else {
                Object::Boolean(Boolean::new(true))
            }
        },
        Object::Null(_) => Object::Boolean(Boolean::new(true)),
        _ => Object::Boolean(Boolean::new(false))
    }
}

fn eval_minus_prefix_operator_expression(right: &Object) -> Object {
    if let Object::Integer(i) = right {
        Object::Integer(Integer::new(-i.value))
    } else {
        new_error(format!("unknown operator: -{}", right.type_of()).as_str())
    }
}

fn eval_infix_expression(operator: &str, left: &Object, right: &Object) -> Object {
    match (left, right) {
        (Object::Integer(l), Object::Integer(r)) => eval_integer_infix_expression(operator, l, r),
        (Object::Boolean(l), Object::Boolean(r)) => eval_boolean_infix_expression(operator, l, r),
        (Object::String(l), Object::String(r)) => eval_string_infix_expression(operator, l, r),
        _ => new_error(format!("type mismatch: {} {} {}", left.type_of(), operator, right.type_of()).as_str()),
    }
}

fn eval_integer_infix_expression(operator: &str, left: &Integer, right: &Integer) -> Object {
    let left_val = left.value;
    let right_val = right.value;
    match operator {
        "+" => Object::Integer(Integer::new(left_val + right_val)),
        "-" => Object::Integer(Integer::new(left_val - right_val)),
        "*" => Object::Integer(Integer::new(left_val * right_val)),
        "/" => Object::Integer(Integer::new(left_val / right_val)),
        "<" => Object::Boolean(Boolean::new(left_val < right_val)),
        ">" => Object::Boolean(Boolean::new(left_val > right_val)),
        "==" => Object::Boolean(Boolean::new(left_val == right_val)),
        "!=" => Object::Boolean(Boolean::new(left_val != right_val)),
        _ => new_error(format!("unknown operator: {} {} {}", left.type_of(), operator, right.type_of()).as_str()),
    }
}

fn eval_boolean_infix_expression(operator: &str, left: &Boolean, right: &Boolean) -> Object {
    let left_val = left.value;
    let right_val = right.value;
    match operator {
        "==" => Object::Boolean(Boolean::new(left_val == right_val)),
        "!=" => Object::Boolean(Boolean::new(left_val != right_val)),
        _ => new_error(format!("unknown operator: {} {} {}", left.type_of(), operator, right.type_of()).as_str()),
    }
}

fn eval_string_infix_expression(operator: &str, left: &StringObject, right: &StringObject) -> Object {
    match operator {
        "+" => {
            let left_val = left.value.clone();
            let right_val = right.value.clone();
            let concatenated = left_val + right_val.as_str();

            Object::String(StringObject::new(concatenated.as_str()))
        },
        _ => new_error(format!("unknown operator: {} {} {}", left.type_of(), operator, right.type_of()).as_str()),
    }
}

fn eval_if_expression(if_expression: &IfExpression, environment: Rc<RefCell<Environment>>) -> Object {
    let condition = eval(&Node::Expression(*if_expression.condition.clone()), Rc::clone(&environment));
    if is_error(&condition) {
        return condition;
    }

    if is_truthy(&condition) {
        return eval(&Node::Statement(Statement::BlockStatement(if_expression.consequence.clone())), environment);
    }

    if let Some(alt) = &if_expression.alternative {
        return eval(&Node::Statement(Statement::BlockStatement(alt.clone())), environment);
    }

    Object::Null(Null::new())
}

fn eval_identifier(identifier: &Identifier, environment: Rc<RefCell<Environment>>) -> Object {
    let value = environment.borrow().get(&identifier.value);
    if let Some(val) = value {
        return val;
    }

    let builtin = environment.borrow().get_builtin(&identifier.value);
    if let Some(bi) = builtin {
        return bi;
    }

    new_error(format!("identifier not found: {}", identifier.value).as_str())
}

fn eval_expressions(expressions: &Vec<Expression>, environment: Rc<RefCell<Environment>>) -> Vec<Object> {
    let mut result = vec![];

    for expression in expressions {
        let evaluated = eval(&Node::Expression(expression.clone()), Rc::clone(&environment));
        if is_error(&evaluated) {
            return vec![evaluated];
        }

        result.push(evaluated);
    }

    result
}

fn apply_function(function: &Object, arguments: &Vec<Object>) -> Object {
    match function {
        Object::Function(func) => {
            let extended_env = extend_function_env(&func, &arguments);
            let evaluated = eval(&Node::Statement(Statement::BlockStatement(func.body.clone())), Rc::new(RefCell::new(extended_env)));

            unwrap_return_value(evaluated)
        },
        Object::Builtin(bi) => {
            let builtin = bi.builtin_function;
            builtin(arguments)
        }
        _ => new_error(format!("not a function: {}", function.type_of()).as_str()),
    }
}

fn extend_function_env(function: &Function, arguments: &Vec<Object>) -> Environment {
    let mut environment = Environment::new_enclosed(Rc::clone(&function.environment));

    for (i, parameter) in function.parameters.iter().enumerate() {
        environment.set(parameter.value.clone(), arguments[i].clone());
    }

    environment
}

fn unwrap_return_value(object: Object) -> Object {
    match object {
        Object::ReturnValue(rv) => *rv.value.clone(), 
        _ => object,
    }
}

fn eval_index_expression(left: &Object, index: &Object) -> Object {
    match (left, index) {
        (Object::Array(arr), Object::Integer(idx)) => eval_array_index_expression(arr, idx),
        (Object::Hash(h), _) => eval_hash_index_expression(h, index),
        _ => new_error(format!("index operator not supported: {}", left.type_of()).as_str()),
    }
}

fn eval_array_index_expression(array: &Array, index: &Integer) -> Object {
    let max = (array.elements.len() - 1) as i64;

    if index.value < 0 || index.value > max {
        Object::Null(Null::new())
    } else {
        array.elements[index.value as usize].clone()
    }
}

fn eval_hash_literal(hash: &HashLiteral, environment: Rc<RefCell<Environment>>) -> Object {
    let mut pairs: HashMap<Object, Object> = HashMap::new();

    for (k, v) in &hash.pairs {
        let key = eval(&Node::Expression(k.clone()), Rc::clone(&environment));
        if is_error(&key) {
            return key;
        }

        if !key.is_hashable() {
            return new_error(format!("unusable as hash key: {}", key.type_of()).as_str());
        }

        let value = eval(&Node::Expression(v.clone()), Rc::clone(&environment));
        if is_error(&value) {
            return value;
        }

        pairs.insert(key, value);
    }

    Object::Hash(HashObject::new(pairs))
}

fn eval_hash_index_expression(hash: &HashObject, index: &Object) -> Object {
    if !index.is_hashable() {
        return new_error(format!("unusable as hash key: {}", index.type_of()).as_str());
    }

    match hash.pairs.get(&index) {
        Some(p) => p.clone(),
        None => Object::Null(Null::new()),
    }
}

fn is_truthy(object: &Object) -> bool {
    match object {
        Object::Null(_) => false,
        Object::Boolean(bl) => bl.value,
        _ => true,
    }
}

fn new_error(message: &str) -> Object {
    Object::Error(Error::new(message))
}

fn is_error(object: &Object) -> bool {
    if let Object::Error(_) = object {
        true
    } else {
        false
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use super::super::lexer::Lexer;
    use super::super::parser::Parser;
    use std::collections::hash_map::HashMap;

    #[test]
    fn test_eval_integer_expression() {
        struct IntegerTest<'a> {
            input: &'a str,
            expected: i64,
        };

        let integer_tests = vec![
            IntegerTest { input: "5", expected: 5 },
            IntegerTest { input: "10", expected: 10 },
            IntegerTest { input: "-5", expected: -5 },
            IntegerTest { input: "-10", expected: -10 },
            IntegerTest { input: "5 + 5 + 5 + 5 - 10", expected: 10 },
            IntegerTest { input: "2 * 2 * 2 * 2 * 2", expected: 32 },
            IntegerTest { input: "-50 + 100 + -50", expected: 0 },
            IntegerTest { input: "5 * 2 + 10", expected: 20 },
            IntegerTest { input: "5 + 2 * 10", expected: 25 },
            IntegerTest { input: "20 + 2 * -10", expected: 0 },
            IntegerTest { input: "50 / 2 * 2 + 10", expected: 60 },
            IntegerTest { input: "2 * (5 + 10)", expected: 30 },
            IntegerTest { input: "3 * 3 * 3 + 10", expected: 37 },
            IntegerTest { input: "3 * (3 * 3) + 10", expected: 37 },
            IntegerTest { input: "(5 + 10 * 2 + 15 / 3) * 2 + -10", expected: 50 },
        ];

        for integer_test in integer_tests {
            match test_eval(integer_test.input) {
                Some(e) => test_integer_object(&e, integer_test.expected),
                None => assert!(false, "Program could not be evaluated."),
            }
        }
    }

    #[test]
    fn test_eval_boolean_expression() {
        struct BooleanTest<'a> {
            input: &'a str,
            expected: bool,
        };

        let boolean_tests = vec![
            BooleanTest { input: "true", expected: true },
            BooleanTest { input: "false", expected: false },
            BooleanTest { input: "1 < 2", expected: true },
            BooleanTest { input: "1 > 2", expected: false },
            BooleanTest { input: "1 < 1", expected: false },
            BooleanTest { input: "1 > 1", expected: false },
            BooleanTest { input: "1 == 1", expected: true },
            BooleanTest { input: "1 != 1", expected: false },
            BooleanTest { input: "1 == 2", expected: false },
            BooleanTest { input: "1 != 2", expected: true },
            BooleanTest { input: "true == true", expected: true },
            BooleanTest { input: "false == false", expected: true },
            BooleanTest { input: "true == false", expected: false },
            BooleanTest { input: "true != false", expected: true },
            BooleanTest { input: "false != true", expected: true },
            BooleanTest { input: "(1 < 2) == true", expected: true },
            BooleanTest { input: "(1 < 2) == false", expected: false },
            BooleanTest { input: "(1 > 2) == true", expected: false },
            BooleanTest { input: "(1 > 2) == false", expected: true },
        ];

        for boolean_test in boolean_tests {
            match test_eval(boolean_test.input) {
                Some(e) => test_boolean_object(&e, boolean_test.expected),
                None => assert!(false, "Program could not be evaluated."),
            }
        }
    }

    #[test]
    fn test_bang_operator() {
        struct BangTest<'a> {
            input: &'a str,
            expected: bool,
        };

        let bang_tests = vec![
            BangTest { input: "!true", expected: false },
            BangTest { input: "!false", expected: true },
            BangTest { input: "!5", expected: false },
            BangTest { input: "!!true", expected: true },
            BangTest { input: "!!false", expected: false },
            BangTest { input: "!!5", expected: true },
        ];

        for bang_test in bang_tests {
            match test_eval(bang_test.input) {
                Some(e) => test_boolean_object(&e, bang_test.expected),
                None => assert!(false, "Program could not be evaluated."),
            }
        }
    }

    #[test]
    fn test_if_else_expressions() {
        struct IfElseTest<'a> {
            input: &'a str,
            expected: Object,
        };

        let if_else_tests = vec![
            IfElseTest { input: "if (true) { 10 }", expected: Object::Integer(Integer::new(10)) },
            IfElseTest { input: "if (false) { 10 }", expected: Object::Null(Null::new()) },
            IfElseTest { input: "if (1) { 10 }", expected: Object::Integer(Integer::new(10)) },
            IfElseTest { input: "if (1 < 2) { 10 }", expected: Object::Integer(Integer::new(10)) },
            IfElseTest { input: "if (1 > 2) { 10 }", expected: Object::Null(Null::new()) },
            IfElseTest { input: "if (1 > 2) { 10 } else { 20 }", expected: Object::Integer(Integer::new(20)) },
            IfElseTest { input: "if (1 < 2) { 10 } else { 20 }", expected: Object::Integer(Integer::new(10)) },
        ];

        for if_else_test in if_else_tests {
            match test_eval(if_else_test.input) {
                Some(e) => {
                    match (&e, if_else_test.expected) {
                        (Object::Integer(_), Object::Integer(i)) => test_integer_object(&e, i.value),
                        _ => test_null_object(&e),
                    };
                },
                None => assert!(false, "Program could not be evaluated."),
            }
        }
    }

    #[test]
    fn test_return_statements() {
        struct ReturnTest<'a> {
            input: &'a str,
            expected: i64,
        };

        let multiline1 = r#"
if (10 > 1) {
  if (10 > 1) {
    return 10;
  }
  
  return 1;
}"#;
        let multiline2 = r#"
let f = fn(x) {
  return x;
  x + 10;
};
f(10);"#;
        let multiline3 = r#"
let f = fn(x) {
  let result = x + 10;
  return result;
  return 10;
};
f(10);"#;
        let return_tests = vec![
            ReturnTest { input: "return 10;", expected: 10 },
            ReturnTest { input: "return 10; 9;", expected: 10 },
            ReturnTest { input: "return 2 * 5; 9;", expected: 10 },
            ReturnTest { input: "9; return 2 * 5; 9;", expected: 10 },
            ReturnTest { input: multiline1, expected: 10 },
            ReturnTest { input: multiline2, expected: 10 },
            ReturnTest { input: multiline3, expected: 20 },
        ];

        for return_test in return_tests {
            match test_eval(return_test.input) {
                Some(e) => test_integer_object(&e, return_test.expected),
                None => assert!(false, "Program could not be evaluated."),
            }
        }
    }

    #[test]
    fn test_error_handling() {
        struct ErrorTest<'a> {
            input: &'a str,
            expected: &'a str,
        };

        let multiline_input = r#"
if (10 > 1) {
  if (10 > 1) {
    return true + false;
  }
  
  return 1;
}"#;
        let error_tests = vec![
            ErrorTest { input: "5 + true;", expected: "type mismatch: INTEGER + BOOLEAN" },
            ErrorTest { input: "5 + true; 5;", expected: "type mismatch: INTEGER + BOOLEAN" },
            ErrorTest { input: "-true", expected: "unknown operator: -BOOLEAN" },
            ErrorTest { input: "true + false;", expected: "unknown operator: BOOLEAN + BOOLEAN" },
            ErrorTest { input: "true + false + true + false;;", expected: "unknown operator: BOOLEAN + BOOLEAN" },
            ErrorTest { input: "5; true + false; 5", expected: "unknown operator: BOOLEAN + BOOLEAN" },
            ErrorTest { input: "if (10 > 1) { true + false; }", expected: "unknown operator: BOOLEAN + BOOLEAN" },
            ErrorTest { input: multiline_input, expected: "unknown operator: BOOLEAN + BOOLEAN" },
            ErrorTest { input: "foobar", expected: "identifier not found: foobar" },
            ErrorTest { input: "\"Hello\" - \"World\"", expected: "unknown operator: STRING - STRING" },
            ErrorTest { input: "{\"name\": \"Monkey\"}[fn(x) { x }];", expected: "unusable as hash key: FUNCTION" },
            ErrorTest { input: "999[1]", expected: "index operator not supported: INTEGER" },
        ];

        for error_test in error_tests {
            match test_eval(error_test.input) {
                Some(e) => {
                    match e {
                        Object::Error(err) => assert_eq!(err.message, error_test.expected),
                        _ => assert!(false, "Object was not an Error."),
                    }
                },
                None => assert!(false, "Program could not be evaluated."),
            }
        }
    }

    #[test]
    fn test_let_statements() {
        struct LetTest<'a> {
            input: &'a str,
            expected: i64,
        };

        let let_tests = vec![
            LetTest { input: "let a = 5; a;", expected: 5 },
            LetTest { input: "let a = 5 * 5; a;", expected: 25 },
            LetTest { input: "let a = 5; let b = a; b;", expected: 5 },
            LetTest { input: "let a = 5; let b = a; let c = a + b + 5; c;", expected: 15 },
        ];

        for let_test in let_tests {
            match test_eval(let_test.input) {
                Some(e) => test_integer_object(&e, let_test.expected),
                None => assert!(false, "Program could not be evaluated."),
            }
        }
    }

    #[test]
    fn test_function_object() {
        let input = "fn(x) { x + 2; };";

        match test_eval(input) {
            Some(e) => {
                match e {
                    Object::Function(func) => {
                        assert_eq!(func.parameters.len(), 1);
                        assert_eq!(func.parameters[0].value, "x");
                        assert_eq!(format!("{}", func.body), "(x + 2)");
                    },
                    _ => assert!(false, "Object was not a Function."),
                }
            },
            None => assert!(false, "Program could not be evaluated."),
        }
    }

    #[test]
    fn test_function_application() {
        struct FunctionTest<'a> {
            input: &'a str,
            expected: i64,
        };

        let function_tests = vec![
            FunctionTest { input: "let identity = fn(x) { x; }; identity(5);", expected: 5 },
            FunctionTest { input: "let identity = fn(x) { return x; }; identity(5);", expected: 5 },
            FunctionTest { input: "let double = fn(x) { x * 2; }; double(5);", expected: 10 },
            FunctionTest { input: "let add = fn(x, y) { x + y; }; add(5, 5);", expected: 10 },
            FunctionTest { input: "let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", expected: 20 },
            FunctionTest { input: "fn(x) { x; }(5)", expected: 5 },
        ];

        for function_test in function_tests {
            match test_eval(function_test.input) {
                Some(e) => test_integer_object(&e, function_test.expected),
                None => assert!(false, "Program could not be evaluated."),
            }
        }
    }

    #[test]
    fn test_enclosing_environments() {
        let input = r#"
let first = 10;
let second = 10;
let third = 10;

let ourFunction = fn(first) {
  let second = 20;
  
  first + second + third;
};

ourFunction(20) + first + second;"#;

        match test_eval(input) {
            Some(e) => test_integer_object(&e, 70),
            None => assert!(false, "Program could not be evaluated."),
        }
    }

    #[test]
    fn test_closures() {
        let input = r#"
let newAdder = fn(x) {
  fn(y) { x + y };
};

let addTwo = newAdder(2);
addTwo(2);"#;

        match test_eval(input) {
            Some(e) => test_integer_object(&e, 4),
            None => assert!(false, "Program could not be evaluated."),
        }
    }

    #[test]
    fn test_string_literal() {
        let input = "\"Hello World!\"";

        match test_eval(input) {
            Some(e) => {
                match e {
                    Object::String(so) => assert_eq!(so.value, "Hello World!"),
                    _ => assert!(false, "Object was not a StringObject"),
                }
            },
            None => assert!(false, "Program could not be evaluated."),
        }
    }

    #[test]
    fn test_string_concatenation() {
        let input = "\"Hello\" + \" \" + \"World!\"";

        match test_eval(input) {
            Some(e) => {
                match e {
                    Object::String(so) => assert_eq!(so.value, "Hello World!"),
                    _ => assert!(false, "Object was not a StringObject"),
                }
            },
            None => assert!(false, "Program could not be evaluated."),
        }
    }

    #[test]
    fn test_builtin_functions() {
        struct BuiltinTest<'a> {
            input: &'a str,
            expected: Object,
        };

        let builtin_tests = vec![
            BuiltinTest { input: "len(\"\")", expected: Object::Integer(Integer::new(0)) },
            BuiltinTest { input: "len(\"four\")", expected: Object::Integer(Integer::new(4)) },
            BuiltinTest { input: "len(\"hello world\")", expected: Object::Integer(Integer::new(11)) },
            BuiltinTest { input: "len(1)", expected: Object::String(StringObject::new("argument to \"len\" not supported, got INTEGER")) },
            BuiltinTest { input: "len(\"one\", \"two\")", expected: Object::String(StringObject::new("wrong number of arguments. got 2, want 1")) },
            BuiltinTest { input: "len([1, 2, 3])", expected: Object::Integer(Integer::new(3)) },
            BuiltinTest { input: "len([])", expected: Object::Integer(Integer::new(0)) },
            BuiltinTest { input: "puts(\"hello\", \"world!\")", expected: Object::Null(Null::new()) },
            BuiltinTest { input: "first([1, 2, 3])", expected: Object::Integer(Integer::new(1)) },
            BuiltinTest { input: "first([])", expected: Object::Null(Null::new()) },
            BuiltinTest { input: "first(1)", expected: Object::String(StringObject::new("argument to \"first\" must be ARRAY, got INTEGER")) },
            BuiltinTest { input: "last([1, 2, 3])", expected: Object::Integer(Integer::new(3)) },
            BuiltinTest { input: "last([])", expected: Object::Null(Null::new()) },
            BuiltinTest { input: "last(1)", expected: Object::String(StringObject::new("argument to \"last\" must be ARRAY, got INTEGER")) },
            BuiltinTest { input: "rest([1, 2, 3])", expected: Object::Array(Array::new(vec![Object::Integer(Integer::new(2)), Object::Integer(Integer::new(3))])) },
            BuiltinTest { input: "rest([])", expected: Object::Null(Null::new()) },
            BuiltinTest { input: "push([], 1)", expected: Object::Array(Array::new(vec![Object::Integer(Integer::new(1))])) },
            BuiltinTest { input: "push(1, 1)", expected: Object::String(StringObject::new("argument to \"push\" must be ARRAY, got INTEGER")) },
        ];

        for builtin_test in builtin_tests {
            match test_eval(builtin_test.input) {
                Some(e) => {
                    match (&e, &builtin_test.expected) {
                        (Object::Integer(_), Object::Integer(exp)) => test_integer_object(&e, exp.value),
                        (Object::Null(_), Object::Null(_)) => test_null_object(&e),
                        (Object::Error(act), Object::String(exp)) => assert_eq!(act.message, exp.value),
                        (Object::Array(act), Object::Array(exp)) => {
                            assert_eq!(act.elements.len(), exp.elements.len());
                            for (i, elem) in exp.elements.iter().enumerate() {
                                match elem {
                                    Object::Integer(int) => test_integer_object(&act.elements[i], int.value),
                                    _ => assert!(false, "Array object was not an Integer."),
                                };
                            }
                        },
                        _ => assert!(false, "Object type was not recognized."),
                    }
                },
                None => assert!(false, "Program could not be evaluated."),
            }
        }
    }

    #[test]
    fn test_array_literals() {
        let input = "[1, 2 * 2, 3 + 3]";

        match test_eval(input) {
            Some(e) => {
                match e {
                    Object::Array(a) => {
                        assert_eq!(a.elements.len(), 3);
                        test_integer_object(&a.elements[0], 1);
                        test_integer_object(&a.elements[1], 4);
                        test_integer_object(&a.elements[2], 6);
                    },
                    _ => assert!(false, "Object was not an Array"),
                }
            },
            None => assert!(false, "Program could not be evaluated."),
        }
    }

    #[test]
    fn test_array_index_expressions() {
        struct IndexTest<'a> {
            input: &'a str,
            expected: Object,
        };

        let index_tests = vec![
            IndexTest { input: "[1, 2, 3][0]", expected: Object::Integer(Integer::new(1)) },
            IndexTest { input: "[1, 2, 3][1]", expected: Object::Integer(Integer::new(2)) },
            IndexTest { input: "[1, 2, 3][2]", expected: Object::Integer(Integer::new(3)) },
            IndexTest { input: "let i = 0; [1][i];", expected: Object::Integer(Integer::new(1)) },
            IndexTest { input: "[1, 2, 3][1 + 1]", expected: Object::Integer(Integer::new(3)) },
            IndexTest { input: "let myArray = [1, 2, 3]; myArray[2];", expected: Object::Integer(Integer::new(3)) },
            IndexTest { input: "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];", expected: Object::Integer(Integer::new(6)) },
            IndexTest { input: "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]", expected: Object::Integer(Integer::new(2)) },
            IndexTest { input: "[1, 2, 3][3]", expected: Object::Null(Null::new()) },
            IndexTest { input: "[1, 2, 3][-1]", expected: Object::Null(Null::new()) },
        ];

        for index_test in index_tests {
            match test_eval(index_test.input) {
                Some(e) => {
                    match (&e, &index_test.expected) {
                        (Object::Integer(_), Object::Integer(exp)) => test_integer_object(&e, exp.value),
                        (Object::Null(_), Object::Null(_)) => test_null_object(&e),
                        _ => assert!(false, "Object type was not recognized."),
                    }
                },
                None => assert!(false, "Program could not be evaluated."),
            }
        }
    }

    #[test]
    fn test_hash_literals() {
        let input = r#"let two = "two";
{
    "one": 10 - 9,
    two: 1 + 1,
    "thr" + "ee": 6 / 2,
    4: 4,
    true: 5,
    false: 6
}"#;

        let mut expected: HashMap<Object, i64> = HashMap::new();
        expected.insert(Object::String(StringObject::new("one")), 1);
        expected.insert(Object::String(StringObject::new("two")), 2);
        expected.insert(Object::String(StringObject::new("three")), 3);
        expected.insert(Object::Integer(Integer::new(4)), 4);
        expected.insert(Object::Boolean(Boolean::new(true)), 5);
        expected.insert(Object::Boolean(Boolean::new(false)), 6);
        match test_eval(input) {
            Some(e) => {
                match e {
                    Object::Hash(h) => {
                        assert_eq!(h.pairs.len(), expected.len());
                        for (expected_key, expected_value) in expected {
                            match h.pairs.get(&expected_key) {
                                Some(p) => test_integer_object(&p, expected_value),
                                None => assert!(false, "Key not found in HashObject pairs."),
                            }
                        }
                    },
                    _ => assert!(false, "Object was not a Hash."),
                }
            },
            None => assert!(false, "Program could not be evaluated."),
        }
    }

    #[test]
    fn test_hash_index_expressions() {
        struct HashIndexTest<'a> {
            input: &'a str,
            expected: Object,
        };

        let hash_index_tests = vec![
            HashIndexTest { input: "{\"foo\": 5}[\"foo\"]", expected: Object::Integer(Integer::new(5)) },
            HashIndexTest { input: "{\"foo\": 5}[\"bar\"]", expected: Object::Null(Null::new()) },
            HashIndexTest { input: "let key = \"foo\"; {\"foo\": 5}[key]", expected: Object::Integer(Integer::new(5)) },
            HashIndexTest { input: "{}[\"foo\"]", expected: Object::Null(Null::new()) },
            HashIndexTest { input: "{5: 5}[5]", expected: Object::Integer(Integer::new(5)) },
            HashIndexTest { input: "{true: 5}[true]", expected: Object::Integer(Integer::new(5)) },
            HashIndexTest { input: "{false: 5}[false]", expected: Object::Integer(Integer::new(5)) },
        ];

        for hash_index_test in hash_index_tests {
            match test_eval(hash_index_test.input) {
                Some(e) => {
                    match (&e, &hash_index_test.expected) {
                        (Object::Integer(_), Object::Integer(exp)) => test_integer_object(&e, exp.value),
                        (Object::Null(_), Object::Null(_)) => test_null_object(&e),
                        _ => assert!(false, "Object type was not recognized."),
                    }
                },
                None => assert!(false, "Program could not be evaluated."),
            }
        }
    }

    fn test_eval(input: &str) -> Option<Object> {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        let environment = Rc::new(RefCell::new(Environment::new(true)));

        match program {
            Some(p) => Some(eval(&Node::Program(p), environment)),
            None => {
                assert!(false, "Program could not be parsed.");
                None
            },
        }
    }

    fn test_integer_object(object: &Object, expected: i64) {
        match object {
            Object::Integer(i) => assert_eq!(i.value, expected),
            _ => assert!(false, "Object was not an Integer."),
        }
    }

    fn test_boolean_object(object: &Object, expected: bool) {
        match object {
            Object::Boolean(i) => assert_eq!(i.value, expected),
            _ => assert!(false, "Object was not a Boolean."),
        }
    }

    fn test_null_object(object: &Object) {
        if let Object::Null(_) = object {
            return;
        }

        assert!(false, "Object was not Null.");
    }
}