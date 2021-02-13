use super::ast::*;
use super::environment::Environment;
use super::object::*;
use std::cell::RefCell;
use std::rc::Rc;

pub fn eval(node: Box<&dyn Node>, env: Rc<RefCell<Environment>>) -> Box<dyn Object> {
    match node.type_of() {
        // Statements
        "Program" => {
            match node.as_any().downcast_ref::<Program>() {
                Some(p) => eval_program(&p, env),
                None => new_error(format!("type error: expected {}", node.type_of())),
            }
        },
        "ExpressionStatement" => {
            match node.as_any().downcast_ref::<ExpressionStatement>() {
                Some(es) => {
                    match &es.expression {
                        Some(exp) => eval(Box::new(exp.as_base()), env),
                        None => new_error(String::from("expression error: expression not found")),
                    }
                },
                None => new_error(format!("type error: expected {}", node.type_of())),
            }
        },
        "BlockStatement" => {
            match node.as_any().downcast_ref::<BlockStatement>() {
                Some(bs) => eval_block_statement(&bs, env),
                None => new_error(format!("type error: expected {}", node.type_of())),
            }
        },
        "ReturnStatement" => {
            match node.as_any().downcast_ref::<ReturnStatement>() {
                Some(rs) => {
                    match &rs.return_value {
                        Some(r_val) => {
                            let val = eval(Box::new(r_val.as_base()), env);
                            if is_error(&val) {
                                return val;
                            }
                            Box::new(ReturnValue::new(val))
                        },
                        None => Box::new(Null::new()),
                    }
                },
                None => new_error(format!("type error: expected {}", node.type_of())),
            }
        },
        "LetStatement" => {
            match node.as_any().downcast_ref::<LetStatement>() {
                Some(ls) => {
                    match &ls.value {
                        Some(v) => {
                            let val = eval(Box::new(v.clone().as_base()), Rc::clone(&env));
                            if is_error(&val) {
                                return val;
                            }
                            env.borrow_mut().set(ls.name.value.clone(), val)
                        },
                        None => Box::new(Null::new()),
                    }
                },
                None => new_error(format!("type error: expected {}", node.type_of())),
            }
        },
        // Expressions
        "IntegerLiteral" => {
            match node.as_any().downcast_ref::<IntegerLiteral>() {
                Some(il) => Box::new(Integer::new(il.value)),
                None => new_error(format!("type error: expected {}", node.type_of())),
            }
        },
        "Boolean" => {
            match node.as_any().downcast_ref::<BooleanLiteral>() {
                Some(b) => Box::new(super::object::Boolean::new(b.value)),
                None => new_error(format!("type error: expected {}", node.type_of())),
            }
        },
        "PrefixExpression" => {
            match node.as_any().downcast_ref::<PrefixExpression>() {
                Some(pe) => {
                    let right = eval(Box::new(pe.right.as_base()), env);
                    if is_error(&right) {
                        return right;
                    }
                    eval_prefix_expression(&pe.operator, right)
                },
                None => new_error(format!("type error: expected {}", node.type_of())),
            }
        },
        "InfixExpression" => {
            match node.as_any().downcast_ref::<InfixExpression>() {
                Some(ie) => {
                    let left = eval(Box::new(ie.left.as_base()), Rc::clone(&env));
                    if is_error(&left) {
                        return left;
                    }
                    let right = eval(Box::new(ie.right.as_base()), Rc::clone(&env));
                    if is_error(&right) {
                        return right;
                    }
                    eval_infix_expression(&ie.operator, left, right)
                },
                None => new_error(format!("type error: expected {}", node.type_of())),
            }
        },
        "IfExpression" => {
            match node.as_any().downcast_ref::<IfExpression>() {
                Some(ie) => eval_if_expression(&ie, env),
                None => new_error(format!("type error: expected {}", node.type_of())),
            }
        },
        "Identifier" => {
            match node.as_any().downcast_ref::<Identifier>() {
                Some(i) => eval_identifier(&i, env),
                None => new_error(format!("type error: expected {}", node.type_of())),
            }
        },
        "FunctionLiteral" => {
            match node.as_any().downcast_ref::<FunctionLiteral>() {
                Some(fl) => Box::new(Function::new(fl.parameters.clone(), fl.body.clone(), Rc::clone(&env))),
                None => new_error(format!("type error: expected {}", node.type_of())),
            }
        },
        "CallExpression" => {
            match node.as_any().downcast_ref::<CallExpression>() {
                Some(ce) => {
                    let function = eval(Box::new(ce.function.as_base()), Rc::clone(&env));
                    if is_error(&function) {
                        return function;
                    }
                    
                    let args = eval_expressions(&ce.arguments, Rc::clone(&env));
                    if args.len() == 1 && is_error(&args[0]) {
                        return args[0].clone();
                    }

                    apply_function(function, args)
                },
                None => new_error(format!("type error: expected {}", node.type_of())),
            }
        },
        _ => new_error(String::from("type error: type not found")),
    }
}

fn eval_program(program: &Program, env: Rc<RefCell<Environment>>) -> Box<dyn Object> {
    let mut result: Box<dyn Object> = Box::new(Null::new());

    for statement in &program.statements {
        result = eval(Box::new(statement.as_base()), Rc::clone(&env));

        match result.as_any().downcast_ref::<ReturnValue>() {
            Some(res) => return res.value.clone(),
            None => (),
        }

        match result.as_any().downcast_ref::<Error>() {
            Some(_) => return result,
            None => (),
        }
    }

    result
}

fn eval_block_statement(block: &BlockStatement, env: Rc<RefCell<Environment>>) -> Box<dyn Object> {
    let mut result: Box<dyn Object> = Box::new(Null::new());

    for statement in &block.statements {
        result = eval(Box::new(statement.as_base()), Rc::clone(&env));

        match result.type_of().as_str() {
            RETURN_VALUE_OBJ | ERROR_OBJ => return result,
            _ => (),
        };
    }

    result
}

fn eval_prefix_expression(operator: &String, right: Box<dyn Object>) -> Box<dyn Object> {
    match operator.as_str() {
        "!" => eval_bang_operator_expression(right),
        "-" => eval_minus_operator_expression(right),
        _ => new_error(format!("unknown operator: {}{}", operator, right.type_of())),
    }
}

fn eval_bang_operator_expression(right: Box<dyn Object>) -> Box<dyn Object> {
    match right.type_of().as_str() {
        BOOLEAN_OBJ => {
            match right.as_any().downcast_ref::<Boolean>() {
                Some(r) => {
                    if r.value { 
                        Box::new(Boolean::new(false))
                    } else {
                        Box::new(Boolean::new(true))
                    }
                },
                None => Box::new(Boolean::new(false)),
            }
        },
        NULL_OBJ => Box::new(Boolean::new(true)),
        _ => Box::new(Boolean::new(false)),
    }
}

fn eval_minus_operator_expression(right: Box<dyn Object>) -> Box<dyn Object> {
    match right.as_any().downcast_ref::<Integer>() {
        Some(r) => {
            let value = r.value;
            Box::new(Integer::new(-value))
        },
        None => new_error(format!("unknown operator: -{}", right.type_of())),
    }
}

fn eval_infix_expression(operator: &String, left: Box<dyn Object>, right: Box<dyn Object>) -> Box<dyn Object> {
    match (left.type_of().as_str(), right.type_of().as_str()) {
        (INTEGER_OBJ, INTEGER_OBJ) => eval_integer_infix_expression(operator, left, right),
        (BOOLEAN_OBJ, BOOLEAN_OBJ) => eval_boolean_infix_expression(operator, left, right),
        _ => {
            if left.type_of() != right.type_of() {
                return new_error(format!("type mismatch: {} {} {}", left.type_of(), operator, right.type_of()));
            }
            new_error(format!("unknown operator: {} {} {}", left.type_of(), operator, right.type_of()))
        },
    }
}

fn eval_integer_infix_expression(operator: &String, left: Box<dyn Object>, right: Box<dyn Object>) -> Box<dyn Object> {
    match (left.as_any().downcast_ref::<Integer>(), right.as_any().downcast_ref::<Integer>()) {
        (Some(l), Some(r)) => {
            let left_value = l.value;
            let right_value = r.value;
            match operator.as_str() {
                "+" => Box::new(Integer::new(left_value + right_value)),
                "-" => Box::new(Integer::new(left_value - right_value)),
                "*" => Box::new(Integer::new(left_value * right_value)),
                "/" => Box::new(Integer::new(left_value / right_value)),
                "<" => Box::new(Boolean::new(left_value < right_value)),
                ">" => Box::new(Boolean::new(left_value > right_value)),
                "==" => Box::new(Boolean::new(left_value == right_value)),
                "!=" => Box::new(Boolean::new(left_value != right_value)),
                _ => new_error(format!("unknown operator: {} {} {}", left.type_of(), operator, right.type_of())),
            }
        },
        _ => new_error(format!("unknown operator: {} {} {}", left.type_of(), operator, right.type_of()))
    }
}

fn eval_boolean_infix_expression(operator: &String, left: Box<dyn Object>, right: Box<dyn Object>) -> Box<dyn Object> {
    match (left.as_any().downcast_ref::<Boolean>(), right.as_any().downcast_ref::<Boolean>()) {
        (Some(l), Some(r)) => {
            let left_value = l.value;
            let right_value = r.value;
            match operator.as_str() {
                "==" => Box::new(Boolean::new(left_value == right_value)),
                "!=" => Box::new(Boolean::new(left_value != right_value)),
                _ => new_error(format!("unknown operator: {} {} {}", left.type_of(), operator, right.type_of())),
            }
        },
        _ => Box::new(Null::new()),
    }
}

fn eval_if_expression(if_expression: &IfExpression, env: Rc<RefCell<Environment>>) -> Box<dyn Object> {
    let condition = eval(Box::new(if_expression.condition.as_base()), Rc::clone(&env)); 
    if is_error(&condition) {
        return condition;
    }

    if is_truthy(condition) {
        return eval(Box::new(if_expression.consequence.as_base()), Rc::clone(&env));
    }

    match &if_expression.alternative {
        Some(alt) => return eval(Box::new(alt.as_base()), env),
        None => (),
    }

    Box::new(Null::new())
}

fn eval_identifier(identifier: &Identifier, env: Rc<RefCell<Environment>>) -> Box<dyn Object> {
    let val = env.borrow().get(&identifier.value);
    match val {
        Some(v) => v,
        None => new_error(format!("identifier not found: {}", identifier.value.clone())),
    }
}

fn eval_expressions(exps: &Vec<Box<dyn Expression>>, env: Rc<RefCell<Environment>>) -> Vec<Box<dyn Object>> {
    let mut result = vec![];

    for exp in exps {
        let evaluated = eval(Box::new(exp.as_base()), Rc::clone(&env));
        if is_error(&evaluated) {
            return vec![evaluated];
        }

        result.push(evaluated);
    }

    result
}

fn apply_function(fun: Box<dyn Object>, args: Vec<Box<dyn Object>>) -> Box<dyn Object> {
    match fun.as_any().downcast_ref::<Function>() {
        Some(f) => {
            let extended_env = extend_function_env(&f, args);
            let evaluated = eval(Box::new(f.body.as_base()), Rc::new(RefCell::new(extended_env)));

            unwrap_return_value(evaluated)
        },
        None => new_error(format!("not a function: {}", fun.type_of())),
    }
}

fn extend_function_env(fun: &Function, args: Vec<Box<dyn Object>>) -> Environment {
    let mut env = Environment::new_enclosed(Rc::clone(&fun.env));

    for (i, param) in fun.parameters.iter().enumerate() {
        env.set(param.value(), args[i].clone());
    }

    env
}

fn unwrap_return_value(obj: Box<dyn Object>) -> Box<dyn Object> {
    match obj.as_any().downcast_ref::<ReturnValue>() {
        Some(rv) => rv.value.clone(),
        None => obj,
    }
}

fn is_truthy(obj: Box<dyn Object>) -> bool {
    match obj.type_of().as_str() {
        NULL_OBJ => false,
        BOOLEAN_OBJ => {
            match obj.as_any().downcast_ref::<Boolean>() {
                Some(o) => o.value,
                None => false,
            }
        },
        _ => true,
    }
}

fn is_error(obj: &Box<dyn Object>) -> bool {
    match obj.as_any().downcast_ref::<Error>() {
        Some(_) => true,
        None => false,
    }
}

fn new_error(message: String) -> Box<dyn Object> {
    Box::new(Error::new(message))
}

#[cfg(test)]
mod tests {
    use super::*;
    use super::super::lexer::Lexer;
    use super::super::parser::Parser;

    #[test]
    fn verify_integer_expressions_are_evaluated() {
        struct IntegerTest {
            input: String,
            expected: i64,
        };

        let integer_tests = vec![
            IntegerTest { input: String::from("5"), expected: 5 },
            IntegerTest { input: String::from("10"), expected: 10 },
            IntegerTest { input: String::from("-5"), expected: -5 },
            IntegerTest { input: String::from("-10"), expected: -10 },
            IntegerTest { input: String::from("5 + 5 + 5 + 5 - 10"), expected: 10 },
            IntegerTest { input: String::from("2 * 2 * 2 * 2 * 2"), expected: 32 },
            IntegerTest { input: String::from("-50 + 100 + -50"), expected: 0 },
            IntegerTest { input: String::from("5 * 2 + 10"), expected: 20 },
            IntegerTest { input: String::from("5 + 2 * 10"), expected: 25 },
            IntegerTest { input: String::from("20 + 2 * -10"), expected: 0 },
            IntegerTest { input: String::from("50 / 2 * 2 + 10"), expected: 60 },
            IntegerTest { input: String::from("2 * (5 + 10)"), expected: 30 },
            IntegerTest { input: String::from("3 * 3 * 3 + 10"), expected: 37 },
            IntegerTest { input: String::from("3 * (3 * 3) + 10"), expected: 37 },
            IntegerTest { input: String::from("(5 + 10 * 2 + 15 / 3) * 2 + -10"), expected: 50 },
        ];

        for integer_test in integer_tests {
            let evaluated = get_eval(&integer_test.input);
            match evaluated {
                Some(eval) => verify_integer_object(&eval, integer_test.expected),
                None => assert!(false, "Integer expression could not be evaluated."),
            }
        }
    }

    #[test]
    fn verify_boolean_expressions_are_evaluated() {
        struct BooleanTest {
            input: String,
            expected: bool,
        };

        let boolean_tests = vec![
            BooleanTest { input: String::from("true"), expected: true },
            BooleanTest { input: String::from("false"), expected: false },
            BooleanTest { input: String::from("1 < 2"), expected: true },
            BooleanTest { input: String::from("1 > 2"), expected: false },
            BooleanTest { input: String::from("1 < 1"), expected: false },
            BooleanTest { input: String::from("1 > 1"), expected: false },
            BooleanTest { input: String::from("1 == 1"), expected: true },
            BooleanTest { input: String::from("1 != 1"), expected: false },
            BooleanTest { input: String::from("1 == 2"), expected: false },
            BooleanTest { input: String::from("1 != 2"), expected: true },
            BooleanTest { input: String::from("true == true"), expected: true },
            BooleanTest { input: String::from("false == false"), expected: true },
            BooleanTest { input: String::from("true == false"), expected: false },
            BooleanTest { input: String::from("true != false"), expected: true },
            BooleanTest { input: String::from("false != true"), expected: true },
            BooleanTest { input: String::from("(1 < 2) == true"), expected: true },
            BooleanTest { input: String::from("(1 < 2) == false"), expected: false },
            BooleanTest { input: String::from("(1 > 2) == true"), expected: false },
            BooleanTest { input: String::from("(1 > 2) == false"), expected: true },
        ];

        for boolean_test in boolean_tests {
            let evaluated = get_eval(&boolean_test.input);
            match evaluated {
                Some(eval) => verify_boolean_object(&eval, boolean_test.expected),
                None => assert!(false, "Boolean expression could not be evaluated."),
            }
        }
    }

    #[test]
    fn verify_bang_operators_are_evaluated() {
        struct BangTest {
            input: String,
            expected: bool,
        };

        let bang_tests = vec![
            BangTest { input: String::from("!true"), expected: false },
            BangTest { input: String::from("!false"), expected: true },
            BangTest { input: String::from("!5"), expected: false },
            BangTest { input: String::from("!!true"), expected: true },
            BangTest { input: String::from("!!false"), expected: false },
            BangTest { input: String::from("!!5"), expected: true },
        ];

        for bang_test in bang_tests {
            let evaluated = get_eval(&bang_test.input);
            match evaluated {
                Some(eval) => verify_boolean_object(&eval, bang_test.expected),
                None => assert!(false, "Boolean expression could not be evaluated."),
            }
        }
    }

    #[test]
    fn verify_if_else_expressions_are_evaluated() {
        struct IfElseTest {
            input: String,
            expected: Option<i64>,
        };

        let if_else_tests = vec![
            IfElseTest { input: String::from("if (true) { 10 }"), expected: Some(10) },
            IfElseTest { input: String::from("if (false) { 10 }"), expected: None },
            IfElseTest { input: String::from("if (1) { 10 }"), expected: Some(10) },
            IfElseTest { input: String::from("if (1 < 2) { 10 }"), expected: Some(10) },
            IfElseTest { input: String::from("if (1 > 2) { 10 }"), expected: None },
            IfElseTest { input: String::from("if (1 > 2) { 10 } else { 20 }"), expected: Some(20) },
            IfElseTest { input: String::from("if (1 < 2) { 10 } else { 20 }"), expected: Some(10) },
        ];

        for if_else_test in if_else_tests {
            let evaluated = get_eval(&if_else_test.input);
            match evaluated {
                Some(eval) => {
                    match if_else_test.expected {
                        Some(e) => verify_integer_object(&eval, e),
                        None => verify_null_object(&eval),
                    };
                },
                None => assert!(false, "If-Else expression could not be evaluated."),
            }
        }
    }

    #[test]
    fn verify_return_statements_are_evaluated() {
        struct ReturnTest {
            input: String,
            expected: i64,
        };

        let block_return = String::from(r#"if (10 > 1) {
  if (10 > 1) {
    return 10;
  }
  
  return 1;
}"#);
        let return_tests = vec![
            ReturnTest { input: String::from("return 10;"), expected: 10 },
            ReturnTest { input: String::from("return 10; 9;"), expected: 10 },
            ReturnTest { input: String::from("return 2 * 5; 9;"), expected: 10 },
            ReturnTest { input: String::from("9; return 2 * 5; 9;"), expected: 10 },
            ReturnTest { input: block_return, expected: 10 },
        ];

        for return_test in return_tests {
            let evaluated = get_eval(&return_test.input);
            match evaluated {
                Some(eval) => verify_integer_object(&eval, return_test.expected),
                None => assert!(false, "Return statement could not be evaluated."),
            }
        }
    }

    #[test]
    fn verify_errors_are_handled() {
        struct ErrorTest {
            input: String,
            expected_message: String,
        };

        let block_error = String::from(r#"if (10 > 1) {
  if (10 > 1) {
    return true + false;
  }
  
  return 1;
}"#);
        let error_tests = vec![
            ErrorTest { input: String::from("5 + true;"), expected_message: String::from("type mismatch: INTEGER + BOOLEAN") },
            ErrorTest { input: String::from("5 + true; 5;"), expected_message: String::from("type mismatch: INTEGER + BOOLEAN") },
            ErrorTest { input: String::from("-true"), expected_message: String::from("unknown operator: -BOOLEAN") },
            ErrorTest { input: String::from("true + false;"), expected_message: String::from("unknown operator: BOOLEAN + BOOLEAN") },
            ErrorTest { input: String::from("5; true + false; 5"), expected_message: String::from("unknown operator: BOOLEAN + BOOLEAN") },
            ErrorTest { input: String::from("if (10 > 1) { true + false; }"), expected_message: String::from("unknown operator: BOOLEAN + BOOLEAN") },
            ErrorTest { input: block_error, expected_message: String::from("unknown operator: BOOLEAN + BOOLEAN") },
            ErrorTest { input: String::from("foobar"), expected_message: String::from("identifier not found: foobar") },
        ];

        for error_test in error_tests {
            let evaluated = get_eval(&error_test.input);
            match evaluated {
                Some(eval) => {
                    match eval.as_any().downcast_ref::<Error>() {
                        Some(e) => assert_eq!(e.message, error_test.expected_message),
                        None => assert!(false, "Error object was not returned."),
                    }
                },
                None => assert!(false, "Error object was not returned."),
            }
        }
    }

    #[test]
    fn verify_let_statements_are_evaluated() {
        struct LetTest {
            input: String,
            expected: i64,
        };

        let let_tests = vec![
            LetTest { input: String::from("let a = 5; a;"), expected: 5 },
            LetTest { input: String::from("let a = 5 * 5; a;"), expected: 25 },
            LetTest { input: String::from("let a = 5; let b = a; b;"), expected: 5 },
            LetTest { input: String::from("let a = 5; let b = a; let c = a + b + 5; c;"), expected: 15 },
        ];

        for let_test in let_tests {
            let evaluated = get_eval(&let_test.input);
            match evaluated {
                Some(eval) => verify_integer_object(&eval, let_test.expected),
                None => assert!(false, "Integer expression could not be evaluated."),
            }
        }
    }

    #[test]
    fn verify_function_objects_are_evaluated() {
        let input = String::from("fn(x) { x + 2; };");
        let expected_body = String::from("(x + 2)");

        let evaluated = get_eval(&input);
        match evaluated {
            Some(eval) => {
                match eval.as_any().downcast_ref::<Function>() {
                    Some(e) => {
                        assert_eq!(e.parameters.len(), 1);
                        assert_eq!(e.parameters[0].to_string(), String::from("x"));                            
                        assert_eq!(e.body.to_string(), expected_body);
                    },
                    None => assert!(false, "Object is not a function."),
                }
            },
            None => assert!(false, "Function object could not be evaluated."),
        }
    }

    #[test]
    fn verify_functions_are_applied() {
        struct FunctionTest {
            input: String,
            expected: i64,
        };

        let function_tests = vec![
            FunctionTest { input: String::from("let identity = fn(x) { x; }; identity(5);"), expected: 5 },
            FunctionTest { input: String::from("let identity = fn(x) { return x; }; identity(5);"), expected: 5 },
            FunctionTest { input: String::from("let double = fn(x) { x * 2; }; double(5);"), expected: 10 },
            FunctionTest { input: String::from("let add = fn(x, y) { x + y; }; add(5, 5);"), expected: 10 },
            FunctionTest { input: String::from("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));"), expected: 20 },
            FunctionTest { input: String::from("fn(x) { x; }(5)"), expected: 5 },
        ];

        for function_test in function_tests {
            let evaluated = get_eval(&function_test.input);
            match evaluated {
                Some(eval) => verify_integer_object(&eval, function_test.expected),
                None => assert!(false, "Integer expression could not be evaluated."),
            }
        }
    }

    #[test]
    fn verify_closures_are_evaluated() {
        let input = String::from(r#"
let newAdder = fn(x) {
  fn(y) { x + y };
};

let addTwo = newAdder(2);
addTwo(2);"#);

        let evaluated = get_eval(&input);
        match evaluated {
            Some(eval) => verify_integer_object(&eval, 4),
            None => assert!(false, "Function object could not be evaluated."),
        }
    }

    fn get_eval(input: &String) -> Option<Box<dyn Object>> {
        let lexer = Lexer::new(input.clone());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        let env = Rc::new(RefCell::new(Environment::new()));

        match &program {
            Some(p) => Some(eval(Box::new(p), env)),
            None => {
                assert!(false, "parse_program returned None.");
                None
            }
        }
    }

    fn verify_integer_object(object: &Box<dyn Object>, expected: i64) {
        match object.as_any().downcast_ref::<Integer>() {
            Some(o) => assert_eq!(o.value, expected),
            None => assert!(false, "Object is not an Integer"),
        };
    }

    fn verify_boolean_object(object: &Box<dyn Object>, expected: bool) {
        match object.as_any().downcast_ref::<Boolean>() {
            Some(o) => assert_eq!(o.value, expected),
            None => assert!(false, "Object is not a Boolean"),
        };
    }

    fn verify_null_object(object: &Box<dyn Object>) {
        match object.as_any().downcast_ref::<Null>() {
            Some(_) => (),
            None => assert!(false, "Object is not a Boolean"),
        };
    }
}