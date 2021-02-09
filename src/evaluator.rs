use super::ast::*;
use super::object::*;

pub fn eval(node: Box<&dyn Node>) -> Option<Box<dyn Object>> {
    // Statements
    match node.type_of() {
        // Statements
        "Program" => {
            match node.as_any().downcast_ref::<Program>() {
                Some(p) => eval_program(&p),
                None => return None,
            }
        },
        "ExpressionStatement" => {
            match node.as_any().downcast_ref::<ExpressionStatement>() {
                Some(es) => {
                    match &es.expression {
                        Some(exp) => eval(Box::new(exp.as_base())),
                        None => return None,
                    }
                },
                None => return None,
            }
        },
        "BlockStatement" => {
            match node.as_any().downcast_ref::<BlockStatement>() {
                Some(bs) => eval_block_statement(&bs),
                None => return None,
            }
        },
        "ReturnStatement" => {
            match node.as_any().downcast_ref::<ReturnStatement>() {
                Some(rs) => {
                    match &rs.return_value {
                        Some(r_val) => {
                            let val = eval(Box::new(r_val.as_base()));
                            match val {
                                Some(v) => Some(Box::new(ReturnValue::new(v))),
                                None => None,
                            }
                        },
                        None => return None,
                    }
                },
                None => return None,
            }
        },
        // Expressions
        "IntegerLiteral" => {
            match node.as_any().downcast_ref::<IntegerLiteral>() {
                Some(il) => Some(Box::new(Integer::new(il.value))),
                None => return None,
            }
        },
        "Boolean" => {
            match node.as_any().downcast_ref::<BooleanLiteral>() {
                Some(b) => Some(Box::new(super::object::Boolean::new(b.value))),
                None => return None,
            }
        },
        "PrefixExpression" => {
            match node.as_any().downcast_ref::<PrefixExpression>() {
                Some(pe) => {
                    let right = eval(Box::new(pe.right.as_base()));
                    match right {
                        Some(r) => eval_prefix_expression(&pe.operator, r),
                        None => return None,
                    }
                },
                None => return None,
            }
        },
        "InfixExpression" => {
            match node.as_any().downcast_ref::<InfixExpression>() {
                Some(ie) => {
                    let left = eval(Box::new(ie.left.as_base()));
                    let right = eval(Box::new(ie.right.as_base()));
                    match (left, right) {
                        (Some(l), Some(r)) => eval_infix_expression(&ie.operator, l, r),
                        _ => return None,
                    }
                },
                None => return None,
            }
        },
        "IfExpression" => {
            match node.as_any().downcast_ref::<IfExpression>() {
                Some(ie) => eval_if_expression(&ie),
                None => return None,
            }
        },
        _ => None,
    }
}

fn eval_program(program: &Program) -> Option<Box<dyn Object>> {
    let mut result = None;

    for statement in &program.statements {
        result = eval(Box::new(statement.as_base()));

        match &result {
            Some(r) => {
                match r.as_any().downcast_ref::<ReturnValue>() {
                    Some(rv) => return Some(rv.value.clone()),
                    None => (),
                };
            },
            None => (),
        };
    }

    result
}

fn eval_block_statement(block: &BlockStatement) -> Option<Box<dyn Object>> {
    let mut result = None;

    for statement in &block.statements {
        result = eval(Box::new(statement.as_base()));

        match &result {
            Some(r) => {
                match r.type_of().as_str() {
                    RETURN_VALUE_OBJ => return result,
                    _ => (),
                };
            },
            None => (),
        };
    }

    result
}

fn eval_prefix_expression(operator: &String, right: Box<dyn Object>) -> Option<Box<dyn Object>> {
    match operator.as_str() {
        "!" => Some(eval_bang_operator_expression(right)),
        "-" => Some(eval_minus_operator_expression(right)),
        _ => Some(Box::new(Null::new())),
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
        None => Box::new(Null::new()),
    }
}

fn eval_infix_expression(operator: &String, left: Box<dyn Object>, right: Box<dyn Object>) -> Option<Box<dyn Object>> {
    match (left.type_of().as_str(), right.type_of().as_str()) {
        (INTEGER_OBJ, INTEGER_OBJ) => Some(eval_integer_infix_expression(operator, left, right)),
        (BOOLEAN_OBJ, BOOLEAN_OBJ) => Some(eval_boolean_infix_expression(operator, left, right)),
        _ => Some(Box::new(Null::new())),
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
                _ => Box::new(Null::new()),
            }
        },
        _ => Box::new(Null::new()),
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
                _ => Box::new(Null::new()),
            }
        },
        _ => Box::new(Null::new()),
    }
}

fn eval_if_expression(if_expression: &IfExpression) -> Option<Box<dyn Object>> {
    let condition = eval(Box::new(if_expression.condition.as_base())); 

    match condition {
        Some(con) => {
            if is_truthy(con) {
                return eval(Box::new(if_expression.consequence.as_base()));
            }

            match &if_expression.alternative {
                Some(alt) => return eval(Box::new(alt.as_base())),
                None => (),
            }

            Some(Box::new(Null::new()))
        },
        None => Some(Box::new(Null::new())),
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

    fn get_eval(input: &String) -> Option<Box<dyn Object>> {
        let lexer = Lexer::new(input.clone());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        match &program {
            Some(p) => eval(Box::new(p)),
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