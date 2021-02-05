use super::ast::*;
use super::object::*;
use super::parser::Parser;

pub fn eval(node: Box<&dyn Node>) -> Option<Box<dyn Object>> {
    // Statements
    match node.type_of() {
        // Statements
        "Program" => {
            match node.as_any().downcast_ref::<Program>() {
                Some(p) => eval_statements(&p.statements),
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
        // Expressions
        "IntegerLiteral" => {
            match node.as_any().downcast_ref::<IntegerLiteral>() {
                Some(il) => Some(Box::new(Integer::new(il.value))),
                None => return None,
            }
        },
        _ => None,
    }
}

fn eval_statements(statements: &Vec<Box<dyn Statement>>) -> Option<Box<dyn Object>> {
    let mut result = None;
    for statement in statements {
        result = eval(Box::new(statement.as_base()));
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use super::super::lexer::Lexer;

    #[test]
    fn verify_integer_expressions_are_evaluated() {
        struct IntegerTest {
            input: String,
            expected: i64,
        };

        let integer_tests = vec![
            IntegerTest { input: String::from("5"), expected: 5 },
            IntegerTest { input: String::from("10"), expected: 10 },
        ];

        for integer_test in integer_tests {
            let evaluated = get_eval(&integer_test.input);
            match evaluated {
                Some(eval) => verify_integer_object(&eval, integer_test.expected),
                None => assert!(false, "Integer expression could not be evaluated."),
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
}