use super::ast::*;
use super::object::*;
use super::parser::Parser;

//pub fn eval(node: Box<dyn Node>) -> Option<Box<dyn Object>> {
//    // Statements
//    match node.type_of() {
//        "Program" => {
//            match node.as_any().downcast_ref::<Program>() {
//                Some(p) => eval_statements(p.statements),
//                None => (),
//            };
//        },
//        "ExpressionStatement" => {
//            match node.as_any().downcast_ref::<ExpressionStatement>() {
//                Some(es) => eval(Box::new(es.expression.unwrap().as_any())),
//                None => (),
//            };
//        }
//    }
//
//    // Expressions
//
//    None
//}
//
//#[cfg(test)]
//mod tests {
//    use super::*;
//    use super::super::lexer::Lexer;
//
//    #[test]
//    fn verify_integer_expressions_are_evaluated() {
//        struct IntegerTest {
//            input: String,
//            expected: i64,
//        };
//
//        let integer_tests = vec![
//            IntegerTest { input: String::from("5"), expected: 5 },
//            IntegerTest { input: String::from("10"), expected: 10 },
//        ];
//
//        for integer_test in integer_tests {
//            let evaluated = get_eval(&integer_test.input);
//            verify_integer_object(&evaluated, integer_test.expected);
//        }
//    }
//
//    fn get_eval(input: &String) -> Box<dyn Object> {
//        let lexer = Lexer::new(input.clone());
//        let mut parser = Parser::new(lexer);
//        let program = parser.parse_program();
//
//        eval(Box::new(program.unwrap()))
//    }
//
//    fn verify_integer_object(object: &Box<dyn Object>, expected: i64) {
//        match object.as_any().downcast_ref::<Integer>() {
//            Some(o) => assert_eq!(o.value, expected),
//            None => assert!(false, "Object is not an Integer"),
//        };
//    }
//}