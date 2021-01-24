use super::ast::*;
use super::lexer::Lexer;
use super::token::{Token, TokenType};
use std::collections::HashMap;

#[derive(PartialEq, PartialOrd)]
pub enum Precedence {
    Blank = 0,
    Lowest,
    Equals, // ==
    LessGreater, // > or <
    Sum, // +
    Product, // *
    Prefix, // -X or !X
    Call, // myFunction(X)
}

type PrefixCallback = fn(&mut Parser) -> Option<Box<dyn Expression>>;
type InfixCallback = fn(&mut Parser, Box<dyn Expression>) -> Option<Box<dyn Expression>>;

pub struct Parser {
    lexer: Lexer,
    current_token: Token,
    peek_token: Token,
    errors: Vec<String>,
    prefix_parse_fns: HashMap<TokenType, PrefixCallback>,
    infix_parse_fns: HashMap<TokenType, InfixCallback>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let mut parser = Self { 
            lexer, 
            current_token: Token::new(TokenType::Illegal, String::new()),
            peek_token: Token::new(TokenType::Illegal, String::new()),
            errors: vec![],
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
        };

        parser.prefix_parse_fns.insert(TokenType::Ident, Parser::parse_identifier);
        parser.prefix_parse_fns.insert(TokenType::Int, Parser::parse_integer_literal);
        parser.prefix_parse_fns.insert(TokenType::Bang, Parser::parse_prefix_expression);
        parser.prefix_parse_fns.insert(TokenType::Minus, Parser::parse_prefix_expression);

        parser.infix_parse_fns.insert(TokenType::Plus, Parser::parse_infix_expression);
        parser.infix_parse_fns.insert(TokenType::Minus, Parser::parse_infix_expression);
        parser.infix_parse_fns.insert(TokenType::Slash, Parser::parse_infix_expression);
        parser.infix_parse_fns.insert(TokenType::Asterisk, Parser::parse_infix_expression);
        parser.infix_parse_fns.insert(TokenType::Eq, Parser::parse_infix_expression);
        parser.infix_parse_fns.insert(TokenType::NotEq, Parser::parse_infix_expression);
        parser.infix_parse_fns.insert(TokenType::Lt, Parser::parse_infix_expression);
        parser.infix_parse_fns.insert(TokenType::Gt, Parser::parse_infix_expression);

        parser
    }

    pub fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    pub fn parse_program(&mut self) -> Option<Program> {
        let mut program = Program::new();

        // Prime the first valid token.
        self.next_token();
        self.next_token();
        while self.current_token.token_type != TokenType::Eof {
            let stmt = self.parse_statement();

            match stmt {
                Some(s) => program.statements.push(s),
                None => (),
            };

            self.next_token();
        }

        Some(program)
    }

    fn parse_statement(&mut self) -> Option<Box<dyn Statement>> {
        match self.current_token.token_type {
            TokenType::Let => self.parse_let_statement(),
            TokenType::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Option<Box<dyn Statement>> {
        let current_token = self.current_token.clone();

        if !self.expect_peek(&TokenType::Ident) {
            return None;
        }

        let name = Identifier::new(self.current_token.clone(), self.current_token.literal.clone());

        if !self.expect_peek(&TokenType::Assign) {
            return None;
        }

        while !self.current_token_is(&TokenType::Semicolon) {
            self.next_token();
        }

        Some(Box::new(LetStatement::new(current_token, name, None)))
    }

    fn parse_return_statement(&mut self) -> Option<Box<dyn Statement>> {
        let current_token = self.current_token.clone();

        self.next_token();

        while !self.current_token_is(&TokenType::Semicolon) {
            self.next_token();
        }

        Some(Box::new(ReturnStatement::new(current_token, None)))
    }

    fn parse_expression_statement(&mut self) -> Option<Box<dyn Statement>> {
        let current_token = self.current_token.clone();

        let expression = self.parse_expression(&Precedence::Lowest);

        if self.peek_token_is(&TokenType::Semicolon) {
            self.next_token();
        }

        match expression {
            Some(_) => Some(Box::new(ExpressionStatement::new(current_token, expression))),
            None => None,
        }
    }

    fn parse_expression(&mut self, precedence: &Precedence) -> Option<Box<dyn Expression>> {
        let prefix = self.prefix_parse_fns.get(&self.current_token.token_type);
        match prefix {
            Some(p) => {
                let mut left_exp = p(self);
                eprintln!("Token: {}", &self.current_token);

                while !self.peek_token_is(&TokenType::Semicolon) && *precedence < self.peek_precedence() {
                    // This clone is avoiding a mutable-immutable borrow here.
                    let infix_fns = self.infix_parse_fns.clone();
                    let infix = infix_fns.get(&self.peek_token.token_type);
                    match infix {
                        Some(i) => {
                            self.next_token();

                            left_exp = i(self, left_exp.unwrap().clone());
                        },
                        None => return left_exp,
                    }
                }

                left_exp
            },
            None => {
                self.no_prefix_parse_fn_error(&self.current_token.token_type.clone());

                None
            },
        }
    }

    fn parse_identifier(&mut self) -> Option<Box<dyn Expression>> {
        Some(Box::new(Identifier::new(self.current_token.clone(), self.current_token.literal.clone())))
    }

    fn parse_integer_literal(&mut self) -> Option<Box<dyn Expression>> {
        match self.current_token.literal.parse::<i64>() {
            Ok(v) => Some(Box::new(IntegerLiteral::new(self.current_token.clone(), v))),
            Err(_e) => {
                let msg = format!("Could not parse {} as an integer.", self.current_token.literal);
                self.errors.push(msg);

                None
            },
        }
    }

    fn parse_prefix_expression(&mut self) -> Option<Box<dyn Expression>> {
        let current_token = self.current_token.clone();

        self.next_token();

        let right = self.parse_expression(&Precedence::Prefix);

        Some(Box::new(PrefixExpression::new(current_token.clone(), 
            current_token.literal.clone(), right.unwrap())))
    }

    fn parse_infix_expression(&mut self, left: Box<dyn Expression>) -> Option<Box<dyn Expression>> {
        let current_token = self.current_token.clone();

        let precedence = self.current_precedence();
        self.next_token();
        let right = self.parse_expression(&precedence);

        Some(Box::new(InfixExpression::new(current_token.clone(), 
            left.clone(), current_token.literal.clone(), right.unwrap())))
    }

    fn current_token_is(&self, token_type: &TokenType) -> bool {
        self.current_token.token_type == *token_type
    }

    fn peek_token_is(&self, token_type: &TokenType) -> bool {
        self.peek_token.token_type == *token_type
    }

    fn expect_peek(&mut self, token_type: &TokenType) -> bool {
        if self.peek_token_is(token_type) {
            self.next_token();

            true
        } else {
            self.peek_error(token_type);

            false
        }
    }

    fn current_precedence(&self) -> Precedence {
        Self::match_precedence(&self.current_token.token_type)
    }

    fn peek_precedence(&self) -> Precedence {
        Self::match_precedence(&self.peek_token.token_type)
    }

    fn match_precedence(token_type: &TokenType) -> Precedence {
        match token_type {
            TokenType::Eq | TokenType::NotEq => Precedence::Equals,
            TokenType::Lt | TokenType::Gt => Precedence::LessGreater,
            TokenType::Plus | TokenType::Minus => Precedence::Sum,
            TokenType::Slash | TokenType::Asterisk => Precedence::Product,
            _ => Precedence::Lowest,
        }
    }

    fn get_errors(&self) -> Vec<String> {
        self.errors.clone()
    }

    fn peek_error(&mut self, token_type: &TokenType) {
        let msg = format!("Expected next token to be {}, but received {} instead", 
            token_type, self.peek_token.token_type);
        self.errors.push(msg); 
    }

    fn no_prefix_parse_fn_error(&mut self, token_type: &TokenType) {
        let msg = format!("No prefix function for {} found.", token_type);
        self.errors.push(msg);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use super::super::lexer::Lexer;

    #[test]
    fn verify_let_statements_are_parsed() {
        let input = String::from(r#"let x = 5;
let y = 10;
let foobar = 838383;"#);

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parser_errors(&parser);
        match program.clone() {
            Some(p) => assert_eq!(p.statements.len(), 3),
            None => assert!(false, "parse_program() returned None."),
        };

        let tests = vec![
            String::from("x"),
            String::from("y"),
            String::from("foobar"),
        ];

        for (i, test) in tests.iter().enumerate() {
            let stmt = program.clone().unwrap().statements[i].clone();
            assert_eq!(stmt.clone().token_literal(), String::from("let"));
            assert_eq!(stmt.clone().type_of(), "LetStatement");
            assert_eq!(stmt.clone().get_name().value, *test);
            assert_eq!(stmt.clone().get_name().token_literal(), *test);
        }
    }

    #[test]
    fn verify_return_statements_are_parsed() {
        let input = String::from(r#"return 5;
return 10;
return 993322;"#);

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parser_errors(&parser);
        match program.clone() {
            Some(p) => assert_eq!(p.statements.len(), 3),
            None => assert!(false, "parse_program() returned None."),
        };

        let tests = vec![
            String::from("x"),
            String::from("y"),
            String::from("foobar"),
        ];

        for (i, _test) in tests.iter().enumerate() {
            let stmt = program.clone().unwrap().statements[i].clone();
            assert_eq!(stmt.clone().token_literal(), String::from("return"));
            assert_eq!(stmt.clone().type_of(), "ReturnStatement");
        }
    }

    #[test]
    fn verify_identifier_expressions_are_parsed() {
        let input = String::from("foobar;");

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parser_errors(&parser);
        match &program {
            Some(p) => assert_eq!(p.statements.len(), 1),
            None => assert!(false, "parse_program() returned None."),
        };

        let stmt = &program.unwrap().statements[0];
        match stmt.expression() {
            Some(e) => {
                match e.as_any().downcast_ref::<Identifier>() {
                    Some(what) => {
                        assert_eq!(e.type_of(), "Identifier");
                        assert_eq!(what.value, String::from("foobar"));
                        assert_eq!(what.token_literal(), String::from("foobar"));
                    },
                    None => panic!("Not an Identifier"),
                };
            },
            None => assert!(false, "Expression statement was not returned."),
        };
    }

    #[test]
    fn verify_integer_literal_expressions_are_parsed() {
        let input = String::from("5;");

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parser_errors(&parser);
        match &program {
            Some(p) => assert_eq!(p.statements.len(), 1),
            None => assert!(false, "parse_program() returned None."),
        };

        let stmt = &program.unwrap().statements[0];
        match stmt.expression() {
            Some(e) => {
                match e.as_any().downcast_ref::<IntegerLiteral>() {
                    Some(what) => {
                        assert_eq!(e.type_of(), "IntegerLiteral");
                        assert_eq!(what.value, 5);
                        assert_eq!(what.token_literal(), String::from("5"));
                    },
                    None => panic!("Not an IntegerLiteral"),
                };
            },
            None => assert!(false, "Expression statement was not returned."),
        };
    }

    #[test]
    fn verify_prefix_expressions_are_parsed() {
        struct PrefixTest {
            input: String,
            operator: String,
            integer_value: i64,
        };

        let prefix_tests = vec![
            PrefixTest { input: String::from("!5"), operator: String::from("!"), integer_value: 5, },
            PrefixTest { input: String::from("-15"), operator: String::from("-"), integer_value: 15, },
        ];

        for prefix_test in prefix_tests {
            let lexer = Lexer::new(prefix_test.input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            check_parser_errors(&parser);

            match &program {
                Some(p) => assert_eq!(p.statements.len(), 1),
                None => assert!(false, "parse_program() returned None."),
            };

            let stmt = &program.unwrap().statements[0];
            match stmt.expression() {
                Some(e) => {
                    match e.as_any().downcast_ref::<PrefixExpression>() {
                        Some(exp) => {
                            assert_eq!(e.type_of(), "PrefixExpression");
                            assert_eq!(exp.operator, prefix_test.operator);
                            match exp.right.as_any().downcast_ref::<IntegerLiteral>() {
                                Some(r) => {
                                    assert_eq!(r.type_of(), "IntegerLiteral");
                                    assert_eq!(r.value, prefix_test.integer_value);
                                    assert_eq!(r.token_literal(), prefix_test.integer_value.to_string());
                                },
                                None => panic!("Not an IntegerLiteral"),
                            };
                        },
                        None => panic!("Not a PrefixExpression"),
                    };
                },
                None => assert!(false, "Expression statement was not returned."),
            };
        }
    }

    #[test]
    fn verify_infix_expressions_are_parsed() {
        struct InfixTest {
            input: String,
            left_value: i64,
            operator: String,
            right_value: i64,
        };

        let infix_tests = vec![
            InfixTest { input: String::from("5 + 5;"), left_value: 5, operator: String::from("+"), right_value: 5, },
            InfixTest { input: String::from("5 - 5;"), left_value: 5, operator: String::from("-"), right_value: 5, },
            InfixTest { input: String::from("5 * 5;"), left_value: 5, operator: String::from("*"), right_value: 5, },
            InfixTest { input: String::from("5 / 5;"), left_value: 5, operator: String::from("/"), right_value: 5, },
            InfixTest { input: String::from("5 > 5;"), left_value: 5, operator: String::from(">"), right_value: 5, },
            InfixTest { input: String::from("5 < 5;"), left_value: 5, operator: String::from("<"), right_value: 5, },
            InfixTest { input: String::from("5 == 5;"), left_value: 5, operator: String::from("=="), right_value: 5, },
            InfixTest { input: String::from("5 != 5;"), left_value: 5, operator: String::from("!="), right_value: 5, },
        ];

        for infix_test in infix_tests {
            let lexer = Lexer::new(infix_test.input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            check_parser_errors(&parser);

            match &program {
                Some(p) => assert_eq!(p.statements.len(), 1),
                None => assert!(false, "parse_program() returned None."),
            };

            let stmt = &program.unwrap().statements[0];
            match stmt.expression() {
                Some(e) => {
                    match e.as_any().downcast_ref::<InfixExpression>() {
                        Some(exp) => {
                            assert_eq!(e.type_of(), "InfixExpression");
                            match exp.left.as_any().downcast_ref::<IntegerLiteral>() {
                                Some(r) => {
                                    assert_eq!(r.type_of(), "IntegerLiteral");
                                    assert_eq!(r.value, infix_test.left_value);
                                    assert_eq!(r.token_literal(), infix_test.left_value.to_string());
                                },
                                None => panic!("Not an IntegerLiteral"),
                            };
                            assert_eq!(exp.operator, infix_test.operator);
                            match exp.right.as_any().downcast_ref::<IntegerLiteral>() {
                                Some(r) => {
                                    assert_eq!(r.type_of(), "IntegerLiteral");
                                    assert_eq!(r.value, infix_test.right_value);
                                    assert_eq!(r.token_literal(), infix_test.right_value.to_string());
                                },
                                None => panic!("Not an IntegerLiteral"),
                            };
                        },
                        None => panic!("Not an InfixExpression"),
                    };
                },
                None => assert!(false, "Expression statement was not returned."),
            };
        }
    }

    fn check_parser_errors(parser: &Parser) {
        if parser.get_errors().is_empty() {
            return;
        }

        eprintln!("Parser has {} errors.", parser.get_errors().len());
        for error in parser.get_errors() {
            eprintln!("Parser error: {}", error);
        }

        assert!(false);
    }
}