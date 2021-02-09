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
        parser.prefix_parse_fns.insert(TokenType::True, Parser::parse_boolean);
        parser.prefix_parse_fns.insert(TokenType::False, Parser::parse_boolean);
        parser.prefix_parse_fns.insert(TokenType::LParen, Parser::parse_grouped_expression);
        parser.prefix_parse_fns.insert(TokenType::If, Parser::parse_if_expression);
        parser.prefix_parse_fns.insert(TokenType::Function, Parser::parse_function_literal);

        parser.infix_parse_fns.insert(TokenType::Plus, Parser::parse_infix_expression);
        parser.infix_parse_fns.insert(TokenType::Minus, Parser::parse_infix_expression);
        parser.infix_parse_fns.insert(TokenType::Slash, Parser::parse_infix_expression);
        parser.infix_parse_fns.insert(TokenType::Asterisk, Parser::parse_infix_expression);
        parser.infix_parse_fns.insert(TokenType::Eq, Parser::parse_infix_expression);
        parser.infix_parse_fns.insert(TokenType::NotEq, Parser::parse_infix_expression);
        parser.infix_parse_fns.insert(TokenType::Lt, Parser::parse_infix_expression);
        parser.infix_parse_fns.insert(TokenType::Gt, Parser::parse_infix_expression);
        parser.infix_parse_fns.insert(TokenType::LParen, Parser::parse_call_expression);

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

        self.next_token();

        let value = self.parse_expression(&Precedence::Lowest);

        if self.peek_token_is(&TokenType::Semicolon) {
            self.next_token();
        }

        Some(Box::new(LetStatement::new(current_token, name, value)))
    }

    fn parse_return_statement(&mut self) -> Option<Box<dyn Statement>> {
        let current_token = self.current_token.clone();

        self.next_token();

        let return_value = self.parse_expression(&Precedence::Lowest);

        if self.peek_token_is(&TokenType::Semicolon) {
            self.next_token();
        }

        Some(Box::new(ReturnStatement::new(current_token, return_value)))
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

    fn parse_block_statement(&mut self) -> Option<Box<dyn Statement>> {
        let current_token = self.current_token.clone();
        let mut statements: Vec<Box<dyn Statement>> = Vec::new();

        self.next_token();

        while !self.current_token_is(&TokenType::RBrace) && !self.current_token_is(&TokenType::Eof) {
            let statement = self.parse_statement();
            match statement {
                Some(s) => statements.push(s),
                None => (),
            };
            self.next_token();
        }

        Some(Box::new(BlockStatement::new(current_token, statements)))
    }

    fn parse_expression(&mut self, precedence: &Precedence) -> Option<Box<dyn Expression>> {
        let prefix = self.prefix_parse_fns.get(&self.current_token.token_type);
        match prefix {
            Some(p) => {
                let mut left_exp = p(self);
                if left_exp.is_none() {
                    return None;
                }

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

    fn parse_call_expression(&mut self, function: Box<dyn Expression>) -> Option<Box<dyn Expression>> {
        let current_token = self.current_token.clone();

        let arguments = self.parse_call_arguments();

        match arguments {
            Some(arg) => Some(Box::new(CallExpression::new(current_token.clone(), function.clone(), arg))),
            None => None,
        }
    }

    fn parse_call_arguments(&mut self) -> Option<Vec<Box<dyn Expression>>> {
        let mut args = vec![];

        if self.peek_token_is(&TokenType::RParen) {
            self.next_token();
            return Some(args);
        }

        self.next_token();
        match self.parse_expression(&Precedence::Lowest) {
            Some(pe) => args.push(pe),
            None => (),
        };

        while self.peek_token_is(&TokenType::Comma) {
            self.next_token();
            self.next_token();
            match self.parse_expression(&Precedence::Lowest) {
                Some(pe) => args.push(pe),
                None => (),
            };
        }

        if !self.expect_peek(&TokenType::RParen) {
            return None;
        }

        Some(args)
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

    fn parse_boolean(&mut self) -> Option<Box<dyn Expression>> {
        Some(Box::new(BooleanLiteral::new(self.current_token.clone(), self.current_token_is(&TokenType::True))))
    }

    fn parse_grouped_expression(&mut self) -> Option<Box<dyn Expression>> {
        self.next_token();

        let expression = self.parse_expression(&Precedence::Lowest);
        if !self.expect_peek(&TokenType::RParen) {
            return None;
        }

        expression
    }

    fn parse_if_expression(&mut self) -> Option<Box<dyn Expression>> {
        let current_token = self.current_token.clone();

        if !self.expect_peek(&TokenType::LParen) {
            return None;
        }

        self.next_token();
        let condition = self.parse_expression(&Precedence::Lowest);

        if !self.expect_peek(&TokenType::RParen) {
            return None;
        }

        if !self.expect_peek(&TokenType::LBrace) {
            return None;
        }

        let consequence = self.parse_block_statement();

        let mut alternative = None;
        if self.peek_token_is(&TokenType::Else) {
            self.next_token();
            if !self.expect_peek(&TokenType::LBrace) {
                return None;
            }

            alternative = self.parse_block_statement()
        }

        match (condition, consequence) {
            (Some(cond), Some(cons)) => Some(Box::new(IfExpression::new(current_token, cond, cons, alternative))),
            _ => None,
        }
    }

    fn parse_function_literal(&mut self) -> Option<Box<dyn Expression>> {
        let current_token = self.current_token.clone();

        if !self.expect_peek(&TokenType::LParen) {
            return None;
        }

        let parameters = self.parse_function_parameters();

        if !self.expect_peek(&TokenType::LBrace) {
            return None;
        }

        let body = self.parse_block_statement();

        match (parameters, body) {
            (Some(param), Some(b)) => Some(Box::new(FunctionLiteral::new(current_token, param, b))),
            _ => None,
        }
    }

    fn parse_function_parameters(&mut self) -> Option<Vec<Box<dyn Expression>>> {
        let mut identifiers = vec![];

        if self.peek_token_is(&TokenType::RParen) {
            self.next_token();
            return Some(identifiers);
        }

        self.next_token();

        let mut identifier = Identifier::new(self.current_token.clone(), self.current_token.literal.clone());
        identifiers.push(Box::new(identifier));

        while self.peek_token_is(&TokenType::Comma) {
            self.next_token();
            self.next_token();

            identifier = Identifier::new(self.current_token.clone(), self.current_token.literal.clone());
            identifiers.push(Box::new(identifier));
        }

        if !self.expect_peek(&TokenType::RParen) {
            return None;
        }

        Some(identifiers)
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
            TokenType::LParen => Precedence::Call,
            _ => Precedence::Lowest,
        }
    }

    pub fn get_errors(&self) -> Vec<String> {
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

    enum ExpType {
        Int(i64),
        Bool(bool),
        String(String),
    }

    trait ValueType {
        fn get_type(&self) -> String;
        fn get_i64(&self) -> i64;
        fn get_string(&self) -> String;
        fn get_boolean(&self) -> bool;
    }

    impl ValueType for i64 {
        fn get_type(&self) -> String { String::from("i64") }
        fn get_i64(&self) -> i64 { *self }
        fn get_string(&self) -> String { panic!("Value is not a string.") }
        fn get_boolean(&self) -> bool { panic!("Value is not a string.") }
    }

    impl ValueType for String {
        fn get_type(&self) -> String { String::from("String") }
        fn get_i64(&self) -> i64 { panic!("Value is not an i64.") }
        fn get_string(&self) -> String { self.clone() }
        fn get_boolean(&self) -> bool { panic!("Value is not an i64.") }
    }

    impl ValueType for bool {
        fn get_type(&self) -> String { String::from("bool") }
        fn get_i64(&self) -> i64 { panic!("Value is not a boolean.") }
        fn get_string(&self) -> String { panic!("Value is not a boolean.") }
        fn get_boolean(&self) -> bool { *self }
    }

    #[test]
    fn verify_let_statements_are_parsed() {
        struct LetTest {
            input: String,
            expected_identifier: String,
            expected_value: ExpType,
        };

        let let_tests = vec![
            LetTest { input: String::from("let x = 5;"), expected_identifier: String::from("x"), expected_value: ExpType::Int(5), },
            LetTest { input: String::from("let y = true;"), expected_identifier: String::from("y"), expected_value: ExpType::Bool(true), },
            LetTest { input: String::from("let foobar = y;"), expected_identifier: String::from("foobar"), expected_value: ExpType::String(String::from("y")), },
        ];

        for let_test in let_tests {
            let lexer = Lexer::new(let_test.input);
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program();
            check_parser_errors(&parser);
            match &program {
                Some(p) => assert_eq!(p.statements.len(), 1),
                None => assert!(false, "parse_program() returned None."),
            };

            let stmt = &program.unwrap().statements[0];
            verify_let_statement(&stmt, &let_test.expected_identifier);
            match stmt.as_any().downcast_ref::<LetStatement>() {
                Some(s) => {
                    match &s.value {
                        Some(v) => { 
                            match let_test.expected_value {
                                ExpType::Int(ei) => verify_literal_expression(&v, ei),
                                ExpType::String(es) => verify_literal_expression(&v, es),
                                ExpType::Bool(eb) => verify_literal_expression(&v, eb),
                            }
                        },
                        None => assert!(false, "Statement value was None."),
                    };
                }
                None => assert!(false, "Statement is not of type LetStatement"),
            }
        }
    }

    #[test]
    fn verify_return_statements_are_parsed() {
        struct ReturnTest {
            input: String,
            expected_value: ExpType,
        };

        let return_tests = vec![
            ReturnTest { input: String::from("return 5;"), expected_value: ExpType::Int(5), },
            ReturnTest { input: String::from("return true;"), expected_value: ExpType::Bool(true), },
            ReturnTest { input: String::from("return foobar;"), expected_value: ExpType::String(String::from("foobar")), },
        ];

        for return_test in return_tests {
            let lexer = Lexer::new(return_test.input);
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program();
            check_parser_errors(&parser);
            match &program {
                Some(p) => assert_eq!(p.statements.len(), 1),
                None => assert!(false, "parse_program() returned None."),
            };

            let stmt = &program.unwrap().statements[0];
            match stmt.as_any().downcast_ref::<ReturnStatement>() {
                Some(s) => {
                    assert_eq!(s.token_literal(), "return");
                    match &s.return_value {
                        Some(r) => { 
                            match return_test.expected_value {
                                ExpType::Int(ei) => verify_literal_expression(&r, ei),
                                ExpType::String(es) => verify_literal_expression(&r, es),
                                ExpType::Bool(eb) => verify_literal_expression(&r, eb),
                            }
                        },
                        None => assert!(false, "Statement value was None."),
                    };
                }
                None => assert!(false, "Statement is not of type ReturnStatement"),
            }
        }
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
                    Some(ident) => {
                        assert_eq!(e.type_of(), "Identifier");
                        assert_eq!(ident.value, String::from("foobar"));
                        assert_eq!(ident.token_literal(), String::from("foobar"));
                    },
                    None => assert!(false, "Expression was not an Identifier."),
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
                    Some(int_lit) => {
                        assert_eq!(e.type_of(), "IntegerLiteral");
                        assert_eq!(int_lit.value, 5);
                        assert_eq!(int_lit.token_literal(), String::from("5"));
                    },
                    None => assert!(false, "Expression was not an IntegerLiteral."),
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
            value: ExpType,
        };

        let prefix_tests = vec![
            PrefixTest { input: String::from("!5"), operator: String::from("!"), value: ExpType::Int(5), },
            PrefixTest { input: String::from("-15"), operator: String::from("-"), value: ExpType::Int(15), },
            PrefixTest { input: String::from("!true"), operator: String::from("!"), value: ExpType::Bool(true), },
            PrefixTest { input: String::from("!false"), operator: String::from("!"), value: ExpType::Bool(false), },
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
                    match prefix_test.value {
                        ExpType::Int(iv) => verify_prefix_expression(&e, &prefix_test.operator, iv),
                        ExpType::Bool(bv) => verify_prefix_expression(&e, &prefix_test.operator, bv),
                        ExpType::String(_) => (),
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
            left_value: ExpType,
            operator: String,
            right_value: ExpType,
        };

        let infix_tests = vec![
            InfixTest { input: String::from("5 + 5;"), left_value: ExpType::Int(5), operator: String::from("+"), right_value: ExpType::Int(5), },
            InfixTest { input: String::from("5 - 5;"), left_value: ExpType::Int(5), operator: String::from("-"), right_value: ExpType::Int(5), },
            InfixTest { input: String::from("5 * 5;"), left_value: ExpType::Int(5), operator: String::from("*"), right_value: ExpType::Int(5), },
            InfixTest { input: String::from("5 / 5;"), left_value: ExpType::Int(5), operator: String::from("/"), right_value: ExpType::Int(5), },
            InfixTest { input: String::from("5 > 5;"), left_value: ExpType::Int(5), operator: String::from(">"), right_value: ExpType::Int(5), },
            InfixTest { input: String::from("5 < 5;"), left_value: ExpType::Int(5), operator: String::from("<"), right_value: ExpType::Int(5), },
            InfixTest { input: String::from("5 == 5;"), left_value: ExpType::Int(5), operator: String::from("=="), right_value: ExpType::Int(5), },
            InfixTest { input: String::from("5 != 5;"), left_value: ExpType::Int(5), operator: String::from("!="), right_value: ExpType::Int(5), },
            InfixTest { input: String::from("true == true"), left_value: ExpType::Bool(true), operator: String::from("=="), right_value: ExpType::Bool(true), },
            InfixTest { input: String::from("true != false"), left_value: ExpType::Bool(true), operator: String::from("!="), right_value: ExpType::Bool(false), },
            InfixTest { input: String::from("false == false"), left_value: ExpType::Bool(false), operator: String::from("=="), right_value: ExpType::Bool(false), },
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
                    match (infix_test.left_value, infix_test.right_value) {
                        (ExpType::Int(il), ExpType::Int(ir)) => verify_infix_expression(&e, il, &infix_test.operator, ir),
                        (ExpType::Bool(bl), ExpType::Bool(br)) => verify_infix_expression(&e, bl, &infix_test.operator, br),
                        _ => assert!(false, "Left and right value types are mismatched."),
                    };
                },
                None => assert!(false, "Expression statement was not returned."),
            };
        }
    }

    #[test]
    fn verify_operator_precedences_are_parsed() {
        struct OperatorTest {
            input: String,
            expected: String,
        };

        let operator_tests = vec![
            OperatorTest { input: String::from("-a * b"), expected: String::from("((-a) * b)"), },
            OperatorTest { input: String::from("!-a"), expected: String::from("(!(-a))"), },
            OperatorTest { input: String::from("a + b + c"), expected: String::from("((a + b) + c)"), },
            OperatorTest { input: String::from("a + b - c"), expected: String::from("((a + b) - c)"), },
            OperatorTest { input: String::from("a * b * c"), expected: String::from("((a * b) * c)"), },
            OperatorTest { input: String::from("a * b / c"), expected: String::from("((a * b) / c)"), },
            OperatorTest { input: String::from("a + b / c"), expected: String::from("(a + (b / c))"), },
            OperatorTest { input: String::from("a + b * c + d / e - f"), expected: String::from("(((a + (b * c)) + (d / e)) - f)"), },
            OperatorTest { input: String::from("3 + 4; -5 * 5"), expected: String::from("(3 + 4)((-5) * 5)"), },
            OperatorTest { input: String::from("5 > 4 == 3 < 4"), expected: String::from("((5 > 4) == (3 < 4))"), },
            OperatorTest { input: String::from("5 < 4 != 3 > 4"), expected: String::from("((5 < 4) != (3 > 4))"), },
            OperatorTest { input: String::from("3 + 4 * 5 == 3 * 1 + 4 * 5"), expected: String::from("((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"), },
            OperatorTest { input: String::from("true"), expected: String::from("true"), },
            OperatorTest { input: String::from("false"), expected: String::from("false"), },
            OperatorTest { input: String::from("3 > 5 == false"), expected: String::from("((3 > 5) == false)"), },
            OperatorTest { input: String::from("3 < 5 == true"), expected: String::from("((3 < 5) == true)"), },
            OperatorTest { input: String::from("1 + (2 + 3) + 4"), expected: String::from("((1 + (2 + 3)) + 4)"), },
            OperatorTest { input: String::from("(5 + 5) * 2"), expected: String::from("((5 + 5) * 2)"), },
            OperatorTest { input: String::from("2 / (5 + 5)"), expected: String::from("(2 / (5 + 5))"), },
            OperatorTest { input: String::from("-(5 + 5)"), expected: String::from("(-(5 + 5))"), },
            OperatorTest { input: String::from("!(true == true)"), expected: String::from("(!(true == true))"), },
            OperatorTest { input: String::from("a + add(b * c) + d"), expected: String::from("((a + add((b * c))) + d)"), },
            OperatorTest { input: String::from("add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))"), expected: String::from("add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))"), },
            OperatorTest { input: String::from("add(a + b + c * d / f + g)"), expected: String::from("add((((a + b) + ((c * d) / f)) + g))"), },
        ];

        for operator_test in operator_tests {
            let lexer = Lexer::new(operator_test.input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            check_parser_errors(&parser);

            match &program {
                Some(p) => {
                    let actual = p.to_string();
                    assert_eq!(operator_test.expected, actual);
                },
                None => assert!(false, "parse_program() returned None."),
            };
        }
    }

    #[test]
    fn verify_boolean_expressions_are_parsed() {
        let input = String::from("true;");

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
                match e.as_any().downcast_ref::<BooleanLiteral>() {
                    Some(boolean) => {
                        assert_eq!(boolean.value, true);
                        assert_eq!(boolean.token_literal(), true.to_string());
                    },
                    None => panic!("Not Boolean"),
                };
            },
            None => assert!(false, "Expression statement was not returned."),
        };
    }

    #[test]
    fn verify_if_expressions_are_parsed() {
        let input = String::from("if (x < y) { x }");

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
                match e.as_any().downcast_ref::<IfExpression>() {
                    Some(if_exp) => {
                        verify_infix_expression(&if_exp.condition, String::from("x"), &String::from("<"), String::from("y"));
                        match if_exp.consequence.as_any().downcast_ref::<BlockStatement>() {
                            Some(block) => {
                                assert_eq!(block.statements.len(), 1);
                                match block.statements[0].as_any().downcast_ref::<ExpressionStatement>() {
                                    Some(stmt_exp) => verify_identifier(&stmt_exp.expression.clone().unwrap(), &String::from("x")),
                                    None => assert!(false, "Not an ExpressionStatement"),
                                };
                            },
                            None => assert!(false, "Not a BlockStatement"),
                        }
                        match &if_exp.alternative {
                            Some(_) => assert!(false, "Alternative should be None."),
                            None => (),
                        };
                    },
                    None => assert!(false, "Not an IfExpression")
                }
            },
            None => assert!(false, "Expression statement was not returned."),
        };
    }

    #[test]
    fn verify_if_else_expressions_are_parsed() {
        let input = String::from("if (x < y) { x } else { y }");

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
                match e.as_any().downcast_ref::<IfExpression>() {
                    Some(if_exp) => {
                        verify_infix_expression(&if_exp.condition, String::from("x"), &String::from("<"), String::from("y"));
                        match if_exp.consequence.as_any().downcast_ref::<BlockStatement>() {
                            Some(block) => {
                                assert_eq!(block.statements.len(), 1);
                                match block.statements[0].as_any().downcast_ref::<ExpressionStatement>() {
                                    Some(stmt_exp) => verify_identifier(&stmt_exp.expression.clone().unwrap(), &String::from("x")),
                                    None => assert!(false, "Not an ExpressionStatement"),
                                };
                            },
                            None => assert!(false, "Not a BlockStatement"),
                        }
                        match &if_exp.alternative {
                            Some(alt) => {
                                match alt.as_any().downcast_ref::<BlockStatement>() {
                                    Some(block) => {
                                        assert_eq!(block.statements.len(), 1);
                                        match block.statements[0].as_any().downcast_ref::<ExpressionStatement>() {
                                            Some(stmt_exp) => verify_identifier(&stmt_exp.expression.clone().unwrap(), &String::from("y")),
                                            None => assert!(false, "Not an ExpressionStatement"),
                                        };
                                    },
                                    None => assert!(false, "Not a BlockStatement"),
                                }
                            },
                            None => assert!(false, "Alternative should not be None."),
                        };
                    },
                    None => assert!(false, "Not an IfExpression")
                }
            },
            None => assert!(false, "Expression statement was not returned."),
        };
    }

    #[test]
    fn verify_function_literals_are_parsed() {
        let input = String::from("fn(x, y) { x + y; }");

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
                match e.as_any().downcast_ref::<FunctionLiteral>() {
                    Some(func_lit) => {
                        assert_eq!(func_lit.parameters.len(), 2);
                        verify_literal_expression(&func_lit.parameters[0], String::from("x"));
                        verify_literal_expression(&func_lit.parameters[1], String::from("y"));
                        match func_lit.body.as_any().downcast_ref::<BlockStatement>() {
                            Some(body) => {
                                assert_eq!(body.statements.len(), 1);
                                match body.statements[0].as_any().downcast_ref::<ExpressionStatement>() {
                                    Some(stmt_exp) => verify_infix_expression(&stmt_exp.expression.clone().unwrap(), String::from("x"), &String::from("+"), String::from("y")),
                                    None => assert!(false, "Not an ExpressionStatement"),
                                };
                            },
                            None => assert!(false, "Not a BlockStatement"),
                        }
                    },
                    None => assert!(false, "Not a FunctionLiteral")
                }
            },
            None => assert!(false, "Expression statement was not returned."),
        };
    }

    #[test]
    fn verify_function_parameters_are_parsed() {
        struct ParamTest {
            input: String,
            expected_params: Vec<String>,
        };

        let param_tests = vec![
            ParamTest { input: String::from("fn() {};"), expected_params: vec![] },
            ParamTest { input: String::from("fn(x) {};"), expected_params: vec![String::from("x")] },
            ParamTest { input: String::from("fn(x, y, z) {};"), expected_params: vec![
                String::from("x"), String::from("y"), String::from("z")] },
        ];

        for param_test in param_tests {
            let lexer = Lexer::new(param_test.input);
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
                    match e.as_any().downcast_ref::<FunctionLiteral>() {
                        Some(func_lit) => {
                            assert_eq!(func_lit.parameters.len(), param_test.expected_params.len());
                            for (i, param) in param_test.expected_params.iter().enumerate() {
                                verify_literal_expression(&func_lit.parameters[i], param.clone());
                            }
                        },
                        None => assert!(false, "Not a FunctionLiteral")
                    }
                },
                None => assert!(false, "Expression statement was not returned."),
            };
        }
    }

    #[test]
    fn verify_call_expressions_are_parsed() {
        let input = String::from("add(1, 2 * 3, 4 + 5);");

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
                match e.as_any().downcast_ref::<CallExpression>() {
                    Some(call_exp) => {
                        verify_identifier(&call_exp.function, &String::from("add"));
                        assert_eq!(call_exp.arguments.len(), 3);
                        verify_literal_expression(&call_exp.arguments[0], 1);
                        verify_infix_expression(&call_exp.arguments[1].clone(), 2, &String::from("*"), 3);
                        verify_infix_expression(&call_exp.arguments[2].clone(), 4, &String::from("+"), 5);
                    },
                    None => assert!(false, "Not a CallExpression")
                }
            },
            None => assert!(false, "Expression statement was not returned."),
        };
    }

    #[test]
    fn verify_call_expression_parameters_are_parsed() {
        struct ArgTest {
            input: String,
            expected_ident: String,
            expected_args: Vec<String>,
        };

        let arg_tests = vec![
            ArgTest { input: String::from("add();"), expected_ident: String::from("add"), expected_args: vec![] },
            ArgTest { input: String::from("add(1);"), expected_ident: String::from("add"), expected_args: vec![String::from("1")] },
            ArgTest { input: String::from("add(1, 2 * 3, 4 + 5);"), expected_ident: String::from("add"), expected_args: vec![
                String::from("1"), String::from("(2 * 3)"), String::from("(4 + 5)")] },
        ];

        for arg_test in arg_tests {
            let lexer = Lexer::new(arg_test.input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            check_parser_errors(&parser);

            let stmt = &program.unwrap().statements[0];
            match stmt.expression() {
                Some(e) => {
                    match e.as_any().downcast_ref::<CallExpression>() {
                        Some(call_exp) => {
                            verify_identifier(&call_exp.function, &arg_test.expected_ident);
                            assert_eq!(call_exp.arguments.len(), arg_test.expected_args.len());
                            for (i, arg) in arg_test.expected_args.iter().enumerate() {
                                assert_eq!(&call_exp.arguments[i].to_string(), arg);
                            }
                        },
                        None => assert!(false, "Not a CallExpression")
                    }
                },
                None => assert!(false, "Expression statement was not returned."),
            };
        }
    }

    fn verify_literal_expression<T>(expression: &Box<dyn Expression>, expected: T) where T: ValueType {
        match expected.get_type().as_str() {
            "i64" => verify_integer_literal(expression, expected.get_i64()),
            "String" => verify_identifier(expression, &expected.get_string()),
            "bool" => verify_boolean_literal(expression, expected.get_boolean()),
            _ => assert!(false, "Expression type was not handled"),
        }
    }

    fn verify_prefix_expression<T>(expression: &Box<dyn Expression>, operator: &String, value: T) where T: ValueType {
        match expression.as_any().downcast_ref::<PrefixExpression>() {
            Some(exp) => {
                assert_eq!(exp.operator, *operator);
                verify_literal_expression(&exp.right, value);
            },
            None => assert!(false, "Expression is not of type PrefixExpression."),
        };
    }

    fn verify_infix_expression<T>(expression: &Box<dyn Expression>, left: T, 
        operator: &String, right: T) where T: ValueType {

        match expression.as_any().downcast_ref::<InfixExpression>() {
            Some(exp) => {
                verify_literal_expression(&exp.left, left);
                assert_eq!(exp.operator, *operator);
                verify_literal_expression(&exp.right, right);
            },
            None => assert!(false, "Expression is not of type InfixExpression."),
        };
    }

    fn verify_integer_literal(expression: &Box<dyn Expression>, value: i64) {
        match expression.as_any().downcast_ref::<IntegerLiteral>() {
            Some(e) => {
                assert_eq!(e.value, value);
                assert_eq!(e.token_literal(), value.to_string());
            },
            None => assert!(false, "Expression is not of type IntegerLiteral"),
        };
    }

    fn verify_identifier(expression: &Box<dyn Expression>, value: &String) {
        match expression.as_any().downcast_ref::<Identifier>() {
            Some(e) => {
                eprintln!("Value: {}", e.value);
                assert_eq!(e.value, *value);
                assert_eq!(e.token_literal(), *value);
            },
            None => assert!(false, "Expression is not of type Identifier"),
        };
    }

    fn verify_boolean_literal(expression: &Box<dyn Expression>, value: bool) {
        match expression.as_any().downcast_ref::<BooleanLiteral>() {
            Some(e) => {
                assert_eq!(e.value, value);
                assert_eq!(e.token_literal(), value.to_string());
            },
            None => assert!(false, "Expression is not of type Boolean"),
        };
    }

    fn verify_let_statement(statement: &Box<dyn Statement>, name: &String) {
        match statement.as_any().downcast_ref::<LetStatement>() {
            Some(s) => {
                assert_eq!(s.name.value, *name);
                assert_eq!(s.name.token_literal(), *name);
            }
            None => assert!(false, "Statement is not of type LetStatement"),
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