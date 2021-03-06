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
    Index, // array[index]
}

type PrefixCallback = fn(&mut Parser) -> Option<Expression>;
type InfixCallback = fn(&mut Parser, Expression) -> Option<Expression>;

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

        parser.prefix_parse_fns.insert(TokenType::Ident, Self::parse_identifier);
        parser.prefix_parse_fns.insert(TokenType::Int, Self::parse_integer_literal);
        parser.prefix_parse_fns.insert(TokenType::Bang, Self::parse_prefix_expression);
        parser.prefix_parse_fns.insert(TokenType::Minus, Self::parse_prefix_expression);
        parser.prefix_parse_fns.insert(TokenType::True, Self::parse_boolean);
        parser.prefix_parse_fns.insert(TokenType::False, Self::parse_boolean);
        parser.prefix_parse_fns.insert(TokenType::LParen, Self::parse_grouped_expression);
        parser.prefix_parse_fns.insert(TokenType::If, Self::parse_if_expression);
        parser.prefix_parse_fns.insert(TokenType::Function, Self::parse_function_literal);

        parser.infix_parse_fns.insert(TokenType::Plus, Self::parse_infix_expression);
        parser.infix_parse_fns.insert(TokenType::Minus, Self::parse_infix_expression);
        parser.infix_parse_fns.insert(TokenType::Slash, Self::parse_infix_expression);
        parser.infix_parse_fns.insert(TokenType::Asterisk, Self::parse_infix_expression);
        parser.infix_parse_fns.insert(TokenType::Eq, Self::parse_infix_expression);
        parser.infix_parse_fns.insert(TokenType::NotEq, Self::parse_infix_expression);
        parser.infix_parse_fns.insert(TokenType::Lt, Self::parse_infix_expression);
        parser.infix_parse_fns.insert(TokenType::Gt, Self::parse_infix_expression);
        parser.infix_parse_fns.insert(TokenType::LParen, Self::parse_call_expression);

        // Prime the tokens.
        parser.next_token();
        parser.next_token();

        parser
    }

    pub fn parse_program(&mut self) -> Option<Program> {
        let mut statements = vec![];

        while self.current_token.token_type != TokenType::Eof {
            let statement = self.parse_statement();
            match statement {
                Some(s) => statements.push(s),
                None => (),
            };

            self.next_token();
        }

        Some(Program::new(statements))
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.current_token.token_type {
            TokenType::Let => {
                match self.parse_let_statement() {
                    Some(ls) => Some(Statement::LetStatement(ls)),
                    None => None,
                }
            },
            TokenType::Return => Some(Statement::ReturnStatement(self.parse_return_statement())),
            _ => Some(Statement::ExpressionStatement(self.parse_expression_statement())),
        }
    }

    fn parse_let_statement(&mut self) -> Option<LetStatement> {
        let current_token = self.current_token.clone();

        if !self.expect_peek(&TokenType::Ident) {
            return None;
        }

        let name = Identifier::new(self.current_token.clone(), self.current_token.literal.as_str());

        if !self.expect_peek(&TokenType::Assign) {
            return None;
        }

        self.next_token();
        let value = self.parse_expression(&Precedence::Lowest);

        if !self.current_token_is(&TokenType::Semicolon) {
            self.next_token();
        }

        Some(LetStatement::new(current_token, name, value))
    }

    fn parse_return_statement(&mut self) -> ReturnStatement {
        let current_token = self.current_token.clone();
        self.next_token();

        let return_value = self.parse_expression(&Precedence::Lowest);

        if !self.current_token_is(&TokenType::Semicolon) {
            self.next_token();
        }

        ReturnStatement::new(current_token, return_value)
    }

    fn parse_expression_statement(&mut self) -> ExpressionStatement {
        let current_token = self.current_token.clone();
        let expression = self.parse_expression(&Precedence::Lowest);

        if self.peek_token_is(&TokenType::Semicolon) {
            self.next_token();
        }

        ExpressionStatement::new(current_token, expression)
    }

    fn parse_block_statement(&mut self) -> BlockStatement {
        let current_token = self.current_token.clone();
        self.next_token();

        let mut statements = vec![];
        while !self.current_token_is(&TokenType::RBrace) && !self.current_token_is(&TokenType::Eof) {
            let statement = self.parse_statement();
            if let Some(st) = statement {
                statements.push(st);
            }
            self.next_token();
        }

        BlockStatement::new(current_token, statements)
    }

    fn parse_expression(&mut self, precedence: &Precedence) -> Option<Expression> {
        let prefix = self.prefix_parse_fns.get(&self.current_token.token_type);
        match prefix {
            Some(p) => {
                let mut left_expression = p(self);
                if left_expression.is_none() {
                    return None;
                }

                while !self.peek_token_is(&TokenType::Semicolon) && *precedence < self.peek_precedence() {
                    // This clone is to avoid a mutable-immutable borrow here.
                    let infix_fns = self.infix_parse_fns.clone();
                    let infix = infix_fns.get(&self.peek_token.token_type);
                    match infix {
                        Some(i) => {
                            self.next_token();
                            left_expression = i(self, left_expression.unwrap().clone());
                        }
                        None => return left_expression,
                    }
                }

                left_expression
            },
            None => {
                self.no_prefix_parse_fn_error();
                None
            },
        }
    }

    fn parse_identifier(&mut self) -> Option<Expression> {
        Some(Expression::Identifier(Identifier::new(self.current_token.clone(), self.current_token.literal.as_str())))
    }

    fn parse_integer_literal(&mut self) -> Option<Expression> {
        match self.current_token.literal.parse::<i64>() {
            Ok(v) => Some(Expression::IntegerLiteral(IntegerLiteral::new(self.current_token.clone(), v))),
            Err(_e) => {
                let msg = format!("Could not parse {} as an integer", self.current_token.literal);
                self.errors.push(msg);

                None
            },
        }
    }

    fn parse_prefix_expression(&mut self) -> Option<Expression> {
        let current_token = self.current_token.clone();
        let literal = self.current_token.literal.clone();

        self.next_token();

        let right = self.parse_expression(&Precedence::Prefix);
        match right {
            Some(r) => Some(Expression::PrefixExpression(PrefixExpression::new(current_token, literal.as_str(), Box::new(r)))),
            None => None,
        }
    }

    fn parse_boolean(&mut self) -> Option<Expression> {
        Some(Expression::BooleanLiteral(BooleanLiteral::new(self.current_token.clone(), self.current_token_is(&TokenType::True))))
    }

    fn parse_grouped_expression(&mut self) -> Option<Expression> {
        self.next_token();

        let expression = self.parse_expression(&Precedence::Lowest);
        if !self.expect_peek(&TokenType::RParen) {
            None
        } else {
            expression
        }
    }

    fn parse_if_expression(&mut self) -> Option<Expression> {
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

        let mut alternative: Option<BlockStatement> = None;
        if self.peek_token_is(&TokenType::Else) {
            self.next_token();
            if !self.expect_peek(&TokenType::LBrace) {
                return None;
            }

            alternative = Some(self.parse_block_statement());
        }

        match (condition, alternative) {
            (Some(con), Some(alt)) => Some(Expression::IfExpression(IfExpression::new(current_token, Box::new(con), consequence, Some(alt)))),
            (Some(con), _) => Some(Expression::IfExpression(IfExpression::new(current_token, Box::new(con), consequence, None))),
            _ => None,
        }
    }

    fn parse_function_literal(&mut self) -> Option<Expression> {
        let current_token = self.current_token.clone();
        if !self.expect_peek(&TokenType::LParen) {
            return None;
        }

        let parameters = self.parse_function_parameters();
        if !self.expect_peek(&TokenType::LBrace) {
            return None;
        }

        let body = self.parse_block_statement();

        match parameters {
            Some(param) => Some(Expression::FunctionLiteral(FunctionLiteral::new(current_token, param, body))),
            None => None
        }
    }

    fn parse_function_parameters(&mut self) -> Option<Vec<Identifier>> {
        let mut identifiers = vec![];
        if self.peek_token_is(&TokenType::RParen) {
            self.next_token();
            return Some(identifiers);
        }

        self.next_token();

        identifiers.push(Identifier::new(self.current_token.clone(), self.current_token.literal.as_str()));

        while self.peek_token_is(&TokenType::Comma) {
            self.next_token();
            self.next_token();
            identifiers.push(Identifier::new(self.current_token.clone(), self.current_token.literal.as_str()));
        }

        if !self.expect_peek(&TokenType::RParen) {
            return None;
        }

        Some(identifiers)
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Option<Expression> {
        let current_token = self.current_token.clone();
        let operator = self.current_token.literal.clone();

        let precedence = self.current_precedence();
        self.next_token();

        let right = self.parse_expression(&precedence);
        match right {
            Some(r) => Some(Expression::InfixExpression(InfixExpression::new(current_token, Box::new(left.clone()), operator.as_str(), Box::new(r)))),
            None => None,
        }
    }

    fn parse_call_expression(&mut self, function: Expression) -> Option<Expression> {
        let current_token = self.current_token.clone();
        let arguments = self.parse_call_arguments();

        match arguments {
            Some(args) => Some(Expression::CallExpression(CallExpression::new(current_token, Box::new(function.clone()), args))),
            None => None,
        }
    }

    fn parse_call_arguments(&mut self) -> Option<Vec<Box<Expression>>> {
        let mut arguments = vec![];
        if self.peek_token_is(&TokenType::RParen) {
            self.next_token();
            return Some(arguments);
        }

        self.next_token();
        if let Some(exp) = self.parse_expression(&Precedence::Lowest) {
            arguments.push(Box::new(exp));
        }

        while self.peek_token_is(&TokenType::Comma) {
            self.next_token();
            self.next_token();
            if let Some(exp) = self.parse_expression(&Precedence::Lowest) {
                arguments.push(Box::new(exp));
            }
        }

        if !self.expect_peek(&TokenType::RParen) {
            return None;
        }

        Some(arguments)
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
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

    fn peek_precedence(&self) -> Precedence {
        Self::match_precedence(&self.peek_token.token_type)
    }

    fn current_precedence(&self) -> Precedence {
        Self::match_precedence(&self.current_token.token_type)
    }

    fn match_precedence(token_type: &TokenType) -> Precedence {
        match token_type {
            TokenType::Eq | TokenType::NotEq => Precedence::Equals,
            TokenType::Lt | TokenType::Gt => Precedence::LessGreater,
            TokenType::Plus | TokenType::Minus => Precedence::Sum,
            TokenType::Slash | TokenType::Asterisk => Precedence::Product,
            TokenType::LParen => Precedence::Call,
            //TokenType::LBracket => Precedence::Index,
            _ => Precedence::Lowest,
        }
    }

    pub fn errors(&self) -> Vec<String> {
        self.errors.clone()
    }

    fn peek_error(&mut self, token_type: &TokenType) {
        let message = format!("expected next token to be {}, got {} instead",
            token_type, self.peek_token.token_type);
        self.errors.push(message);
    }

    fn no_prefix_parse_fn_error(&mut self) {
        let message = format!("no prefix parse function for {} found", self.current_token.token_type);
        self.errors.push(message);
    }
}

#[cfg(test)]
mod test {
    use super::*;

    enum ExpressionType {
        Int(i64),
        Bool(bool),
        String(String),
    }

    trait ValueType {
        fn get_type(&self) -> &str;
        fn get_i64(&self) -> i64;
        fn get_string(&self) -> String;
        fn get_bool(&self) -> bool;
    }

    impl ValueType for i64 {
        fn get_type(&self) -> &str { "i64" }
        fn get_i64(&self) -> i64 { *self }
        fn get_string(&self) -> String { panic!("Value is not an i64.") }
        fn get_bool(&self) -> bool { panic!("Value is not an i64.") }
    }

    impl ValueType for String {
        fn get_type(&self) -> &str { "String" }
        fn get_i64(&self) -> i64 { panic!("Value is not a String.") }
        fn get_string(&self) -> String { self.clone() }
        fn get_bool(&self) -> bool { panic!("Value is not a String.") }
    }

    impl ValueType for bool {
        fn get_type(&self) -> &str { "bool" }
        fn get_i64(&self) -> i64 { panic!("Value is not a bool.") }
        fn get_string(&self) -> String { panic!("Value is not a bool.") }
        fn get_bool(&self) -> bool { *self }
    }

    #[test]
    fn test_let_statements() {
        struct LetTest<'a> {
            input: &'a str,
            expected_identifier: &'a str,
            expected_value: ExpressionType,
        };

        let let_tests = vec![
            LetTest { input: "let x = 5;", expected_identifier: "x", expected_value: ExpressionType::Int(5) },
            LetTest { input: "let y = true;", expected_identifier: "y", expected_value: ExpressionType::Bool(true) },
            LetTest { input: "let foobar = y;", expected_identifier: "foobar", expected_value: ExpressionType::String(String::from("y")) },
        ];

        for let_test in let_tests {
            let lexer = Lexer::new(let_test.input);
            let mut parser = Parser::new(lexer);
    
            let program = parser.parse_program();
            check_parser_errors(&parser);

            match program {
                Some(p) => {
                    assert_eq!(p.statements.len(), 1);
                    test_let_statement(&p.statements[0], let_test.expected_identifier);
                    match &p.statements[0] {
                        Statement::LetStatement(ls) => {
                            match &ls.value {
                                Some(val) => {
                                    match let_test.expected_value {
                                        ExpressionType::Int(i) => test_literal_expression(val, i),
                                        ExpressionType::Bool(b) => test_literal_expression(val, b),
                                        ExpressionType::String(s) => test_literal_expression(val, s),
                                    };
                                },
                                None => assert!(false, "LetStatement value could not be parsed."),
                            }
                        },
                        _ => assert!(false, "Statement was not a LetStatement."),
                    };
                },
                None => assert!(false, "Program could not be parsed."),
            };
        }
    }

    #[test]
    fn test_return_statements() {
        struct ReturnTest<'a> {
            input: &'a str,
            expected_value: ExpressionType,
        };

        let return_tests = vec![
            ReturnTest { input: "return 5;", expected_value: ExpressionType::Int(5) },
            ReturnTest { input: "return true;", expected_value: ExpressionType::Bool(true) },
            ReturnTest { input: "return foobar;", expected_value: ExpressionType::String(String::from("foobar")) },
        ];

        for return_test in return_tests {
            let lexer = Lexer::new(return_test.input);
            let mut parser = Parser::new(lexer);
    
            let program = parser.parse_program();
            check_parser_errors(&parser);

            match program {
                Some(p) => {
                    assert_eq!(p.statements.len(), 1);
                    match &p.statements[0] {
                        Statement::ReturnStatement(rs) => {
                            assert_eq!(rs.token.literal, "return");
                            match &rs.return_value {
                                Some(ret_val) => {
                                    match return_test.expected_value {
                                        ExpressionType::Int(i) => test_literal_expression(ret_val, i),
                                        ExpressionType::Bool(b) => test_literal_expression(ret_val, b),
                                        ExpressionType::String(s) => test_literal_expression(ret_val, s),
                                    };
                                },
                                None => assert!(false, "ReturnStatement value could not be parsed."),
                            }
                        },
                        _ => assert!(false, "Statement was not a ReturnStatement."),
                    };
                },
                None => assert!(false, "Program could not be parsed."),
            };
        }
    }

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parser_errors(&parser);

        match program {
            Some(p) => {
                assert_eq!(p.statements.len(), 1);

                match &p.statements[0] {
                    Statement::ExpressionStatement(es) => {
                        match &es.expression {
                            Some(ex) => {
                                match ex {
                                    Expression::Identifier(id) => {
                                        assert_eq!(id.value, "foobar");
                                        assert_eq!(id.token.literal, "foobar");
                                    },
                                    _ => assert!(false, "Expression was not an Identifier."),
                                };
                            },
                            None => assert!(false, "Expression could not be parsed."),
                        }
                    },
                    _ => assert!(false, "Statement was not an ExpressionStatement."),
                };
            },
            None => assert!(false, "Program could not be parsed."),
        };
    }

    #[test]
    fn test_integer_literal_expression() {
        let input = "5;";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parser_errors(&parser);

        match program {
            Some(p) => {
                assert_eq!(p.statements.len(), 1);

                match &p.statements[0] {
                    Statement::ExpressionStatement(es) => {
                        match &es.expression {
                            Some(ex) => {
                                match ex {
                                    Expression::IntegerLiteral(il) => {
                                        assert_eq!(il.value, 5);
                                        assert_eq!(il.token.literal, "5");
                                    },
                                    _ => assert!(false, "Expression was not an IntegerLiteral."),
                                };
                            },
                            None => assert!(false, "Expression could not be parsed."),
                        }
                    },
                    _ => assert!(false, "Statement was not an ExpressionStatement."),
                };
            },
            None => assert!(false, "Program could not be parsed."),
        };
    }

    #[test]
    fn test_parsing_prefix_expressions() {
        struct PrefixTest<'a> {
            input: &'a str,
            operator: &'a str,
            value: ExpressionType,
        };

        let prefix_tests = vec![
            PrefixTest { input: "!5", operator: "!", value: ExpressionType::Int(5) },
            PrefixTest { input: "-15", operator: "-", value: ExpressionType::Int(15) },
            PrefixTest { input: "!true", operator: "!", value: ExpressionType::Bool(true) },
            PrefixTest { input: "!false", operator: "!", value: ExpressionType::Bool(false) },
        ];

        for prefix_test in prefix_tests {
            let lexer = Lexer::new(prefix_test.input);
            let mut parser = Parser::new(lexer);
    
            let program = parser.parse_program();
            check_parser_errors(&parser);

            match program {
                Some(p) => {
                    assert_eq!(p.statements.len(), 1);
    
                    match &p.statements[0] {
                        Statement::ExpressionStatement(es) => {
                            match &es.expression {
                                Some(ex) => {
                                    match ex {
                                        Expression::PrefixExpression(pe) => {
                                            assert_eq!(pe.operator, prefix_test.operator);
                                            match prefix_test.value {
                                                ExpressionType::Int(i) => test_integer_literal(&pe.right, i),
                                                ExpressionType::Bool(b) => test_boolean_literal(&pe.right, b),
                                                _ => assert!(false, "Expression type was not supported."),
                                            }
                                        },
                                        _ => assert!(false, "Expression was not a PrefixExpression."),
                                    };
                                },
                                None => assert!(false, "Expression could not be parsed."),
                            }
                        },
                        _ => assert!(false, "Statement was not an ExpressionStatement."),
                    };
                },
                None => assert!(false, "Program could not be parsed."),
            };
        }
    }

    #[test]
    fn test_parsing_infix_expressions() {
        struct InfixTest<'a> {
            input: &'a str,
            left_value: ExpressionType,
            operator: &'a str,
            right_value: ExpressionType,
        };

        let infix_tests = vec![
            InfixTest { input: "5 + 5;", left_value: ExpressionType::Int(5), operator: "+", right_value: ExpressionType::Int(5) },
            InfixTest { input: "5 - 5;", left_value: ExpressionType::Int(5), operator: "-", right_value: ExpressionType::Int(5) },
            InfixTest { input: "5 * 5;", left_value: ExpressionType::Int(5), operator: "*", right_value: ExpressionType::Int(5) },
            InfixTest { input: "5 / 5;", left_value: ExpressionType::Int(5), operator: "/", right_value: ExpressionType::Int(5) },
            InfixTest { input: "5 > 5;", left_value: ExpressionType::Int(5), operator: ">", right_value: ExpressionType::Int(5) },
            InfixTest { input: "5 < 5;", left_value: ExpressionType::Int(5), operator: "<", right_value: ExpressionType::Int(5) },
            InfixTest { input: "5 == 5;", left_value: ExpressionType::Int(5), operator: "==", right_value: ExpressionType::Int(5) },
            InfixTest { input: "5 != 5;", left_value: ExpressionType::Int(5), operator: "!=", right_value: ExpressionType::Int(5) },
            InfixTest { input: "true == true", left_value: ExpressionType::Bool(true), operator: "==", right_value: ExpressionType::Bool(true) },
            InfixTest { input: "true != false", left_value: ExpressionType::Bool(true), operator: "!=", right_value: ExpressionType::Bool(false) },
            InfixTest { input: "false == false", left_value: ExpressionType::Bool(false), operator: "==", right_value: ExpressionType::Bool(false) },
        ];

        for infix_test in infix_tests {
            let lexer = Lexer::new(infix_test.input);
            let mut parser = Parser::new(lexer);
    
            let program = parser.parse_program();
            check_parser_errors(&parser);

            match program {
                Some(p) => {
                    assert_eq!(p.statements.len(), 1);
    
                    match &p.statements[0] {
                        Statement::ExpressionStatement(es) => {
                            match &es.expression {
                                Some(ex) => {
                                    match (infix_test.left_value, infix_test.right_value) {
                                        (ExpressionType::Int(l), ExpressionType::Int(r)) => test_infix_expression(ex, l, infix_test.operator, r),
                                        (ExpressionType::Bool(l), ExpressionType::Bool(r)) => test_infix_expression(ex, l, infix_test.operator, r),
                                        _ => assert!(false, "Expression types were not supported."),
                                    };
                                },
                                None => assert!(false, "Expression could not be parsed."),
                            }
                        },
                        _ => assert!(false, "Statement was not an ExpressionStatement."),
                    };
                },
                None => assert!(false, "Program could not be parsed."),
            };
        }
    }

    #[test]
    fn test_operator_precedence_parsing() {
        struct OperatorTest<'a> {
            input: &'a str,
            expected: &'a str,
        };

        let operator_tests = vec![
            OperatorTest { input: "-a * b", expected: "((-a) * b)" },
            OperatorTest { input: "!-a", expected: "(!(-a))" },
            OperatorTest { input: "a + b + c", expected: "((a + b) + c)" },
            OperatorTest { input: "a + b - c", expected: "((a + b) - c)" },
            OperatorTest { input: "a * b * c", expected: "((a * b) * c)" },
            OperatorTest { input: "a * b / c", expected: "((a * b) / c)" },
            OperatorTest { input: "a + b / c", expected: "(a + (b / c))" },
            OperatorTest { input: "a + b * c + d / e - f", expected: "(((a + (b * c)) + (d / e)) - f)" },
            OperatorTest { input: "3 + 4; -5 * 5", expected: "(3 + 4)((-5) * 5)" },
            OperatorTest { input: "5 > 4 == 3 < 4", expected: "((5 > 4) == (3 < 4))" },
            OperatorTest { input: "5 < 4 != 3 > 4", expected: "((5 < 4) != (3 > 4))" },
            OperatorTest { input: "3 + 4 * 5 == 3 * 1 + 4 * 5", expected: "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))" },
            OperatorTest { input: "true", expected: "true" },
            OperatorTest { input: "false", expected: "false" },
            OperatorTest { input: "3 > 5 == false", expected: "((3 > 5) == false)" },
            OperatorTest { input: "3 < 5 == true", expected: "((3 < 5) == true)" },
            OperatorTest { input: "1 + (2 + 3) + 4", expected: "((1 + (2 + 3)) + 4)" },
            OperatorTest { input: "(5 + 5) * 2", expected: "((5 + 5) * 2)" },
            OperatorTest { input: "2 / (5 + 5)", expected: "(2 / (5 + 5))" },
            OperatorTest { input: "-(5 + 5)", expected: "(-(5 + 5))" },
            OperatorTest { input: "!(true == true)", expected: "(!(true == true))" },
            OperatorTest { input: "a + add(b * c) + d", expected: "((a + add((b * c))) + d)" },
            OperatorTest { input: "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))", expected: "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))" },
            OperatorTest { input: "add(a + b + c * d / f + g)", expected: "add((((a + b) + ((c * d) / f)) + g))" },
        ];

        for operator_test in operator_tests {
            let lexer = Lexer::new(operator_test.input);
            let mut parser = Parser::new(lexer);
    
            let program = parser.parse_program();
            check_parser_errors(&parser);

            match program {
                Some(p) => {
                    assert_eq!(format!("{}", p), operator_test.expected);
                },
                None => assert!(false, "Program could not be parsed."),
            };
        }
    }

    #[test]
    fn test_boolean_expression() {
        struct BooleanTest<'a> {
            input: &'a str,
            expected_boolean: bool,
        };

        let boolean_tests = vec![
            BooleanTest { input: "true;", expected_boolean: true },
            BooleanTest { input: "false;", expected_boolean: false },
        ];

        for boolean_test in boolean_tests {
            let lexer = Lexer::new(boolean_test.input);
            let mut parser = Parser::new(lexer);
    
            let program = parser.parse_program();
            check_parser_errors(&parser);

            match program {
                Some(p) => {
                    assert_eq!(p.statements.len(), 1);
    
                    match &p.statements[0] {
                        Statement::ExpressionStatement(es) => {
                            match &es.expression {
                                Some(ex) => {
                                    match ex {
                                        Expression::BooleanLiteral(bl) => assert_eq!(bl.value, boolean_test.expected_boolean),
                                        _ => assert!(false, "Expression was not a Boolean."),
                                    };
                                },
                                None => assert!(false, "Expression could not be parsed."),
                            }
                        },
                        _ => assert!(false, "Statement was not an ExpressionStatement."),
                    };
                },
                None => assert!(false, "Program could not be parsed."),
            };
        }
    }

    #[test]
    fn test_if_expression() {
        let input = "if (x < y) { x }";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parser_errors(&parser);

        match program {
            Some(p) => {
                assert_eq!(p.statements.len(), 1);

                match &p.statements[0] {
                    Statement::ExpressionStatement(es) => {
                        match &es.expression {
                            Some(ex) => {
                                match ex {
                                    Expression::IfExpression(ie) => {
                                        test_infix_expression(&ie.condition, String::from("x"), "<", String::from("y"));
                                        assert_eq!(ie.consequence.statements.len(), 1);
                                        match &ie.consequence.statements[0] {
                                            Statement::ExpressionStatement(es) => {
                                                match &es.expression {
                                                    Some(exp) => test_identifier(&exp, "x"),
                                                    None => assert!(false, "Expression was not found."),
                                                };
                                            },
                                            _ => assert!(false, "Consequence statement was not an ExpressionStatement."),
                                        };
                                        if ie.alternative.is_some() {
                                            assert!(false, "Alternative should be None.");
                                        }
                                    },
                                    _ => assert!(false, "Expression was not an IfExpression."),
                                };
                            },
                            None => assert!(false, "Expression could not be parsed."),
                        }
                    },
                    _ => assert!(false, "Statement was not an ExpressionStatement."),
                };
            },
            None => assert!(false, "Program could not be parsed."),
        };
    }

    #[test]
    fn test_if_else_expression() {
        let input = "if (x < y) { x } else { y }";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parser_errors(&parser);

        match program {
            Some(p) => {
                assert_eq!(p.statements.len(), 1);

                match &p.statements[0] {
                    Statement::ExpressionStatement(es) => {
                        match &es.expression {
                            Some(ex) => {
                                match ex {
                                    Expression::IfExpression(ie) => {
                                        test_infix_expression(&ie.condition, String::from("x"), "<", String::from("y"));
                                        assert_eq!(ie.consequence.statements.len(), 1);
                                        match &ie.consequence.statements[0] {
                                            Statement::ExpressionStatement(es) => {
                                                match &es.expression {
                                                    Some(exp) => test_identifier(&exp, "x"),
                                                    None => assert!(false, "Expression was not found."),
                                                };
                                            },
                                            _ => assert!(false, "Consequence statement was not an ExpressionStatement."),
                                        };
                                        match &ie.alternative {
                                            Some(alt) => {
                                                match &alt.statements[0] {
                                                    Statement::ExpressionStatement(es) => {
                                                        match &es.expression {
                                                            Some(exp) => test_identifier(&exp, "y"),
                                                            None => assert!(false, "Expression was not found."),
                                                        };
                                                    },
                                                    _ => assert!(false, "Alternative statement was not an ExpressionStatement."),
                                                };
                                            },
                                            _ => assert!(false, "Alternative could not be parsed."),
                                        };
                                    },
                                    _ => assert!(false, "Expression was not an IfExpression."),
                                };
                            },
                            None => assert!(false, "Expression could not be parsed."),
                        }
                    },
                    _ => assert!(false, "Statement was not an ExpressionStatement."),
                };
            },
            None => assert!(false, "Program could not be parsed."),
        };
    }

    #[test]
    fn test_function_literal_parsing() {
        let input = "fn(x, y) { x + y; }";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parser_errors(&parser);

        match program {
            Some(p) => {
                assert_eq!(p.statements.len(), 1);

                match &p.statements[0] {
                    Statement::ExpressionStatement(es) => {
                        match &es.expression {
                            Some(ex) => {
                                match ex {
                                    Expression::FunctionLiteral(fl) => {
                                        assert_eq!(fl.parameters.len(), 2);
                                        test_literal_expression(&Expression::Identifier(fl.parameters[0].clone()), String::from("x"));
                                        test_literal_expression(&Expression::Identifier(fl.parameters[1].clone()), String::from("y"));
                                        assert_eq!(fl.body.statements.len(), 1);
                                        match &fl.body.statements[0] {
                                            Statement::ExpressionStatement(bes) => {
                                                match &bes.expression {
                                                    Some(bes_exp) => test_infix_expression(&bes_exp, String::from("x"), "+", String::from("y")),
                                                    None => assert!(false, "Body expression could not be parsed."),
                                                };
                                            },
                                            _ => assert!(false, "Body statement was not an ExpressionStatement."),
                                        }
                                    },
                                    _ => assert!(false, "Expression was not a FunctionLiteral."),
                                };
                            },
                            None => assert!(false, "Expression could not be parsed."),
                        }
                    },
                    _ => assert!(false, "Statement was not an ExpressionStatement."),
                };
            },
            None => assert!(false, "Program could not be parsed."),
        };
    }

    #[test]
    fn test_function_parameter_parsing() {
        struct ParameterTest<'a> {
            input: &'a str,
            expected_parameters: Vec<&'a str>,
        };

        let parameter_tests = vec![
            ParameterTest { input: "fn() {};", expected_parameters: vec![] },
            ParameterTest { input: "fn(x) {};", expected_parameters: vec!["x"] },
            ParameterTest { input: "fn(x, y, z) {};", expected_parameters: vec!["x", "y", "z"] },
        ];

        for parameter_test in parameter_tests {
            let lexer = Lexer::new(parameter_test.input);
            let mut parser = Parser::new(lexer);
    
            let program = parser.parse_program();
            check_parser_errors(&parser);

            match program {
                Some(p) => {
                    match &p.statements[0] {
                        Statement::ExpressionStatement(es) => {
                            match &es.expression {
                                Some(ex) => {
                                    match ex {
                                        Expression::FunctionLiteral(fl) => {
                                            assert_eq!(fl.parameters.len(), parameter_test.expected_parameters.len());
                                            for (i, _) in parameter_test.expected_parameters.iter().enumerate() {
                                                test_literal_expression(&Expression::Identifier(fl.parameters[i].clone()), String::from(parameter_test.expected_parameters[i]));
                                            }
                                        },
                                        _ => assert!(false, "Expression was not a FunctionLiteral."),
                                    };
                                },
                                None => assert!(false, "Expression could not be parsed."),
                            }
                        },
                        _ => assert!(false, "Statement was not an ExpressionStatement."),
                    };
                },
                None => assert!(false, "Program could not be parsed."),
            };
        }
    }

    #[test]
    fn test_call_expression_parsing() {
        let input = "add(1, 2 * 3, 4 + 5);";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parser_errors(&parser);

        match program {
            Some(p) => {
                assert_eq!(p.statements.len(), 1);

                match &p.statements[0] {
                    Statement::ExpressionStatement(es) => {
                        match &es.expression {
                            Some(ex) => {
                                match ex {
                                    Expression::CallExpression(ce) => {
                                        test_identifier(&ce.function, "add");
                                        assert_eq!(ce.arguments.len(), 3);
                                        test_literal_expression(&ce.arguments[0], 1);
                                        test_infix_expression(&ce.arguments[1], 2, "*", 3);
                                        test_infix_expression(&ce.arguments[2], 4, "+", 5);
                                    },
                                    _ => assert!(false, "Expression was not a CallExpression."),
                                };
                            },
                            None => assert!(false, "Expression could not be parsed."),
                        }
                    },
                    _ => assert!(false, "Statement was not an ExpressionStatement."),
                };
            },
            None => assert!(false, "Program could not be parsed."),
        };
    }

    fn test_let_statement(statement: &Statement, name: &str) {
        assert_eq!(statement.token_literal(), "let");
        match statement {
            Statement::LetStatement(ls) => {
                assert_eq!(ls.name.value, name);
            },
            _ => assert!(false, "Statement was not a LetStatement."),
        }
    }

    fn test_integer_literal(integer_literal: &Expression, value: i64) {
        match integer_literal {
            Expression::IntegerLiteral(il) => {
                assert_eq!(il.value, value);
                assert_eq!(il.token.literal, value.to_string())
            },
            _ => assert!(false, "Expression was not an IntegerLiteral."),
        }
    }

    fn test_identifier(identifier: &Expression, value: &str) {
        match identifier {
            Expression::Identifier(id) => {
                assert_eq!(id.value, value);
                assert_eq!(id.token.literal, value);
            },
            _ => assert!(false, "Expression was not an Identifier."),
        }
    }

    fn test_literal_expression<T>(expression: &Expression, expected: T) where T: ValueType {
        match expected.get_type() {
            "i64" => test_integer_literal(expression, expected.get_i64()),
            "String" => test_identifier(expression, expected.get_string().as_str()),
            "bool" => test_boolean_literal(expression, expected.get_bool()),
            _ => assert!(false, "Expected type was not supported."),
        }
    }

    fn test_infix_expression<T>(expression: &Expression, left: T, operator: &str, right: T) where T: ValueType {
        match expression {
            Expression::InfixExpression(ie) => {
                test_literal_expression(&ie.left, left);
                assert_eq!(ie.operator, operator);    
                test_literal_expression(&ie.right, right);
            },
            _ => assert!(false, "Expression was not an InfixExpression."),
        }
    }

    fn test_boolean_literal(boolean_literal: &Expression, value: bool) {
        match boolean_literal {
            Expression::BooleanLiteral(bl) => {
                assert_eq!(bl.value, value);
                assert_eq!(bl.token.literal, value.to_string())
            },
            _ => assert!(false, "Expression was not a BooleanLiteral."),
        }
    }

    fn check_parser_errors(parser: &Parser) {
        let errors = parser.errors();
        if errors.is_empty() {
            return;
        }

        eprintln!("parser has {} errors", errors.len());
        for error in errors {
            eprintln!("parser error: {}", error);
        }
        assert!(false);
    }
}