use super::ast::*;
use super::lexer::Lexer;
use super::token::{Token, TokenType};

pub struct Parser {
    lexer: Lexer,
    current_token: Token,
    peek_token: Token,
    errors: Vec<String>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        Self { 
            lexer, 
            current_token: Token::new(TokenType::Illegal, String::new()),
            peek_token: Token::new(TokenType::Illegal, String::new()),
            errors: vec![],
        }
    }

    pub fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    pub fn parse_program(&mut self) -> Option<Program> {
        let mut program = Program::new();

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
            _ => None
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

        Some(Box::new(LetStatement::new(current_token, name)))
    }

    fn parse_return_statement(&mut self) -> Option<Box<dyn Statement>> {
        let current_token = self.current_token.clone();

        self.next_token();

        while !self.current_token_is(&TokenType::Semicolon) {
            self.next_token();
        }

        Some(Box::new(ReturnStatement::new(current_token)))
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

    fn get_errors(&self) -> Vec<String> {
        self.errors.clone()
    }

    fn peek_error(&mut self, token_type: &TokenType) {
        let msg = format!("Expected next token to be {}, but received {} instead", 
            token_type, self.peek_token.token_type);
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
            Some(p) => assert_eq!(p.statements.len(), 3,
                "program.statements does not contain 3 statements, but rather {}.", p.statements.len()),
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
            Some(p) => assert_eq!(p.statements.len(), 3,
                "program.statements does not contain 3 statements, but rather {}.", p.statements.len()),
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