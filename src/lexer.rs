use super::token::{Token, TokenType};

pub struct Lexer {
    input: Vec<char>,
    position: usize,
    read_position: usize,
    ch: char,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let mut lexer = Self { 
            input: input.chars().collect(), 
            position: 0, 
            read_position: 0, 
            ch: '\0' 
        };

        lexer.read_char();

        lexer
    }

    pub fn next_token(&mut self) -> Token {
        let mut literal = String::new();

        self.skip_whitespace();

        let token_type = match self.ch {
            '=' => {
                eprintln!("Peek {}", self.peek_char());
                if self.peek_char() == '=' {
                    let ch = self.ch;
                    self.read_char();
                    literal.push(ch);
                    literal.push(self.ch);
                    self.read_char();

                    TokenType::Eq
                } else {
                    TokenType::Assign
                }
            },
            '+' => TokenType::Plus,
            '-' => TokenType::Minus,
            '!' => {
                if self.peek_char() == '=' {
                    let ch = self.ch;
                    self.read_char();
                    literal.push(ch);
                    literal.push(self.ch);
                    self.read_char();

                    TokenType::NotEq
                } else {
                    TokenType::Bang
                }
            },
            '/' => TokenType::Slash,
            '*' => TokenType::Asterisk,
            '<' => TokenType::Lt,
            '>' => TokenType::Gt,
            ';' => TokenType::Semicolon,
            ',' => TokenType::Comma,
            '(' => TokenType::LParen,
            ')' => TokenType::RParen,
            '{' => TokenType::LBrace,
            '}' => TokenType::RBrace,
            '\0' => TokenType::Eof,
            _ => {
                if self.is_letter() {
                    literal = self.read_identifier();

                    Token::lookup_ident(literal.clone())
                } else if self.is_digit() {
                    literal = self.read_number();

                    TokenType::Int
                } else {
                    TokenType::Illegal
                }
            },
        };

        // Exit early here since the identifier's characters are already consumed.
        if !literal.is_empty() {
            return Token::new(token_type, literal);
        }

        let token = Token::new(token_type, self.ch.to_string());

        self.read_char();

        token
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = '\0';
        } else {
            self.ch = self.input[self.read_position];
        }

        self.position = self.read_position;
        self.read_position += 1;
    }

    fn read_identifier(&mut self) -> String {
        let position = self.position;
        while self.is_letter() {
            self.read_char();
        }

        self.input[position..self.position].iter().collect()
    }

    fn read_number(&mut self) -> String {
        let position = self.position;
        while self.is_digit() {
            self.read_char();
        }

        self.input[position..self.position].iter().collect()
    }

    fn is_letter(&self) -> bool {
        'a' <= self.ch && self.ch <= 'z' || 'A' <= self.ch && self.ch <= 'Z' || self.ch == '_'
    }

    fn is_digit(&self) -> bool {
        '0' <= self.ch && self.ch <= '9'
    }

    fn skip_whitespace(&mut self) {
        loop {
            match self.ch {
                ' ' | '\t' | '\n' | '\r' => self.read_char(),
                _ => break,
            }
        }
    }

    fn peek_char(&self) -> char {
        if self.read_position >= self.input.len() {
            '\0'
        } else {
            self.input[self.read_position]
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use super::super::token::{Token, TokenType};

    #[test]
    fn verifying_next_token_returns_token() {
        let input = String::from(r#"let five = 5;
let ten = 10;

let add = fn(x, y) {
    x + y;
};

let result = add(five, ten);
!-/*5;
5 < 10 > 5;

if (5 < 10) {
    return true;
} else {
    return false;
}

10 == 10;
10 != 9;"#);

        let tests = vec![
            Token::new(TokenType::Let, String::from("let")), 
            Token::new(TokenType::Ident, String::from("five")),
            Token::new(TokenType::Assign, String::from("=")),
            Token::new(TokenType::Int, String::from("5")),
            Token::new(TokenType::Semicolon, String::from(";")),
            Token::new(TokenType::Let, String::from("let")),
            Token::new(TokenType::Ident, String::from("ten")),
            Token::new(TokenType::Assign, String::from("=")),
            Token::new(TokenType::Int, String::from("10")),
            Token::new(TokenType::Semicolon, String::from(";")),
            Token::new(TokenType::Let, String::from("let")),
            Token::new(TokenType::Ident, String::from("add")),
            Token::new(TokenType::Assign, String::from("=")),
            Token::new(TokenType::Function, String::from("fn")),
            Token::new(TokenType::LParen, String::from("(")),
            Token::new(TokenType::Ident, String::from("x")),
            Token::new(TokenType::Comma, String::from(",")),
            Token::new(TokenType::Ident, String::from("y")),
            Token::new(TokenType::RParen, String::from(")")),
            Token::new(TokenType::LBrace, String::from("{")),
            Token::new(TokenType::Ident, String::from("x")),
            Token::new(TokenType::Plus, String::from("+")),
            Token::new(TokenType::Ident, String::from("y")),
            Token::new(TokenType::Semicolon, String::from(";")),
            Token::new(TokenType::RBrace, String::from("}")),
            Token::new(TokenType::Semicolon, String::from(";")),
            Token::new(TokenType::Let, String::from("let")),
            Token::new(TokenType::Ident, String::from("result")),
            Token::new(TokenType::Assign, String::from("=")),
            Token::new(TokenType::Ident, String::from("add")),
            Token::new(TokenType::LParen, String::from("(")),
            Token::new(TokenType::Ident, String::from("five")),
            Token::new(TokenType::Comma, String::from(",")),
            Token::new(TokenType::Ident, String::from("ten")),
            Token::new(TokenType::RParen, String::from(")")),
            Token::new(TokenType::Semicolon, String::from(";")),
            Token::new(TokenType::Bang, String::from("!")),
            Token::new(TokenType::Minus, String::from("-")),
            Token::new(TokenType::Slash, String::from("/")),
            Token::new(TokenType::Asterisk, String::from("*")),
            Token::new(TokenType::Int, String::from("5")),
            Token::new(TokenType::Semicolon, String::from(";")),
            Token::new(TokenType::Int, String::from("5")),
            Token::new(TokenType::Lt, String::from("<")),
            Token::new(TokenType::Int, String::from("10")),
            Token::new(TokenType::Gt, String::from(">")),
            Token::new(TokenType::Int, String::from("5")),
            Token::new(TokenType::Semicolon, String::from(";")),
            Token::new(TokenType::If, String::from("if")),
            Token::new(TokenType::LParen, String::from("(")),
            Token::new(TokenType::Int, String::from("5")),
            Token::new(TokenType::Lt, String::from("<")),
            Token::new(TokenType::Int, String::from("10")),
            Token::new(TokenType::RParen, String::from(")")),
            Token::new(TokenType::LBrace, String::from("{")),
            Token::new(TokenType::Return, String::from("return")),
            Token::new(TokenType::True, String::from("true")),
            Token::new(TokenType::Semicolon, String::from(";")),
            Token::new(TokenType::RBrace, String::from("}")),
            Token::new(TokenType::Else, String::from("else")),
            Token::new(TokenType::LBrace, String::from("{")),
            Token::new(TokenType::Return, String::from("return")),
            Token::new(TokenType::False, String::from("false")),
            Token::new(TokenType::Semicolon, String::from(";")),
            Token::new(TokenType::RBrace, String::from("}")),
            Token::new(TokenType::Int, String::from("10")),
            Token::new(TokenType::Eq, String::from("==")),
            Token::new(TokenType::Int, String::from("10")),
            Token::new(TokenType::Semicolon, String::from(";")),
            Token::new(TokenType::Int, String::from("10")),
            Token::new(TokenType::NotEq, String::from("!=")),
            Token::new(TokenType::Int, String::from("9")),
            Token::new(TokenType::Semicolon, String::from(";")),
            Token::new(TokenType::Eof, String::from("\0")),
        ];

        let mut lexer = Lexer::new(input);
        for expected_token in tests {
            let token = lexer.next_token();

            assert_eq!(expected_token.token_type, token.token_type);
            assert_eq!(expected_token.literal, token.literal);
        }
    }
}