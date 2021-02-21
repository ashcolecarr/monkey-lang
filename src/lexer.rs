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
            '"' => {
                match self.read_string() {
                    Some(s) => {
                        literal = s;
                        TokenType::String
                    },
                    None => TokenType::Illegal,
                }
            },
            '[' => TokenType::LBracket,
            ']' => TokenType::RBracket,
            ':' => TokenType::Colon,
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
        if !literal.is_empty() || token_type == TokenType::String {
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

    fn read_string(&mut self) -> Option<String> {
        let mut output = String::new();
        loop {
            self.read_char();
            match self.ch {
                '"' => {
                    self.read_char();
                    break
                },
                '\\' => {
                    // Capture an escaped character.
                    match self.peek_char() {
                        '"' => {
                            output.push('"');
                            self.read_char();
                        },
                        't' => {
                            output.push('\t');
                            self.read_char();
                        },
                        'n' => {
                            output.push('\n');
                            self.read_char();
                        },
                        _ => (),
                    };
                    continue;
                }
                '\0' => return None,
                _ => (),
            };

            output.push(self.ch);
        }

        Some(output)
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
    use super::super::token::TokenType;

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
10 != 9;
"foobar"
"foo bar"
"hello \"world\""
"hello\n world"
"hello\t\t\tworld"
[1, 2];
{"foo": "bar"}"#);

        struct TokenTest {
            expected_type: TokenType,
            expected_literal: String,
        };

        let token_tests = vec![
            TokenTest { expected_type: TokenType::Let, expected_literal: String::from("let") }, 
            TokenTest { expected_type: TokenType::Ident, expected_literal: String::from("five") },
            TokenTest { expected_type: TokenType::Assign, expected_literal: String::from("=") },
            TokenTest { expected_type: TokenType::Int, expected_literal: String::from("5") },
            TokenTest { expected_type: TokenType::Semicolon, expected_literal: String::from(";") },
            TokenTest { expected_type: TokenType::Let, expected_literal: String::from("let") },
            TokenTest { expected_type: TokenType::Ident, expected_literal: String::from("ten") },
            TokenTest { expected_type: TokenType::Assign, expected_literal: String::from("=") },
            TokenTest { expected_type: TokenType::Int, expected_literal: String::from("10") },
            TokenTest { expected_type: TokenType::Semicolon, expected_literal: String::from(";") },
            TokenTest { expected_type: TokenType::Let, expected_literal: String::from("let") },
            TokenTest { expected_type: TokenType::Ident, expected_literal: String::from("add") },
            TokenTest { expected_type: TokenType::Assign, expected_literal: String::from("=") },
            TokenTest { expected_type: TokenType::Function, expected_literal: String::from("fn") },
            TokenTest { expected_type: TokenType::LParen, expected_literal: String::from("(") },
            TokenTest { expected_type: TokenType::Ident, expected_literal: String::from("x") },
            TokenTest { expected_type: TokenType::Comma, expected_literal: String::from(",") },
            TokenTest { expected_type: TokenType::Ident, expected_literal: String::from("y") },
            TokenTest { expected_type: TokenType::RParen, expected_literal: String::from(")") },
            TokenTest { expected_type: TokenType::LBrace, expected_literal: String::from("{") },
            TokenTest { expected_type: TokenType::Ident, expected_literal: String::from("x") },
            TokenTest { expected_type: TokenType::Plus, expected_literal: String::from("+") },
            TokenTest { expected_type: TokenType::Ident, expected_literal: String::from("y") },
            TokenTest { expected_type: TokenType::Semicolon, expected_literal: String::from(";") },
            TokenTest { expected_type: TokenType::RBrace, expected_literal: String::from("}") },
            TokenTest { expected_type: TokenType::Semicolon, expected_literal: String::from(";") },
            TokenTest { expected_type: TokenType::Let, expected_literal: String::from("let") },
            TokenTest { expected_type: TokenType::Ident, expected_literal: String::from("result") },
            TokenTest { expected_type: TokenType::Assign, expected_literal: String::from("=") },
            TokenTest { expected_type: TokenType::Ident, expected_literal: String::from("add") },
            TokenTest { expected_type: TokenType::LParen, expected_literal: String::from("(") },
            TokenTest { expected_type: TokenType::Ident, expected_literal: String::from("five") },
            TokenTest { expected_type: TokenType::Comma, expected_literal: String::from(",") },
            TokenTest { expected_type: TokenType::Ident, expected_literal: String::from("ten") },
            TokenTest { expected_type: TokenType::RParen, expected_literal: String::from(")") },
            TokenTest { expected_type: TokenType::Semicolon, expected_literal: String::from(";") },
            TokenTest { expected_type: TokenType::Bang, expected_literal: String::from("!") },
            TokenTest { expected_type: TokenType::Minus, expected_literal: String::from("-") },
            TokenTest { expected_type: TokenType::Slash, expected_literal: String::from("/") },
            TokenTest { expected_type: TokenType::Asterisk, expected_literal: String::from("*") },
            TokenTest { expected_type: TokenType::Int, expected_literal: String::from("5") },
            TokenTest { expected_type: TokenType::Semicolon, expected_literal: String::from(";") },
            TokenTest { expected_type: TokenType::Int, expected_literal: String::from("5") },
            TokenTest { expected_type: TokenType::Lt, expected_literal: String::from("<") },
            TokenTest { expected_type: TokenType::Int, expected_literal: String::from("10") },
            TokenTest { expected_type: TokenType::Gt, expected_literal: String::from(">") },
            TokenTest { expected_type: TokenType::Int, expected_literal: String::from("5") },
            TokenTest { expected_type: TokenType::Semicolon, expected_literal: String::from(";") },
            TokenTest { expected_type: TokenType::If, expected_literal: String::from("if") },
            TokenTest { expected_type: TokenType::LParen, expected_literal: String::from("(") },
            TokenTest { expected_type: TokenType::Int, expected_literal: String::from("5") },
            TokenTest { expected_type: TokenType::Lt, expected_literal: String::from("<") },
            TokenTest { expected_type: TokenType::Int, expected_literal: String::from("10") },
            TokenTest { expected_type: TokenType::RParen, expected_literal: String::from(")") },
            TokenTest { expected_type: TokenType::LBrace, expected_literal: String::from("{") },
            TokenTest { expected_type: TokenType::Return, expected_literal: String::from("return") },
            TokenTest { expected_type: TokenType::True, expected_literal: String::from("true") },
            TokenTest { expected_type: TokenType::Semicolon, expected_literal: String::from(";") },
            TokenTest { expected_type: TokenType::RBrace, expected_literal: String::from("}") },
            TokenTest { expected_type: TokenType::Else, expected_literal: String::from("else") },
            TokenTest { expected_type: TokenType::LBrace, expected_literal: String::from("{") },
            TokenTest { expected_type: TokenType::Return, expected_literal: String::from("return") },
            TokenTest { expected_type: TokenType::False, expected_literal: String::from("false") },
            TokenTest { expected_type: TokenType::Semicolon, expected_literal: String::from(";") },
            TokenTest { expected_type: TokenType::RBrace, expected_literal: String::from("}") },
            TokenTest { expected_type: TokenType::Int, expected_literal: String::from("10") },
            TokenTest { expected_type: TokenType::Eq, expected_literal: String::from("==") },
            TokenTest { expected_type: TokenType::Int, expected_literal: String::from("10") },
            TokenTest { expected_type: TokenType::Semicolon, expected_literal: String::from(";") },
            TokenTest { expected_type: TokenType::Int, expected_literal: String::from("10") },
            TokenTest { expected_type: TokenType::NotEq, expected_literal: String::from("!=") },
            TokenTest { expected_type: TokenType::Int, expected_literal: String::from("9") },
            TokenTest { expected_type: TokenType::Semicolon, expected_literal: String::from(";") },
            TokenTest { expected_type: TokenType::String, expected_literal: String::from("foobar") },
            TokenTest { expected_type: TokenType::String, expected_literal: String::from("foo bar") },
            TokenTest { expected_type: TokenType::String, expected_literal: String::from("hello \"world\"") },
            TokenTest { expected_type: TokenType::String, expected_literal: String::from("hello\n world") },
            TokenTest { expected_type: TokenType::String, expected_literal: String::from("hello\t\t\tworld") },
            TokenTest { expected_type: TokenType::LBracket, expected_literal: String::from("[") },
            TokenTest { expected_type: TokenType::Int, expected_literal: String::from("1") },
            TokenTest { expected_type: TokenType::Comma, expected_literal: String::from(",") },
            TokenTest { expected_type: TokenType::Int, expected_literal: String::from("2") },
            TokenTest { expected_type: TokenType::RBracket, expected_literal: String::from("]") },
            TokenTest { expected_type: TokenType::Semicolon, expected_literal: String::from(";") },
            TokenTest { expected_type: TokenType::LBrace, expected_literal: String::from("{") },
            TokenTest { expected_type: TokenType::String, expected_literal: String::from("foo") },
            TokenTest { expected_type: TokenType::Colon, expected_literal: String::from(":") },
            TokenTest { expected_type: TokenType::String, expected_literal: String::from("bar") },
            TokenTest { expected_type: TokenType::RBrace, expected_literal: String::from("}") },
            TokenTest { expected_type: TokenType::Eof, expected_literal: String::from("\0") },
        ];

        let mut lexer = Lexer::new(input);
        for token_test in token_tests {
            let token = lexer.next_token();

            assert_eq!(token_test.expected_type, token.token_type);
            assert_eq!(token_test.expected_literal, token.literal);
        }
    }
}