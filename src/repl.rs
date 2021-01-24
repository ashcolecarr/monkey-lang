use super::lexer::Lexer;
use super::token::TokenType;
use std::io::{stdin, stdout, Write};

pub const PROMPT: &str = ">> ";

pub fn start() {
    let mut line = String::new();
    loop {
        print!("{}", PROMPT);
        stdout().flush().expect("Output could not be written.");

        match stdin().read_line(&mut line) {
            Ok(_r) => {
                if line.trim().is_empty() {
                    break;
                }

                let mut lexer = Lexer::new(line.clone());

                loop {
                    let token = lexer.next_token();
                    if token.token_type == TokenType::Eof {
                        break;
                    }

                    println!("{}", token);
                }
            },
            Err(e) => eprintln!("Error: {}", e),
        };
        line.clear();
    }
}