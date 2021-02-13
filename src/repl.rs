use super::ast::BaseTrait;
use super::environment::Environment;
use super::evaluator::*;
use super::lexer::Lexer; 
use super::parser::Parser;
use super::MONKEY_FACE;
use super::PROMPT;
use std::cell::RefCell;
use std::io::{stdin, stdout, Write};
use std::rc::Rc;

pub fn start() {
    let mut line = String::new();
    let env = Rc::new(RefCell::new(Environment::new()));
    loop {
        print!("{}", PROMPT);
        stdout().flush().expect("Output could not be written.");

        match stdin().read_line(&mut line) {
            Ok(_r) => {
                if line.trim().is_empty() {
                    break;
                }

                let lexer = Lexer::new(line.clone());

                let mut parser = Parser::new(lexer);
                let program = parser.parse_program();
                let errors = parser.get_errors();
                if errors.len() > 0 {
                    print_parser_errors(&errors);
                    line.clear();
                    continue;
                }

                match &program {
                    Some(p) => {
                        let evaluated = eval(Box::new(p.as_base()), Rc::clone(&env));
                        println!("{}", evaluated.inspect());
                    },
                    None => (),
                };
            },
            Err(e) => eprintln!("Error: {}", e),
        };
        line.clear();
    }
}

fn print_parser_errors(errors: &Vec<String>) {
    eprintln!("{}", MONKEY_FACE);
    eprintln!("Woops! We ran into some monkey business here!");
    eprintln!(" Parser errors:");
    for error in errors {
        eprintln!("\t{}", error);
    }
}