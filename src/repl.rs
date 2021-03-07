use super::ast::Node;
use super::environment::Environment;
use super::evaluator::eval;
use super::lexer::Lexer;
use super::MONKEY_FACE;
use super::object::Object;
use super::parser::Parser;
use super::PROMPT;
use std::cell::RefCell;
use std::io::{stdin, stdout, Write};
use std::rc::Rc;

pub fn start() {
    let mut line = String::new();
    let env = Rc::new(RefCell::new(Environment::new(true)));

    loop {
        print!("{}", PROMPT);
        stdout().flush().expect("Output could not be written.");

        match stdin().read_line(&mut line) {
            Ok(_) => {
                if line.trim().is_empty() {
                    break;
                }

                let lexer = Lexer::new(line.as_str());
                let mut parser = Parser::new(lexer);
                let program = parser.parse_program();
                let parser_errors = parser.errors();

                if !parser_errors.is_empty() {
                    print_parser_error(&parser_errors);
                    line.clear();
                    continue;
                }

                if let Some(p) = program {
                    let evaluated = eval(&Node::Program(p), Rc::clone(&env));
                    if let Object::NonPrint = evaluated {
                        continue;
                    }
                    println!("{}", evaluated);
                }
            },
            Err(e) => eprintln!("Error: {}", e),
        };
        line.clear();
    }
}

fn print_parser_error(errors: &Vec<String>) {
    eprintln!("{}", MONKEY_FACE);
    eprintln!("Whoops! We ran into some monkey business here!");
    eprintln!(" Parser errors:");
    for error in errors {
        eprintln!("\t{}", error);
    }
}