use super::ast::Node;
use super::lexer::Lexer; 
use super::parser::Parser;
use std::io::{stdin, stdout, Write};

const PROMPT: &str = ">> ";
const MONKEY_FACE: &str = r#"           __,__
  .--.  .-"     "-.  .--.
 / .. \/  .-. .-.  \/ .. \
| |  '|  /   Y   \  |'  | |
| \   \  \ 0 | 0 /  /   / |
 \ '- ,\.-"""""""-./, -' /
  ''-' /_   ^ ^   _\ '-''
      |  \._   _./  |
      \   \ '~' /   /
       '._ '-=-' _.'
          '-----'
"#;

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

                let lexer = Lexer::new(line.clone());

                let mut parser = Parser::new(lexer);
                let program = parser.parse_program();
                let errors = parser.get_errors();
                if errors.len() > 0 {
                    print_parser_errors(&errors);
                    line.clear();
                    continue;
                }

                match program {
                    Some(p) => println!("{}", p.to_string()),
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