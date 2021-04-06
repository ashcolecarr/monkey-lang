use super::ast::Node;
use super::builtins::Builtins;
use super::compiler::Compiler;
use super::GLOBALS_SIZE;
use super::lexer::Lexer;
use super::MONKEY_FACE;
use super::object::*;
use super::parser::Parser;
use super::PROMPT;
use super::symbol_table::SymbolTable;
use super::vm::VM;
use std::cell::RefCell;
use std::io::{stdin, stdout, Write};
use std::rc::Rc;

pub fn start() {
    let mut line = String::new();

    let constants: Rc<RefCell<Vec<Object>>> = Rc::new(RefCell::new(vec![]));
    let globals: Rc<RefCell<Vec<Object>>> = Rc::new(RefCell::new(vec![Object::NonPrint; GLOBALS_SIZE]));
    let builtins = Builtins::new();
    let symbol_table = Rc::new(RefCell::new(SymbolTable::new()));
    for (key, _) in &builtins.builtins {
        let index = builtins.get_index(key.as_str());
        symbol_table.borrow_mut().define_builtin(index, key.as_str());
    }

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
                    let mut compiler = Compiler::new_with_state(Rc::clone(&symbol_table), Rc::clone(&constants));
                    if let Err(e) = compiler.compile(&Node::Program(p)) {
                        eprintln!("Compilation failed:\n {}", e);
                        line.clear();
                        continue;
                    }

                    let code = compiler.bytecode();
                    *constants.borrow_mut() = code.constants.clone();
                    let mut machine = VM::new_with_globals_store(code, Rc::clone(&globals));
                    if let Err(e) = machine.run() {
                        eprintln!("Executing bytecode failed:\n {}", e);
                        line.clear();
                        continue;
                    }

                    let last_popped = machine.last_popped_stack_element();
                    println!("{}", last_popped);
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