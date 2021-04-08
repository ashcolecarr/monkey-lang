use super::ast::Node;
use super::compiler::Compiler;
use super::environment::Environment;
use super::evaluator::eval;
use super::lexer::Lexer;
use super::object::*;
use super::parser::Parser;
use super::vm::VM;
use std::cell::RefCell;
use std::rc::Rc;
use std::time::{Duration, Instant};

pub fn start(engine: &String) {
    println!("Engine: {}", engine);
    let result: Object;
    let duration: Duration;
    let input = r#"
let fibonacci = fn(x) {
    if (x == 0) {
        0
    } else {
        if (x == 1) {
            return 1;
        } else {
            fibonacci(x - 1) + fibonacci(x - 2);
        }
    }
};
fibonacci(10);
"#;

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();
    if let Some(p) = program {
        if engine == "vm" {
            let mut compiler = Compiler::new();
            if let Err(e) = compiler.compile(&Node::Program(p)) {
                eprintln!("Compilation failed:\n {}", e);
            }
    
            let start = Instant::now();
            let mut machine = VM::new(compiler.bytecode());
            duration = start.elapsed();
            if let Err(e) = machine.run() {
                eprintln!("Executing bytecode failed:\n {}", e);
            }

            result = machine.last_popped_stack_element();
        } else if engine == "eval" {
            let env = Rc::new(RefCell::new(Environment::new(true)));
            let start = Instant::now();
            result = eval(&Node::Program(p), Rc::clone(&env));
            duration = start.elapsed();
        } else {
            let start = Instant::now();
            result = Object::Null(Null::new());
            duration = start.elapsed();
        }

        println!("Engine: {}, Result: {}, Duration: {:?}", engine, result, duration);
    }
}