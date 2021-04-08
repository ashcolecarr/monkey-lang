use std::env;
use monkey_lang::profiler;
use monkey_lang::repl;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() == 1 {
        println!("This is the Monkey programming language!");
        println!("Type in commands:");
        repl::start();
    }
    else {
        println!("Running profiler...");
        profiler::start(&args[1]);
    }
}