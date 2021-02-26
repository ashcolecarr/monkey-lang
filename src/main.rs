use std::env;
use monkey_lang::repl;

fn main() {
    let args: Vec<String> = env::args().collect();

    println!("This is the Monkey programming language!");
    println!("Type in commands:");

    if args.len() == 1 {
        repl::start();
    }
    else {
        println!("N/A");
    }
}