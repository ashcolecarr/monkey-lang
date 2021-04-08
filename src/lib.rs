pub mod ast;
pub mod code;
pub mod compiler;
pub mod builtins;
pub mod environment;
pub mod evaluator;
pub mod frame;
pub mod lexer;
pub mod object;
pub mod parser;
pub mod profiler;
pub mod repl;
pub mod symbol_table;
pub mod token;
pub mod vm;

pub const GLOBALS_SIZE: usize = 65536;
pub const PROMPT: &str = ">> ";
pub const MONKEY_FACE: &str = r#"           __,__
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