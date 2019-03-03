extern crate env_logger;
extern crate inkwell;
extern crate regex;
extern crate log;

use std::{env, process};

mod lexer;
use lexer::lexer::Lexer;
mod parser;
use parser::parser::parser;
use parser::emitter::Emitter;

/// BNF:
/// PROGRAM    := (STATEMENT)+
/// STATEMENT  := NODE
/// NODE       := EXPRESSION|BINDING|FIN;
/// EXPRESSION := FIN (OP FIN)+
/// BINDING    := IDEN = NODE
/// FIN        := NUM|IDEN
/// IDEN       := [\s]+
/// NUM        := [0-9]+
fn compiler(code: String) {
    let lexer = Lexer::new();
    let tokens = lexer.lex(code);
    let statements = parser(tokens);

    let mut emitter = Emitter::new();
    emitter.emit(statements);
    emitter.print_to_file("compiled.ll");
}

fn main() {
    env_logger::init();
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage rcc \"<code>\"");
        process::exit(1);
    }
    let code = args[1].to_string();
    compiler(code);
}
