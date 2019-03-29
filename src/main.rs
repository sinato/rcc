extern crate env_logger;
extern crate inkwell;
extern crate log;
extern crate regex;

use std::{env, process};

mod lexer;
use lexer::lexer::Lexer;
mod parser;
use parser::ast::AstProgram;
use parser::emitter::Emitter;

/// EBNF:
/// program              := function
/// function             := "int" identifier "()" "{" statement+ "}"
/// statement            := [ instruction_statement | compound_statement | if_statement | while_statement ]
/// instruction_statement:= instruction ";"
/// compound_statement   := "{" {instruction";"} "}"
/// if_statement         := "if" "(" condition_statement ")" compound_statement
/// while_statement      := "while" "(" condition_statement ")" compound_statement
/// instruction          := [ binding | return ]
/// condition_statement  := identifier condition_op val
/// return               := "return" val
/// val                  := [ fin | expression | call ]
/// fin                  := number | identifier
/// call                 := (identifier"(") ")"
/// expression           := fin {op fin}
/// op                   := [ "+" | "*" | "=" ]
/// condition_op         := "==" | "!="
/// number               := \d+(\.\d)*
/// identifier           := [a-z]+
fn compiler(code: String) {
    let lexer = Lexer::new();
    let mut tokens = lexer.lex(code);
    let ast = AstProgram::new(&mut tokens);

    let mut emitter = Emitter::new();
    emitter.emit(ast);
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
