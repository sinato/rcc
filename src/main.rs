extern crate env_logger;
extern crate inkwell;
extern crate log;
extern crate rand;
extern crate regex;

use std::{env, process};

mod lexer;
use lexer::lexer::Lexer;
mod parser;
use parser::ast::AstProgram;
use parser::emitter::Emitter;

/// EBNF:
/// program                    := function+
/// function                   := type identifier "(" (type val)* ")" "{" statement+ "}"
/// statement                  := [ instruction_statement | compound_statement | if_statement | while_statement ]
/// compound_statement         := "{" instruction_statement* "}"
/// if_statement               := "if" "(" condition_statement ")" compound_statement
/// while_statement            := "while" "(" condition_statement ")" compound_statement
/// instruction_statement      := [ binding_statement | declare_statement | return_statement ]
/// binding_statement          := identifier "=" val ";"
/// declare_statement          := [simple_declare_statement | bind_declare_statement]
/// simple_declare_statement   := type identifier ";"
/// bind_declare_statement     := type identifier "=" val ";"
/// return_statement           := "return" val ";"
/// condition_statement        := identifier condition_op val
/// val                        := [ fin | expression | call ]
/// fin                        := number | identifier
/// call                       := (identifier"(") val* ")"
/// expression                 := fin {op fin}
/// op                         := [ "+" | "*" | "=" ]
/// condition_op               := "==" | "!="
/// type                       := "int"
/// number                     := \d+(\.\d)*
/// identifier                 := [a-z]+
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
