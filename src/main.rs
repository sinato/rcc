extern crate env_logger;
extern crate inkwell;
extern crate regex;
extern crate log;

use inkwell::context::Context;
use std::{env, path, process};

mod lexer;
use lexer::lexer::lexer;
mod parser;
use parser::parser;

fn compiler(code: String) {
    // initialize
    let context = Context::create();
    let module = context.create_module("my_module");
    let builder = context.create_builder();

    // generate function prototype
    let function = module.add_function("main", context.i32_type().fn_type(&[], false), None);
    let basic_block = context.append_basic_block(&function, "entry");
    builder.position_at_end(&basic_block);

    // define main function
    let tokens = lexer(code);
    let ast = parser(tokens);
    ast.emit(&context, &builder);

    // print_to_file
    let _ = module.print_to_file(path::Path::new("compiled.ll"));
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
