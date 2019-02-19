extern crate inkwell;

use inkwell::context::Context;
use std::{env, path, process};

fn compile(code: String) {
    // initialize
    let context = Context::create();
    let module = context.create_module("my_module");
    let builder = context.create_builder();

    // generate function prototype
    let function = module.add_function("main", context.i32_type().fn_type(&[], false), None);
    let basic_block = context.append_basic_block(&function, "entry");
    builder.position_at_end(&basic_block);

    // define main function
    let num = code.parse::<u64>().unwrap();
    let a = context.i32_type().const_int(num, false);
    builder.build_return(Some(&a));

    // print_to_file
    let _ = module.print_to_file(path::Path::new("compiled.ll"));
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage rcc \"<code>\"");
        process::exit(1);
    }
    let code = args[1].to_string();
    println!("code: {}", code);
    compile(code);
}
