extern crate inkwell;

use inkwell::context::Context;
use std::path;

fn main() {
    // initialize
    let context = Context::create();
    let module = context.create_module("my_module");
    let builder = context.create_builder();

    // generate function prototype
    let function = module.add_function("main", context.i32_type().fn_type(&[], false), None);
    let basic_block = context.append_basic_block(&function, "entry");
    builder.position_at_end(&basic_block);

    // define main function
    let a = context.i32_type().const_int(77u64, false);
    builder.build_return(Some(&a));

    // print_to_file
    let _ = module.print_to_file(path::Path::new("compiled.ll"));
}
