extern crate inkwell;

use inkwell::context::Context;
use std::{env, path, process};

#[derive(Debug, PartialEq)]
enum Token {
    Num(u64),
    Op(String),
}

fn lexer(code: String) -> Vec<Token> {
    let elements = code.split(" ").collect::<Vec<&str>>();
    println!("elements: {:?}", elements);

    let mut tokens: Vec<Token> = Vec::new();
    for element in elements.iter() {
        let first_char = element
            .chars()
            .nth(0)
            .expect("Lexing error: illigal input.");
        if first_char.is_digit(10) {
            let num = element
                .parse::<u64>()
                .expect(&format!("Expect a number, but got {}", element));
            tokens.push(Token::Num(num));
        } else if first_char == '+' {
            tokens.push(Token::Op(element.to_string()));
        } else {
            panic!(format!("This token is not implemented: {:?}", element));
        }
    }
    tokens
}

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
    compiler(code);
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_lexer_add() {
        let code = String::from("10 + 20");
        let expect = vec![Token::Num(10), Token::Op(String::from("+")), Token::Num(20)];
        let actual = lexer(code);
        assert_eq!(actual, expect);
    }
}
