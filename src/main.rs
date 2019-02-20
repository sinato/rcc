extern crate inkwell;

use inkwell::context::Context;
use std::{env, path, process};

#[derive(Debug, PartialEq, Clone)]
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

#[derive(Debug, PartialEq)]
struct AstExp {
    op: Token,
    lhs: Token,
    rhs: Token,
}
#[derive(Debug, PartialEq)]
struct AstNum {
    num: Token,
}

#[derive(Debug, PartialEq)]
enum Ast {
    Exp(AstExp),
    Num(AstNum),
}

fn parser(tokens: Vec<Token>) -> Ast {
    match tokens.len() {
        1 => match tokens[0] {
            Token::Num(_) => Ast::Num(AstNum {
                num: tokens[0].clone(),
            }),
            Token::Op(_) => panic!("Not implemented"),
        },
        3 => {
            // TODO: token type validation
            let lhs = tokens[0].clone();
            let op = tokens[1].clone();
            let rhs = tokens[2].clone();
            Ast::Exp(AstExp { op, lhs, rhs })
        }
        _ => panic!("Not implemented"),
    }
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

    #[test]
    fn test_parser_num() {
        let expect = Ast::Num(AstNum {
            num: Token::Num(2434),
        });
        let tokens = vec![Token::Num(2434)];
        let actual = parser(tokens);
        assert_eq!(actual, expect);
    }

    #[test]
    #[should_panic]
    fn test_parser_num_illegal() {
        let tokens = vec![Token::Op(String::from("+"))];
        parser(tokens);
    }

    #[test]
    fn test_parser_exp() {
        let expect = Ast::Exp(AstExp {
            op: Token::Op(String::from("+")),
            lhs: Token::Num(10),
            rhs: Token::Num(20),
        });
        let tokens = vec![Token::Num(10), Token::Op(String::from("+")), Token::Num(20)];
        let actual = parser(tokens);
        assert_eq!(actual, expect);
    }

}
