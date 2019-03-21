use std::fs::File;
use std::io::prelude::*;
use std::process::Command;

fn run(input: &str, expect: &str) {
    // compile
    Command::new("sh")
        .arg("-c")
        .arg(format!("./target/debug/rcc \"{}\"", input))
        .status()
        .expect("process failed to execute");

    // run generated IR and get returned status code
    let status = Command::new("sh")
        .arg("-c")
        .arg("llvm-as compiled.ll; lli compiled.bc")
        .status()
        .expect("failed to execute process");

    println!("{:?} => {:?}", status.to_string(), expect);
    assert!(status.to_string() == String::from(format!("exit code: {}", expect)));
}

fn get_code(filename: &str) -> String {
    let filename = String::from("./tests/resources/") + filename;
    let mut f = File::open(filename).expect("file not found");
    let mut contents = String::new();
    f.read_to_string(&mut contents)
        .expect("somethig went wrong reading the file");
    contents
}

#[test]
fn test_one_num() {
    let code = get_code("test_one_num");
    run(&code, "10");
}

#[test]
fn test_one_num2() {
    let code = get_code("test_one_num2");
    run(&code, "22");
}

#[test]
fn test_binary_addition() {
    let code = get_code("test_binary_addition");
    run(&code, "30");
}

#[test]
fn test_binary_addition2() {
    let code = get_code("test_binary_addition2");
    run(&code, "22");
}

#[test]
fn test_binary_addition_multi_term() {
    let code = get_code("test_binary_addition_multi_term");
    run(&code, "10");
}

#[test]
fn test_prioritize_expression() {
    let code = get_code("test_prioritize_expression");
    run(&code, "7");
}

#[test]
fn test_prioritize_expression2() {
    let code = get_code("test_prioritize_expression2");
    run(&code, "25");
}

#[test]
fn test_prioritize_expression3() {
    let code = get_code("test_prioritize_expression3");
    run(&code, "14");
}

#[test]
fn test_prioritize_expression4() {
    let code = get_code("test_prioritize_expression4");
    run(&code, "14");
}

#[test]
fn test_multi_expression() {
    let code = get_code("test_multi_expression");
    run(&code, "30");
}

#[test]
fn test_multi_expression2() {
    let code = get_code("test_multi_expression2");
    run(&code, "10");
}

#[test]
fn test_compound_statement() {
    let code = get_code("test_compound_statement");
    run(&code, "36");
}

#[test]
fn test_variable() {
    let code = get_code("test_variable");
    run(&code, "24");
}

#[test]
fn test_if_true() {
    let code = get_code("test_if_true");
    run(&code, "204");
}

#[test]
fn test_if_false() {
    let code = get_code("test_if_false");
    run(&code, "4");
}

#[test]
fn test_while() {
    let code = get_code("test_while");
    run(&code, "11");
}

#[test]
fn test_function() {
    let code = get_code("test_function");
    run(&code, "10");
}
