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

#[test]
fn test_one_num() {
    run("10", "10");
}

#[test]
fn test_one_num2() {
    run("22", "22");
}

#[test]
fn test_binary_addition() {
    run("10 + 20", "30");
}

#[test]
fn test_binary_addition2() {
    run("5 + 17", "22");
}

#[test]
fn test_binary_addition_multi_term() {
    run("1 + 2 + 3 + 4", "10");
}

#[test]
fn test_prioritize_expression() {
    run("1 + 2 * 3", "7");
}

#[test]
fn test_prioritize_expression2() {
    run("1 + 2 * 3 * 4", "25");
}

#[test]
fn test_prioritize_expression3() {
    run("1 * 2 + 3 * 4", "14");
}

#[test]
fn test_prioritize_expression4() {
    run("2 + 3 * 4", "14");
}
