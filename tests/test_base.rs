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
