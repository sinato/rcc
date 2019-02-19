use std::process::Command;

#[test]
fn main() {
    let status = Command::new("sh")
        .arg("-c")
        .arg("llvm-as compiled.ll; lli compiled.bc")
        .status()
        .expect("failed to execute process");
    println!("process exited with: {}", status);
    let s = status.to_string();
    assert!(status.to_string() == String::from("77"));
}
