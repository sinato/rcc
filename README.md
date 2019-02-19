a toy compiler to learn rust/LLVM.

### Usage
```bash
# Running tests
$ cargo test -- --tset-threads=1

# compiling <code>
$ cargo build
$ ./target/debug/rcc "<code>"
```

### Example
```bash
$ cargo build
$ ./target/debug/rcc "10"
$ llvm-as compiled.ll
$ lli compiled.bc
$ echo $?  # output returned value
```
