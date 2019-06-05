use monkeyrs;
use std::io;

fn main() {
    println!("Hello! This is the Monkey programming language");
    println!("Feel free to type in commands");

    let stdin = io::stdin();
    let stdout = io::stdout();

    monkeyrs::repl::start(&mut stdin.lock(), &mut stdout.lock())
        .unwrap_or_else(|e| panic!("An error occurred: {}", e));
}
