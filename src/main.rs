use std::env;
use std::fs;

mod tibetan;
mod parser;

fn main() {
    let args: Vec<String> = env::args().collect();
    let file_path = &args[1];
    println!("In file {}", file_path);

    let contents = fs::read_to_string(file_path)
        .expect("Should have been able to read the file");

    // TODO: replace unwrap with Result
    let (_rest, result) = parser::parse(&contents).unwrap();
    println!("{result}");
}
