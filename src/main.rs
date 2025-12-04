use colored::Colorize;
use my_first_compiler::lexer_base::lexer;
use my_first_compiler::parser_base::parser;

use std::fs;
use walkdir::WalkDir;

fn main() {
    let test_root = "tests/Ch1";
    for entry in WalkDir::new(test_root)
        .into_iter()
        .filter_map(|e| e.ok())
        .filter(|e| e.path().is_file() && e.path().extension().is_some_and(|ext| ext == "c"))
    {
        let path_str = entry.path().to_str().unwrap();
        println!("==============================");
        println!("Testing file: {}", path_str.blue().bold());

        println!("Lexing file: {}", path_str.green());
        let input = fs::read_to_string(path_str).expect("failed to load test input");
        let lexer = lexer::Lexer::new(input.as_str());

        println!("Parsing tokens...");
        match parser::Parser::new(lexer).parse() {
            Err(e) => {
                println!("Parser Error: {}", e.red());
            }
            Ok(ast) => {
                println!("Parsed AST: {:?}", ast);
            }
        }
        println!("==============================\n");
    }
}
