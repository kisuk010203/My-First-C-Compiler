use std::fs;

use compiler_core::{error::CompilerError, lexer_base, parser_base};
use walkdir::WalkDir;

fn test_file(path: &str) -> Result<(), CompilerError<parser_base::ParseError>> {
    let input = fs::read_to_string(path).expect("Given file path is not Valid");

    let lexer = lexer_base::Lexer::new(input.as_str());
    parser_base::Parser::new(lexer).parse()?;

    Ok(())
}

#[test]
fn test_valid_programs() {
    let test_dir = "tests/Ch1/valid";
    let mut test_count = 0;
    let mut failed = Vec::new();

    for entry in WalkDir::new(test_dir)
        .into_iter()
        .filter_map(|e| e.ok())
        .filter(|e| e.path().is_file() && e.path().extension().is_some_and(|ext| ext == "c"))
    {
        test_count += 1;
        let path = entry.path().to_str().unwrap();

        if let Err(e) = test_file(path) {
            failed.push(format!("  ✗ {}: {}", path, e));
        } else {
            println!("  ✓ {}", path);
        }
    }

    if !failed.is_empty() {
        panic!(
            "\n{} valid test(s) failed:\n{}\n",
            failed.len(),
            failed.join("\n")
        );
    }

    assert!(test_count > 0, "No test files found in {}", test_dir);
    println!("\n✓ All {} valid tests passed", test_count);
}

#[test]
fn test_invalid_lex() {
    let test_dir = "tests/Ch1/invalid_lex";
    let mut test_count = 0;
    let mut unexpected_pass = Vec::new();

    for entry in WalkDir::new(test_dir)
        .into_iter()
        .filter_map(|e| e.ok())
        .filter(|e| e.path().is_file() && e.path().extension().is_some_and(|ext| ext == "c"))
    {
        test_count += 1;
        let path = entry.path().to_str().unwrap();

        let input = fs::read_to_string(path).expect("Failed to read file");
        let lexer = lexer_base::Lexer::new(input.as_str());

        // Try to consume the lexer - if it succeeds, that's bad
        let tokens_result: Result<Vec<_>, _> = lexer.collect();

        if tokens_result.is_ok() {
            unexpected_pass.push(format!("  ✗ {}: Should have failed lexing", path));
        } else {
            println!("  ✓ {} (correctly rejected)", path);
        }
    }

    if !unexpected_pass.is_empty() {
        panic!(
            "\n{} invalid_lex test(s) unexpectedly passed:\n{}\n",
            unexpected_pass.len(),
            unexpected_pass.join("\n")
        );
    }

    assert!(test_count > 0, "No test files found in {}", test_dir);
    println!("\n✓ All {} invalid_lex tests passed", test_count);
}

#[test]
fn test_invalid_parse() {
    let test_dir = "tests/Ch1/invalid_parse";
    let mut test_count = 0;
    let mut unexpected_pass = Vec::new();

    for entry in WalkDir::new(test_dir)
        .into_iter()
        .filter_map(|e| e.ok())
        .filter(|e| e.path().is_file() && e.path().extension().is_some_and(|ext| ext == "c"))
    {
        test_count += 1;
        let path = entry.path().to_str().unwrap();

        let input = fs::read_to_string(path).expect("Failed to read file");
        let lexer = lexer_base::Lexer::new(input.as_str());

        match parser_base::Parser::new(lexer).parse() {
            Ok(_) => {
                unexpected_pass.push(format!("  ✗ {}: Should have failed parsing", path));
            }
            Err(_) => {
                println!("  ✓ {} (correctly rejected)", path);
            }
        }
    }

    if !unexpected_pass.is_empty() {
        panic!(
            "\n{} invalid_parse test(s) unexpectedly passed:\n{}\n",
            unexpected_pass.len(),
            unexpected_pass.join("\n")
        );
    }

    assert!(test_count > 0, "No test files found in {}", test_dir);
    println!("\n✓ All {} invalid_parse tests passed", test_count);
}
