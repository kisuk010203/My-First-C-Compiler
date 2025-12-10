use std::{fs, path::PathBuf};

use clap::Parser;
use colored::Colorize;
use compiler_core::{codegen_base::CodeGenerator, ir_base::Emitter, lexer_base, parser_base};

#[derive(Parser)]
struct Cli {
    /// Path to input C source file
    #[arg(value_name = "FILE")]
    input: PathBuf,

    /// Output assembly file
    #[arg(short, long, value_name = "FILE")]
    output: Option<PathBuf>,

    /// Stop after lexing
    #[arg(long)]
    lex_only: bool,

    /// Stop after parsing
    #[arg(long)]
    parse_only: bool,

    /// Stop after IR generation
    #[arg(long)]
    ir_only: bool,
}

fn main() {
    let cli = Cli::parse();

    if !cli.input.exists() {
        eprintln!(
            "{}: input file not found : {}",
            "Error".red().bold(),
            cli.input.display()
        );
        std::process::exit(1);
    }

    if cli.input.extension().and_then(|s| s.to_str()) != Some("c") {
        eprintln!(
            "{}: expected .c file, but found {}",
            "Error".red().bold(),
            cli.input.display()
        );
        std::process::exit(1);
    }

    let source = match fs::read_to_string(&cli.input) {
        Ok(content) => content,
        Err(err) => {
            eprintln!(
                "{}: failed to read file {}: {}",
                "Error".red().bold(),
                cli.input.display(),
                err
            );
            std::process::exit(1);
        }
    };

    let lexer = lexer_base::Lexer::new(&source);
    if cli.lex_only {
        println!("{}:", "Tokens".yellow().bold());
        for token in lexer {
            println!("{:?}", token);
        }
        return;
    }

    let ast = match parser_base::Parser::new(lexer).parse() {
        Ok(ast) => ast,
        Err(e) => {
            eprintln!(
                "{}: failed to parse file {}: {}",
                "Error".red().bold(),
                cli.input.display(),
                e
            );
            std::process::exit(1);
        }
    };

    if cli.parse_only {
        println!("{}:", "AST".yellow().bold());
        println!("{:#?}", ast);
        return;
    }

    let mut codegen = CodeGenerator::new();
    let ir_program = codegen.generate(&ast);

    if cli.ir_only {
        println!("{}:", "IR".yellow().bold());
        println!("{:#?}", ir_program);
        return;
    }

    let mut emitter = Emitter::new();
    let assembly = emitter.emit_program(&ir_program);

    let output_path = cli.output.unwrap_or_else(|| cli.input.with_extension("s"));
    if let Err(err) = fs::write(&output_path, assembly) {
        eprintln!(
            "{}: failed to write output file {}: {}",
            "Error".red().bold(),
            output_path.display(),
            err
        );
        std::process::exit(1);
    }
}
