mod ast;
mod env;
mod error;
mod lexer;
mod value;

use clap::Parser;
use std::path::PathBuf;

#[derive(Parser)]
#[command(name = "patch-rexx")]
#[command(about = "A modern REXX interpreter")]
#[command(version)]
struct Cli {
    /// REXX source file to execute
    source: Option<PathBuf>,

    /// Execute a REXX expression directly
    #[arg(short = 'e', long)]
    eval: Option<String>,

    /// Start interactive REPL
    #[arg(short, long)]
    interactive: bool,

    /// Arguments passed to the REXX program (accessible via ARG)
    #[arg(trailing_var_arg = true)]
    args: Vec<String>,
}

fn main() {
    let cli = Cli::parse();

    if let Some(expr) = &cli.eval {
        run_source(expr, &cli.args);
    } else if let Some(path) = &cli.source {
        match std::fs::read_to_string(path) {
            Ok(source) => run_source(&source, &cli.args),
            Err(e) => {
                eprintln!("patch-rexx: cannot read {}: {}", path.display(), e);
                std::process::exit(1);
            }
        }
    } else {
        // Default to REPL when no file given
        run_repl();
    }
}

fn run_source(source: &str, _args: &[String]) {
    let mut lexer = lexer::Lexer::new(source);
    match lexer.tokenize() {
        Ok(tokens) => {
            // TODO: parse and evaluate
            for tok in &tokens {
                println!("{:?}", tok.kind);
            }
        }
        Err(e) => {
            eprintln!("{}", e);
            std::process::exit(1);
        }
    }
}

fn run_repl() {
    println!("patch-rexx {} — interactive mode", env!("CARGO_PKG_VERSION"));
    println!("Type REXX statements. Use EXIT to quit.\n");

    let mut rl = match rustyline::DefaultEditor::new() {
        Ok(rl) => rl,
        Err(e) => {
            eprintln!("patch-rexx: cannot initialize line editor: {}", e);
            std::process::exit(1);
        }
    };

    let _env = env::Environment::new();

    loop {
        match rl.readline("rexx> ") {
            Ok(line) => {
                let trimmed = line.trim();
                if trimmed.is_empty() {
                    continue;
                }
                let _ = rl.add_history_entry(trimmed);
                if trimmed.eq_ignore_ascii_case("exit") {
                    break;
                }
                run_source(trimmed, &[]);
            }
            Err(
                rustyline::error::ReadlineError::Interrupted
                | rustyline::error::ReadlineError::Eof,
            ) => {
                break;
            }
            Err(e) => {
                eprintln!("patch-rexx: {}", e);
                break;
            }
        }
    }
}
