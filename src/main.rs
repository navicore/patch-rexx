use clap::Parser;
use patch_rexx::{env, error, eval, lexer, parser, value};
use std::path::PathBuf;

#[derive(Parser)]
#[command(name = "rexx")]
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
        let mut environment = env::Environment::new();
        match run_line(expr, &mut environment, &[]) {
            Ok(code) => {
                if code != 0 {
                    std::process::exit(code);
                }
            }
            Err(e) => {
                eprintln!("{e}");
                std::process::exit(1);
            }
        }
    } else if let Some(path) = &cli.source {
        match std::fs::read_to_string(path) {
            Ok(source) => {
                let mut environment = env::Environment::new();
                let canonical = path.canonicalize().unwrap_or_else(|_| path.clone());
                environment.set_source_path(canonical);
                match run_line(&source, &mut environment, &cli.args) {
                    Ok(code) => {
                        if code != 0 {
                            std::process::exit(code);
                        }
                    }
                    Err(e) => {
                        eprintln!("{e}");
                        std::process::exit(1);
                    }
                }
            }
            Err(e) => {
                eprintln!("rexx: cannot read {}: {}", path.display(), e);
                std::process::exit(1);
            }
        }
    } else {
        run_repl();
    }
}

fn run_line(
    source: &str,
    environment: &mut env::Environment,
    args: &[String],
) -> error::RexxResult<i32> {
    let mut lex = lexer::Lexer::new(source);
    let tokens = lex.tokenize()?;
    let mut p = parser::Parser::new(tokens);
    let program = p.parse()?;
    let mut evaluator = eval::Evaluator::new(environment, &program);
    if !args.is_empty() {
        let joined = args.join(" ");
        evaluator.set_main_args(vec![value::RexxValue::new(joined)]);
    }
    let signal = evaluator.exec()?;
    match signal {
        eval::ExecSignal::Exit(Some(val)) => {
            let code = val.to_string().trim().parse::<i32>().unwrap_or(0);
            Ok(code)
        }
        eval::ExecSignal::Normal | eval::ExecSignal::Return(_) | eval::ExecSignal::Exit(None) => {
            Ok(0)
        }
        eval::ExecSignal::Leave(_) | eval::ExecSignal::Iterate(_) => Err(
            error::RexxDiagnostic::new(error::RexxError::InvalidLeaveIterate)
                .with_detail("LEAVE/ITERATE outside of DO loop"),
        ),
        eval::ExecSignal::Signal(label) => {
            Err(error::RexxDiagnostic::new(error::RexxError::LabelNotFound)
                .with_detail(format!("label '{label}' not found")))
        }
    }
}

fn run_repl() {
    println!("rexx {} — interactive mode", env!("CARGO_PKG_VERSION"));
    println!("Type REXX statements. Use EXIT to quit.\n");

    let mut rl = match rustyline::DefaultEditor::new() {
        Ok(rl) => rl,
        Err(e) => {
            eprintln!("rexx: cannot initialize line editor: {e}");
            std::process::exit(1);
        }
    };

    let mut environment = env::Environment::new();

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
                match run_line(trimmed, &mut environment, &[]) {
                    Ok(_) => {}
                    Err(e) => eprintln!("{e}"),
                }
            }
            Err(
                rustyline::error::ReadlineError::Interrupted | rustyline::error::ReadlineError::Eof,
            ) => {
                break;
            }
            Err(e) => {
                eprintln!("rexx: {e}");
                break;
            }
        }
    }
}
