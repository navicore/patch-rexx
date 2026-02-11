//! External function resolution — search filesystem for `.rexx`/`.rex` files.
//!
//! Per ANSI X3.274-1996, when a function or subroutine call cannot be resolved
//! to an internal label or built-in function, the interpreter searches for an
//! external program file. This module handles that search.

use std::path::{Path, PathBuf};

use crate::ast::Program;
use crate::error::{RexxDiagnostic, RexxError, RexxResult};
use crate::lexer::Lexer;
use crate::parser::Parser;

/// Search for an external REXX program matching `name`.
///
/// Search path (in order):
/// 1. Directory of the calling script (`source_dir`)
/// 2. Entries from `REXXPATH` environment variable (platform path separator)
/// 3. Current working directory
///
/// Candidate filenames for name `FOO`: `foo.rexx`, `foo.rex`, `FOO.rexx`, `FOO.rex`
/// (uppercase candidates only tried if different from lowercase).
///
/// Returns `Ok(Some((program, path)))` on first match, `Ok(None)` if not found.
pub fn resolve_external(
    name: &str,
    source_dir: Option<&Path>,
) -> RexxResult<Option<(Program, PathBuf)>> {
    let lower = name.to_lowercase();
    let upper = name.to_uppercase();

    let mut candidates: Vec<String> = vec![format!("{lower}.rexx"), format!("{lower}.rex")];
    if upper != lower {
        candidates.push(format!("{upper}.rexx"));
        candidates.push(format!("{upper}.rex"));
    }

    // Build search directories
    let mut search_dirs: Vec<PathBuf> = Vec::new();
    if let Some(dir) = source_dir {
        search_dirs.push(dir.to_path_buf());
    }
    if let Ok(rexxpath) = std::env::var("REXXPATH") {
        for entry in std::env::split_paths(&rexxpath) {
            if entry.is_dir() {
                search_dirs.push(entry);
            }
        }
    }
    if let Ok(cwd) = std::env::current_dir() {
        search_dirs.push(cwd);
    }

    for dir in &search_dirs {
        for candidate in &candidates {
            let full = dir.join(candidate);
            if full.is_file() {
                let source = std::fs::read_to_string(&full).map_err(|e| {
                    RexxDiagnostic::new(RexxError::SystemFailure)
                        .with_detail(format!("cannot read '{}': {e}", full.display()))
                })?;
                let mut lexer = Lexer::new(&source);
                let tokens = lexer.tokenize()?;
                let mut parser = Parser::new(tokens);
                let program = parser.parse()?;
                let canonical = full.canonicalize().unwrap_or(full);
                return Ok(Some((program, canonical)));
            }
        }
    }

    Ok(None)
}
