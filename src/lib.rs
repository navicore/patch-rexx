#![allow(dead_code)]
#![allow(clippy::return_self_not_must_use)]

pub mod ast;
pub mod builtins;
pub mod env;
pub mod error;
pub mod eval;
pub mod external;
pub mod lexer;
pub mod parser;
pub mod value;

#[cfg(feature = "lsp")]
pub mod lsp;
