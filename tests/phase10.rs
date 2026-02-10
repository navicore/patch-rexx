#![cfg(feature = "lsp")]

use patch_rexx::lsp::analysis::DocumentAnalysis;
use patch_rexx::lsp::bif_docs;

// ── Analysis: labels ────────────────────────────────────────────────

#[test]
fn analysis_finds_labels() {
    let source = "say 'hello'; myLabel:; say 'world'";
    let analysis = DocumentAnalysis::analyze(source);
    assert!(analysis.errors.is_empty(), "errors: {:?}", analysis.errors);
    assert_eq!(analysis.labels.len(), 1);
    assert_eq!(analysis.labels[0].name, "MYLABEL");
}

#[test]
fn analysis_finds_multiple_labels() {
    let source = "start:; say 'a'; helper:; say 'b'; finish:; say 'c'";
    let analysis = DocumentAnalysis::analyze(source);
    assert!(analysis.errors.is_empty(), "errors: {:?}", analysis.errors);
    assert_eq!(analysis.labels.len(), 3);
    assert_eq!(analysis.labels[0].name, "START");
    assert_eq!(analysis.labels[1].name, "HELPER");
    assert_eq!(analysis.labels[2].name, "FINISH");
}

// ── Analysis: subroutines ───────────────────────────────────────────

#[test]
fn analysis_detects_subroutine_with_procedure() {
    let source = "call myFunc; exit; myFunc:; procedure; say 'in func'; return";
    let analysis = DocumentAnalysis::analyze(source);
    assert!(analysis.errors.is_empty(), "errors: {:?}", analysis.errors);
    let subs: Vec<_> = analysis
        .subroutines
        .iter()
        .filter(|s| s.has_procedure)
        .collect();
    assert_eq!(subs.len(), 1);
    assert_eq!(subs[0].name, "MYFUNC");
}

#[test]
fn analysis_detects_procedure_expose() {
    let source = "x = 10; call myFunc; exit; myFunc:; procedure expose x; say x; return";
    let analysis = DocumentAnalysis::analyze(source);
    assert!(analysis.errors.is_empty(), "errors: {:?}", analysis.errors);
    let sub = analysis
        .subroutines
        .iter()
        .find(|s| s.name == "MYFUNC")
        .expect("MYFUNC subroutine not found");
    assert!(sub.has_procedure);
    assert_eq!(sub.exposed, vec!["X"]);
}

#[test]
fn analysis_label_without_procedure() {
    let source = "goto_here:; say 'jumped'; exit";
    let analysis = DocumentAnalysis::analyze(source);
    assert!(analysis.errors.is_empty(), "errors: {:?}", analysis.errors);
    assert_eq!(analysis.subroutines.len(), 1);
    assert!(!analysis.subroutines[0].has_procedure);
}

// ── Analysis: variables ─────────────────────────────────────────────

#[test]
fn analysis_finds_assignments() {
    let source = "x = 10; y = 20; z = x + y";
    let analysis = DocumentAnalysis::analyze(source);
    assert!(analysis.errors.is_empty(), "errors: {:?}", analysis.errors);
    let names: Vec<&str> = analysis
        .assignments
        .iter()
        .map(|v| v.name.as_str())
        .collect();
    assert!(names.contains(&"X"), "missing X in {names:?}");
    assert!(names.contains(&"Y"), "missing Y in {names:?}");
    assert!(names.contains(&"Z"), "missing Z in {names:?}");
}

#[test]
fn analysis_finds_references() {
    let source = "x = 10; say x";
    let analysis = DocumentAnalysis::analyze(source);
    let ref_names: Vec<&str> = analysis
        .references
        .iter()
        .map(|v| v.name.as_str())
        .collect();
    assert!(ref_names.contains(&"X"));
}

#[test]
fn analysis_finds_stem_variables() {
    let source = "arr.1 = 'first'; arr.2 = 'second'";
    let analysis = DocumentAnalysis::analyze(source);
    assert!(analysis.stems.contains(&"ARR.".to_string()));
}

// ── Analysis: function calls ────────────────────────────────────────

#[test]
fn analysis_finds_call_statements() {
    let source = "call myRoutine 'arg1', 'arg2'";
    let analysis = DocumentAnalysis::analyze(source);
    assert_eq!(analysis.calls.len(), 1);
    assert_eq!(analysis.calls[0].name, "MYROUTINE");
    assert_eq!(analysis.calls[0].arg_count, 2);
}

#[test]
fn analysis_finds_function_calls_in_expressions() {
    let source = "say length('hello')";
    let analysis = DocumentAnalysis::analyze(source);
    let func_calls: Vec<&str> = analysis.calls.iter().map(|c| c.name.as_str()).collect();
    assert!(func_calls.contains(&"LENGTH"));
}

// ── Analysis: parse template variables ──────────────────────────────

#[test]
fn analysis_finds_parse_template_vars() {
    let source = "parse arg first second rest";
    let analysis = DocumentAnalysis::analyze(source);
    let names: Vec<&str> = analysis
        .assignments
        .iter()
        .map(|v| v.name.as_str())
        .collect();
    assert!(names.contains(&"FIRST"));
    assert!(names.contains(&"SECOND"));
    assert!(names.contains(&"REST"));
}

#[test]
fn analysis_finds_arg_template_vars() {
    let source = "arg name age";
    let analysis = DocumentAnalysis::analyze(source);
    let names: Vec<&str> = analysis
        .assignments
        .iter()
        .map(|v| v.name.as_str())
        .collect();
    assert!(names.contains(&"NAME"));
    assert!(names.contains(&"AGE"));
}

// ── Analysis: errors ────────────────────────────────────────────────

#[test]
fn analysis_captures_lex_error() {
    let source = "say /* unterminated comment";
    let analysis = DocumentAnalysis::analyze(source);
    assert!(!analysis.errors.is_empty());
    assert!(analysis.program.is_none());
}

#[test]
fn analysis_captures_parse_error() {
    let source = "if 1 then";
    let analysis = DocumentAnalysis::analyze(source);
    assert!(!analysis.errors.is_empty());
}

#[test]
fn analysis_valid_program_no_errors() {
    let source = "say 'hello world'";
    let analysis = DocumentAnalysis::analyze(source);
    assert!(analysis.errors.is_empty());
    assert!(analysis.program.is_some());
}

// ── Analysis: DO loops ──────────────────────────────────────────────

#[test]
fn analysis_finds_controlled_loop_var() {
    let source = "do i = 1 to 10; say i; end";
    let analysis = DocumentAnalysis::analyze(source);
    assert!(analysis.errors.is_empty(), "errors: {:?}", analysis.errors);
    let names: Vec<&str> = analysis
        .assignments
        .iter()
        .map(|v| v.name.as_str())
        .collect();
    assert!(names.contains(&"I"), "missing I in {names:?}");
}

// ── BIF docs ────────────────────────────────────────────────────────

#[test]
fn bif_docs_lookup_length() {
    let bif = bif_docs::lookup_bif("LENGTH").expect("LENGTH not found");
    assert_eq!(bif.name, "LENGTH");
    assert!(bif.signature.contains("LENGTH"));
    assert!(!bif.description.is_empty());
}

#[test]
fn bif_docs_lookup_case_insensitive() {
    assert!(bif_docs::lookup_bif("substr").is_some());
    assert!(bif_docs::lookup_bif("Substr").is_some());
    assert!(bif_docs::lookup_bif("SUBSTR").is_some());
}

#[test]
fn bif_docs_all_bifs_not_empty() {
    let all = bif_docs::all_bifs();
    assert!(
        all.len() >= 40,
        "expected at least 40 BIFs, got {}",
        all.len()
    );
}

#[test]
fn bif_docs_covers_all_known_bifs() {
    let expected = [
        "LENGTH",
        "SUBSTR",
        "LEFT",
        "RIGHT",
        "POS",
        "LASTPOS",
        "INDEX",
        "COPIES",
        "REVERSE",
        "STRIP",
        "SPACE",
        "OVERLAY",
        "INSERT",
        "DELSTR",
        "TRANSLATE",
        "CHANGESTR",
        "COUNTSTR",
        "COMPARE",
        "ABBREV",
        "WORDS",
        "WORD",
        "WORDINDEX",
        "WORDLENGTH",
        "SUBWORD",
        "WORDPOS",
        "DELWORD",
        "ABS",
        "SIGN",
        "MAX",
        "MIN",
        "TRUNC",
        "FORMAT",
        "RANDOM",
        "D2C",
        "C2D",
        "D2X",
        "X2D",
        "C2X",
        "X2C",
        "B2X",
        "X2B",
        "DATATYPE",
        "VERIFY",
        "XRANGE",
        "DATE",
        "TIME",
        "CONDITION",
        "ADDRESS",
    ];
    for name in &expected {
        assert!(
            bif_docs::lookup_bif(name).is_some(),
            "BIF '{name}' missing from bif_docs"
        );
    }
}

#[test]
fn bif_docs_unknown_returns_none() {
    assert!(bif_docs::lookup_bif("NOTAFUNCTION").is_none());
}

// ── Complex analysis ────────────────────────────────────────────────

#[test]
fn analysis_complex_program() {
    let source = "/* Fibonacci */; n = 10; call fibonacci n; exit; fibonacci:; procedure expose n; arg limit; a = 0; b = 1; do i = 1 to limit; say a; c = a + b; a = b; b = c; end; return";
    let analysis = DocumentAnalysis::analyze(source);
    assert!(analysis.errors.is_empty(), "errors: {:?}", analysis.errors);
    assert!(analysis.program.is_some());

    // Should find the FIBONACCI label
    let label_names: Vec<&str> = analysis.labels.iter().map(|l| l.name.as_str()).collect();
    assert!(label_names.contains(&"FIBONACCI"));

    // Should detect FIBONACCI as subroutine with PROCEDURE
    let fib_sub = analysis
        .subroutines
        .iter()
        .find(|s| s.name == "FIBONACCI")
        .expect("FIBONACCI subroutine not found");
    assert!(fib_sub.has_procedure);
    assert_eq!(fib_sub.exposed, vec!["N"]);

    // Should find CALL
    let call_names: Vec<&str> = analysis.calls.iter().map(|c| c.name.as_str()).collect();
    assert!(call_names.contains(&"FIBONACCI"));

    // Should find variables
    let var_names: Vec<&str> = analysis
        .assignments
        .iter()
        .map(|v| v.name.as_str())
        .collect();
    assert!(var_names.contains(&"N"));
    assert!(var_names.contains(&"A"));
    assert!(var_names.contains(&"B"));
}

#[test]
fn analysis_select_program() {
    let source = "x = 2; select; when x = 1 then say 'one'; when x = 2 then say 'two'; otherwise say 'other'; end";
    let analysis = DocumentAnalysis::analyze(source);
    assert!(analysis.errors.is_empty(), "errors: {:?}", analysis.errors);
    assert!(analysis.program.is_some());
}

#[test]
fn analysis_nested_do_loops() {
    let source = "do i = 1 to 3; do j = 1 to 3; say i '*' j '=' i * j; end; end";
    let analysis = DocumentAnalysis::analyze(source);
    assert!(analysis.errors.is_empty(), "errors: {:?}", analysis.errors);
    let var_names: Vec<&str> = analysis
        .assignments
        .iter()
        .map(|v| v.name.as_str())
        .collect();
    assert!(var_names.contains(&"I"));
    assert!(var_names.contains(&"J"));
}
