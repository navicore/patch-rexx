//! REXX variable environments — scoping, stem variables, PROCEDURE EXPOSE.
//!
//! REXX uses dynamic scoping by default (all variables in one pool),
//! with PROCEDURE creating new isolated scopes and EXPOSE selectively
//! bringing variables into scope.

use std::collections::HashMap;

use crate::value::RexxValue;

/// A variable environment (scope).
#[derive(Debug, Clone)]
pub struct Environment {
    /// Stack of variable scopes. Last is the current (innermost) scope.
    scopes: Vec<Scope>,
}

/// A single variable scope.
#[derive(Debug, Clone)]
struct Scope {
    /// Simple variables: name (uppercased) → value.
    vars: HashMap<String, RexxValue>,
    /// Stem variables: "STEM." → { tail → value }.
    stems: HashMap<String, StemVar>,
}

/// A stem variable with default and compound entries.
#[derive(Debug, Clone)]
struct StemVar {
    /// Default value when a specific tail hasn't been set.
    /// If None, compound references return their own name.
    default: Option<RexxValue>,
    /// Explicit compound entries: resolved tail → value.
    entries: HashMap<String, RexxValue>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            scopes: vec![Scope::new()],
        }
    }

    /// Get a simple variable's value. In REXX, an unset variable
    /// returns its own name in uppercase.
    ///
    /// REXX scoping: PROCEDURE creates an opaque wall. Only the
    /// current (topmost) scope is searched. EXPOSE copies specific
    /// variables into the new scope — that's the only way through.
    pub fn get(&self, name: &str) -> RexxValue {
        let upper = name.to_uppercase();
        let scope = self.scopes.last().expect("environment has no scopes");
        if let Some(val) = scope.vars.get(&upper) {
            return val.clone();
        }
        // REXX default: unset variable returns its own name
        RexxValue::new(upper)
    }

    /// Set a simple variable.
    pub fn set(&mut self, name: &str, value: RexxValue) {
        let upper = name.to_uppercase();
        self.current_scope_mut().vars.insert(upper, value);
    }

    /// Get a compound variable: stem.tail
    /// The tail components are resolved (each variable in the tail
    /// is looked up) and concatenated with dots.
    pub fn get_compound(&self, stem: &str, resolved_tail: &str) -> RexxValue {
        let stem_upper = format!("{}.", stem.to_uppercase());
        let tail_upper = resolved_tail.to_uppercase();

        let scope = self.scopes.last().expect("environment has no scopes");
        if let Some(stem_var) = scope.stems.get(&stem_upper) {
            if let Some(val) = stem_var.entries.get(&tail_upper) {
                return val.clone();
            }
            if let Some(ref default) = stem_var.default {
                return default.clone();
            }
        }
        // Default: return the compound name itself
        RexxValue::new(format!("{}{}", stem_upper, tail_upper))
    }

    /// Set a compound variable.
    pub fn set_compound(&mut self, stem: &str, resolved_tail: &str, value: RexxValue) {
        let stem_upper = format!("{}.", stem.to_uppercase());
        let tail_upper = resolved_tail.to_uppercase();

        let scope = self.current_scope_mut();
        let stem_var = scope
            .stems
            .entry(stem_upper)
            .or_insert_with(StemVar::new);
        stem_var.entries.insert(tail_upper, value);
    }

    /// Set the default value for a stem (e.g., `stem. = 0`).
    pub fn set_stem_default(&mut self, stem: &str, value: RexxValue) {
        let stem_upper = format!("{}.", stem.to_uppercase());
        let scope = self.current_scope_mut();
        let stem_var = scope
            .stems
            .entry(stem_upper)
            .or_insert_with(StemVar::new);
        stem_var.default = Some(value);
    }

    /// DROP a variable — restore it to its uninitialized state.
    pub fn drop(&mut self, name: &str) {
        let upper = name.to_uppercase();
        self.current_scope_mut().vars.remove(&upper);
    }

    /// PROCEDURE — push a new empty scope.
    pub fn push_procedure(&mut self) {
        self.scopes.push(Scope::new());
    }

    /// PROCEDURE EXPOSE — push a new scope that shares specified variables.
    pub fn push_procedure_expose(&mut self, names: &[String]) {
        let mut new_scope = Scope::new();

        for name in names {
            let upper = name.to_uppercase();
            if upper.ends_with('.') {
                // Expose a stem — copy the entire stem
                for scope in self.scopes.iter().rev() {
                    if let Some(stem_var) = scope.stems.get(&upper) {
                        new_scope.stems.insert(upper.clone(), stem_var.clone());
                        break;
                    }
                }
            } else {
                // Expose a simple variable
                for scope in self.scopes.iter().rev() {
                    if let Some(val) = scope.vars.get(&upper) {
                        new_scope.vars.insert(upper.clone(), val.clone());
                        break;
                    }
                }
            }
        }

        self.scopes.push(new_scope);
    }

    /// Pop the current scope (on RETURN from a PROCEDURE).
    pub fn pop_procedure(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
        }
    }

    /// Check if a variable has been explicitly set (for SIGNAL ON NOVALUE).
    pub fn is_set(&self, name: &str) -> bool {
        let upper = name.to_uppercase();
        self.scopes
            .last()
            .is_some_and(|s| s.vars.contains_key(&upper))
    }

    fn current_scope_mut(&mut self) -> &mut Scope {
        self.scopes.last_mut().expect("environment has no scopes")
    }
}

impl Default for Environment {
    fn default() -> Self {
        Self::new()
    }
}

impl Scope {
    fn new() -> Self {
        Self {
            vars: HashMap::new(),
            stems: HashMap::new(),
        }
    }
}

impl StemVar {
    fn new() -> Self {
        Self {
            default: None,
            entries: HashMap::new(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn unset_variable_returns_name() {
        let env = Environment::new();
        assert_eq!(env.get("foo").as_str(), "FOO");
    }

    #[test]
    fn set_and_get() {
        let mut env = Environment::new();
        env.set("name", RexxValue::new("Alice"));
        assert_eq!(env.get("name").as_str(), "Alice");
    }

    #[test]
    fn case_insensitive() {
        let mut env = Environment::new();
        env.set("Name", RexxValue::new("Bob"));
        assert_eq!(env.get("NAME").as_str(), "Bob");
        assert_eq!(env.get("name").as_str(), "Bob");
    }

    #[test]
    fn stem_variables() {
        let mut env = Environment::new();
        env.set_compound("arr", "1", RexxValue::new("first"));
        env.set_compound("arr", "2", RexxValue::new("second"));
        assert_eq!(env.get_compound("arr", "1").as_str(), "first");
        assert_eq!(env.get_compound("arr", "2").as_str(), "second");
        // Unset compound returns its name
        assert_eq!(env.get_compound("arr", "3").as_str(), "ARR.3");
    }

    #[test]
    fn stem_default() {
        let mut env = Environment::new();
        env.set_stem_default("count", RexxValue::new("0"));
        assert_eq!(env.get_compound("count", "anything").as_str(), "0");
        env.set_compound("count", "special", RexxValue::new("99"));
        assert_eq!(env.get_compound("count", "special").as_str(), "99");
        assert_eq!(env.get_compound("count", "other").as_str(), "0");
    }

    #[test]
    fn procedure_scope() {
        let mut env = Environment::new();
        env.set("x", RexxValue::new("outer"));
        env.push_procedure();
        // x is not visible in the new scope
        assert_eq!(env.get("x").as_str(), "X");
        env.set("x", RexxValue::new("inner"));
        assert_eq!(env.get("x").as_str(), "inner");
        env.pop_procedure();
        assert_eq!(env.get("x").as_str(), "outer");
    }

    #[test]
    fn procedure_expose() {
        let mut env = Environment::new();
        env.set("x", RexxValue::new("shared"));
        env.set("y", RexxValue::new("hidden"));
        env.push_procedure_expose(&["x".into()]);
        assert_eq!(env.get("x").as_str(), "shared");
        assert_eq!(env.get("y").as_str(), "Y"); // not exposed
        env.pop_procedure();
    }

    #[test]
    fn drop_variable() {
        let mut env = Environment::new();
        env.set("x", RexxValue::new("42"));
        assert!(env.is_set("x"));
        env.drop("x");
        assert!(!env.is_set("x"));
        assert_eq!(env.get("x").as_str(), "X");
    }
}
