//! REXX abstract syntax tree.
//!
//! REXX programs are sequences of clauses. Each clause is either a label,
//! an instruction, a command (string sent to the environment), or an
//! assignment.

use crate::error::SourceLoc;

/// A complete REXX program.
#[derive(Debug, Clone)]
pub struct Program {
    pub clauses: Vec<Clause>,
}

/// A single REXX clause with its source location.
#[derive(Debug, Clone)]
pub struct Clause {
    pub kind: ClauseKind,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone)]
pub enum ClauseKind {
    /// A label (e.g., `myLabel:`)
    Label(String),

    /// Variable assignment: `symbol = expr`
    Assignment { target: AssignTarget, expr: Expr },

    /// SAY expr
    Say(Expr),

    /// CALL routine [args...]
    Call { name: String, args: Vec<Expr> },

    /// DO block (many variants)
    Do(Box<DoBlock>),

    /// IF expr THEN clause [ELSE clause]
    If {
        condition: Expr,
        then_clause: Box<Clause>,
        else_clause: Option<Box<Clause>>,
    },

    /// SELECT [WHEN expr THEN clause]... [OTHERWISE clause...] END
    Select {
        when_clauses: Vec<(Expr, Vec<Clause>)>,
        otherwise: Option<Vec<Clause>>,
    },

    /// RETURN [expr]
    Return(Option<Expr>),

    /// EXIT [expr]
    Exit(Option<Expr>),

    /// ITERATE [name]
    Iterate(Option<String>),

    /// LEAVE [name]
    Leave(Option<String>),

    /// NOP
    Nop,

    /// PARSE [UPPER] source template
    Parse {
        upper: bool,
        source: ParseSource,
        template: ParseTemplate,
    },

    /// SIGNAL label | SIGNAL ON condition | SIGNAL OFF condition
    Signal(SignalAction),

    /// NUMERIC DIGITS [expr] | NUMERIC FORM ... | NUMERIC FUZZ [expr]
    Numeric(NumericSetting),

    /// ADDRESS environment [command] | ADDRESS [VALUE] expr
    Address(AddressAction),

    /// DROP name [name...]
    Drop(Vec<String>),

    /// PROCEDURE [EXPOSE name [name...]]
    Procedure(Option<Vec<String>>),

    /// PUSH [expr] — push onto external data queue
    Push(Option<Expr>),

    /// PULL [template] — pull from queue or stdin, uppercase
    Pull(Option<ParseTemplate>),

    /// QUEUE [expr] — queue onto external data queue
    Queue(Option<Expr>),

    /// TRACE setting
    Trace(Expr),

    /// INTERPRET expr
    Interpret(Expr),

    /// ARG template — shorthand for PARSE UPPER ARG template
    Arg(ParseTemplate),

    /// A command clause: expression evaluated and sent to current environment
    Command(Expr),
}

/// Assignment targets — simple variables or stem compounds.
#[derive(Debug, Clone)]
pub enum AssignTarget {
    /// Simple variable: `x = 5`
    Simple(String),
    /// Stem compound: `stem.tail = 5`
    Stem {
        stem: String,
        tail: Vec<TailElement>,
    },
}

/// Elements of a compound variable tail.
#[derive(Debug, Clone)]
pub enum TailElement {
    Const(String),
    Var(String),
}

/// DO block variants.
#[derive(Debug, Clone)]
pub struct DoBlock {
    pub kind: DoKind,
    pub body: Vec<Clause>,
    /// Optional label name for LEAVE/ITERATE targeting.
    pub name: Option<String>,
}

#[derive(Debug, Clone)]
pub enum DoKind {
    /// DO; ... END (simple grouping)
    Simple,
    /// DO FOREVER; ... END
    Forever,
    /// DO expr; ... END (counted loop)
    Count(Expr),
    /// DO WHILE expr; ... END
    While(Expr),
    /// DO UNTIL expr; ... END
    Until(Expr),
    /// DO var = start TO end [BY step] [FOR count]; ... END
    Controlled(Box<ControlledLoop>),
}

/// Controlled DO loop parameters.
#[derive(Debug, Clone)]
pub struct ControlledLoop {
    pub var: String,
    pub start: Expr,
    pub to: Option<Expr>,
    pub by: Option<Expr>,
    pub r#for: Option<Expr>,
    pub while_cond: Option<Expr>,
    pub until_cond: Option<Expr>,
}

/// PARSE sources.
#[derive(Debug, Clone)]
pub enum ParseSource {
    /// PARSE ARG — subroutine arguments
    Arg,
    /// PARSE PULL — data queue or stdin
    Pull,
    /// PARSE SOURCE — program source info
    Source,
    /// PARSE VERSION — interpreter version
    Version,
    /// PARSE LINEIN — read from stdin
    LineIn,
    /// PARSE VALUE expr WITH — expression result
    Value(Expr),
    /// PARSE VAR name — variable contents
    Var(String),
}

/// PARSE template — a sequence of targets and patterns.
#[derive(Debug, Clone)]
pub struct ParseTemplate {
    pub elements: Vec<TemplateElement>,
}

#[derive(Debug, Clone)]
pub enum TemplateElement {
    /// A variable name to receive data.
    Variable(String),
    /// A literal string pattern to match.
    Literal(String),
    /// An absolute column position.
    AbsolutePos(Expr),
    /// A relative column position (+ or -).
    RelativePos(i32),
    /// A variable holding a pattern string.
    VariablePattern(String),
    /// The dot placeholder (discard data).
    Dot,
    /// Comma separating multiple argument strings.
    Comma,
}

/// SIGNAL variants.
#[derive(Debug, Clone)]
pub enum SignalAction {
    /// SIGNAL label
    Label(String),
    /// SIGNAL VALUE expr
    Value(Expr),
    /// SIGNAL ON condition [NAME label]
    On {
        condition: Condition,
        name: Option<String>,
    },
    /// SIGNAL OFF condition
    Off(Condition),
}

/// Trappable conditions.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Condition {
    Error,
    Failure,
    Halt,
    NoValue,
    NotReady,
    Syntax,
    LostDigits,
}

/// NUMERIC instruction settings.
#[derive(Debug, Clone)]
pub enum NumericSetting {
    Digits(Option<Expr>),
    Form(NumericFormSetting),
    Fuzz(Option<Expr>),
}

#[derive(Debug, Clone)]
pub enum NumericFormSetting {
    Scientific,
    Engineering,
    Value(Expr),
}

/// ADDRESS instruction actions.
#[derive(Debug, Clone)]
pub enum AddressAction {
    /// ADDRESS environment — set default
    SetEnvironment(String),
    /// ADDRESS environment command — one-shot
    Temporary { environment: String, command: Expr },
    /// ADDRESS VALUE expr — dynamic environment name
    Value(Expr),
}

/// Expressions.
#[derive(Debug, Clone)]
pub enum Expr {
    /// String literal
    StringLit(String),
    /// Number literal (stored as string per REXX semantics)
    Number(String),
    /// Variable reference
    Symbol(String),
    /// Compound variable: stem.tail
    Compound {
        stem: String,
        tail: Vec<TailElement>,
    },
    /// Binary operation
    BinOp {
        left: Box<Expr>,
        op: BinOp,
        right: Box<Expr>,
    },
    /// Unary prefix operation
    UnaryOp { op: UnaryOp, operand: Box<Expr> },
    /// Function call: name(args)
    FunctionCall { name: String, args: Vec<Expr> },
    /// Parenthesized expression
    Paren(Box<Expr>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    IntDiv,
    Remainder,
    Power,
    Concat,      // abuttal or ||
    ConcatBlank, // implicit blank concatenation

    // Comparison
    Eq,
    NotEq,
    Gt,
    Lt,
    GtEq,
    LtEq,
    StrictEq,
    StrictNotEq,
    StrictGt,
    StrictLt,
    StrictGtEq,
    StrictLtEq,

    // Logical
    And,
    Or,
    Xor,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Plus,
    Minus,
    Not,
}
