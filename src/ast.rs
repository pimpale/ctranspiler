use lsp_types::Range;
use num_bigint::BigInt;
use num_rational::BigRational;
use serde::{Deserialize, Serialize};
use strum::AsRefStr;

use crate::builtin::Builtin;

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct Augmented<T> {
    pub range: Range,
    pub val: T,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub enum StructItemExpr<T> {
    Error,
    Eponymous(Identifier),
    Identified {
        identifier: Identifier,
        expr: Box<Augmented<T>>,
    },
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct Identifier {
    pub identifier: Option<String>,
    pub range: Range,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct Label {
    pub label: Option<String>,
    pub range: Range,
}

#[derive(Serialize, Deserialize, Clone, Debug, AsRefStr)]
pub enum ValBinaryOpKind {
    // Function call
    Pipe,
    // Math
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    // Booleans
    And,
    Or,
    // Comparison
    Equal,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    // Assign
    Assign,
    AssignAdd,
    AssignSub,
    AssignMul,
    AssignDiv,
    AssignRem,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct CaseExpr {
    pub target: Box<Augmented<Expr>>,
    pub body: Box<Augmented<Expr>>,
}

#[derive(Serialize, Deserialize, Clone, Debug, AsRefStr)]
pub enum IdentifierModifier {
    Mutable,
    Nominal,
    None,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct RangeExpr {
    pub start: Box<Augmented<Expr>>,
    pub end: Box<Augmented<Expr>>,
    pub inclusive: bool,
}

#[derive(Clone, Debug, AsRefStr, Serialize, Deserialize)]
pub enum Expr {
    // An error when parsing
    Error,
    Ignore,
    Int(BigInt),
    Bool(bool),
    Float(BigRational),
    String {
        value: Vec<u8>,
        block: bool,
    },
    FnDef {
        params: Vec<Augmented<Expr>>,
        body: Box<Augmented<Expr>>,
    },
    // Constructs a new anonymous struct type
    StructLiteral(Vec<Augmented<StructItemExpr<Expr>>>),
    // Creates a new instance of a nominal type
    New {
        ty: Box<Augmented<Expr>>,
        val: Box<Augmented<Expr>>,
    },
    // Binary operation
    BinaryOp {
        op: ValBinaryOpKind,
        left_operand: Box<Augmented<Expr>>,
        right_operand: Box<Augmented<Expr>>,
    },
    // Matches an expression to the first matching pattern and destructures it
    CaseOf {
        expr: Box<Augmented<Expr>>,
        cases: Vec<Augmented<CaseExpr>>,
    },
    // Block
    Block {
        statements: Vec<Augmented<BlockStatement>>,
        trailing_semicolon: bool,
    },
    // Group
    Group(Box<Augmented<Expr>>),
    // Inline array
    Array(Vec<Augmented<Expr>>),
    // A reference to a previously defined variable
    Identifier {
        modifier: IdentifierModifier,
        identifier: Identifier,
    },
    // a builtin
    Builtin {
        builtin: Builtin,
        level: usize,
    },
    // Function application
    App {
        root: Box<Augmented<Expr>>,
        args: Vec<Augmented<Expr>>,
    },
    // index into an array
    ArrayAccess {
        root: Box<Augmented<Expr>>,
        index: Box<Augmented<Expr>>,
    },
    // FieldAccess
    FieldAccess {
        root: Box<Augmented<Expr>>,
        field: String,
    },
    Ref(Box<Augmented<Expr>>),
    Deref(Box<Augmented<Expr>>),
    Typed {
        pat: Box<Augmented<Expr>>,
        ty: Box<Augmented<Expr>>,
    },
    ReverseTyped {
        pat: Box<Augmented<Expr>>,
        ty: Box<Augmented<Expr>>,
    },
    // structs and enums
    StructTy(Vec<Augmented<StructItemExpr<Expr>>>),
    EnumTy(Vec<Augmented<StructItemExpr<Expr>>>),
    UnionTy(Vec<Augmented<StructItemExpr<Expr>>>),
    // type of a function
    FnTy {
        param_tys: Vec<Augmented<Expr>>,
        dep_return_ty: Box<Augmented<Expr>>,
    },
    // External function/value
    Extern {
        name: Vec<u8>,
        ty: Box<Augmented<Expr>>,
    },
    // Loop
    Loop {
        body: Box<Augmented<Expr>>,
    },
    // Return
    Ret {
        label: Label,
        value: Box<Augmented<Expr>>,
    },
    // Annotated
    Annotated {
        metadata: Vec<u8>,
        significant: bool,
        value: Box<Augmented<Expr>>,
    },
    // Labeled
    Labeled {
        label: Label,
        value: Box<Augmented<Expr>>,
    },
}

#[derive(Serialize, Deserialize, Clone, Debug, AsRefStr)]
pub enum BlockStatement {
    Error,
    Annotated {
        metadata: Vec<u8>,
        significant: bool,
        value: Box<Augmented<BlockStatement>>,
    },
    Let {
        pat: Box<Augmented<Expr>>,
        value: Box<Augmented<Expr>>,
    },
    Use {
        namespace: Identifier,
    },
    Do(Box<Augmented<Expr>>),
}

#[derive(Serialize, Deserialize, Clone, Debug, AsRefStr)]
pub enum FileStatement {
    Error,
    Annotated {
        metadata: Vec<u8>,
        significant: bool,
        value: Box<Augmented<FileStatement>>,
    },
    Let {
        pat: Box<Augmented<Expr>>,
        value: Box<Augmented<Expr>>,
    },
    Use {
        namespace: Identifier,
    },
    Namespace {
        namespace: Identifier,
        items: Vec<Augmented<FileStatement>>,
    },
}
