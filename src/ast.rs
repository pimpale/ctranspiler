use lsp_types::Range;
use num_bigint::BigInt;
use num_rational::BigRational;
use serde::{Deserialize, Serialize};
use strum::AsRefStr;

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct Metadata {
    pub range: Range,
    pub significant: bool,
    pub value: Vec<u8>,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct Augmented<T> {
    pub range: Range,
    pub metadata: Vec<Metadata>,
    pub val: T
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct TypeStructItemExpr {
    pub identifier: Vec<u8>,
    pub type_expr: Box<Augmented<TypeExpr>>,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub enum TypeUnaryOpKind {
    Ref,
    UniqRef,
    Array(BigInt),
    Slice,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub enum TypeExpr {
    // An error when parsing
    Error,
    Identifier(Vec<u8>),
    Unit,
    // const literals
    Int(BigInt),
    Bool(bool),
    Float(BigRational),
    // unary ops (syntax sugar)
    UnaryOp(Box<Augmented<TypeExpr>>, Augmented<TypeUnaryOpKind>),
    // struct and enumify
    Struct(Vec<Augmented<TypeStructItemExpr>>),
    Enum(Vec<Augmented<TypeStructItemExpr>>),
    Union(Vec<Augmented<TypeStructItemExpr>>),
    // For grouping apps
    Group(Box<TypeExpr>),
    // generic
    App {
        fun: Box<Augmented<TypeExpr>>,
        arg: Box<Augmented<TypeExpr>>,
    },
}

#[derive(Serialize, Deserialize, Clone, Debug, AsRefStr)]
pub enum ValBinaryOpKind {
    // Type coercion
    Constrain,
    // Function definition
    Defun,
    // CaseOption
    CaseOption,
    // Function call
    Apply,
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
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    // Sequence
    Sequence,
    // Module Access
    ModuleAccess,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct ValStructItemExpr {
    pub identifier: Vec<u8>,
    pub type_expr: Box<Augmented<ValExprKind>>,
}

pub enum CaseTargetKind {
    Error,
    Int(BigInt),
    Bool(bool),
    Unit,
    
}

pub struct CaseExpr {
    pub type_expr: Box<ValExprKind>,
}

#[derive(Serialize, Deserialize, Clone, Debug, AsRefStr)]
pub enum ValExprKind {
    // An error when parsing
    Error,
    Unit,
    Int(BigInt),
    Bool(bool),
    Float(BigRational),
    String {
        value: Vec<u8>,
        block: bool,
    },
    // Constructs a new compound type
    StructLiteral(Vec<ValStructItemExpr>),
    // Binary operation
    BinaryOp {
        op: ValBinaryOpKind,
        left_operand: Box<ValExpr>,
        right_operand: Box<ValExpr>,
    },
    // these fields can only be used with module access
    // they signify a memory ref, uniqref, or deref
    Ref,
    UniqRef,
    Deref,
    // Matches an expression to the first matching pattern and destructures it
    CaseOf {
        expr: Box<CaseTargetExpr>,
        cases: Vec<CaseExpr>,
    },
    // Introduces new scope and label
    Group(Box<ValExpr>),
    // A reference to a previously defined variable
    Identifier(Vec<u8>),
    // a let
    Let {
        is_const: bool,
        pattern: Box<PatExpr>,
        value: Box<ValExpr>,
        body: Box<ValExpr>,
    },
    While {
        cond: Box<ValExpr>,
        body: Box<ValExpr>,
    },
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct ValExpr {
    pub range: Range,
    pub metadata: Vec<Metadata>,
    pub kind: ValExprKind,
}
