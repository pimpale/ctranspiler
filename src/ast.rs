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
    pub val: T,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub enum StructItemExpr<T> {
    Error,
    Eponymous(Vec<u8>),
    Identified {
        identifier: Vec<u8>,
        expr: Box<Augmented<T>>,
    },
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct ArgsExpr<T> {
    pub args: Vec<Augmented<T>>,
    pub terminating_bool: bool,
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
    Ref(Box<Augmented<TypeExpr>>),
    UniqRef(Box<Augmented<TypeExpr>>),
    Array(BigInt, Box<Augmented<TypeExpr>>),
    Slice(Box<Augmented<TypeExpr>>),
    // struct and enumify
    Struct(Vec<Augmented<StructItemExpr<TypeExpr>>>),
    Enum(Vec<Augmented<StructItemExpr<TypeExpr>>>),
    Union(Vec<Augmented<StructItemExpr<TypeExpr>>>),
    // For grouping apps
    Group(Box<TypeExpr>),
    // generic
    Generic {
        fun: Box<Augmented<TypeExpr>>,
        args: Box<Augmented<ArgsExpr<TypeExpr>>>,
    },
    // type of a function
    Fn {
        args: Box<Augmented<ArgsExpr<TypeExpr>>>,
        returntype: Box<Augmented<TypeExpr>>,
    },
}

#[derive(Serialize, Deserialize, Clone, Debug, AsRefStr)]
pub enum ValBinaryOpKind {
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
    // Module Access
    ModuleAccess,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub enum CaseTargetExpr {
    Error,
    Ignore,
    Unit,
    Bool(bool),
    Int(BigInt),
    Identifier(Vec<u8>),
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct CaseExpr {
    pub target: Box<Augmented<CaseTargetExpr>>,
    pub body: Box<Augmented<ValExpr>>,
}

#[derive(Serialize, Deserialize, Clone, Debug, AsRefStr)]
pub enum PlaceExpr {
    // An error when parsing
    Error,
    // A reference to a previously defined variable
    Identifier(Vec<u8>),
    StructField {
        root: Box<Augmented<PlaceExpr>>,
        field: Vec<u8>,
    },
    Deref(Box<Augmented<ValExpr>>),
    Array(BigInt, Box<Augmented<ValExpr>>),
}

#[derive(Serialize, Deserialize, Clone, Debug, AsRefStr)]
pub enum PatExpr {
    Error,
    Ignore {
        ty: Box<Augmented<TypeExpr>>,
    },
    Identifier {
        mutable: bool,
        identifier: Vec<u8>,
        ty: Box<Augmented<TypeExpr>>,
    },
    StructLiteral(Vec<Augmented<StructItemExpr<PatExpr>>>),
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct RangeExpr {
    pub start: Box<Augmented<ValExpr>>,
    pub end: Box<Augmented<ValExpr>>,
}

#[derive(Serialize, Deserialize, Clone, Debug, AsRefStr)]
pub enum ValExpr {
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
    StructLiteral(Vec<Augmented<StructItemExpr<ValExpr>>>),
    // Binary operation
    BinaryOp {
        op: ValBinaryOpKind,
        left_operand: Box<Augmented<ValExpr>>,
        right_operand: Box<Augmented<ValExpr>>,
    },
    // Matches an expression to the first matching pattern and destructures it
    CaseOf {
        expr: Box<Augmented<ValExpr>>,
        cases: Vec<Augmented<CaseExpr>>,
    },
    // Parens
    Group {
        statements: Vec<Augmented<BodyStatement>>,
        trailing_semicolon: bool,
    },
    // A reference to a previously defined variable
    Identifier(Vec<u8>),
    // Function application
    App {
        fun: Box<Augmented<ValExpr>>,
        args: Box<Augmented<ArgsExpr<ValExpr>>>,
    },
    // Lambda function
    FnDef {
        args: Box<Augmented<ArgsExpr<PatExpr>>>,
        returntype: Box<Augmented<TypeExpr>>,
        body: Box<Augmented<ValExpr>>,
    },
}

#[derive(Serialize, Deserialize, Clone, Debug, AsRefStr)]
pub enum BodyStatement {
    TypeDef {
        identifier: Vec<u8>,
        value: Box<Augmented<TypeExpr>>,
    },
    Let {
        pattern: Box<Augmented<PatExpr>>,
        value: Box<Augmented<ValExpr>>,
    },
    FnDef {
        identifier: Vec<u8>,
        args: Box<Augmented<ArgsExpr<PatExpr>>>,
        returntype: Box<Augmented<TypeExpr>>,
        body: Box<Augmented<ValExpr>>,
    },
    Do {
        value: Box<Augmented<ValExpr>>,
    },
    Set {
        pattern: Box<Augmented<PatExpr>>,
        value: Box<Augmented<ValExpr>>,
    },
    While {
        cond: Box<Augmented<ValExpr>>,
        body: Box<Augmented<ValExpr>>,
    },
    For {
        pattern: Box<Augmented<PatExpr>>,
        range: Box<Augmented<RangeExpr>>,
        body: Box<Augmented<ValExpr>>,
        by: Option<Box<Augmented<ValExpr>>>,
    },
}

#[derive(Serialize, Deserialize, Clone, Debug, AsRefStr)]
pub enum FileStatement {
    TypeDef {
        identifier: Vec<u8>,
        value: Box<Augmented<TypeExpr>>,
    },
    Let {
        is_const: bool,
        pattern: Box<Augmented<PatExpr>>,
        value: Box<Augmented<ValExpr>>,
    },
    FnDef {
        identifier: Vec<u8>,
        args: Box<Augmented<ArgsExpr<PatExpr>>>,
        returntype: Box<Augmented<TypeExpr>>,
        body: Box<Augmented<ValExpr>>,
    },
    WithPrefix {
        prefix: Vec<u8>,
        items: Vec<Augmented<FileStatement>>,
    },
}

pub struct File {
    declarations: Vec<Augmented<FileStatement>>,
}
