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
    // const literals
    Unit,
    Int(BigInt),
    Bool(bool),
    Float(BigRational),
    // unary ops (syntax sugar)
    Ref(Box<Augmented<TypeExpr>>),
    Array {
        root: Box<Augmented<TypeExpr>>,
        index: Box<Augmented<TypeExpr>>,
    },
    Slice(Box<Augmented<TypeExpr>>),
    // struct and enumify
    Struct(Vec<Augmented<StructItemExpr<TypeExpr>>>),
    Enum(Vec<Augmented<StructItemExpr<TypeExpr>>>),
    Union(Vec<Augmented<StructItemExpr<TypeExpr>>>),
    // For grouping apps
    Group(Box<Augmented<TypeExpr>>),
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
    pub inclusive: bool,
}

#[derive(Serialize, Deserialize, Clone, Debug, AsRefStr)]
pub enum ElseExpr {
    // An error when parsing
    Error,
    Else(Box<Augmented<BlockExpr>>),
    Elif {
        cond: Box<Augmented<ValExpr>>,
        then_branch: Box<Augmented<BlockExpr>>,
        else_branch: Option<Box<Augmented<ElseExpr>>>,
    },
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
    // lambda function
    Fn {
        args: Box<Augmented<ArgsExpr<TypeExpr>>>,
        returntype: Box<Augmented<TypeExpr>>,
    },
    Ref(Box<Augmented<ValExpr>>),
    Deref(Box<Augmented<ValExpr>>),
    // Constructs a new compound type
    StructLiteral(Vec<Augmented<StructItemExpr<ValExpr>>>),
    // Binary operation
    BinaryOp {
        op: ValBinaryOpKind,
        left_operand: Box<Augmented<ValExpr>>,
        right_operand: Box<Augmented<ValExpr>>,
    },
    IfThen {
        cond: Box<Augmented<ValExpr>>,
        then_branch: Box<Augmented<BlockExpr>>,
        else_branch: Option<Box<Augmented<ElseExpr>>>,
    },
    // Matches an expression to the first matching pattern and destructures it
    CaseOf {
        expr: Box<Augmented<ValExpr>>,
        cases: Vec<Augmented<CaseExpr>>,
    },
    // Block
    Block(Box<Augmented<BlockExpr>>),
    // Group
    Group(Box<Augmented<ValExpr>>),
    // Inline array
    Array(Vec<Augmented<ValExpr>>),
    // A reference to a previously defined variable
    Identifier(Vec<u8>),
    // Function application
    App {
        root: Box<Augmented<ValExpr>>,
        args: Box<Augmented<ArgsExpr<ValExpr>>>,
    },
    // index into an array
    ArrayIndex {
        root: Box<Augmented<ValExpr>>,
        index: Box<Augmented<ValExpr>>,
    },
    // FieldAccess
    FieldAccess {
        root: Box<Augmented<ValExpr>>,
        field: Vec<u8>,
    },
    // Lambda function
    FnDef {
        args: Box<Augmented<ArgsExpr<PatExpr>>>,
        returntype: Box<Augmented<TypeExpr>>,
        body: Box<Augmented<ValExpr>>,
    },
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct BlockExpr {
    statements: Vec<Augmented<BlockStatement>>,
    trailing_semicolon: bool,
}

#[derive(Serialize, Deserialize, Clone, Debug, AsRefStr)]
pub enum BlockStatement {
    Error,
    TypeDef {
        identifier: Vec<u8>,
        value: Box<Augmented<TypeExpr>>,
    },
    Use {
        prefix: Vec<u8>,
    },
    Let {
        pattern: Box<Augmented<PatExpr>>,
        value: Box<Augmented<ValExpr>>,
    },
    FnDef {
        identifier: Vec<u8>,
        args: Box<Augmented<ArgsExpr<PatExpr>>>,
        returntype: Box<Augmented<TypeExpr>>,
        body: Box<Augmented<BlockExpr>>,
    },
    Set {
        place: Box<Augmented<ValExpr>>,
        value: Box<Augmented<ValExpr>>,
    },
    While {
        cond: Box<Augmented<ValExpr>>,
        body: Box<Augmented<BlockExpr>>,
    },
    For {
        pattern: Box<Augmented<PatExpr>>,
        range: Box<Augmented<RangeExpr>>,
        by: Option<Box<Augmented<ValExpr>>>,
        body: Box<Augmented<BlockExpr>>,
    },
    Do(Box<Augmented<ValExpr>>),
}

#[derive(Serialize, Deserialize, Clone, Debug, AsRefStr)]
pub enum FileStatement {
    Error,
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
    Prefix {
        prefix: Vec<u8>,
        items: Vec<Augmented<FileStatement>>,
    },
    Use {
        prefix: Vec<u8>,
    },
}

// TODO: add imports

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct TranslationUnit {
    declarations: Vec<Augmented<FileStatement>>,
}
