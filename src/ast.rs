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
pub struct TypeStructItemExpr {
    pub identifier: Vec<u8>,
    pub type_expr: Box<Augmented<TypeExpr>>,
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
    Struct(Vec<Augmented<TypeStructItemExpr>>),
    Enum(Vec<Augmented<TypeStructItemExpr>>),
    Union(Vec<Augmented<TypeStructItemExpr>>),
    // For grouping apps
    Group(Box<TypeExpr>),
    // generic
    App {
        fun: Box<Augmented<TypeExpr>>,
        args: Box<Augmented<TypeArgsExpr>>,
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
pub struct ValStructItemExpr {
    pub identifier: Vec<u8>,
    pub type_expr: Box<Augmented<ValExpr>>,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub enum CaseTargetKind {
    Error,
    Int(BigInt),
    Bool(bool),
    Unit,
    Identifier(Vec<u8>),
    Ignore,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct CaseExpr {
    pub target: Box<Augmented<CaseTargetKind>>,
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

pub struct PatExpr {

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
    StructLiteral(Box<Augmented<ValStructItemExpr>>),
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
    // Introduces new scope and label
    Group(Box<ValExpr>),
    // A reference to a previously defined variable
    Identifier(Vec<u8>),
    // a let
    Let {
        is_const: bool,
        pattern: Box<Augmented<PatExpr>>,
        value: Box<Augmented<ValExpr>>,
        body: Box<Augmented<ValExpr>>,
    },
    While {
        cond: Box<Augmented<ValExpr>>,
        body: Box<Augmented<ValExpr>>,
    },
    For {
        pattern: Box<Augmented<PatExpr>>,
        range: Box<Augmented<RangeExpr>>,
        body: Box<Augmented<ValExpr>>,
    },
    App {
        fun: Box<Augmented<ValExpr>>,
        args: Box<Augmented<ValArgsExpr>>,
    },
}

pub enum BodyStatement {
    Let {
        is_const: bool,
        pattern: Box<Augmented<PatExpr>>,
        value: Box<Augmented<ValExpr>>,
    },
    Assign {
        pattern: Box<Augmented<PatExpr>>,
        value: Box<Augmented<ValExpr>>,
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
        args: Box<Augmented<PatArgsExpr>>,
        returntype: Box<Augmented<TypeExpr>>,
    },
}
