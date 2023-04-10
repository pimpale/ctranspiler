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
    Eponymous(String),
    Identified {
        identifier: String,
        expr: Box<Augmented<T>>,
    },
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct ArgsExpr<T> {
    pub args: Vec<Augmented<T>>,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub enum KindExpr {
    Error,
    Type,
    Int,
    Float,
    Bool,
    // this is the kind of a generic function
    GenericFn {
        args: Vec<Augmented<KindExpr>>,
        returnkind: Box<Augmented<KindExpr>>,
    },
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct Identifier {
    pub identifier: Option<String>,
    pub range: Range,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub enum TypeExpr {
    // An error when parsing
    Error,
    Identifier(Identifier),
    // types
    UnitTy,
    ArrayTy,
    SliceTy,
    IntTy,
    UIntTy,
    FloatTy,
    BoolTy,
    // const literals
    Int(BigInt),
    Bool(bool),
    Float(BigRational),
    // unary ops (syntax sugar)
    Ref(Box<Augmented<TypeExpr>>),
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
        paramtys: Box<Augmented<ArgsExpr<TypeExpr>>>,
        returnty: Box<Augmented<TypeExpr>>,
    },
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub enum TypePatExpr {
    Error,
    Identifier {
        identifier: Identifier,
        kind: Option<Box<Augmented<KindExpr>>>,
    },
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
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub enum CaseTargetExpr {
    Error,
    Unit,
    Bool(bool),
    Int(BigInt),
    PatExpr(Box<Augmented<PatExpr>>),
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct CaseExpr {
    pub target: Box<Augmented<CaseTargetExpr>>,
    pub body: Box<Augmented<ValExpr>>,
}

#[derive(Serialize, Deserialize, Clone, Debug, AsRefStr)]
pub enum PatExpr {
    Error,
    Ignore,
    Identifier {
        mutable: bool,
        identifier: Identifier,
    },
    StructLiteral(Vec<Augmented<StructItemExpr<PatExpr>>>),
    Typed {
        pat: Box<Augmented<PatExpr>>,
        ty: Box<Augmented<TypeExpr>>,
    },
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
    Identifier(Identifier),
    // Concretize
    Concretize {
        root: Box<Augmented<ValExpr>>,
        tyargs: Box<Augmented<ArgsExpr<TypeExpr>>>,
    },
    // Function application
    App {
        root: Box<Augmented<ValExpr>>,
        args: Box<Augmented<ArgsExpr<ValExpr>>>,
    },
    // index into an array
    ArrayAccess {
        root: Box<Augmented<ValExpr>>,
        index: Box<Augmented<ValExpr>>,
    },
    // FieldAccess
    FieldAccess {
        root: Box<Augmented<ValExpr>>,
        field: String,
    },
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct BlockExpr {
    pub statements: Vec<Augmented<BlockStatement>>,
    pub trailing_semicolon: bool,
}

#[derive(Serialize, Deserialize, Clone, Debug, AsRefStr)]
pub enum BlockStatement {
    Error,
    TypeDef {
        typarams: Option<Box<Augmented<ArgsExpr<TypePatExpr>>>>,
        typat: Box<Augmented<TypePatExpr>>,
        value: Box<Augmented<TypeExpr>>,
    },
    ValDef {
        typarams: Option<Box<Augmented<ArgsExpr<TypePatExpr>>>>,
        pat: Box<Augmented<PatExpr>>,
        value: Box<Augmented<ValExpr>>,
    },
    FnDef {
        identifier: Identifier,
        typarams: Option<Box<Augmented<ArgsExpr<TypePatExpr>>>>,
        params: Box<Augmented<ArgsExpr<PatExpr>>>,
        returnty: Box<Augmented<TypeExpr>>,
        body: Box<Augmented<ValExpr>>,
    },
    Use {
        prefix: String,
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
        typarams: Option<Box<Augmented<ArgsExpr<TypePatExpr>>>>,
        typat: Box<Augmented<TypePatExpr>>,
        value: Box<Augmented<TypeExpr>>,
    },
    ValDef {
        typarams: Option<Box<Augmented<ArgsExpr<TypePatExpr>>>>,
        pat: Box<Augmented<PatExpr>>,
        value: Box<Augmented<ValExpr>>,
    },
    FnDef {
        identifier: Identifier,
        typarams: Option<Box<Augmented<ArgsExpr<TypePatExpr>>>>,
        params: Box<Augmented<ArgsExpr<PatExpr>>>,
        returnty: Box<Augmented<TypeExpr>>,
        body: Box<Augmented<BlockExpr>>,
    },
    Prefix {
        prefix: String,
        items: Vec<Augmented<FileStatement>>,
    },
    Use {
        prefix: String,
    },
}

// TODO: add imports

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct TranslationUnit {
    pub declarations: Vec<Augmented<FileStatement>>,
}
