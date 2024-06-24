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
    Eponymous(Identifier),
    Identified {
        identifier: Identifier,
        expr: Box<Augmented<T>>,
    },
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub enum KindExpr {
    Error,
    Type,
    Int,
    Float,
    Bool,
    // this is the kind of a generic function
    Generic {
        args: Vec<Augmented<KindExpr>>,
        returnkind: Box<Augmented<KindExpr>>,
    },
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct Identifier {
    pub identifier: Option<String>,
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
    NotEqual,
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
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub enum CaseTargetExpr {
    Error,
    Bool(bool),
    Int(BigInt),
    PatExpr(Box<Augmented<ValPatExpr>>),
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct CaseExpr {
    pub target: Box<Augmented<CaseTargetExpr>>,
    pub body: Box<Augmented<ValExpr>>,
}

#[derive(Serialize, Deserialize, Clone, Debug, AsRefStr)]
pub enum IdentifierModifier {
    Mutable,
    Nominal,
    None,
}

#[derive(Serialize, Deserialize, Clone, Debug, AsRefStr)]
pub enum ValPatExpr {
    Error,
    Ignore,
    Identifier {
        modifier: IdentifierModifier,
        identifier: Identifier,
    },
    // destructure anonymous struct
    StructLiteral(Vec<Augmented<StructItemExpr<ValPatExpr>>>),

    // destructure nominal type
    New {
        ty: Box<Augmented<ValExpr>>,
        pat: Box<Augmented<ValPatExpr>>,
    },
    // assert pattern has some type
    Typed {
        pat: Box<Augmented<ValPatExpr>>,
        ty: Box<Augmented<ValExpr>>,
    },
    Kinded {
        pat: Box<Augmented<ValPatExpr>>,
        kind: Box<Augmented<KindExpr>>,
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
    Int(BigInt),
    Bool(bool),
    Float(BigRational),
    String {
        value: Vec<u8>,
        block: bool,
    },
    FnDef {
        typarams: Option<Vec<Augmented<ValPatExpr>>>,
        params: Vec<Augmented<ValPatExpr>>,
        returnty: Option<Box<Augmented<ValExpr>>>,
        body: Box<Augmented<ValExpr>>,
    },
    Ref(Box<Augmented<ValExpr>>),
    Deref(Box<Augmented<ValExpr>>),
    // Constructs a new anonymous struct type
    StructLiteral(Vec<Augmented<StructItemExpr<ValExpr>>>),
    // Creates a new instance of a nominal type
    New {
        ty: Box<Augmented<ValExpr>>,
        val: Box<Augmented<ValExpr>>,
    },
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
    // Block
    Block(Box<Augmented<BlockExpr>>),
    // Group
    Group(Box<Augmented<ValExpr>>),
    // Inline array
    Array(Vec<Augmented<ValExpr>>),
    // A reference to a previously defined variable
    Identifier(Identifier),
    // Function application
    App {
        root: Box<Augmented<ValExpr>>,
        args: Vec<Augmented<ValExpr>>,
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
    // concretization
    Concretization {
        root: Box<Augmented<ValExpr>>,
        tyargs: Vec<Augmented<ValExpr>>,
    },
    // structs and enums
    StructTy(Vec<Augmented<StructItemExpr<ValExpr>>>),
    EnumTy(Vec<Augmented<StructItemExpr<ValExpr>>>),
    UnionTy(Vec<Augmented<StructItemExpr<ValExpr>>>),
    // generic is the equivalent of a function on the type level
    Generic {
        params: Vec<Augmented<ValPatExpr>>,
        returnkind: Option<Box<Augmented<KindExpr>>>,
        body: Box<Augmented<ValExpr>>,
    },
    // type of a function
    FnTy {
        paramtys: Vec<Augmented<ValExpr>>,
        returnty: Box<Augmented<ValExpr>>,
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
    Let {
        pat: Box<Augmented<ValPatExpr>>,
        value: Box<Augmented<ValExpr>>,
    },
    Use {
        namespace: Identifier,
    },
    IfThen {
        cond: Box<Augmented<ValExpr>>,
        then_branch: Box<Augmented<BlockExpr>>,
        else_branch: Option<Box<Augmented<ElseExpr>>>,
    },
    While {
        cond: Box<Augmented<ValExpr>>,
        body: Box<Augmented<BlockExpr>>,
    },
    For {
        pattern: Box<Augmented<ValPatExpr>>,
        range: Box<Augmented<RangeExpr>>,
        by: Option<Box<Augmented<ValExpr>>>,
        body: Box<Augmented<BlockExpr>>,
    },
    Do(Box<Augmented<ValExpr>>),
}

#[derive(Serialize, Deserialize, Clone, Debug, AsRefStr)]
pub enum FileStatement {
    Error,
    Let {
        pat: Box<Augmented<ValPatExpr>>,
        value: Box<Augmented<ValExpr>>,
    },
    Use {
        namespace: Identifier,
    },
    Namespace {
        namespace: Identifier,
        items: Vec<Augmented<FileStatement>>,
    },
}