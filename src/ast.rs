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
    PatExpr(Box<Augmented<PatExpr>>),
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct CaseExpr {
    pub target: Box<Augmented<CaseTargetExpr>>,
    pub body: Box<Augmented<Expr>>,
}

#[derive(Serialize, Deserialize, Clone, Debug, AsRefStr)]
pub enum IdentifierModifier {
    Mutable,
    Nominal,
    None,
}

#[derive(Serialize, Deserialize, Clone, Debug, AsRefStr)]
pub enum PatExpr {
    Error,
    Ignore,
    Identifier {
        modifier: IdentifierModifier,
        identifier: Identifier,
    },
    // destructure anonymous struct
    StructLiteral(Vec<Augmented<StructItemExpr<PatExpr>>>),

    // destructure nominal type
    New {
        ty: Box<Augmented<Expr>>,
        pat: Box<Augmented<PatExpr>>,
    },
    // assert pattern has some type
    Typed {
        pat: Box<Augmented<PatExpr>>,
        ty: Box<Augmented<Expr>>,
    },
    Kinded {
        pat: Box<Augmented<PatExpr>>,
        kind: Box<Augmented<KindExpr>>,
    },
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct RangeExpr {
    pub start: Box<Augmented<Expr>>,
    pub end: Box<Augmented<Expr>>,
    pub inclusive: bool,
}

#[derive(Serialize, Deserialize, Clone, Debug, AsRefStr)]
pub enum ElseExpr {
    // An error when parsing
    Error,
    Else(Box<Augmented<BlockExpr>>),
    Elif {
        cond: Box<Augmented<Expr>>,
        then_branch: Box<Augmented<BlockExpr>>,
        else_branch: Option<Box<Augmented<ElseExpr>>>,
    },
}

#[derive(Serialize, Deserialize, Clone, Debug, AsRefStr)]
pub enum Expr {
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
        typarams: Option<Vec<Augmented<PatExpr>>>,
        params: Vec<Augmented<PatExpr>>,
        returnty: Option<Box<Augmented<Expr>>>,
        body: Box<Augmented<Expr>>,
    },
    Ref(Box<Augmented<Expr>>),
    Deref(Box<Augmented<Expr>>),
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
    Block(Box<Augmented<BlockExpr>>),
    // Group
    Group(Box<Augmented<Expr>>),
    // Inline array
    Array(Vec<Augmented<Expr>>),
    // A reference to a previously defined variable
    Identifier(Identifier),
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
    // concretization
    Concretization {
        root: Box<Augmented<Expr>>,
        tyargs: Vec<Augmented<Expr>>,
    },
    // structs and enums
    StructTy(Vec<Augmented<StructItemExpr<Expr>>>),
    EnumTy(Vec<Augmented<StructItemExpr<Expr>>>),
    UnionTy(Vec<Augmented<StructItemExpr<Expr>>>),
    // generic is the equivalent of a function on the type level
    Generic {
        params: Vec<Augmented<PatExpr>>,
        returnkind: Option<Box<Augmented<KindExpr>>>,
        body: Box<Augmented<Expr>>,
    },
    // type of a function
    FnTy {
        paramtys: Vec<Augmented<Expr>>,
        returnty: Box<Augmented<Expr>>,
    },
    // External function/value
    Extern {
        name: Vec<u8>,
        ty: Box<Augmented<Expr>>,
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
        pat: Box<Augmented<PatExpr>>,
        value: Box<Augmented<Expr>>,
    },
    Use {
        namespace: Identifier,
    },
    IfThen {
        cond: Box<Augmented<Expr>>,
        then_branch: Box<Augmented<BlockExpr>>,
        else_branch: Option<Box<Augmented<ElseExpr>>>,
    },
    While {
        cond: Box<Augmented<Expr>>,
        body: Box<Augmented<BlockExpr>>,
    },
    For {
        pattern: Box<Augmented<PatExpr>>,
        range: Box<Augmented<RangeExpr>>,
        by: Option<Box<Augmented<Expr>>>,
        body: Box<Augmented<BlockExpr>>,
    },
    Do(Box<Augmented<Expr>>),
}

#[derive(Serialize, Deserialize, Clone, Debug, AsRefStr)]
pub enum FileStatement {
    Error,
    Let {
        pat: Box<Augmented<PatExpr>>,
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