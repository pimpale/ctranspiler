use num_bigint::BigInt;
use num_rational::BigRational;

use crate::ast::Augmented;
use crate::ast::StructItemExpr;
use crate::ast::CaseTargetExpr;

#[derive(Clone, Debug)]
pub enum HirPhase {
    Raw,
    SyntaxDesugared,
    NameResolved,
    TypeChecked,
    TempsGenerated,
    BorrowChecking,
    Monomorphization,
}

#[derive(Clone, Debug)]
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
        args: Vec<Augmented<TypeExpr>>,
    },
    // type of a function
    Fn {
        args: Vec<Augmented<TypeExpr>>,
        returntype: Box<Augmented<TypeExpr>>,
    },
}

#[derive(Clone, Debug)]
pub enum ValBinaryOpKind {
    // Not permitted after SyntaxDesugared
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
    Eq,
    Neq,
    Lt,
    Leq,
    Gt,
    Geq,
}

#[derive(Clone, Debug)]
pub struct CaseExpr {
    pub target: Box<Augmented<CaseTargetExpr>>,
    pub body: Box<Augmented<ValExpr>>,
}

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
pub enum ValExpr {
    // An error when parsing
    Error,
    Unit,
    Int(BigInt),
    Bool(bool),
    Float(BigRational),
    String(Vec<u8>),
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
    // Inline array
    Array(Vec<Augmented<ValExpr>>),
    // A reference to a previously defined variable
    Identifier(Vec<u8>),
    // Function application
    App {
        root: Box<Augmented<ValExpr>>,
        args: Vec<Augmented<ValExpr>>,
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
        args: Vec<Augmented<PatExpr>>,
        returntype: Box<Augmented<TypeExpr>>,
        body: Box<Augmented<ValExpr>>,
    },
}

#[derive(Clone, Debug)]
pub struct BlockExpr {
    statements: Vec<Augmented<BlockStatement>>,
    trailing_semicolon: bool,
}

#[derive(Clone, Debug)]
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
        args: Vec<Augmented<PatExpr>>,
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
        start: Box<Augmented<ValExpr>>,
        end: Box<Augmented<ValExpr>>,
        inclusive: bool,
        by: Option<Box<Augmented<ValExpr>>>,
        body: Box<Augmented<BlockExpr>>,
    },
    Do(Box<Augmented<ValExpr>>),
}

#[derive(Clone, Debug)]
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
        args: Vec<Augmented<PatExpr>>,
        returntype: Box<Augmented<TypeExpr>>,
        body: Box<Augmented<BlockExpr>>,
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

#[derive(Clone, Debug)]
pub struct TranslationUnit {
    declarations: Vec<Augmented<FileStatement>>,
    phase: HirPhase,
}
