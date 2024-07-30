use lsp_types::Range;

use crate::ast;
use crate::values::{ExecutionEnvironment, Value};


#[derive(Clone, Debug, PartialEq)]
pub struct Augmented<T> {
    pub range: Range,
    pub val: T,
}

#[derive(Clone, Debug)]
pub enum PatExpr {
    Error,
    Ignore {
        ty: Value,
    },
    Identifier {
        original: String,
        modifier: ast::IdentifierModifier,
        id: usize,
    },
    StructLiteral(Vec<(Augmented<String>, Augmented<PatExpr>)>),
    New {
        pat: Box<Augmented<PatExpr>>,
        ty_annotation: Box<Augmented<ValExpr>>,
    },
    Typed {
        pat: Box<Augmented<PatExpr>>,
        ty_annotation: Box<Augmented<ValExpr>>,
        ty: Value,
    },
    Literal(ValExpr),
}

impl std::default::Default for PatExpr {
    fn default() -> Self {
        PatExpr::Error
    }
}

#[derive(Clone, Debug)]
pub enum PlaceExpr {
    Error,
    Identifier(usize),
    Deref{
        root: Box<Augmented<ValExpr>>,
        ty: Value,
    },
    ArrayAccess {
        root: Box<Augmented<ValExpr>>,
        index: Box<Augmented<ValExpr>>,
        ty: Value,
    },
    FieldAccess {
        root: Box<Augmented<PlaceExpr>>,
        field: String,
        ty: Value,
    },
}

#[derive(Clone, Debug)]
pub enum ValExpr {
    // An error when parsing
    Error,
    Literal(Value),
    Ref(Box<Augmented<PlaceExpr>>),
    Place(Box<Augmented<PlaceExpr>>),
    // Function
    FnDef {
        params: Vec<Augmented<PatExpr>>,
        body: Box<Augmented<ValExpr>>,
        ty: Value,
    },
    // Constructs a new compound type
    StructLiteral(Vec<(Augmented<String>, Augmented<ValExpr>)>),
    // Creates a new instance of a nominal type
    New {
        ty: Box<Augmented<ValExpr>>,
        val: Box<Augmented<ValExpr>>,
    },
    // Matches an expression to the first matching pattern and destructures it
    CaseOf {
        expr: Box<Augmented<ValExpr>>,
        cases: Vec<(Augmented<PatExpr>, Augmented<ValExpr>)>,
        ty: Value,
    },
    // Block
    Block {
        label: usize,
        statements: Vec<Augmented<BlockStatement>>,
        ty: Value,
    },
    // Inline array
    ArrayLiteral {
        entries: Vec<Augmented<ValExpr>>,
        ty: Value,
    },
    // FieldAccess
    FieldAccess {
        root: Box<Augmented<ValExpr>>,
        field: String,
    },
    // Assign
    Assign {
        target: Box<Augmented<PlaceExpr>>,
        value: Box<Augmented<ValExpr>>,
    },
    // Function application
    App {
        fun: Box<Augmented<ValExpr>>,
        args: Vec<Augmented<ValExpr>>,
    },
    // type of a function
    FnTy {
        param_tys: Vec<Augmented<ValExpr>>,
        dep_ty: Box<Augmented<ValExpr>>,
    },
    // struct and enum
    Struct(Vec<(Augmented<String>, Augmented<ValExpr>)>),
    Enum(Vec<(Augmented<String>, Augmented<ValExpr>)>),
    Union(Vec<(Augmented<String>, Augmented<ValExpr>)>),
    Extern {
        name: Vec<u8>,
        ty: Box<Augmented<ValExpr>>,
    },
    Loop {
        label: usize,
        body: Box<Augmented<ValExpr>>,
        ty: Value,
    },
    Ret {
        label: usize,
        value: Box<Augmented<ValExpr>>,
    },
}

impl std::default::Default for ValExpr {
    fn default() -> Self {
        ValExpr::Error
    }
}

#[derive(Clone, Debug)]
pub enum BlockStatement {
    Error,
    Let {
        pat: Box<Augmented<PatExpr>>,
        value: Box<Augmented<ValExpr>>,
    },
    Do(Box<Augmented<ValExpr>>),
}

#[derive(Clone, Debug)]
pub enum FileStatement {
    Error,
    Let {
        pat: Box<Augmented<PatExpr>>,
        value: Box<Augmented<ValExpr>>,
    },
}