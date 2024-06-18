use std::collections::HashMap;

use indexmap::IndexMap;
use lsp_types::Range;
use num_bigint::BigInt;
use num_rational::BigRational;
use strum::AsRefStr;

use crate::types::{KindValue, TypeValue};

#[derive(Clone, Debug, PartialEq)]
pub struct Augmented<T> {
    pub range: Range,
    pub val: T,
}

#[derive(Clone, Debug, PartialEq)]
pub enum KindExpr {
    Error,
    Int,
    Float,
    Bool,
    Type,
    Constructor {
        paramkinds: Vec<Augmented<KindExpr>>,
        returnkind: Box<Augmented<KindExpr>>,
    },
}

impl std::default::Default for KindExpr {
    fn default() -> Self {
        KindExpr::Error
    }
}

#[derive(Clone, Debug)]
pub enum TypeExpr {
    // An error when parsing
    Error,
    Identifier(usize),
    // types
    BoolTy,
    RefConstructorTy,
    ArrayConstructorTy,
    SliceConstructorTy,
    IntConstructorTy,
    FloatConstructorTy,
    // const literals
    Int(BigInt),
    Bool(bool),
    Float(BigRational),
    // type of a function
    Fn {
        paramtys: Vec<Augmented<TypeExpr>>,
        returnty: Box<Augmented<TypeExpr>>,
    },
    // struct and enum
    Struct(Vec<(Augmented<String>, Augmented<TypeExpr>)>),
    Enum(Vec<(Augmented<String>, Augmented<TypeExpr>)>),
    Union(Vec<(Augmented<String>, Augmented<TypeExpr>)>),
    // generic
    Concretization {
        genericty: Box<Augmented<TypeExpr>>,
        tyargs: Vec<Augmented<TypeExpr>>,
    },
    Generic {
        params: Vec<Augmented<TypePatExpr>>,
        returnkind: Option<Box<Augmented<KindExpr>>>,
        body: Box<Augmented<TypeExpr>>,
    },
}

impl std::default::Default for TypeExpr {
    fn default() -> Self {
        TypeExpr::Error
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypePatExpr {
    Error,
    Identifier(usize),
    Typed {
        id: usize,
        kind: Box<Augmented<KindExpr>>,
    },
}

impl std::default::Default for TypePatExpr {
    fn default() -> Self {
        TypePatExpr::Error
    }
}

#[derive(Clone, Debug)]
pub enum CaseTargetExpr {
    Error,
    Bool(bool),
    Int(BigInt),
    PatExpr(Box<Augmented<ValPatExpr>>),
}

impl std::default::Default for CaseTargetExpr {
    fn default() -> Self {
        CaseTargetExpr::Error
    }
}

#[derive(Clone, Debug)]
pub enum ValPatExpr {
    Error,
    Ignore,
    Identifier {
        mutable: bool,
        id: usize,
    },
    StructLiteral(Vec<(Augmented<String>, Augmented<ValPatExpr>)>),
    New {
        pat: Box<Augmented<ValPatExpr>>,
        ty: Box<Augmented<TypeExpr>>,
    },
    Typed {
        pat: Box<Augmented<ValPatExpr>>,
        ty: Box<Augmented<TypeExpr>>,
    },
}

impl std::default::Default for ValPatExpr {
    fn default() -> Self {
        ValPatExpr::Error
    }
}

#[derive(Clone, Debug, AsRefStr)]
pub enum ValBinaryOpKind {
    // Math
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    // Comparison
    Lt,
    Leq,
    Gt,
    Geq,
    // Booleans
    And,
    Or,
    // Equality
    Eq,
    Neq,
}

#[derive(Clone, Debug)]
pub enum ValExpr {
    // An error when parsing
    Error,
    Int(BigInt),
    Bool(bool),
    Float(BigRational),
    String(Vec<u8>),
    Ref(Box<Augmented<ValExpr>>),
    Deref(Box<Augmented<ValExpr>>),
    // Generic (only used in functions atm)
    Generic {
        params: Vec<Augmented<TypePatExpr>>,
        // return kind is always TYPE
        body: Box<Augmented<ValExpr>>,
    },
    // Function
    FnDef {
        params: Vec<Augmented<ValPatExpr>>,
        returnty: Option<Box<Augmented<TypeExpr>>>,
        body: Box<Augmented<ValExpr>>,
    },
    // Constructs a new compound type
    StructLiteral(Vec<(Augmented<String>, Augmented<ValExpr>)>),
    // Creates a new instance of a nominal type
    New {
        ty: Box<Augmented<TypeExpr>>,
        val: Box<Augmented<ValExpr>>,
    },
    // Matches an expression to the first matching pattern and destructures it
    CaseOf {
        expr: Box<Augmented<ValExpr>>,
        cases: Vec<(Augmented<CaseTargetExpr>, Augmented<ValExpr>)>,
    },
    // Block
    Block {
        statements: Vec<Augmented<BlockStatement>>,
        last_expression: Option<Box<Augmented<ValExpr>>>,
    },
    // Inline array
    ArrayLiteral(Vec<Augmented<ValExpr>>),
    BinaryOp {
        op: ValBinaryOpKind,
        left_operand: Box<Augmented<ValExpr>>,
        right_operand: Box<Augmented<ValExpr>>,
    },
    // A reference to a previously defined variable
    Identifier(usize),
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
    // Concretization
    Concretization {
        generic: Box<Augmented<ValExpr>>,
        tyargs: Vec<Augmented<TypeExpr>>,
    },
    // Function application
    App {
        fun: Box<Augmented<ValExpr>>,
        args: Vec<Augmented<ValExpr>>,
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
    NoOp,
    TypeDef {
        typat: Box<Augmented<TypePatExpr>>,
        value: Box<Augmented<TypeExpr>>,
    },
    ValDef {
        pat: Box<Augmented<ValPatExpr>>,
        value: Box<Augmented<ValExpr>>,
    },
    Set {
        place: Box<Augmented<ValExpr>>,
        value: Box<Augmented<ValExpr>>,
    },
    IfThen {
        cond: Box<Augmented<ValExpr>>,
        then_branch: Vec<Augmented<BlockStatement>>,
        else_branch: Vec<Augmented<BlockStatement>>,
    },
    While {
        cond: Box<Augmented<ValExpr>>,
        body: Vec<Augmented<BlockStatement>>,
    },
    For {
        pattern: Box<Augmented<ValPatExpr>>,
        start: Box<Augmented<ValExpr>>,
        end: Box<Augmented<ValExpr>>,
        inclusive: bool,
        by: Option<Box<Augmented<ValExpr>>>,
        body: Vec<Augmented<BlockStatement>>,
    },
    Do(Box<Augmented<ValExpr>>),
}

#[derive(Clone, Debug)]
pub enum FileStatement {
    Error,
    TypeDef {
        typat: Box<Augmented<TypePatExpr>>,
        value: Box<Augmented<TypeExpr>>,
    },
    ValDef {
        pat: Box<Augmented<ValPatExpr>>,
        value: Box<Augmented<ValExpr>>,
    },
}

#[derive(Clone, Debug)]
pub enum Name {
    Value(usize),
    Namespace(HashMap<String, Name>),
}

pub struct Scope {
    pub names: IndexMap<String, Name>,
    pub namespaces: Vec<String>,
}

pub struct Environment {
    // namespaces that we are nested in
    pub namespaces: Vec<Vec<String>>,
    // these are the names that are in scope
    pub names_in_scope: Vec<HashMap<String, Name>>,

    // the identifier table
    pub name_table: Vec<Vec<String>>,
    pub range_table: Vec<Range>,
    pub kind_table: Vec<Option<KindValue>>,
    pub type_table: Vec<Option<TypeValue>>,
}