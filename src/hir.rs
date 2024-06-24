use std::collections::HashMap;

use lsp_types::Range;
use num_bigint::BigInt;
use num_rational::BigRational;
use strum::AsRefStr;

use crate::{
    ast,
    dlogger::DiagnosticLogger,
    types::{KindValue, TypeValue},
};

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
        original: String,
        modifier: ast::IdentifierModifier,
        id: usize,
    },
    StructLiteral(Vec<(Augmented<String>, Augmented<ValPatExpr>)>),
    New {
        pat: Box<Augmented<ValPatExpr>>,
        ty: Box<Augmented<ValExpr>>,
    },
    Typed {
        pat: Box<Augmented<ValPatExpr>>,
        ty: Box<Augmented<ValExpr>>,
    },
    Kinded {
        pat: Box<Augmented<ValPatExpr>>,
        kind: Box<Augmented<KindExpr>>,
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
    // Assignment
    Assign,
    AssignAdd,
    AssignSub,
    AssignMul,
    AssignDiv,
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
    // Function
    FnDef {
        params: Vec<Augmented<ValPatExpr>>,
        returnty: Option<Box<Augmented<ValExpr>>>,
        body: Box<Augmented<ValExpr>>,
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
    // Function application
    App {
        fun: Box<Augmented<ValExpr>>,
        args: Vec<Augmented<ValExpr>>,
    },
    // type of a function
    FnTy {
        paramtys: Vec<Augmented<ValExpr>>,
        returnty: Box<Augmented<ValExpr>>,
    },
    // struct and enum
    Struct(Vec<(Augmented<String>, Augmented<ValExpr>)>),
    Enum(Vec<(Augmented<String>, Augmented<ValExpr>)>),
    Union(Vec<(Augmented<String>, Augmented<ValExpr>)>),
    // generic
    Concretization {
        generic: Box<Augmented<ValExpr>>,
        tyargs: Vec<Augmented<ValExpr>>,
    },
    Generic {
        params: Vec<Augmented<ValPatExpr>>,
        returnkind: Option<Box<Augmented<KindExpr>>>,
        body: Box<Augmented<ValExpr>>,
    },
    // builtins
    // builtin types
    BoolTy,
    RefConstructorTy,
    ArrayConstructorTy,
    SliceConstructorTy,
    IntConstructorTy,
    FloatConstructorTy,
    // builtin operators
    // boolean operators
    BoolNot, // bool -> bool
    // integer operators
    IntAddGen,         // [U](u, u) -> u
    IntSubGen,         // [U](u, u) -> u
    IntMulGen,         // [U](u, u) -> u
    IntDivGen,         // [U](u, u) -> u
    IntRemGen,         // [U](u, u) -> u
    IntShlLGen,        // [U](u, u) -> u
    IntShrLGen,        // [U](u, u) -> u
    IntShrAGen,        // [U](u, u) -> u
    IntRolGen,         // [U](u, u) -> u
    IntRorGen,         // [U](u, u) -> u
    IntAndGen,         // [U](u, u) -> u
    IntOrGen,          // [U](u, u) -> u
    IntXorGen,         // [U](u, u) -> u
    IntInvGen,         // [U]u -> u
    IntNegGen,         // [U]u -> u
    // float operators
    FloatAddGen,    // [F](f, f) -> f
    FloatSubGen,    // [F](f, f) -> f
    FloatMulGen,    // [F](f, f) -> f
    FloatDivGen,    // [F](f, f) -> f
    FloatRemGen,    // [F](f, f) -> f
    FloatNegGen,    // [F]f -> f
    // conversion operators
    // convert int to int
    ConvIntIntGen, // [T, U] t -> u
    // convert float to float
    ConvFloatFloatGen, // [T, U] f -> f
    // convert int to float
    ConvIntFloatGen, // [T, U] u -> f
    // convert float to int
    ConvFloatIntGen, // [T, U] f -> u
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
    Let {
        pat: Box<Augmented<ValPatExpr>>,
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
    Let {
        pat: Box<Augmented<ValPatExpr>>,
        value: Box<Augmented<ValExpr>>,
    },
}

#[derive(Clone, Debug)]
pub enum Name {
    Value(usize),
    Namespace(HashMap<String, Name>),
}

pub struct Environment {
    // namespaces that we are nested in
    pub namespaces: Vec<Vec<String>>,
    // these are the names that are in scope
    pub names_in_scope: Vec<HashMap<String, Name>>,

    // the identifier table
    pub name_table: Vec<Vec<String>>,
    pub range_table: Vec<Range>,
    pub modifier_table: Vec<ast::IdentifierModifier>,

    // contains x for type variables, and type(x) for val variables
    pub type_table: Vec<Option<TypeValue>>,
    // contains type(x) for type variables, and kind(x) for val variables
    pub kind_table: Vec<Option<KindValue>>,
}

impl Environment {
    pub fn introduce_identifier(
        &mut self,
        ast::Identifier { identifier, range }: ast::Identifier,
        dlogger: &mut DiagnosticLogger,
    ) -> Option<(usize, String)> {
        match identifier {
            Some(identifier) => {
                if let Some(previous_range) = self
                    .names_in_scope
                    .iter()
                    .rev()
                    .flat_map(|scope| match scope.get(&identifier) {
                        Some(Name::Value(id)) => Some(self.range_table[*id]),
                        _ => None,
                    })
                    .next()
                {
                    dlogger.log_duplicate_identifier(range, previous_range, &identifier);
                    None
                } else {
                    let id = self.name_table.len();
                    self.names_in_scope
                        .last_mut()
                        .unwrap()
                        .insert(identifier.clone(), Name::Value(id));
                    self.name_table.push(
                        [
                            self.namespaces.last().unwrap(),
                            [identifier.clone()].as_slice(),
                        ]
                        .concat(),
                    );
                    self.range_table.push(range);
                    self.kind_table.push(None);
                    self.type_table.push(None);
                    self.modifier_table.push(ast::IdentifierModifier::None);
                    Some((id, identifier))
                }
            }
            None => None,
        }
    }

    pub fn use_namespace(&mut self, identifier: ast::Identifier, dlogger: &mut DiagnosticLogger) {
        match &identifier.identifier {
            Some(identifier_name) => match self
                .names_in_scope
                .iter()
                .rev()
                .flat_map(|scope| match scope.get(identifier_name).cloned() {
                    Some(Name::Namespace(v)) => Some(v),
                    _ => None,
                })
                .next()
            {
                Some(id) => self.names_in_scope.last_mut().unwrap().extend(id),
                None => {
                    dlogger.log_unknown_identifier(identifier.range, identifier_name);
                }
            },
            None => {}
        }
    }

    pub fn lookup_identifier(
        &self,
        identifier: ast::Identifier,
        dlogger: &mut DiagnosticLogger,
    ) -> Option<usize> {
        match &identifier.identifier {
            Some(identifier_name) => match self
                .names_in_scope
                .iter()
                .rev()
                .flat_map(|scope| match scope.get(identifier_name) {
                    Some(Name::Value(id)) => Some(id),
                    _ => None,
                })
                .next()
            {
                Some(id) => Some(*id),
                None => {
                    dlogger.log_unknown_identifier(identifier.range, identifier_name);
                    None
                }
            },
            None => None,
        }
    }

    pub fn new() -> Self {
        Environment {
            namespaces: vec![vec![]],
            names_in_scope: vec![HashMap::new()],
            name_table: vec![],
            modifier_table: vec![],
            range_table: vec![],
            type_table: vec![],
            kind_table: vec![],
        }
    }
}
