use std::collections::HashMap;

use lsp_types::Range;
use num_bigint::BigInt;
use num_rational::BigRational;

use crate::{ast, builtin::Builtin, dlogger::DiagnosticLogger};

#[derive(Clone, Debug, PartialEq)]
pub struct Augmented<T> {
    pub range: Range,
    pub val: T,
}

#[derive(Clone, Debug)]
pub enum PatExpr {
    Error,
    Ignore,
    Identifier {
        original: String,
        modifier: ast::IdentifierModifier,
        id: usize,
    },
    StructLiteral(Vec<(Augmented<String>, Augmented<PatExpr>)>),
    New {
        pat: Box<Augmented<PatExpr>>,
        ty: Box<Augmented<ValExpr>>,
    },
    Typed {
        pat: Box<Augmented<PatExpr>>,
        ty: Box<Augmented<ValExpr>>,
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
    Deref(Box<Augmented<ValExpr>>),
    ArrayAccess {
        root: Box<Augmented<ValExpr>>,
        index: Box<Augmented<ValExpr>>,
    },
    FieldAccess {
        root: Box<Augmented<PlaceExpr>>,
        field: String,
    },
}

#[derive(Clone, Debug)]
pub enum ValExpr {
    // An error when parsing
    Error,
    Hole,
    Int {
        value: BigInt,
    },
    Bool {
        value: bool,
    },
    Float {
        value: BigRational,
    },
    String(Vec<u8>),
    Ref(Box<Augmented<PlaceExpr>>),
    MutRef(Box<Augmented<PlaceExpr>>),
    Copy(Box<Augmented<PlaceExpr>>),
    Move(Box<Augmented<PlaceExpr>>),
    // Builtin
    Builtin {
        builtin: Builtin,
        level: usize,
    },
    // Function
    FnDef {
        params: Vec<Augmented<PatExpr>>,
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
        cases: Vec<(Augmented<PatExpr>, Augmented<ValExpr>)>,
    },
    // Block
    Block {
        label: usize,
        statements: Vec<Augmented<BlockStatement>>,
    },
    // Inline array
    ArrayLiteral(Vec<Augmented<ValExpr>>),
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
    // short circuiting operators
    And {
        left: Box<Augmented<ValExpr>>,
        right: Box<Augmented<ValExpr>>,
    },
    Or {
        left: Box<Augmented<ValExpr>>,
        right: Box<Augmented<ValExpr>>,
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
    },
    Ret {
        label: usize,
        value: Box<Augmented<ValExpr>>,
    },
    Typed {
        value: Box<Augmented<ValExpr>>,
        ty: Box<Augmented<ValExpr>>,
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
    pub id_name_table: Vec<Vec<String>>,
    pub id_range_table: Vec<Range>,
    pub id_modifier_table: Vec<ast::IdentifierModifier>,

    // these are the labels that are in scope
    pub labels_in_scope: Vec<Vec<(String, usize)>>,

    // the label table
    pub lb_name_table: Vec<String>,
    pub lb_range_table: Vec<Range>,
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
                        Some(Name::Value(id)) => Some(self.id_range_table[*id]),
                        _ => None,
                    })
                    .next()
                {
                    dlogger.log_duplicate_identifier(range, previous_range, &identifier);
                    None
                } else {
                    let id = self.id_name_table.len();
                    self.names_in_scope
                        .last_mut()
                        .unwrap()
                        .insert(identifier.clone(), Name::Value(id));
                    self.id_name_table.push(
                        [
                            self.namespaces.last().unwrap(),
                            [identifier.clone()].as_slice(),
                        ]
                        .concat(),
                    );
                    self.id_range_table.push(range);
                    self.id_modifier_table.push(ast::IdentifierModifier::None);
                    Some((id, identifier))
                }
            }
            None => None,
        }
    }

    pub fn introduce_label(
        &mut self,
        ast::Label { label, range }: ast::Label,
        _dlogger: &mut DiagnosticLogger,
    ) -> Option<(usize, String)> {
        match label {
            Some(label) => {
                let id = self.lb_name_table.len();
                self.labels_in_scope
                    .last_mut()
                    .unwrap()
                    .push((label.clone(), id));
                self.lb_name_table.push(label.clone());
                self.lb_range_table.push(range);
                Some((id, label))
            }
            None => None,
        }
    }

    pub fn introduce_anonymous_label(&mut self, range: Range) -> usize {
        let id = self.lb_name_table.len();
        let label = format!("__anon_{}", id);
        self.labels_in_scope
            .last_mut()
            .unwrap()
            .push((label.clone(), id));
        self.lb_name_table.push(label.clone());
        self.lb_range_table.push(range);
        return id;
    }

    pub fn unintroduce_label(&mut self) {
        self.labels_in_scope.last_mut().unwrap().pop();
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

    pub fn lookup_label(&self, label: ast::Label, dlogger: &mut DiagnosticLogger) -> Option<usize> {
        match &label.label {
            Some(label_name) => match self
                .labels_in_scope
                .last()
                .iter()
                .flat_map(|scope| scope.iter().rev())
                .filter(|(name, _)| name == label_name)
                .next()
            {
                Some((_, id)) => Some(*id),
                None => {
                    dlogger.log_unknown_label(label.range, label_name);
                    None
                }
            },
            None => None,
        }
    }

    pub fn push_block_scope(&mut self) {
        self.names_in_scope.push(HashMap::new());
    }

    pub fn pop_block_scope(&mut self) {
        self.names_in_scope.pop();
    }

    pub fn push_fn_scope(&mut self) {
        self.names_in_scope.push(HashMap::new());
        self.labels_in_scope.push(vec![]);
    }

    pub fn pop_fn_scope(&mut self) {
        self.names_in_scope.pop();
        assert_eq!(self.labels_in_scope.last().unwrap().len(), 0);
        self.labels_in_scope.pop();
    }

    pub fn new() -> Self {
        Environment {
            namespaces: vec![vec![]],
            names_in_scope: vec![HashMap::new()],
            id_name_table: vec![],
            id_modifier_table: vec![],
            id_range_table: vec![],
            labels_in_scope: vec![vec![]],
            lb_name_table: vec![],
            lb_range_table: vec![],
        }
    }
}
