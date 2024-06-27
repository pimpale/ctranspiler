use std::collections::HashMap;

use lsp_types::{GenericParams, Range};
use num_bigint::BigInt;
use num_rational::BigRational;
use strum::AsRefStr;

use crate::{
    ast,
    dlogger::DiagnosticLogger,
    types::{typevalue_kind, KindValue, TypeParam, TypeValue, TypeValueConstructor},
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
        label: usize,
        statements: Vec<Augmented<BlockStatement>>,
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
    Extern {
        name: Vec<u8>,
        ty: Box<Augmented<ValExpr>>,
    },
    If {
        cond: Box<Augmented<ValExpr>>,
        then_branch: Box<Augmented<ValExpr>>,
        else_branch: Option<Box<Augmented<ValExpr>>>,
    },
    Loop {
        label: usize,
        body: Box<Augmented<ValExpr>>,
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
    NoOp,
    Let {
        pat: Box<Augmented<ValPatExpr>>,
        value: Box<Augmented<ValExpr>>,
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
    pub id_name_table: Vec<Vec<String>>,
    pub id_range_table: Vec<Range>,
    pub id_modifier_table: Vec<ast::IdentifierModifier>,

    // contains x for type variables, and type(x) for val variables
    pub id_type_table: Vec<TypeValue>,
    // contains type(x) for type variables, and kind(x) for val variables
    pub id_kind_table: Vec<KindValue>,

    // these are the labels that are in scope
    pub labels_in_scope: Vec<Vec<(String, usize)>>,

    // the label table
    pub lb_name_table: Vec<String>,
    pub lb_range_table: Vec<Range>,
    pub lb_type_table: Vec<TypeValue>,
    pub lb_type_hint_table: Vec<TypeValue>,
    pub lb_kind_table: Vec<KindValue>,
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
                    self.id_kind_table.push(KindValue::Unknown);
                    self.id_type_table.push(TypeValue::Unknown);
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
                self.lb_kind_table.push(KindValue::Unknown);
                self.lb_type_table.push(TypeValue::Unknown);
                self.lb_type_hint_table.push(TypeValue::Unknown);
                Some((id, label))
            }
            None => None,
        }
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

    fn unconditional_insert_identifier(
        &mut self,
        identifier: &str,
        kind: KindValue,
        ty: TypeValue,
    ) -> usize {
        let id = self.id_name_table.len();
        self.names_in_scope
            .last_mut()
            .unwrap()
            .insert(identifier.to_string(), Name::Value(id));
        self.id_name_table.push(
            [
                self.namespaces.last().unwrap(),
                [identifier.to_string()].as_slice(),
            ]
            .concat(),
        );
        self.id_range_table.push(Range::default());
        self.id_kind_table.push(kind);
        self.id_type_table.push(ty);
        self.id_modifier_table.push(ast::IdentifierModifier::None);
        return id;
    }

    fn intro_typarams(
        &mut self,
        params: Vec<(&str, KindValue)>,
    ) -> (Vec<Option<TypeParam>>, Vec<TypeValue>) {
        let mut typarams = vec![];
        let mut tyvars = vec![];
        for (name, kind) in params {
            let id = self.unconditional_insert_identifier(name, kind, TypeValue::Unknown);
            typarams.push(Some(TypeParam {
                id,
                range: Range::default(),
            }));
            tyvars.push(TypeValue::SymbolicVariable(id));
        }
        (typarams, tyvars)
    }

    fn insert_builtin(&mut self, identifier: &str, ty: TypeValue) {
        let kind = typevalue_kind(&ty, self);
        self.unconditional_insert_identifier(identifier, kind, ty);
    }

    pub fn new() -> Self {
        let mut env = Environment {
            namespaces: vec![vec![]],
            names_in_scope: vec![HashMap::new()],
            id_name_table: vec![],
            id_modifier_table: vec![],
            id_range_table: vec![],
            id_type_table: vec![],
            id_kind_table: vec![],
            labels_in_scope: vec![vec![]],
            lb_name_table: vec![],
            lb_range_table: vec![],
            lb_type_table: vec![],
            lb_type_hint_table: vec![],
            lb_kind_table: vec![],
        };

        // insert builtins into the environment
        env.insert_builtin("Never", TypeValue::Never);
        env.insert_builtin("Bool", TypeValue::Bool);
        env.insert_builtin("Ref", TypeValue::RefConstructor);
        env.insert_builtin("Array", TypeValue::ArrayConstructor);
        env.insert_builtin("Slice", TypeValue::SliceConstructor);
        env.insert_builtin("Int", TypeValue::IntConstructor);
        env.insert_builtin("Float", TypeValue::FloatConstructor);

        let arg_ArrayX_ret_SliceX_ty = {
            let (typarams, tyvars) =
                env.intro_typarams(vec![("T", KindValue::Type), ("N", KindValue::Int)]);

            TypeValue::Generic {
                typarams,
                body: Box::new(TypeValue::Fn {
                    paramtys: vec![TypeValue::Concretization {
                        constructor: TypeValueConstructor::ArrayConstructor,
                        tyargs: vec![tyvars[0].clone(), tyvars[1].clone()],
                    }],
                    returntype: Box::new(TypeValue::Concretization {
                        constructor: TypeValueConstructor::SliceConstructor,
                        tyargs: vec![tyvars[0].clone()],
                    }),
                }),
            }
        };

        env.insert_builtin("__builtin_array_to_slice", arg_ArrayX_ret_SliceX_ty);

        // type of a generic function that takes two integers of the same sign and size and returns an integer of the same sign and size
        let arg_IntX_IntX_ret_IntX_ty = {
            let (typarams, tyvars) =
                env.intro_typarams(vec![("sgn", KindValue::Bool), ("sz", KindValue::Int)]);

            TypeValue::Generic {
                typarams,
                body: Box::new(TypeValue::Fn {
                    paramtys: vec![
                        TypeValue::Concretization {
                            constructor: TypeValueConstructor::IntConstructor,
                            tyargs: vec![tyvars[0].clone(), tyvars[1].clone()],
                        },
                        TypeValue::Concretization {
                            constructor: TypeValueConstructor::IntConstructor,
                            tyargs: vec![tyvars[0].clone(), tyvars[1].clone()],
                        },
                    ],
                    returntype: Box::new(TypeValue::Concretization {
                        constructor: TypeValueConstructor::IntConstructor,
                        tyargs: vec![tyvars[0].clone(), tyvars[1].clone()],
                    }),
                }),
            }
        };
        env.insert_builtin("__builtin_addi", arg_IntX_IntX_ret_IntX_ty.clone());
        env.insert_builtin("__builtin_subi", arg_IntX_IntX_ret_IntX_ty.clone());
        env.insert_builtin("__builtin_muli", arg_IntX_IntX_ret_IntX_ty.clone());
        env.insert_builtin("__builtin_divi", arg_IntX_IntX_ret_IntX_ty.clone());
        env.insert_builtin("__builtin_remi", arg_IntX_IntX_ret_IntX_ty.clone());
        env.insert_builtin("__builtin_shli", arg_IntX_IntX_ret_IntX_ty.clone());
        env.insert_builtin("__builtin_shrli", arg_IntX_IntX_ret_IntX_ty.clone());
        env.insert_builtin("__builtin_shrai", arg_IntX_IntX_ret_IntX_ty.clone());
        env.insert_builtin("__builtin_roli", arg_IntX_IntX_ret_IntX_ty.clone());
        env.insert_builtin("__builtin_rori", arg_IntX_IntX_ret_IntX_ty.clone());
        env.insert_builtin("__builtin_andi", arg_IntX_IntX_ret_IntX_ty.clone());
        env.insert_builtin("__builtin_ori", arg_IntX_IntX_ret_IntX_ty.clone());
        env.insert_builtin("__builtin_xori", arg_IntX_IntX_ret_IntX_ty.clone());

        // type of a generic function that takes an integer of the any sign and size and returns an integer of the same sign and size
        let arg_IntX_ret_IntX_ty = {
            let (typarams, tyvars) =
                env.intro_typarams(vec![("sgn", KindValue::Bool), ("sz", KindValue::Int)]);

            TypeValue::Generic {
                typarams,
                body: Box::new(TypeValue::Fn {
                    paramtys: vec![TypeValue::Concretization {
                        constructor: TypeValueConstructor::IntConstructor,
                        tyargs: vec![tyvars[0].clone(), tyvars[1].clone()],
                    }],
                    returntype: Box::new(TypeValue::Concretization {
                        constructor: TypeValueConstructor::IntConstructor,
                        tyargs: vec![tyvars[0].clone(), tyvars[1].clone()],
                    }),
                }),
            }
        };
        env.insert_builtin("__builtin_invi", arg_IntX_ret_IntX_ty.clone());
        env.insert_builtin("__builtin_negi", arg_IntX_ret_IntX_ty.clone());

        // type of a generic function that takes two floats of the same size and returns a float of the same size
        let arg_FloatX_FloatX_ret_FloatX_ty = {
            let (typarams, tyvars) = env.intro_typarams(vec![("sz", KindValue::Int)]);

            TypeValue::Generic {
                typarams,
                body: Box::new(TypeValue::Fn {
                    paramtys: vec![
                        TypeValue::Concretization {
                            constructor: TypeValueConstructor::FloatConstructor,
                            tyargs: vec![tyvars[0].clone()],
                        },
                        TypeValue::Concretization {
                            constructor: TypeValueConstructor::FloatConstructor,
                            tyargs: vec![tyvars[0].clone()],
                        },
                    ],
                    returntype: Box::new(TypeValue::Concretization {
                        constructor: TypeValueConstructor::FloatConstructor,
                        tyargs: vec![tyvars[0].clone()],
                    }),
                }),
            }
        };

        env.insert_builtin("__builtin_addf", arg_FloatX_FloatX_ret_FloatX_ty.clone());
        env.insert_builtin("__builtin_subf", arg_FloatX_FloatX_ret_FloatX_ty.clone());
        env.insert_builtin("__builtin_mulf", arg_FloatX_FloatX_ret_FloatX_ty.clone());
        env.insert_builtin("__builtin_divf", arg_FloatX_FloatX_ret_FloatX_ty.clone());
        env.insert_builtin("__builtin_remf", arg_FloatX_FloatX_ret_FloatX_ty.clone());

        // type of a generic function that takes a float of any size and returns a float of the same size
        let arg_FloatX_ret_FloatX_ty = {
            let (typarams, tyvars) = env.intro_typarams(vec![("sz", KindValue::Int)]);

            TypeValue::Generic {
                typarams,
                body: Box::new(TypeValue::Fn {
                    paramtys: vec![TypeValue::Concretization {
                        constructor: TypeValueConstructor::FloatConstructor,
                        tyargs: vec![tyvars[0].clone()],
                    }],
                    returntype: Box::new(TypeValue::Concretization {
                        constructor: TypeValueConstructor::FloatConstructor,
                        tyargs: vec![tyvars[0].clone()],
                    }),
                }),
            }
        };

        env.insert_builtin("__builtin_negf", arg_FloatX_FloatX_ret_FloatX_ty);

        // type of a function that takes an integer and returns an integer
        let arg_IntX_ret_IntY_ty = {
            let (typarams, tyvars) = env.intro_typarams(vec![
                ("sgn1", KindValue::Bool),
                ("sz1", KindValue::Int),
                ("sgn2", KindValue::Bool),
                ("sz2", KindValue::Int),
            ]);

            TypeValue::Generic {
                typarams,
                body: Box::new(TypeValue::Fn {
                    paramtys: vec![TypeValue::Concretization {
                        constructor: TypeValueConstructor::IntConstructor,
                        tyargs: vec![tyvars[0].clone(), tyvars[1].clone()],
                    }],
                    returntype: Box::new(TypeValue::Concretization {
                        constructor: TypeValueConstructor::IntConstructor,
                        tyargs: vec![tyvars[2].clone(), tyvars[3].clone()],
                    }),
                }),
            }
        };

        env.insert_builtin("__builtin_itoi", arg_IntX_ret_IntY_ty);

        // type of a function that takes a float and returns a float
        let arg_FloatX_ret_FloatY_ty = {
            let (typarams, tyvars) =
                env.intro_typarams(vec![("sz1", KindValue::Int), ("sz2", KindValue::Int)]);

            TypeValue::Generic {
                typarams,
                body: Box::new(TypeValue::Fn {
                    paramtys: vec![TypeValue::Concretization {
                        constructor: TypeValueConstructor::FloatConstructor,
                        tyargs: vec![tyvars[0].clone()],
                    }],
                    returntype: Box::new(TypeValue::Concretization {
                        constructor: TypeValueConstructor::FloatConstructor,
                        tyargs: vec![tyvars[1].clone()],
                    }),
                }),
            }
        };

        env.insert_builtin("__builtin_ftof", arg_FloatX_ret_FloatY_ty);

        // type of a function that takes a signed integer and returns a float
        let arg_IntX_ret_FloatY_ty = {
            let (typarams, tyvars) =
                env.intro_typarams(vec![("isz", KindValue::Bool), ("fsz", KindValue::Int)]);

            TypeValue::Generic {
                typarams,
                body: Box::new(TypeValue::Fn {
                    paramtys: vec![TypeValue::Concretization {
                        constructor: TypeValueConstructor::IntConstructor,
                        tyargs: vec![TypeValue::BoolLit(true), tyvars[0].clone()],
                    }],
                    returntype: Box::new(TypeValue::Concretization {
                        constructor: TypeValueConstructor::FloatConstructor,
                        tyargs: vec![tyvars[1].clone()],
                    }),
                }),
            }
        };

        env.insert_builtin("__builtin_itof", arg_IntX_ret_FloatY_ty);

        // type of a function that takes a float and returns a signed integer
        let arg_FloatX_ret_IntY_ty = {
            let (typarams, tyvars) =
                env.intro_typarams(vec![("fsz", KindValue::Int), ("isz", KindValue::Int)]);

            TypeValue::Generic {
                typarams,
                body: Box::new(TypeValue::Fn {
                    paramtys: vec![TypeValue::Concretization {
                        constructor: TypeValueConstructor::FloatConstructor,
                        tyargs: vec![tyvars[0].clone()],
                    }],
                    returntype: Box::new(TypeValue::Concretization {
                        constructor: TypeValueConstructor::IntConstructor,
                        tyargs: vec![TypeValue::BoolLit(true), tyvars[1].clone()],
                    }),
                }),
            }
        };

        env.insert_builtin("__builtin_ftoi", arg_FloatX_ret_IntY_ty);

        env
    }
}
