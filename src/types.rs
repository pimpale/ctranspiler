use crate::dlogger::DiagnosticLogger;
use crate::hir;
use crate::hir::Augmented;
use num_bigint::BigInt;
use num_rational::BigRational;
use num_traits::Signed;

#[derive(Clone, Debug, PartialEq)]
pub enum KindValue {
    Error,
    Int,
    Float,
    Bool,
    Type,
    Constructor {
        paramkinds: Vec<KindValue>,
        returnkind: Box<KindValue>,
    },
}

impl std::fmt::Display for KindValue {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            KindValue::Error => write!(f, "Error"),
            KindValue::Type => write!(f, "Type"),
            KindValue::Int => write!(f, "Int"),
            KindValue::Float => write!(f, "Float"),
            KindValue::Bool => write!(f, "Bool"),
            KindValue::Constructor {
                paramkinds,
                returnkind,
            } => {
                write!(f, "Constructor {{ paramkinds: [")?;
                for arg in paramkinds.iter() {
                    write!(f, "{}, ", arg)?;
                }
                write!(f, "], returnkind: {} }}", returnkind)
            }
        }
    }
}

pub fn evaluate_hir_kind(
    kind: &Augmented<hir::KindExpr>,
    dlogger: &mut DiagnosticLogger,
) -> KindValue {
    match kind.val {
        hir::KindExpr::Error => KindValue::Error,
        hir::KindExpr::Type => KindValue::Type,
        hir::KindExpr::Int => KindValue::Int,
        hir::KindExpr::Float => KindValue::Float,
        hir::KindExpr::Bool => KindValue::Bool,
        hir::KindExpr::Constructor {
            paramkinds,
            returnkind,
        } => {
            let mut paramkinds_out = vec![];
            for arg in paramkinds.iter() {
                paramkinds_out.push(evaluate_hir_kind(arg, dlogger));
            }
            KindValue::Constructor {
                paramkinds: paramkinds_out,
                returnkind: Box::new(evaluate_hir_kind(&returnkind, dlogger)),
            }
        }
    }
}

#[derive(Clone, Debug)]
pub enum TypeValue {
    // An error when parsing
    Error,
    SymbolicVariable(usize),
    // a nominal type
    Nominal(usize),
    // types
    Unit,
    Bool,
    // constructors
    RefConstructor,
    ArrayConstructor,
    SliceConstructor,
    IntConstructor,
    UIntConstructor,
    FloatConstructor,
    // the constructed type
    Ref(Box<TypeValue>),
    Array(Box<TypeValue>, Box<TypeValue>),
    Slice(Box<TypeValue>),
    Int(Box<TypeValue>),
    UInt(Box<TypeValue>),
    Float(Box<TypeValue>),
    // const literals
    IntLit(BigInt),
    BoolLit(bool),
    FloatLit(BigRational),
    // type of a function
    Fn {
        paramtys: Vec<TypeValue>,
        returntype: Box<TypeValue>,
    },
    // struct and enumify
    Struct(Vec<(String, TypeValue)>),
    Enum(Vec<(String, TypeValue)>),
    Union(Vec<(String, TypeValue)>),
    // type of a type constructor. Instantiated at every use point.
    Constructor {
        typarams: Vec<Augmented<hir::TypePatExpr>>,
        body: Box<TypeValue>,
    },
    // type of a generic value. Instantiated at every use point
    Generic {
        typarams: Vec<Augmented<hir::TypePatExpr>>,
        body: Box<TypeValue>,
    },
    // where the type constructor is symbolic
    Concretization {
        symbolic_constructor: usize,
        tyargs: Vec<TypeValue>,
    },
}

// in infermode, we don't already know what kind the given expression has to be, so we infer it.
// if we reach a concretize node, then we can do kindchecking_checkmode, because we know what the type arguments have to be
// if we encounter an error, we log it, and modify the hir to be an error.
// NOTE: we assume that all identifiers have already been resolved.
pub fn kindcheck_hir_type_infermode(
    v: &Augmented<hir::TypeExpr>,
    dlogger: &mut DiagnosticLogger,
    type_name_table: &mut Vec<String>,
    type_kind_table: &mut Vec<Option<KindValue>>,
) -> KindValue {
    match v.val {
        hir::TypeExpr::Error => KindValue::Error,
        hir::TypeExpr::Identifier(id) => type_kind_table[id].expect("kind not initialized yet"),
        hir::TypeExpr::UnitTy => KindValue::Type,
        hir::TypeExpr::BoolTy => KindValue::Type,
        hir::TypeExpr::RefConstructorTy => KindValue::Constructor {
            paramkinds: vec![KindValue::Type],
            returnkind: Box::new(KindValue::Type),
        },
        hir::TypeExpr::ArrayConstructorTy => KindValue::Constructor {
            paramkinds: vec![KindValue::Type, KindValue::Int],
            returnkind: Box::new(KindValue::Type),
        },
        hir::TypeExpr::SliceConstructorTy => KindValue::Constructor {
            paramkinds: vec![KindValue::Type],
            returnkind: Box::new(KindValue::Type),
        },
        hir::TypeExpr::IntConstructorTy => KindValue::Constructor {
            paramkinds: vec![KindValue::Int],
            returnkind: Box::new(KindValue::Type),
        },
        hir::TypeExpr::UIntConstructorTy => KindValue::Constructor {
            paramkinds: vec![KindValue::Int],
            returnkind: Box::new(KindValue::Type),
        },
        hir::TypeExpr::FloatConstructorTy => KindValue::Constructor {
            paramkinds: vec![KindValue::Int],
            returnkind: Box::new(KindValue::Type),
        },
        hir::TypeExpr::Int(_) => KindValue::Int,
        hir::TypeExpr::Bool(_) => KindValue::Bool,
        hir::TypeExpr::Float(_) => KindValue::Float,
        hir::TypeExpr::Fn {
            paramtys,
            ref mut returnty,
        } => {
            for ref mut arg in paramtys {
                kindcheck_hir_type_checkmode(
                    arg,
                    &KindValue::Type,
                    dlogger,
                    type_name_table,
                    type_kind_table,
                );
            }
            kindcheck_hir_type_checkmode(
                returnty,
                &KindValue::Type,
                dlogger,
                type_name_table,
                type_kind_table,
            );
            KindValue::Type
        }
        hir::TypeExpr::Struct(ref fields) => {
            for (_, ref mut expr) in fields {
                kindcheck_hir_type_checkmode(
                    expr,
                    &KindValue::Type,
                    dlogger,
                    type_name_table,
                    type_kind_table,
                );
            }
            KindValue::Type
        }
        hir::TypeExpr::Enum(ref fields) => {
            for (_, ref mut expr) in fields {
                kindcheck_hir_type_checkmode(
                    expr,
                    &KindValue::Type,
                    dlogger,
                    type_name_table,
                    type_kind_table,
                );
            }
            KindValue::Type
        }
        hir::TypeExpr::Union(ref fields) => {
            for (_, ref mut expr) in fields {
                kindcheck_hir_type_checkmode(
                    expr,
                    &KindValue::Type,
                    dlogger,
                    type_name_table,
                    type_kind_table,
                );
            }
            KindValue::Type
        }
        hir::TypeExpr::Concretization {
            ref mut genericty,
            tyargs: mut provided_args,
        } => {
            let kind_of_generic =
                kindcheck_hir_type_infermode(genericty, dlogger, type_name_table, type_kind_table);
            match kind_of_generic {
                KindValue::Constructor {
                    paramkinds: expected_kinds,
                    returnkind,
                } => {
                    if expected_kinds.len() != provided_args.len() {
                        dlogger.log_wrong_number_type_args(
                            v.range,
                            expected_kinds.len(),
                            provided_args.len(),
                        );
                        v.val = hir::TypeExpr::Error;
                        KindValue::Error
                    } else {
                        for (ref argkind, ref mut tyarg) in
                            std::iter::zip(expected_kinds, provided_args)
                        {
                            kindcheck_hir_type_checkmode(
                                tyarg,
                                argkind,
                                dlogger,
                                type_name_table,
                                type_kind_table,
                            );
                        }
                        *returnkind
                    }
                }
                _ => {
                    dlogger.log_cannot_be_concretized(v.range);
                    KindValue::Error
                }
            }
        }
    }
}

// we use this function to verify if a type is well-kinded
// if not, then an error is reported, and we modify the hir to eliminate the error
// NOTE: we assume that all identifiers have already been resolved.
pub fn kindcheck_hir_type_checkmode(
    v: &Augmented<hir::TypeExpr>,
    expected_kind: &KindValue,
    dlogger: &mut DiagnosticLogger,
    type_name_table: &mut Vec<String>,
    type_kind_table: &mut Vec<Option<KindValue>>,
) {
    match v.val {
        hir::TypeExpr::Error => {}
        k => {
            let kind_of_k =
                kindcheck_hir_type_infermode(v, dlogger, type_name_table, type_kind_table);
            if &kind_of_k != expected_kind {
                dlogger.log_kind_mismatch(
                    v.range,
                    &format!("{}", expected_kind),
                    &format!("{}", kind_of_k),
                );
                v.val = hir::TypeExpr::Error;
            }
        }
    }
}

pub struct Env {
    pub type_name_table: Vec<String>,
    pub type_value_table: Vec<Option<TypeValue>>,
    pub type_bindings: Vec<Vec<TypeValue>>,
    pub val_name_table: Vec<String>,
    pub val_type_table: Vec<Option<TypeValue>>,
}
impl Env {
    fn evaluate_nominal(&self, v: TypeValue) -> TypeValue {
        match v {
            TypeValue::Nominal(id) => self.evaluate_nominal(
                self.type_value_table[id]
                    .expect("nominal should be initialized")
                    .clone(),
            ),
            x => x,
        }
    }

    // introduces the variables bound by type pattern to this scope
    fn intro_type_binding(&mut self, typat: &Augmented<hir::TypePatExpr>, tyval: TypeValue) {
        match typat.val {
            hir::TypePatExpr::Error => {}
            hir::TypePatExpr::Identifier { id, .. } => {
                self.type_bindings[id].push(tyval);
            }
        }
    }
    // removes the variables bound by type pattern from this scope
    fn elim_type_binding(&mut self, typat: &Augmented<hir::TypePatExpr>) {
        match typat.val {
            hir::TypePatExpr::Error => {}
            hir::TypePatExpr::Identifier { id, .. } => {
                self.type_bindings[id].pop().unwrap();
            }
        }
    }
}

// evaluate a type expression in a context
// NOTE: we assume that all identifiers have already been resolved.
// NOTE: we assume that all types are well-kinded
pub fn evaluate_hir_type(
    v: &Augmented<hir::TypeExpr>,
    dlogger: &mut DiagnosticLogger,
    env: &mut Env,
) -> TypeValue {
    match v.val {
        hir::TypeExpr::Error => TypeValue::Error,
        hir::TypeExpr::Identifier(id) => match env.type_bindings[id].last() {
            Some(v) => v.clone(),
            _ => TypeValue::Nominal(id),
        },
        hir::TypeExpr::UnitTy => TypeValue::Unit,
        hir::TypeExpr::BoolTy => TypeValue::Bool,
        hir::TypeExpr::RefConstructorTy => TypeValue::RefConstructor,
        hir::TypeExpr::ArrayConstructorTy => TypeValue::ArrayConstructor,
        hir::TypeExpr::SliceConstructorTy => TypeValue::SliceConstructor,
        hir::TypeExpr::IntConstructorTy => TypeValue::IntConstructor,
        hir::TypeExpr::UIntConstructorTy => TypeValue::UIntConstructor,
        hir::TypeExpr::FloatConstructorTy => TypeValue::FloatConstructor,
        hir::TypeExpr::Int(x) => TypeValue::IntLit(x),
        hir::TypeExpr::Bool(x) => TypeValue::BoolLit(x),
        hir::TypeExpr::Float(x) => TypeValue::FloatLit(x),
        hir::TypeExpr::Struct(fields) => {
            let mut s_fields = vec![];
            for (identifier, expr) in fields {
                s_fields.push((identifier, evaluate_hir_type(&expr, dlogger, env)));
            }
            TypeValue::Struct(s_fields)
        }
        hir::TypeExpr::Enum(fields) => {
            let mut s_fields = vec![];
            for (identifier, expr) in fields {
                s_fields.push((identifier, evaluate_hir_type(&expr, dlogger, env)));
            }
            TypeValue::Enum(s_fields)
        }
        hir::TypeExpr::Union(fields) => {
            let mut s_fields = vec![];
            for (identifier, expr) in fields {
                s_fields.push((identifier, evaluate_hir_type(&expr, dlogger, env)));
            }
            TypeValue::Union(s_fields)
        }
        hir::TypeExpr::Fn { paramtys, returnty } => TypeValue::Fn {
            paramtys: paramtys
                .iter()
                .map(|x| evaluate_hir_type(x, dlogger, env))
                .collect(),
            returntype: Box::new(evaluate_hir_type(&returnty, dlogger, env)),
        },
        // substitute the generic arguments into the body
        hir::TypeExpr::Concretization { genericty, tyargs } => {
            // first we evaluate the type of the generic function (peeking past identifiers)
            let generic_val = env.evaluate_nominal(evaluate_hir_type(&genericty, dlogger, env));
            let tyargs = tyargs
                .iter()
                .map(|x| evaluate_hir_type(x, dlogger, env))
                .collect::<Vec<_>>();
            match generic_val {
                TypeValue::Error => TypeValue::Error,
                TypeValue::SymbolicVariable(id) => TypeValue::Concretization {
                    symbolic_constructor: id,
                    tyargs,
                },
                TypeValue::RefConstructor => {
                    assert!(tyargs.len() == 1, "wrong number of arguments");
                    TypeValue::Ref(Box::new(tyargs[0]))
                }
                TypeValue::ArrayConstructor => {
                    assert!(tyargs.len() == 2, "wrong number of arguments");
                    TypeValue::Array(Box::new(tyargs[0]), Box::new(tyargs[1]))
                }
                TypeValue::SliceConstructor => {
                    assert!(tyargs.len() == 1, "wrong number of arguments");
                    TypeValue::Ref(Box::new(tyargs[0]))
                }
                TypeValue::IntConstructor => {
                    assert!(tyargs.len() == 1, "wrong number of arguments");
                    TypeValue::Int(Box::new(tyargs[0]))
                }
                TypeValue::UIntConstructor => {
                    assert!(tyargs.len() == 1, "wrong number of arguments");
                    TypeValue::UInt(Box::new(tyargs[0]))
                }
                TypeValue::FloatConstructor => {
                    assert!(tyargs.len() == 1, "wrong number of arguments");
                    TypeValue::Float(Box::new(tyargs[0]))
                }
                TypeValue::Constructor { typarams, body } => {
                    assert!(typarams.len() == tyargs.len(), "wrong number of arguments");
                    for (ref typat, tyarg) in std::iter::zip(typarams, tyargs) {
                        env.intro_type_binding(typat, tyarg)
                    }
                    let bodytype = substitute_symbolic_typevalue(&body, dlogger, env);
                    for ref typat in typarams {
                        env.elim_type_binding(typat)
                    }
                    bodytype
                }
                _ => {
                    unreachable!("concretization of a non-generic");
                }
            }
        }
    }
}

fn substitute_symbolic_typevalue(
    v: &TypeValue,
    dlogger: &mut DiagnosticLogger,
    env: &mut Env,
) -> TypeValue {
    match v {
        TypeValue::SymbolicVariable(id) => match env.type_bindings[*id].last() {
            Some(ty) => ty.clone(),
            None => TypeValue::SymbolicVariable(*id),
        },
        TypeValue::Ref(v) => {
            TypeValue::Ref(Box::new(substitute_symbolic_typevalue(v, dlogger, env)))
        }
        TypeValue::Array(v, s) => TypeValue::Array(
            Box::new(substitute_symbolic_typevalue(v, dlogger, env)),
            Box::new(substitute_symbolic_typevalue(s, dlogger, env)),
        ),
        TypeValue::Slice(v) => {
            TypeValue::Slice(Box::new(substitute_symbolic_typevalue(v, dlogger, env)))
        }
        TypeValue::Int(v) => {
            TypeValue::Int(Box::new(substitute_symbolic_typevalue(v, dlogger, env)))
        }
        TypeValue::UInt(v) => {
            TypeValue::UInt(Box::new(substitute_symbolic_typevalue(v, dlogger, env)))
        }
        TypeValue::Float(v) => {
            TypeValue::Float(Box::new(substitute_symbolic_typevalue(v, dlogger, env)))
        }
        TypeValue::Fn {
            paramtys,
            returntype,
        } => TypeValue::Fn {
            paramtys: paramtys
                .iter()
                .map(|ty| substitute_symbolic_typevalue(ty, dlogger, env))
                .collect(),
            returntype: Box::new(substitute_symbolic_typevalue(&returntype, dlogger, env)),
        },
        TypeValue::Struct(fields) => TypeValue::Struct(
            fields
                .iter()
                .map(|(name, ty)| {
                    (
                        name.clone(),
                        substitute_symbolic_typevalue(ty, dlogger, env),
                    )
                })
                .collect(),
        ),
        TypeValue::Enum(fields) => TypeValue::Enum(
            fields
                .iter()
                .map(|(name, ty)| {
                    (
                        name.clone(),
                        substitute_symbolic_typevalue(ty, dlogger, env),
                    )
                })
                .collect(),
        ),
        TypeValue::Union(fields) => TypeValue::Union(
            fields
                .iter()
                .map(|(name, ty)| {
                    (
                        name.clone(),
                        substitute_symbolic_typevalue(ty, dlogger, env),
                    )
                })
                .collect(),
        ),
        TypeValue::Constructor { typarams, body } => TypeValue::Constructor {
            typarams: typarams.clone(),
            body: Box::new(substitute_symbolic_typevalue(&body, dlogger, env)),
        },
        TypeValue::Generic { typarams, body } => TypeValue::Generic {
            typarams: typarams.clone(),
            body: Box::new(substitute_symbolic_typevalue(&body, dlogger, env)),
        },
        TypeValue::Concretization {
            symbolic_constructor,
            tyargs,
        } => {
            let tyargs = tyargs
                .iter()
                .map(|ty| substitute_symbolic_typevalue(ty, dlogger, env))
                .collect::<Vec<_>>();
            match env.type_bindings[*symbolic_constructor].last() {
                Some(TypeValue::Constructor { typarams, body }) => {
                    for (typat, tyarg) in std::iter::zip(typarams, tyargs) {
                        env.intro_type_binding(typat, tyarg)
                    }
                    substitute_symbolic_typevalue(&body, dlogger, &mut env)
                }
                Some(_) => unreachable!("concretization of a non-constructor"),
                None => TypeValue::Concretization {
                    symbolic_constructor: *symbolic_constructor,
                    tyargs,
                },
            }
        }
        _ => v.clone(),
    }
}

pub fn typecheck_hir_elseexpr_checkmode(
    v: &Augmented<hir::ElseExpr>,
    dlogger: &mut DiagnosticLogger,
    val_name_table: &mut Vec<String>,
    val_type_table: &mut Vec<Vec<TypeValue>>,
    expected_type: &TypeValue,
) {
    match v.val {
        hir::ElseExpr::Error => {}
        hir::ElseExpr::Else(ref e) => typecheck_hir_blockexpr_checkmode(
            e,
            dlogger,
            val_name_table,
            val_type_table,
            expected_type,
        ),
        hir::ElseExpr::Elif {
            ref cond,
            ref then_branch,
            ref else_branch,
        } => {
            typecheck_hir_value_checkmode(
                cond,
                dlogger,
                val_name_table,
                val_type_table,
                &TypeValue::Bool,
            );
            typecheck_hir_blockexpr_checkmode(
                then_branch,
                dlogger,
                val_name_table,
                val_type_table,
                expected_type,
            );
            if let Some(else_branch) = else_branch {
                typecheck_hir_elseexpr_checkmode(
                    else_branch,
                    dlogger,
                    val_name_table,
                    val_type_table,
                    expected_type,
                );
            }
        }
    }
}

pub fn typecheck_hir_blockstatement(
    v: &Augmented<hir::BlockStatement>,
    dlogger: &mut DiagnosticLogger,
    val_name_table: &mut Vec<String>,
    val_type_table: &mut Vec<Vec<TypeValue>>,
) {
    match v.val {
        hir::BlockStatement::NoOp => {}
        // should already be typechecked
        hir::BlockStatement::TypeDef {
            typarams,
            typat,
            value,
        } => match typarams.as_slice() {
            [] => {
                let tyval = evaluate_hir_type(
                    &value,
                    dlogger,
                    val_name_table,
                    val_type_table,
                    &mut Vec::new(),
                );
                intro_type_binding_typat(&typat, tyval, val_type_table);
            }
        },
        hir::BlockStatement::ValDef {
            typarams,
            pat,
            value,
        } => {
            // need to push the type params onto the type stack
            // then we value in terms of
        }
        hir::BlockStatement::FnDef {
            typarams,
            identifier,
            params,
            returnty,
            body,
        } => todo!(),
        hir::BlockStatement::Set {
            ref place,
            ref value,
        } => {
            let ty = typecheck_hir_value_infermode(place, dlogger, val_name_table, val_type_table);
            typecheck_hir_value_checkmode(value, dlogger, val_name_table, val_type_table, &ty);
        }
        hir::BlockStatement::While { ref cond, ref body } => {
            typecheck_hir_value_checkmode(
                cond,
                dlogger,
                val_name_table,
                val_type_table,
                &TypeValue::Bool,
            );
            typecheck_hir_blockexpr_checkmode(
                body,
                dlogger,
                val_name_table,
                val_type_table,
                &TypeValue::Unit,
            );
        }
        hir::BlockStatement::For {
            pattern,
            start,
            end,
            by,
            body,
            ..
        } => {
            let index_type =
                typecheck_hir_value_infermode(&start, dlogger, val_name_table, val_type_table);
            typecheck_hir_value_checkmode(
                &end,
                dlogger,
                val_name_table,
                val_type_table,
                &index_type,
            );
            if let Some(by) = by {
                typecheck_hir_value_checkmode(
                    &by,
                    dlogger,
                    val_name_table,
                    val_type_table,
                    &index_type,
                );
            }
            typecheck_hir_blockexpr_checkmode(
                &body,
                dlogger,
                val_name_table,
                val_type_table,
                &TypeValue::Unit,
            );
        }
        hir::BlockStatement::Do(ref expr) => {
            typecheck_hir_value_checkmode(
                expr,
                dlogger,
                val_name_table,
                val_type_table,
                &TypeValue::Unit,
            );
        }
    }
}

pub fn typecheck_hir_blockexpr_checkmode(
    v: &Augmented<hir::BlockExpr>,
    dlogger: &mut DiagnosticLogger,
    val_name_table: &mut Vec<String>,
    val_type_table: &mut Vec<Vec<TypeValue>>,
    expected_type: &TypeValue,
) {
    for s in v.val.statements.iter() {
        typecheck_hir_blockstatement(s, dlogger, val_name_table, val_type_table);
    }
    if let Some(ref e) = v.val.last_expression {
        typecheck_hir_value_checkmode(e, dlogger, val_name_table, val_type_table, expected_type);
    }
}

pub fn typecheck_hir_blockexpr_infermode(
    v: &Augmented<hir::BlockExpr>,
    dlogger: &mut DiagnosticLogger,
    val_name_table: &mut Vec<String>,
    val_type_table: &mut Vec<Vec<TypeValue>>,
) -> TypeValue {
    for s in v.val.statements.iter() {
        typecheck_hir_blockstatement(s, dlogger, val_name_table, val_type_table);
    }
    if let Some(ref e) = v.val.last_expression {
        typecheck_hir_value_infermode(e, dlogger, val_name_table, val_type_table)
    } else {
        TypeValue::Unit
    }
}

pub fn typecheck_hir_value_infermode(
    v: &Augmented<hir::ValExpr>,
    dlogger: &mut DiagnosticLogger,
    val_name_table: &mut Vec<String>,
    val_type_table: &mut Vec<Vec<TypeValue>>,
) -> TypeValue {
    match &v.val {
        hir::ValExpr::Error => TypeValue::Error,
        hir::ValExpr::Unit => TypeValue::Unit,
        hir::ValExpr::Int(_) => TypeValue::Int(64),
        hir::ValExpr::Bool(_) => TypeValue::Bool,
        hir::ValExpr::Float(_) => TypeValue::Float(64),
        hir::ValExpr::String(_) => TypeValue::Slice(Box::new(TypeValue::Int(8))),
        hir::ValExpr::Identifier(id) => val_type_table[*id]
            .last()
            .expect("identifier not initialized with type")
            .clone(),
        hir::ValExpr::Ref(v) => TypeValue::Ref(Box::new(typecheck_hir_value_infermode(
            v,
            dlogger,
            val_name_table,
            val_type_table,
        ))),
        hir::ValExpr::Deref(v) => {
            let vtype = typecheck_hir_value_infermode(v, dlogger, val_name_table, val_type_table);
            match vtype {
                TypeValue::Ref(x) => *x,
                _ => {
                    dlogger.log_deref_of_non_reference(v.range);
                    TypeValue::Error
                }
            }
        }
        hir::ValExpr::StructLiteral(fields) => {
            let mut fieldtypes = Vec::new();
            for (name, field) in fields.iter_mut() {
                fieldtypes.push((
                    name.clone(),
                    typecheck_hir_value_infermode(field, dlogger, val_name_table, val_type_table),
                ));
            }
            TypeValue::Struct(fieldtypes)
        }
        hir::ValExpr::BinaryOp {
            op,
            left_operand,
            right_operand,
        } => match op {
            // math or numerical comparison
            hir::ValBinaryOpKind::Add
            | hir::ValBinaryOpKind::Sub
            | hir::ValBinaryOpKind::Mul
            | hir::ValBinaryOpKind::Div
            | hir::ValBinaryOpKind::Rem => {
                let left_type = typecheck_hir_value_infermode(
                    left_operand,
                    dlogger,
                    val_name_table,
                    val_type_table,
                );

                match left_type {
                    binop_type @ (TypeValue::Int(_) | TypeValue::UInt(_) | TypeValue::Float(_)) => {
                        typecheck_hir_value_checkmode(
                            right_operand,
                            dlogger,
                            val_name_table,
                            val_type_table,
                            &binop_type,
                        );
                        binop_type
                    }
                    _ => {
                        dlogger.log_math_on_non_numeric(v.range);
                        TypeValue::Error
                    }
                }
            }
            hir::ValBinaryOpKind::Lt
            | hir::ValBinaryOpKind::Leq
            | hir::ValBinaryOpKind::Gt
            | hir::ValBinaryOpKind::Geq => {
                let left_type = typecheck_hir_value_infermode(
                    left_operand,
                    dlogger,
                    val_name_table,
                    val_type_table,
                );

                match left_type {
                    TypeValue::Int(_) | TypeValue::UInt(_) | TypeValue::Float(_) => {
                        typecheck_hir_value_checkmode(
                            right_operand,
                            dlogger,
                            val_name_table,
                            val_type_table,
                            &left_type,
                        );
                        TypeValue::Bool
                    }
                    _ => {
                        dlogger.log_comparison_on_non_numeric(v.range);
                        TypeValue::Bool
                    }
                }
            }
            hir::ValBinaryOpKind::And | hir::ValBinaryOpKind::Or => {
                typecheck_hir_value_checkmode(
                    left_operand,
                    dlogger,
                    val_name_table,
                    val_type_table,
                    &TypeValue::Bool,
                );
                typecheck_hir_value_checkmode(
                    right_operand,
                    dlogger,
                    val_name_table,
                    val_type_table,
                    &TypeValue::Bool,
                );
                TypeValue::Bool
            }
            hir::ValBinaryOpKind::Eq | hir::ValBinaryOpKind::Neq => {
                let left_type = typecheck_hir_value_infermode(
                    left_operand,
                    dlogger,
                    val_name_table,
                    val_type_table,
                );
                typecheck_hir_value_checkmode(
                    right_operand,
                    dlogger,
                    val_name_table,
                    val_type_table,
                    &left_type,
                );

                match left_type {
                    TypeValue::Int(_) | TypeValue::UInt(_) | TypeValue::Float(_) => TypeValue::Bool,
                    _ => {
                        dlogger.log_equality_on_non_integral(left_operand.range);
                        TypeValue::Bool
                    }
                }
            }
        },
        hir::ValExpr::IfThen {
            cond,
            then_branch,
            else_branch,
        } => {
            typecheck_hir_value_checkmode(
                cond,
                dlogger,
                val_name_table,
                val_type_table,
                &TypeValue::Bool,
            );
            let if_type = typecheck_hir_blockexpr_infermode(
                then_branch,
                dlogger,
                val_name_table,
                val_type_table,
            );
            if let Some(elseexpr) = else_branch {
                typecheck_hir_elseexpr_checkmode(
                    elseexpr,
                    dlogger,
                    val_name_table,
                    val_type_table,
                    &if_type,
                );
            }
            if_type
        }
        hir::ValExpr::CaseOf {
            expr,
            first_case,
            rest_cases,
        } => {
            let expr_type =
                typecheck_hir_value_infermode(expr, dlogger, val_name_table, val_type_table);
            let first_case_type = typecheck_hir_case_checkpat_inferbody(
                first_case,
                dlogger,
                val_name_table,
                val_type_table,
                &expr_type,
            );
            for case in rest_cases {
                let case_type = typecheck_hir_case_checkpat_checkbody(
                    case,
                    dlogger,
                    val_name_table,
                    val_type_table,
                    &expr_type,
                    &first_case_type,
                );
            }
            first_case_type
        }
        hir::ValExpr::Block(block) => {
            typecheck_hir_blockexpr_infermode(block, dlogger, val_name_table, val_type_table)
        }
        hir::ValExpr::ArrayAccess { root, index } => {
            let rtype =
                typecheck_hir_value_infermode(root, dlogger, val_name_table, val_type_table);
            typecheck_hir_value_checkmode(
                index,
                dlogger,
                val_name_table,
                val_type_table,
                &TypeValue::Int(64),
            );
            match rtype {
                TypeValue::Array(x, _) => *x,
                _ => {
                    dlogger.log_array_access_of_non_array(root.range);
                    TypeValue::Error
                }
            }
        }
        hir::ValExpr::FieldAccess { root, field } => {
            let rtype =
                typecheck_hir_value_infermode(root, dlogger, val_name_table, val_type_table);
            match rtype {
                TypeValue::Struct(fields) => {
                    for (i, f) in fields.iter().enumerate() {
                        if f.0 == *field {
                            return f.1.clone();
                        }
                    }
                    dlogger.log_field_access_of_nonexistent_field(root.range, field);
                    TypeValue::Error
                }
                _ => {
                    dlogger.log_field_access_of_non_struct(root.range);
                    TypeValue::Error
                }
            }
        }
        hir::ValExpr::ArrayLiteral(ref elems) => {
            let elems_iter = elems.iter();
            match elems_iter.next() {
                None => {
                    dlogger.log_cannot_infer_array_type(v.range);
                    TypeValue::Error
                }
                Some(first) => {
                    let first_type = typecheck_hir_value_infermode(
                        first,
                        dlogger,
                        val_name_table,
                        val_type_table,
                    );
                    for elem in elems_iter {
                        typecheck_hir_value_checkmode(
                            elem,
                            dlogger,
                            val_name_table,
                            val_type_table,
                            &first_type,
                        );
                    }
                    TypeValue::Array(Box::new(first_type), elems.len().try_into().unwrap())
                }
            }
        }
        // concretize a generic val
        hir::ValExpr::Concretization { generic, tyargs } => {}
        hir::ValExpr::App { fun, args } => todo!(),
    }
}

// checks that the pattern is compatible with the type of the expression
pub fn typecheck_hir_pat_checkmode(
    pat: &Augmented<hir::PatExpr>,
    dlogger: &mut DiagnosticLogger,
    val_name_table: &mut Vec<String>,
    val_type_table: &mut Vec<Vec<TypeValue>>,
    expected_type: &TypeValue,
) {
    match pat.val {}
}

pub fn typecheck_hir_case_checkpat_inferbody(
    case: &Augmented<hir::CaseExpr>,
    dlogger: &mut DiagnosticLogger,
    val_name_table: &mut Vec<String>,
    val_type_table: &mut Vec<Vec<TypeValue>>,
    expr_type: &TypeValue,
) -> TypeValue {
    match (case.val.target.val, expr_type) {
        (hir::CaseTargetExpr::Unit, TypeValue::Unit) => {}
        (hir::CaseTargetExpr::Bool(_), TypeValue::Bool) => {}
        (hir::CaseTargetExpr::Int(i), TypeValue::Int(nbits)) => {
            if i >= BigInt::from(2).pow(*nbits as u32) {
                dlogger.log_int_too_large(case.val.target.range, *nbits as u32);
            } else if i < BigInt::from(2).pow(*nbits as u32 - 1) {
                dlogger.log_int_too_small(case.val.target.range, *nbits as u32);
            }
        }
        (hir::CaseTargetExpr::Int(i), TypeValue::UInt(nbits)) => {
            if i.is_negative() {
                dlogger.log_uint_negative(case.val.target.range);
            } else if i >= BigInt::from(2).pow(*nbits as u32) {
                dlogger.log_uint_too_large(case.val.target.range, *nbits as u32);
            }
        }
        (hir::CaseTargetExpr::PatExpr(ref pat), _) => {
            typecheck_hir_pat_checkmode(pat, dlogger, val_name_table, val_type_table, expr_type);
        }
        _ => {
            dlogger.log_case_target_type_mismatch(
                case.val.target.range,
                expr_type,
                &case.val.target.val,
            );
        }
    }
    typecheck_hir_value_infermode(&case.val.body, dlogger, val_name_table, val_type_table)
}

pub fn typecheck_hir_value_checkmode(
    v: &Augmented<hir::ValExpr>,
    dlogger: &mut DiagnosticLogger,
    val_name_table: &mut Vec<String>,
    val_type_table: &mut Vec<Vec<TypeValue>>,
    expected_type: &TypeValue,
) {
    match (v.val, expected_type) {
        // special case for literals
        (hir::ValExpr::Int(i), TypeValue::Int(nbits)) => {
            if i >= BigInt::from(2).pow(*nbits as u32) {
                dlogger.log_int_too_large(v.range, *nbits as u32);
            } else if i < BigInt::from(2).pow(*nbits as u32 - 1) {
                dlogger.log_int_too_small(v.range, *nbits as u32);
            }
        }
        (hir::ValExpr::Int(i), TypeValue::UInt(nbits)) => {
            if i.is_negative() {
                dlogger.log_uint_negative(v.range);
            } else if i >= BigInt::from(2).pow(*nbits as u32) {
                dlogger.log_uint_too_large(v.range, *nbits as u32);
            }
        }
        (hir::ValExpr::Float(_), TypeValue::Float(_)) => {}
        // block expressions
        (hir::ValExpr::Block(ref block), _) => {
            typecheck_hir_blockexpr_checkmode(
                block,
                dlogger,
                val_name_table,
                val_type_table,
                expected_type,
            );
        }
        (
            hir::ValExpr::IfThen {
                ref cond,
                ref then_branch,
                ref else_branch,
            },
            _,
        ) => {
            typecheck_hir_value_checkmode(
                cond,
                dlogger,
                val_name_table,
                val_type_table,
                &TypeValue::Bool,
            );
            typecheck_hir_blockexpr_checkmode(
                then_branch,
                dlogger,
                val_name_table,
                val_type_table,
                expected_type,
            );
            if let Some(ref elseexpr) = else_branch {
                typecheck_hir_elseexpr_checkmode(
                    elseexpr,
                    dlogger,
                    val_name_table,
                    val_type_table,
                    expected_type,
                );
            }
        }
        (
            hir::ValExpr::CaseOf {
                ref expr,
                ref first_case,
                ref rest_cases,
            },
            _,
        ) => {
            let expr_type =
                typecheck_hir_value_infermode(expr, dlogger, val_name_table, val_type_table);
            typecheck_hir_case_checkpat_checkbody(
                first_case,
                dlogger,
                val_name_table,
                val_type_table,
                &expr_type,
                expected_type,
            );
            for case in rest_cases {
                typecheck_hir_case_checkpat_checkbody(
                    case,
                    dlogger,
                    val_name_table,
                    val_type_table,
                    &expr_type,
                    expected_type,
                );
            }
        }
        // everything else
        _ => {
            let actual_type =
                typecheck_hir_value_infermode(v, dlogger, val_name_table, val_type_table);
            if actual_type.is_subtype_of(expected_type) {
                dlogger.log_type_mismatch(v.range, expected_type, &actual_type);
            }
        }
    }
}
