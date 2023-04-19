use crate::dlogger::DiagnosticLogger;
use crate::hir;
use crate::hir::Augmented;
use lsp_types::Range;
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

pub fn print_type_value(ty: &TypeValue, env: &Env) -> String {
    match ty {
        TypeValue::Error => "Error".to_string(),
        TypeValue::SymbolicVariable(id) => {
            format!("SymbolicVariable({})", env.type_name_table[*id])
        }
        TypeValue::Nominal(id) => format!("Nominal({})", env.type_name_table[*id]),
        TypeValue::Unit => "Unit".to_string(),
        TypeValue::Bool => "Bool".to_string(),
        TypeValue::RefConstructor => "RefConstructor".to_string(),
        TypeValue::ArrayConstructor => "ArrayConstructor".to_string(),
        TypeValue::SliceConstructor => "SliceConstructor".to_string(),
        TypeValue::IntConstructor => "IntConstructor".to_string(),
        TypeValue::UIntConstructor => "UIntConstructor".to_string(),
        TypeValue::FloatConstructor => "FloatConstructor".to_string(),
        TypeValue::Ref(ty) => format!("Ref({})", print_type_value(ty, env)),
        TypeValue::Array(ty, size) => format!(
            "Array({}, {})",
            print_type_value(ty, env),
            print_type_value(size, env)
        ),
        TypeValue::Slice(ty) => format!("Slice({})", print_type_value(ty, env)),
        TypeValue::Int(ty) => format!("Int({})", print_type_value(ty, env)),
        TypeValue::UInt(ty) => format!("UInt({})", print_type_value(ty, env)),
        TypeValue::Float(ty) => format!("Float({})", print_type_value(ty, env)),
        TypeValue::IntLit(n) => format!("IntLit({})", n),
        TypeValue::BoolLit(b) => format!("BoolLit({})", b),
        TypeValue::FloatLit(n) => format!("FloatLit({})", n),
        TypeValue::Fn {
            paramtys,
            returntype,
        } => format!(
            "Fn {{ paramtys: [{}], returntype: {} }}",
            paramtys
                .iter()
                .map(|ty| print_type_value(ty, env))
                .collect::<Vec<String>>()
                .join(", "),
            print_type_value(returntype, env)
        ),
        TypeValue::Struct(fields) => format!(
            "Struct {{ fields: [{}] }}",
            fields
                .iter()
                .map(|(name, ty)| format!("{}: {}", name, print_type_value(ty, env)))
                .collect::<Vec<String>>()
                .join(", ")
        ),
        TypeValue::Enum(fields) => format!(
            "Enum {{ fields: [{}] }}",
            fields
                .iter()
                .map(|(name, ty)| format!("{}: {}", name, print_type_value(ty, env)))
                .collect::<Vec<String>>()
                .join(", ")
        ),
        TypeValue::Union(fields) => format!(
            "Union {{ fields: [{}] }}",
            fields
                .iter()
                .map(|(name, ty)| format!("{}: {}", name, print_type_value(ty, env)))
                .collect::<Vec<String>>()
                .join(", ")
        ),
        TypeValue::Constructor { typarams, body } => format!(
            "Constructor {{ typarams: [{}], body: {} }}",
            typarams
                .iter()
                .map(|typaram| print_typaram(typaram, env))
                .collect::<Vec<String>>()
                .join(", "),
            print_type_value(body, env)
        ),
        TypeValue::Generic { typarams, body } => format!(
            "Generic {{ typarams: [{}], body: {} }}",
            typarams
                .iter()
                .map(|typaram| print_typaram(typaram, env))
                .collect::<Vec<String>>()
                .join(", "),
            print_type_value(body, env)
        ),
        TypeValue::Concretization {
            symbolic_constructor,
            tyargs,
        } => format!(
            "Concretization {{ symbolic_constructor: {}, tyargs: [{}] }}",
            env.type_name_table[*symbolic_constructor],
            tyargs
                .iter()
                .map(|ty| print_type_value(ty, env))
                .collect::<Vec<String>>()
                .join(", ")
        ),
    }
}

fn print_typaram(typaram: &TypeParam, env: &Env) -> String {
    match typaram {
        TypeParam::Error => "Error".to_string(),
        TypeParam::SymbolicVariable { ref id, ref kind } => {
            format!("Identifier {{ id: {}, kind: {} }}", id, kind)
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
pub enum TypeParam {
    Error,
    SymbolicVariable { id: usize, kind: KindValue },
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
        typarams: Vec<TypeParam>,
        body: Box<TypeValue>,
    },
    // type of a generic value. Instantiated at every use point
    Generic {
        typarams: Vec<TypeParam>,
        body: Box<TypeValue>,
    },
    // where the type constructor is symbolic
    Concretization {
        symbolic_constructor: usize,
        tyargs: Vec<TypeValue>,
    },
}

impl TypeValue {
    fn subst(self: &Self, env: &Env) -> TypeValue {
        match self {
            TypeValue::SymbolicVariable(id) => match env.type_bindings[*id].last() {
                Some(ty) => ty.clone(),
                None => TypeValue::SymbolicVariable(*id),
            },
            TypeValue::Ref(v) => TypeValue::Ref(Box::new(v.subst(env))),
            TypeValue::Array(v, s) => {
                TypeValue::Array(Box::new(v.subst(env)), Box::new(s.subst(env)))
            }
            TypeValue::Slice(v) => TypeValue::Slice(Box::new(v.subst(env))),
            TypeValue::Int(v) => TypeValue::Int(Box::new(v.subst(env))),
            TypeValue::UInt(v) => TypeValue::UInt(Box::new(v.subst(env))),
            TypeValue::Float(v) => TypeValue::Float(Box::new(v.subst(env))),
            TypeValue::Fn {
                paramtys,
                returntype,
            } => TypeValue::Fn {
                paramtys: paramtys.iter().map(|ty| ty.subst(env)).collect(),
                returntype: Box::new(returntype.subst(env)),
            },
            TypeValue::Struct(fields) => TypeValue::Struct(
                fields
                    .iter()
                    .map(|(name, ty)| (name.clone(), ty.subst(env)))
                    .collect(),
            ),
            TypeValue::Enum(fields) => TypeValue::Enum(
                fields
                    .iter()
                    .map(|(name, ty)| (name.clone(), ty.subst(env)))
                    .collect(),
            ),
            TypeValue::Union(fields) => TypeValue::Union(
                fields
                    .iter()
                    .map(|(name, ty)| (name.clone(), ty.subst(env)))
                    .collect(),
            ),
            TypeValue::Constructor { typarams, body } => TypeValue::Constructor {
                typarams: typarams.clone(),
                body: Box::new(body.subst(env)),
            },
            TypeValue::Generic { typarams, body } => TypeValue::Generic {
                typarams: typarams.clone(),
                body: Box::new(body.subst(env)),
            },
            TypeValue::Concretization {
                symbolic_constructor,
                tyargs,
            } => {
                let tyargs = tyargs.iter().map(|ty| ty.subst(env)).collect::<Vec<_>>();
                match env.type_bindings[*symbolic_constructor].last() {
                    Some(constructor) => concretize_type_expr(constructor, tyargs, env),
                    None => TypeValue::Concretization {
                        symbolic_constructor: *symbolic_constructor,
                        tyargs,
                    },
                }
            }
            _ => self.clone(),
        }
    }
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
    fn bind_typaram(&mut self, typaram: &TypeParam, tyval: TypeValue) {
        match typaram {
            TypeParam::Error => {}
            TypeParam::SymbolicVariable { id, .. } => {
                self.type_bindings[*id].push(tyval);
            }
        }
    }
    // removes the variables bound by type pattern from this scope
    fn unbind_typaram(&mut self, typaram: &TypeParam) {
        match typaram {
            TypeParam::Error => {}
            TypeParam::SymbolicVariable { id, .. } => {
                self.type_bindings[*id].pop().unwrap();
            }
        }
    }
}

fn concretize_type_expr(constructor: &TypeValue, tyargs: Vec<TypeValue>, env: &Env) -> TypeValue {
    match constructor {
        TypeValue::Error => TypeValue::Error,
        TypeValue::SymbolicVariable(id) => TypeValue::Concretization {
            symbolic_constructor: *id,
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
                env.bind_typaram(typat, tyarg)
            }
            let body = body.subst(env);
            for ref typat in typarams {
                env.unbind_typaram(typat)
            }
            body
        }
        _ => {
            unreachable!("concretization of a non-generic");
        }
    }
}

// evaluate a type expression in a context
// NOTE: we assume that all identifiers have already been resolved.
// NOTE: we assume that all types are well-kinded
pub fn typecheck_hir_type_infermode(
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
                s_fields.push((
                    identifier,
                    typecheck_hir_type_infermode(&expr, dlogger, env),
                ));
            }
            TypeValue::Struct(s_fields)
        }
        hir::TypeExpr::Enum(fields) => {
            let mut s_fields = vec![];
            for (identifier, expr) in fields {
                s_fields.push((
                    identifier,
                    typecheck_hir_type_infermode(&expr, dlogger, env),
                ));
            }
            TypeValue::Enum(s_fields)
        }
        hir::TypeExpr::Union(fields) => {
            let mut s_fields = vec![];
            for (identifier, expr) in fields {
                s_fields.push((
                    identifier,
                    typecheck_hir_type_infermode(&expr, dlogger, env),
                ));
            }
            TypeValue::Union(s_fields)
        }
        hir::TypeExpr::Fn { paramtys, returnty } => TypeValue::Fn {
            paramtys: paramtys
                .iter()
                .map(|x| typecheck_hir_type_infermode(x, dlogger, env))
                .collect(),
            returntype: Box::new(typecheck_hir_type_infermode(&returnty, dlogger, env)),
        },
        // substitute the generic arguments into the body
        hir::TypeExpr::Concretization { genericty, tyargs } => {
            // first we evaluate the type of the generic function (peeking past identifiers)
            let generic_val =
                env.evaluate_nominal(typecheck_hir_type_infermode(&genericty, dlogger, env));
            let tyargs = tyargs
                .iter()
                .map(|x| typecheck_hir_type_infermode(x, dlogger, env))
                .collect::<Vec<_>>();
            concretize_type_expr(&generic_val, tyargs, env)
        }
    }
}

pub fn typecheck_hir_elseexpr_checkmode(
    v: &Augmented<hir::ElseExpr>,
    dlogger: &mut DiagnosticLogger,
    env: &mut Env,
    expected_type: &TypeValue,
) {
    match v.val {
        hir::ElseExpr::Error => {}
        hir::ElseExpr::Else(ref e) => {
            typecheck_hir_blockexpr_checkmode(e, dlogger, env, expected_type)
        }
        hir::ElseExpr::Elif {
            ref cond,
            ref then_branch,
            ref else_branch,
        } => {
            typecheck_hir_value_checkmode(cond, dlogger, env, &TypeValue::Bool);
            typecheck_hir_blockexpr_checkmode(then_branch, dlogger, env, expected_type);
            if let Some(else_branch) = else_branch {
                typecheck_hir_elseexpr_checkmode(else_branch, dlogger, env, expected_type);
            }
        }
    }
}

fn hir_typat_to_param(
    v: &Augmented<hir::TypePatExpr>,
    dlogger: &mut DiagnosticLogger,
    _env: &mut Env,
) -> TypeParam {
    match v.val {
        hir::TypePatExpr::Error => TypeParam::Error,
        hir::TypePatExpr::Identifier { id, ref kind } => TypeParam::SymbolicVariable {
            id,
            kind: evaluate_hir_kind(kind, dlogger),
        },
    }
}

pub fn typecheck_hir_blockstatement(
    v: &Augmented<hir::BlockStatement>,
    dlogger: &mut DiagnosticLogger,
    env: &mut Env,
) {
    match v.val {
        hir::BlockStatement::NoOp => {}
        // should already be typechecked
        hir::BlockStatement::TypeDef {
            ref typarams,
            ref typat,
            ref value,
        } => {
            for typat in typarams {
                intro_hir_typat_symbolic(typat, dlogger, env);
            }
            let tyval = typecheck_hir_type_infermode(value, dlogger, env);
            let typarams = typarams
                .iter()
                .map(|x| hir_typat_to_param(x, dlogger, env))
                .collect::<Vec<_>>();
            intro_hir_typat(typat, dlogger, env, tyval, typarams);
        }
        hir::BlockStatement::ValDef {
            ref typarams,
            ref pat,
            ref value,
        } => {
            for typat in typarams {
                intro_hir_typat_symbolic(typat, dlogger, env);
            }
            let valty = typecheck_hir_value_infermode(value, dlogger, env);
            let typarams = typarams
                .iter()
                .map(|x| hir_typat_to_param(x, dlogger, env))
                .collect::<Vec<_>>();
            intro_and_typecheck_hir_pat_checkmode(pat, dlogger, env, &valty, &typarams);
        }
        hir::BlockStatement::FnDef {
            ref typarams,
            ref identifier,
            ref params,
            ref returnty,
            ref body,
        } => {
            for typat in typarams {
                intro_hir_typat_symbolic(typat, dlogger, env);
            }
            let typarams = typarams
                .iter()
                .map(|x| hir_typat_to_param(x, dlogger, env))
                .collect::<Vec<_>>();
            // get param types and intro them
            let params = params
                .iter()
                .map(|x| intro_and_typecheck_hir_pat_infermode(x, dlogger, env, &typarams))
                .collect::<Vec<_>>();
            // get returntype
            let returnty = typecheck_hir_type_infermode(returnty, dlogger, env);

            // intro the function
            env.val_type_table[*identifier] = Some(TypeValue::Fn {
                paramtys: params,
                returntype: Box::new(returnty),
            }); 

            // typecheck the body now that we know what type it should be as well as the param types
            typecheck_hir_blockexpr_checkmode(body, dlogger, env, &returnty);
        }
        hir::BlockStatement::Set {
            ref place,
            ref value,
        } => {
            let ty = typecheck_hir_value_infermode(place, dlogger, env);
            typecheck_hir_value_checkmode(value, dlogger, env, &ty);
        }
        hir::BlockStatement::While { ref cond, ref body } => {
            typecheck_hir_value_checkmode(cond, dlogger, env, &TypeValue::Bool);
            typecheck_hir_blockexpr_checkmode(body, dlogger, env, &TypeValue::Unit);
        }
        hir::BlockStatement::For {
            pattern,
            start,
            end,
            by,
            body,
            ..
        } => {
            let index_type = typecheck_hir_value_infermode(&start, dlogger, env);
            typecheck_hir_value_checkmode(&end, dlogger, env, &index_type);
            if let Some(by) = by {
                typecheck_hir_value_checkmode(&by, dlogger, env, &index_type);
            }
            typecheck_hir_blockexpr_checkmode(&body, dlogger, env, &TypeValue::Unit);
        }
        hir::BlockStatement::Do(ref expr) => {
            typecheck_hir_value_checkmode(expr, dlogger, env, &TypeValue::Unit);
        }
    }
}

pub fn typecheck_hir_blockexpr_checkmode(
    v: &Augmented<hir::BlockExpr>,
    dlogger: &mut DiagnosticLogger,
    env: &mut Env,
    expected_type: &TypeValue,
) {
    for s in v.val.statements.iter() {
        typecheck_hir_blockstatement(s, dlogger, env);
    }
    if let Some(ref e) = v.val.last_expression {
        typecheck_hir_value_checkmode(e, dlogger, env, expected_type);
    }
}

pub fn typecheck_hir_blockexpr_infermode(
    v: &Augmented<hir::BlockExpr>,
    dlogger: &mut DiagnosticLogger,
    env: &mut Env,
) -> TypeValue {
    for s in v.val.statements.iter() {
        typecheck_hir_blockstatement(s, dlogger, env);
    }
    if let Some(ref e) = v.val.last_expression {
        typecheck_hir_value_infermode(e, dlogger, env)
    } else {
        TypeValue::Unit
    }
}

pub fn typecheck_hir_value_infermode(
    v: &Augmented<hir::ValExpr>,
    dlogger: &mut DiagnosticLogger,
    env: &mut Env,
) -> TypeValue {
    match &v.val {
        hir::ValExpr::Error => TypeValue::Error,
        hir::ValExpr::Unit => TypeValue::Unit,
        hir::ValExpr::Int(_) => TypeValue::Int(Box::new(TypeValue::IntLit(64.into()))),
        hir::ValExpr::Bool(_) => TypeValue::Bool,
        hir::ValExpr::Float(_) => TypeValue::Float(Box::new(TypeValue::IntLit(64.into()))),
        hir::ValExpr::String(_) => TypeValue::Slice(Box::new(TypeValue::Int(Box::new(
            TypeValue::IntLit(8.into()),
        )))),
        hir::ValExpr::Identifier(id) => env.val_type_table[*id]
            .expect("identifier not initialized with type")
            .clone(),
        hir::ValExpr::Ref(v) => {
            TypeValue::Ref(Box::new(typecheck_hir_value_infermode(v, dlogger, env)))
        }
        hir::ValExpr::Deref(v) => {
            let vtype = typecheck_hir_value_infermode(v, dlogger, env);
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
                    typecheck_hir_value_infermode(field, dlogger, env),
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
                let left_type = typecheck_hir_value_infermode(left_operand, dlogger, env);

                match left_type {
                    binop_type @ (TypeValue::Int(_) | TypeValue::UInt(_) | TypeValue::Float(_)) => {
                        typecheck_hir_value_checkmode(right_operand, dlogger, env, &binop_type);
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
                let left_type = typecheck_hir_value_infermode(left_operand, dlogger, env);

                match left_type {
                    TypeValue::Int(_) | TypeValue::UInt(_) | TypeValue::Float(_) => {
                        typecheck_hir_value_checkmode(right_operand, dlogger, env, &left_type);
                        TypeValue::Bool
                    }
                    _ => {
                        dlogger.log_comparison_on_non_numeric(v.range);
                        TypeValue::Bool
                    }
                }
            }
            hir::ValBinaryOpKind::And | hir::ValBinaryOpKind::Or => {
                typecheck_hir_value_checkmode(left_operand, dlogger, env, &TypeValue::Bool);
                typecheck_hir_value_checkmode(right_operand, dlogger, env, &TypeValue::Bool);
                TypeValue::Bool
            }
            hir::ValBinaryOpKind::Eq | hir::ValBinaryOpKind::Neq => {
                let left_type = typecheck_hir_value_infermode(left_operand, dlogger, env);
                typecheck_hir_value_checkmode(right_operand, dlogger, env, &left_type);

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
            typecheck_hir_value_checkmode(cond, dlogger, env, &TypeValue::Bool);
            let if_type = typecheck_hir_blockexpr_infermode(then_branch, dlogger, env);
            if let Some(elseexpr) = else_branch {
                typecheck_hir_elseexpr_checkmode(elseexpr, dlogger, env, &if_type);
            }
            if_type
        }
        hir::ValExpr::CaseOf {
            expr,
            first_case,
            rest_cases,
        } => {
            let expr_type = typecheck_hir_value_infermode(expr, dlogger, env);
            intro_and_typecheck_hir_targetexpr(&first_case.val.target, dlogger, env, &expr_type);
            let first_case_type = typecheck_hir_value_infermode(&first_case.val.body, dlogger, env);
            for case in rest_cases {
                intro_and_typecheck_hir_targetexpr(&case.val.target, dlogger, env, &expr_type);
                typecheck_hir_value_checkmode(&case.val.body, dlogger, env, &first_case_type);
            }
            first_case_type
        }
        hir::ValExpr::Block(block) => typecheck_hir_blockexpr_infermode(block, dlogger, env),
        hir::ValExpr::ArrayAccess { root, index } => {
            let rtype = typecheck_hir_value_infermode(root, dlogger, env);
            typecheck_hir_value_checkmode(
                index,
                dlogger,
                env,
                &TypeValue::Int(Box::new(TypeValue::IntLit(64.into()))),
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
            let rtype = typecheck_hir_value_infermode(root, dlogger, env);
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
        hir::ValExpr::ArrayLiteral(ref elems) => match elems.as_slice() {
            [] => {
                dlogger.log_cannot_infer_array_type(v.range);
                TypeValue::Error
            }
            [first, others @ ..] => {
                let first_type = typecheck_hir_value_infermode(first, dlogger, env);
                for elem in others {
                    typecheck_hir_value_checkmode(elem, dlogger, env, &first_type);
                }
                TypeValue::Array(
                    Box::new(first_type),
                    Box::new(TypeValue::IntLit(elems.len().into())),
                )
            }
        },
        // concretize a generic val
        hir::ValExpr::Concretization { generic, tyargs } => {
            let generic_type = typecheck_hir_value_infermode(generic, dlogger, env);
            let tyargs = tyargs
                .iter()
                .map(|tyarg| typecheck_hir_type_infermode(tyarg, dlogger, env))
                .collect::<Vec<_>>();
            match generic_type {
                TypeValue::Generic {
                    ref body,
                    ref typarams,
                } => {
                    if tyargs.len() != typarams.len() {
                        dlogger.log_wrong_number_type_args(v.range, typarams.len(), tyargs.len());
                        TypeValue::Error
                    } else {
                        for (tyval, typat) in std::iter::zip(tyargs, typarams) {
                            env.bind_typaram(typat, tyval);
                        }
                        let ty = body.subst(env);
                        for typat in typarams {
                            env.unbind_typaram(typat);
                        }
                        ty
                    }
                }
                _ => {
                    dlogger.log_concretization_of_non_generic(v.range);
                    TypeValue::Error
                }
            }
        }
        hir::ValExpr::App { fun, args } => todo!(),
    }
}

fn intro_hir_typat(
    typat: &Augmented<hir::TypePatExpr>,
    dlogger: &mut DiagnosticLogger,
    env: &mut Env,
    typeval: TypeValue,
    typarams: Vec<TypeParam>,
) {
    match typat.val {
        hir::TypePatExpr::Error => {}
        hir::TypePatExpr::Identifier { id, kind } => match typarams.len() {
            0 => env.type_value_table[id] = Some(typeval),
            _ => {
                env.type_value_table[id] = Some(TypeValue::Constructor {
                    typarams,
                    body: Box::new(typeval),
                })
            }
        },
    }
}

fn intro_hir_typat_symbolic(
    typat: &Augmented<hir::TypePatExpr>,
    dlogger: &mut DiagnosticLogger,
    env: &mut Env,
) {
    match typat.val {
        hir::TypePatExpr::Error => {}
        hir::TypePatExpr::Identifier { id, kind } => {
            env.type_value_table[id] = Some(TypeValue::SymbolicVariable(id));
        }
    }
}

fn intro_and_typecheck_hir_pat_checkmode(
    pat: &Augmented<hir::PatExpr>,
    dlogger: &mut DiagnosticLogger,
    env: &mut Env,
    expected_type: &TypeValue,
    typarams: &Vec<TypeParam>,
) {
    match pat.val {
        hir::PatExpr::Error => {}
        hir::PatExpr::Ignore => {}
        hir::PatExpr::Identifier { mutable, id } => {
            env.val_type_table[id] = match typarams.len() {
                0 => Some(expected_type.clone()),
                _ => Some(TypeValue::Generic {
                    body: Box::new(expected_type.clone()),
                    typarams: typarams.clone(),
                }),
            };
        }
        hir::PatExpr::StructLiteral(got_fields) => match expected_type {
            TypeValue::Struct(expected_fields) => {
                for (ref field_name, ref field_pat) in got_fields {
                    let field_type = expected_fields
                        .iter()
                        .find(|(n, _)| n == field_name)
                        .map(|(_, t)| t);
                    match field_type {
                        Some(field_type) => {
                            intro_and_typecheck_hir_pat_checkmode(
                                field_pat, dlogger, env, field_type, typarams,
                            );
                        }
                        None => {
                            dlogger.log_struct_pattern_field_not_in_struct(
                                field_pat.range,
                                field_name,
                            );
                        }
                    }
                }
            }
            _ => {
                dlogger.log_struct_pattern_on_non_struct(pat.range);
            }
        },
        hir::PatExpr::Typed { ref pat, ref ty } => {
            let pat_ty = typecheck_hir_type_infermode(ty, dlogger, env);
            if !pat_ty.is_subtype_of(expected_type) {
                dlogger.log_type_mismatch(
                    ty.range,
                    &print_type_value(expected_type, env),
                    &print_type_value(&pat_ty, env),
                );
            }
            intro_and_typecheck_hir_pat_checkmode(pat, dlogger, env, &pat_ty, typarams);
        }
    }
}

// this function recursively introduces all variables in a pattern into the environment, with the error type.
// we do this to ensure that following pieces of code can assume that all variables are in the environment.
fn intro_hir_pat_errors(
    pat: &Augmented<hir::PatExpr>,
    dlogger: &mut DiagnosticLogger,
    env: &mut Env,
    typarams: &Vec<TypeParam>,
) {
    match pat.val {
        hir::PatExpr::Error => {}
        hir::PatExpr::Ignore => {}
        hir::PatExpr::Identifier { mutable, id } => {
            env.val_type_table[id] = match typarams.len() {
                0 => Some(TypeValue::Error),
                _ => Some(TypeValue::Generic {
                    body: Box::new(TypeValue::Error),
                    typarams: typarams.clone(),
                }),
            };
        }
        hir::PatExpr::StructLiteral(got_fields) => {
            for (_, ref field_pat) in got_fields {
                intro_hir_pat_errors(field_pat, dlogger, env, typarams);
            }
        }
        hir::PatExpr::Typed { ref pat, ref ty } => {
            intro_hir_pat_errors(pat, dlogger, env, typarams);
        }
    }
}

fn intro_and_typecheck_hir_pat_infermode(
    pat: &Augmented<hir::PatExpr>,
    dlogger: &mut DiagnosticLogger,
    env: &mut Env,
    typarams: &Vec<TypeParam>,
) -> TypeValue {
    match pat.val {
        hir::PatExpr::Error => TypeValue::Error,
        hir::PatExpr::Ignore => {
            dlogger.log_cannot_infer_pattern_type(pat.range);
            TypeValue::Error
        }
        hir::PatExpr::Identifier { .. } => {
            dlogger.log_cannot_infer_pattern_type(pat.range);
            intro_hir_pat_errors(pat, dlogger, env, typarams);
            TypeValue::Error
        }
        hir::PatExpr::StructLiteral(_) => {
            dlogger.log_cannot_infer_pattern_type(pat.range);
            intro_hir_pat_errors(pat, dlogger, env, typarams);
            TypeValue::Error
        }
        hir::PatExpr::Typed { ref pat, ref ty } => {
            let pat_ty = typecheck_hir_type_infermode(ty, dlogger, env);
            intro_and_typecheck_hir_pat_checkmode(pat, dlogger, env, &pat_ty, typarams);
            pat_ty
        }
    }
}

fn typecheck_numerical_literal(
    i: &BigInt,
    nbits: &TypeValue,
    signed: bool,
    dlogger: &mut DiagnosticLogger,
    range: Range,
) {
    // the number of bits needed to fully represent the magnitude of i
    let ibits = BigInt::from(i.bits());
    // the number of bits needed to fully represent the magnitude of (i + 1)
    let i_plus_one_bits = BigInt::from((i + BigInt::from(1)).bits());
    match nbits {
        TypeValue::IntLit(nbits) => {
            if signed {
                if i.is_negative() && &i_plus_one_bits >= nbits {
                    dlogger.log_int_too_small(range, nbits);
                } else if ibits >= nbits - 1 {
                    dlogger.log_int_too_large(range, nbits);
                }
            } else {
                if i.is_negative() {
                    dlogger.log_uint_negative(range);
                } else if &ibits >= nbits {
                    dlogger.log_uint_too_large(range, nbits);
                }
            }
        }
        TypeValue::SymbolicVariable(_) => {}
        _ => unreachable!("should have been kindchecked"),
    }
}

fn intro_and_typecheck_hir_targetexpr(
    target: &Augmented<hir::CaseTargetExpr>,
    dlogger: &mut DiagnosticLogger,
    env: &mut Env,
    expr_type: &TypeValue,
) {
    match (target.val, expr_type) {
        (hir::CaseTargetExpr::Error, _) => {}
        (hir::CaseTargetExpr::Unit, TypeValue::Unit) => {}
        (hir::CaseTargetExpr::Bool(_), TypeValue::Bool) => {}
        (hir::CaseTargetExpr::Int(ref i), TypeValue::Int(ref nbits)) => {
            typecheck_numerical_literal(i, nbits, true, dlogger, target.range)
        }
        (hir::CaseTargetExpr::Int(ref i), TypeValue::UInt(ref nbits)) => {
            typecheck_numerical_literal(i, nbits, false, dlogger, target.range)
        }
        (hir::CaseTargetExpr::PatExpr(ref pat), _) => {
            intro_and_typecheck_hir_pat_checkmode(pat, dlogger, env, expr_type, &vec![]);
        }
        _ => {
            let case_target_str = match target.val {
                hir::CaseTargetExpr::Error => unreachable!(),
                hir::CaseTargetExpr::Unit => "unit".to_string(),
                hir::CaseTargetExpr::Bool(_) => "bool".to_string(),
                hir::CaseTargetExpr::Int(_) => "int".to_string(),
                hir::CaseTargetExpr::PatExpr(_) => unreachable!(),
            };
            dlogger.log_case_target_type_mismatch(
                target.range,
                &print_type_value(expr_type, env),
                &case_target_str,
            );
        }
    }
}

pub fn typecheck_hir_value_checkmode(
    v: &Augmented<hir::ValExpr>,
    dlogger: &mut DiagnosticLogger,
    env: &mut Env,
    expected_type: &TypeValue,
) {
    match (v.val, expected_type) {
        (hir::ValExpr::Float(_), TypeValue::Float(_)) => {}
        // block expressions
        (hir::ValExpr::Block(ref block), _) => {
            typecheck_hir_blockexpr_checkmode(block, dlogger, env, expected_type);
        }
        (
            hir::ValExpr::IfThen {
                ref cond,
                ref then_branch,
                ref else_branch,
            },
            _,
        ) => {
            typecheck_hir_value_checkmode(cond, dlogger, env, &TypeValue::Bool);
            typecheck_hir_blockexpr_checkmode(then_branch, dlogger, env, expected_type);
            if let Some(ref elseexpr) = else_branch {
                typecheck_hir_elseexpr_checkmode(elseexpr, dlogger, env, expected_type);
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
            let expr_type = typecheck_hir_value_infermode(expr, dlogger, env);
            intro_and_typecheck_hir_targetexpr(&first_case.val.target, dlogger, env, &expr_type);
            typecheck_hir_value_checkmode(&first_case.val.body, dlogger, env, expected_type);
            for case in rest_cases {
                intro_and_typecheck_hir_targetexpr(&case.val.target, dlogger, env, &expr_type);
                typecheck_hir_value_checkmode(&case.val.body, dlogger, env, expected_type);
            }
        }
        // everything else
        _ => {
            let actual_type = typecheck_hir_value_infermode(v, dlogger, env);
            if actual_type.is_subtype_of(expected_type) {
                dlogger.log_type_mismatch(
                    v.range,
                    &print_type_value(expected_type, env),
                    &print_type_value(&actual_type, env),
                );
            }
        }
    }
}
