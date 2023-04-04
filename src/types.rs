use std::collections::HashMap;

use crate::ast::Augmented;
use crate::dlogger::DiagnosticLogger;
use crate::hir;
use num_bigint::BigInt;
use num_rational::BigRational;

#[derive(Clone, Debug, PartialEq)]
pub enum KindValue {
    Error,
    Type,
    Int,
    Float,
    Bool,
    // this is the kind of a generic function
    TypeLevelFn {
        args: Vec<KindValue>,
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
            KindValue::TypeLevelFn { args, returnkind } => {
                write!(f, "TypeLevelFn {{ args: [")?;
                for arg in args.iter() {
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
        hir::KindExpr::TypeLevelFn { args, returnkind } => {
            let mut argkinds = vec![];
            for arg in args.iter() {
                argkinds.push(evaluate_hir_kind(arg, dlogger));
            }
            KindValue::TypeLevelFn {
                args: argkinds,
                returnkind: Box::new(evaluate_hir_kind(&returnkind, dlogger)),
            }
        }
    }
}

#[derive(Clone, Debug)]
pub enum TypeValue {
    // An error when parsing
    Error,
    Identifier(String),
    // types
    UnitTy,
    BoolTy,
    RefConstructorTy,
    ArrayConstructorTy,
    SliceConstructorTy,
    IntConstructorTy,
    UIntConstructorTy,
    FloatConstructorTy,
    // const literals
    Int(BigInt),
    Bool(bool),
    Float(BigRational),
    // type of a function
    Fn {
        args: Vec<TypeValue>,
        returntype: Box<TypeValue>,
    },
    // struct and enumify
    Struct(Vec<(String, TypeValue)>),
    Enum(Vec<(String, TypeValue)>),
    Union(Vec<(String, TypeValue)>),
    // this is a generic
    TypeLevelFn {
        args: Vec<Augmented<hir::TypePatExpr>>,
        returnkind: Box<Augmented<hir::KindExpr>>,
        body: Box<Augmented<hir::TypeExpr>>,
    },
}

pub fn get_if_in_scope<T: Clone>(s: &String, scope: &Vec<HashMap<String, T>>) -> Option<T> {
    for scope in scope.iter().rev() {
        if let Some(v) = scope.get(s) {
            return Some(v.clone());
        }
    }
    None
}

// in infermode, we don't already know what kind the given expression has to be, so we infer it.
// if we reach a concretize node, then we can do kindchecking_checkmode, because we know what the type arguments have to be
// if we encounter an error, we log it, and modify the hir to be an error.
// NOTE: we assume that all identifiers have already been resolved.
pub fn kindcheck_hir_type_infermode(
    v: &mut Augmented<hir::TypeExpr>,
    dlogger: &mut DiagnosticLogger,
    kind_scope: &mut Vec<HashMap<String, KindValue>>,
) -> KindValue {
    match v.val {
        hir::TypeExpr::Error => KindValue::Error,
        hir::TypeExpr::Identifier(ref id) => get_if_in_scope(id, kind_scope).unwrap(),
        hir::TypeExpr::UnitTy => KindValue::Type,
        hir::TypeExpr::BoolTy => KindValue::Type,
        hir::TypeExpr::RefConstructorTy => KindValue::TypeLevelFn {
            args: vec![KindValue::Type],
            returnkind: Box::new(KindValue::Type),
        },
        hir::TypeExpr::ArrayConstructorTy => KindValue::TypeLevelFn {
            args: vec![KindValue::Type, KindValue::Int],
            returnkind: Box::new(KindValue::Type),
        },
        hir::TypeExpr::SliceConstructorTy => KindValue::TypeLevelFn {
            args: vec![KindValue::Type],
            returnkind: Box::new(KindValue::Type),
        },
        hir::TypeExpr::IntConstructorTy => KindValue::TypeLevelFn {
            args: vec![KindValue::Int],
            returnkind: Box::new(KindValue::Type),
        },
        hir::TypeExpr::UIntConstructorTy => KindValue::TypeLevelFn {
            args: vec![KindValue::Int],
            returnkind: Box::new(KindValue::Type),
        },
        hir::TypeExpr::FloatConstructorTy => KindValue::TypeLevelFn {
            args: vec![KindValue::Int],
            returnkind: Box::new(KindValue::Type),
        },
        hir::TypeExpr::Int(_) => KindValue::Int,
        hir::TypeExpr::Bool(_) => KindValue::Bool,
        hir::TypeExpr::Float(_) => KindValue::Float,
        hir::TypeExpr::Fn { args, returntype } => {
            for ref mut arg in args {
                kindcheck_hir_type_checkmode(arg, &KindValue::Type, dlogger, kind_scope);
            }
            KindValue::Type
        }
        hir::TypeExpr::Struct(ref fields) => {
            for (_, ref mut expr) in fields {
                kindcheck_hir_type_checkmode(expr, &KindValue::Type, dlogger, kind_scope);
            }
            KindValue::Type
        }
        hir::TypeExpr::Enum(ref fields) => {
            for (_, ref mut expr) in fields {
                kindcheck_hir_type_checkmode(expr, &KindValue::Type, dlogger, kind_scope);
            }
            KindValue::Type
        }
        hir::TypeExpr::Union(ref fields) => {
            for (_, ref mut expr) in fields {
                kindcheck_hir_type_checkmode(expr, &KindValue::Type, dlogger, kind_scope);
            }
            KindValue::Type
        }
        hir::TypeExpr::Concretization {
            ref mut generic,
            tyargs: mut provided_args,
        } => {
            let kind_of_generic = kindcheck_hir_type_infermode(generic, dlogger, kind_scope);
            match kind_of_generic {
                KindValue::TypeLevelFn {
                    args: expected_kinds,
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
                            kindcheck_hir_type_checkmode(tyarg, argkind, dlogger, kind_scope);
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
    v: &mut Augmented<hir::TypeExpr>,
    expected_kind: &KindValue,
    dlogger: &mut DiagnosticLogger,
    kind_scope: &mut Vec<HashMap<String, KindValue>>,
) {
    match v.val {
        hir::TypeExpr::Error => {}
        k => {
            let kind_of_k = kindcheck_hir_type_infermode(v, dlogger, kind_scope);
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

// evaluate a type expression in a context
// NOTE: we assume that all identifiers have already been resolved.
// NOTE: we assume that all types are well-kinded
pub fn evaluate_hir_type(
    v: &Augmented<hir::TypeExpr>,
    dlogger: &mut DiagnosticLogger,
    type_scope: &mut Vec<HashMap<String, TypeValue>>,
    kind_scope: &mut Vec<HashMap<String, KindValue>>,
) -> TypeValue {
    match v.val {
        hir::TypeExpr::Error => TypeValue::Error,
        hir::TypeExpr::Identifier(s) => TypeValue::Identifier(s),
        hir::TypeExpr::UnitTy => TypeValue::UnitTy,
        hir::TypeExpr::BoolTy => TypeValue::BoolTy,
        hir::TypeExpr::RefConstructorTy => TypeValue::RefConstructorTy,
        hir::TypeExpr::ArrayConstructorTy => TypeValue::ArrayConstructorTy,
        hir::TypeExpr::SliceConstructorTy => TypeValue::SliceConstructorTy,
        hir::TypeExpr::IntConstructorTy => TypeValue::IntConstructorTy,
        hir::TypeExpr::UIntConstructorTy => TypeValue::UIntConstructorTy,
        hir::TypeExpr::FloatConstructorTy => TypeValue::FloatConstructorTy,
        hir::TypeExpr::Int(x) => TypeValue::Int(x),
        hir::TypeExpr::Bool(x) => TypeValue::Bool(x),
        hir::TypeExpr::Float(x) => TypeValue::Float(x),
        hir::TypeExpr::Struct(fields) => {
            let mut s_fields = vec![];
            for (identifier, expr) in fields {
                s_fields.push((
                    identifier,
                    evaluate_hir_type(&expr, dlogger, type_scope, kind_scope),
                ));
            }
            TypeValue::Struct(s_fields)
        }
        hir::TypeExpr::Enum(fields) => {
            let mut s_fields = vec![];
            for (identifier, expr) in fields {
                s_fields.push((
                    identifier,
                    evaluate_hir_type(&expr, dlogger, type_scope, kind_scope),
                ));
            }
            TypeValue::Enum(s_fields)
        }
        hir::TypeExpr::Union(fields) => {
            let mut s_fields = vec![];
            for (identifier, expr) in fields {
                s_fields.push((
                    identifier,
                    evaluate_hir_type(&expr, dlogger, type_scope, kind_scope),
                ));
            }
            TypeValue::Union(s_fields)
        }
        hir::TypeExpr::Fn { args, returntype } => TypeValue::Fn {
            args: args
                .iter()
                .map(|x| evaluate_hir_type(x, dlogger, type_scope, kind_scope))
                .collect(),
            returntype: Box::new(evaluate_hir_type(
                &returntype,
                dlogger,
                type_scope,
                kind_scope,
            )),
        },
        // substitute the generic arguments into the body
        hir::TypeExpr::Concretization { generic, tyargs } => {
            // first we evaluate the type of the generic function (peeking past identifiers)
            fn evaluate_past_identifiers(
                v: TypeValue,
                type_scope: &mut Vec<HashMap<String, TypeValue>>,
            ) -> TypeValue {
                match v {
                    TypeValue::Identifier(ref s) => evaluate_past_identifiers(
                        get_if_in_scope(s, type_scope).unwrap(),
                        type_scope,
                    ),
                    x => x,
                }
            }
            let generic_val = evaluate_past_identifiers(
                evaluate_hir_type(&generic, dlogger, type_scope, kind_scope),
                type_scope,
            );
            match generic_val {
                TypeValue::Error => TypeValue::Error,
                TypeValue::TypeLevelFn { args, body, .. } => {
                    let mut new_type_scope = HashMap::new();
                    let mut new_kind_scope = HashMap::new();
                    assert!(args.len() == tyargs.len(), "wrong number of arguments");
                    for (typat, provided_tyarg_raw) in std::iter::zip(args.iter(), tyargs.iter()) {
                        let provided_tyarg =
                            evaluate_hir_type(provided_tyarg_raw, dlogger, type_scope, kind_scope);
                        match typat.val {
                            hir::TypePatExpr::Error => {}
                            hir::TypePatExpr::Identifier { identifier, kind } => {
                                new_type_scope.insert(identifier.clone(), provided_tyarg.clone());
                                new_kind_scope
                                    .insert(identifier.clone(), evaluate_hir_kind(&kind, dlogger));
                            }
                        }
                    }
                    type_scope.push(new_type_scope);
                    kind_scope.push(new_kind_scope);
                    let bodytype = evaluate_hir_type(&body, dlogger, type_scope, kind_scope);
                    type_scope.pop();
                    kind_scope.pop();
                    bodytype
                }
                _ => {
                    unreachable!("concretization of a non-generic");
                }
            }
        }
    }
}
