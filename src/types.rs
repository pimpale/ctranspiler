use std::collections::HashMap;

use crate::ast::Augmented;
use crate::dlogger::DiagnosticLogger;
use crate::hir::{self};
use lsp_types::Range;
use num_bigint::BigInt;
use num_rational::BigRational;

#[derive(Clone, Debug, PartialEq)]
pub enum KindValue {
    Error,
    Type,
    Int,
    UInt,
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
            KindValue::UInt => write!(f, "UInt"),
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
        hir::KindExpr::UInt => KindValue::UInt,
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
    ArrayTy,
    SliceTy,
    BitIntTy,
    UBitIntTy,
    BitFloatTy,
    BoolTy,
    // const literals
    Int(BigInt),
    Bool(bool),
    Float(BigRational),
    // unary ops (syntax sugar)
    Ref(Box<TypeValue>),
    // struct and enumify
    Struct(Vec<(String, TypeValue)>),
    Enum(Vec<(String, TypeValue)>),
    Union(Vec<(String, TypeValue)>),
    // type of a function
    Fn {
        args: Vec<TypeValue>,
        returntype: Box<TypeValue>,
    },
    // this is a generic
    TypeLevelFn {
        args: Vec<Augmented<hir::TypePatExpr>>,
        returnkind: Box<Augmented<hir::KindExpr>>,
        body: Box<Augmented<hir::TypeExpr>>,
    },
}

pub fn get_if_in_scope<T: Clone>(
    s: &String,
    dlogger: &mut DiagnosticLogger,
    scope: &mut Vec<HashMap<String, T>>,
) -> Option<T> {
    for scope in scope.iter().rev() {
        if let Some(v) = scope.get(s) {
            return Some(v.clone());
        }
    }
    None
}

pub fn kind_of(
    v: &TypeValue,
    dlogger: &mut DiagnosticLogger,
    kind_scope: &mut Vec<HashMap<String, KindValue>>,
) -> KindValue {
    match v {
        TypeValue::Error => KindValue::Error,
        TypeValue::Identifier(id) => get_if_in_scope(id, dlogger, kind_scope).unwrap(),
        TypeValue::UnitTy => KindValue::Type,
        TypeValue::ArrayTy => KindValue::TypeLevelFn {
            args: vec![KindValue::Type, KindValue::UInt],
            returnkind: Box::new(KindValue::Type),
        },
        TypeValue::SliceTy => KindValue::TypeLevelFn {
            args: vec![KindValue::Type],
            returnkind: Box::new(KindValue::Type),
        },
        TypeValue::BitIntTy => KindValue::TypeLevelFn {
            args: vec![KindValue::UInt],
            returnkind: Box::new(KindValue::Type),
        },
        TypeValue::UBitIntTy => KindValue::TypeLevelFn {
            args: vec![KindValue::UInt],
            returnkind: Box::new(KindValue::Type),
        },
        TypeValue::BitFloatTy => KindValue::TypeLevelFn {
            args: vec![KindValue::UInt],
            returnkind: Box::new(KindValue::Type),
        },
        TypeValue::BoolTy => KindValue::Type,
        TypeValue::Int(_) => KindValue::Int,
        TypeValue::Bool(_) => KindValue::Bool,
        TypeValue::Float(_) => KindValue::Float,
        TypeValue::Ref(_) => KindValue::Type,
        TypeValue::Struct(_) => KindValue::Type,
        TypeValue::Enum(_) => KindValue::Type,
        TypeValue::Union(_) => KindValue::Type,
        TypeValue::TypeLevelFn {
            args, returnkind, ..
        } => KindValue::TypeLevelFn {
            args: args
                .iter()
                .map(|arg| match arg.val {
                    hir::TypePatExpr::Error => KindValue::Error,
                    hir::TypePatExpr::Identifier { kind, .. } => evaluate_hir_kind(&kind, dlogger),
                })
                .collect(),
            returnkind: Box::new(evaluate_hir_kind(returnkind, dlogger)),
        },
        TypeValue::Fn { .. } => KindValue::Type,
    }
}

pub fn deep_evaluate_hir_type(
    v: &Augmented<hir::TypeExpr>,
    dlogger: &mut DiagnosticLogger,
    type_scope: &mut Vec<HashMap<String, TypeValue>>,
    kind_scope: &mut Vec<HashMap<String, KindValue>>,
) -> TypeValue {
    match v.val {
        hir::TypeExpr::Identifier(s) => get_if_in_scope(&s, dlogger, type_scope).unwrap(),
        _ => evaluate_hir_type(v, dlogger, type_scope, kind_scope),
    }
}

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
        hir::TypeExpr::ArrayTy => TypeValue::ArrayTy,
        hir::TypeExpr::SliceTy => TypeValue::SliceTy,
        hir::TypeExpr::IntTy => TypeValue::BitIntTy,
        hir::TypeExpr::UIntTy => TypeValue::UBitIntTy,
        hir::TypeExpr::FloatTy => TypeValue::BitFloatTy,
        hir::TypeExpr::BoolTy => TypeValue::BoolTy,
        hir::TypeExpr::Int(x) => TypeValue::Int(x),
        hir::TypeExpr::Bool(x) => TypeValue::Bool(x),
        hir::TypeExpr::Float(x) => TypeValue::Float(x),
        hir::TypeExpr::Ref(x) => TypeValue::Ref(Box::new(evaluate_hir_type(
            &x, dlogger, type_scope, kind_scope,
        ))),
        hir::TypeExpr::Struct(fields) => {
            let mut s_fields = vec![];
            for k in fields.iter() {
                match k.val {
                    hir::StructItemExpr::Error => {}
                    hir::StructItemExpr::Identified { identifier, expr } => {
                        s_fields.push((
                            identifier,
                            evaluate_hir_type(&expr, dlogger, type_scope, kind_scope),
                        ));
                    }
                }
            }
            TypeValue::Struct(s_fields)
        }
        hir::TypeExpr::Enum(fields) => {
            let mut s_fields = vec![];
            for k in fields.iter() {
                match k.val {
                    hir::StructItemExpr::Error => {}
                    hir::StructItemExpr::Identified { identifier, expr } => {
                        s_fields.push((
                            identifier,
                            evaluate_hir_type(&expr, dlogger, type_scope, kind_scope),
                        ));
                    }
                }
            }
            TypeValue::Enum(s_fields)
        }
        hir::TypeExpr::Union(fields) => {
            let mut s_fields = vec![];
            for k in fields.iter() {
                match k.val {
                    hir::StructItemExpr::Error => {}
                    hir::StructItemExpr::Identified { identifier, expr } => {
                        s_fields.push((
                            identifier,
                            evaluate_hir_type(&expr, dlogger, type_scope, kind_scope),
                        ));
                    }
                }
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
            let generic_val = deep_evaluate_hir_type(&generic, dlogger, type_scope, kind_scope);
            match generic_val {
                TypeValue::Error => TypeValue::Error,
                TypeValue::TypeLevelFn { args, body, .. } => {
                    let mut new_type_scope = HashMap::new();
                    let mut new_kind_scope = HashMap::new();
                    if tyargs.len() != args.len() {
                        dlogger.log_wrong_number_type_args(v.range, args.len(), tyargs.len());
                        TypeValue::Error
                    } else {
                        for (typat, provided_tyarg_raw) in
                            std::iter::zip(args.iter(), tyargs.iter())
                        {
                            let provided_tyarg = evaluate_hir_type(
                                provided_tyarg_raw,
                                dlogger,
                                type_scope,
                                kind_scope,
                            );
                            match typat.val {
                                hir::TypePatExpr::Error => {}
                                hir::TypePatExpr::Identifier { identifier, kind } => {
                                    let argkind = evaluate_hir_kind(&kind, dlogger);
                                    check_kind_compatibility(
                                        &argkind,
                                        &kind_of(&provided_tyarg, dlogger, kind_scope),
                                        provided_tyarg_raw.range,
                                        dlogger,
                                    );
                                    new_type_scope
                                        .insert(identifier.clone(), provided_tyarg.clone());
                                    new_kind_scope.insert(identifier.clone(), argkind);
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
                }
                _ => {
                    dlogger.log_not_a_type_level_fn(generic.range);
                    TypeValue::Error
                }
            }
        }
    }
}

fn check_kind_compatibility(
    expected: &KindValue,
    actual: &KindValue,
    range: Range,
    dlogger: &mut DiagnosticLogger,
) {
    if expected != actual {
        dlogger.log_kind_mismatch(range, &format!("{}", expected), &format!("{}", actual));
    }
}
