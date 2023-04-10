use std::collections::HashMap;

use crate::ast::Augmented;
use crate::dlogger::DiagnosticLogger;
use crate::hir;
use lsp_types::Range;
use num_bigint::BigInt;
use num_rational::BigRational;

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
    Identifier(usize),
    // types
    UnitTy,
    BoolTy,
    // constructors
    RefConstructorTy,
    ArrayConstructorTy,
    SliceConstructorTy,
    IntConstructorTy,
    UIntConstructorTy,
    FloatConstructorTy,
    // the constructed type
    RefTy(Box<TypeValue>),
    ArrayTy(Box<TypeValue>, u64),
    SliceTy(Box<TypeValue>),
    IntTy(u8),
    UIntTy(u8),
    FloatTy(u8),
    // const literals
    Int(BigInt),
    Bool(bool),
    Float(BigRational),
    // type of a function
    Fn {
        paramtys: Vec<TypeValue>,
        returntype: Box<TypeValue>,
    },
    // struct and enumify
    Struct(Vec<(String, TypeValue)>),
    Enum(Vec<(String, TypeValue)>),
    Union(Vec<(String, TypeValue)>),
    Constructor {
        typarams: Vec<Augmented<hir::TypePatExpr>>,
        returnkind: Box<KindValue>,
        body: Box<Augmented<hir::TypeExpr>>,
    },
}

// in infermode, we don't already know what kind the given expression has to be, so we infer it.
// if we reach a concretize node, then we can do kindchecking_checkmode, because we know what the type arguments have to be
// if we encounter an error, we log it, and modify the hir to be an error.
// NOTE: we assume that all identifiers have already been resolved.
pub fn kindcheck_hir_type_infermode(
    v: &mut Augmented<hir::TypeExpr>,
    dlogger: &mut DiagnosticLogger,
    type_name_table: &mut Vec<String>,
    type_kind_table: &mut Vec<Option<KindValue>>,
) -> KindValue {
    match v.val {
        hir::TypeExpr::Error => KindValue::Error,
        hir::TypeExpr::Identifier(_) => {
            unreachable!("identifiers should have been resolved by now")
        }
        hir::TypeExpr::Identifier2(id) => type_kind_table[id].expect("kind not initialized yet"),
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
        hir::TypeExpr::Fn { paramtys, tyreturn } => {
            for ref mut arg in paramtys {
                kindcheck_hir_type_checkmode(
                    arg,
                    &KindValue::Type,
                    dlogger,
                    type_name_table,
                    type_kind_table,
                );
            }
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
    v: &mut Augmented<hir::TypeExpr>,
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

// introduces the variables bound by type pattern to this scope
fn intro_typepat(
    typat: Augmented<hir::TypePatExpr>,
    tyval: TypeValue,
    type_typevalue_table: &mut Vec<Vec<TypeValue>>,
) {
    match typat.val {
        hir::TypePatExpr::Error => {}
        hir::TypePatExpr::Identifier { .. } => {
            unreachable!("identifiers should be resolved by now")
        }
        hir::TypePatExpr::Identifier2 { identifier, .. } => {
            type_typevalue_table[identifier].push(tyval);
        }
    }
}
// removes the variables bound by type pattern from this scope
fn elim_typepat(
    typat: Augmented<hir::TypePatExpr>,
    type_typevalue_table: &mut Vec<Vec<TypeValue>>,
) {
    match typat.val {
        hir::TypePatExpr::Error => {}
        hir::TypePatExpr::Identifier { .. } => {
            unreachable!("identifiers should be resolved by now")
        }
        hir::TypePatExpr::Identifier2 { identifier, .. } => {
            type_typevalue_table[identifier].pop();
        }
    }
}

// evaluate a type expression in a context
// NOTE: we assume that all identifiers have already been resolved.
// NOTE: we assume that all types are well-kinded
pub fn evaluate_hir_type(
    v: &Augmented<hir::TypeExpr>,
    dlogger: &mut DiagnosticLogger,
    type_name_table: &mut Vec<String>,
    // this is a cache, and the values may change depending on instantiation
    type_typevalue_table: &mut Vec<Vec<TypeValue>>,
) -> TypeValue {
    match v.val {
        hir::TypeExpr::Error => TypeValue::Error,
        hir::TypeExpr::Identifier(_) => unreachable!("identifiers should be resolved by now"),
        hir::TypeExpr::Identifier2(id) => TypeValue::Identifier(id),
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
                    evaluate_hir_type(&expr, dlogger, type_name_table, type_typevalue_table),
                ));
            }
            TypeValue::Struct(s_fields)
        }
        hir::TypeExpr::Enum(fields) => {
            let mut s_fields = vec![];
            for (identifier, expr) in fields {
                s_fields.push((
                    identifier,
                    evaluate_hir_type(&expr, dlogger, type_name_table, type_typevalue_table),
                ));
            }
            TypeValue::Enum(s_fields)
        }
        hir::TypeExpr::Union(fields) => {
            let mut s_fields = vec![];
            for (identifier, expr) in fields {
                s_fields.push((
                    identifier,
                    evaluate_hir_type(&expr, dlogger, type_name_table, type_typevalue_table),
                ));
            }
            TypeValue::Union(s_fields)
        }
        hir::TypeExpr::Fn { paramtys, tyreturn } => TypeValue::Fn {
            paramtys: paramtys
                .iter()
                .map(|x| evaluate_hir_type(x, dlogger, type_name_table, type_typevalue_table))
                .collect(),
            returntype: Box::new(evaluate_hir_type(
                &tyreturn,
                dlogger,
                type_name_table,
                type_typevalue_table,
            )),
        },
        // substitute the generic arguments into the body
        hir::TypeExpr::Concretization { genericty, tyargs } => {
            // first we evaluate the type of the generic function (peeking past identifiers)
            fn evaluate_past_identifiers(
                v: TypeValue,
                type_typevalue_table: &mut Vec<Vec<TypeValue>>,
            ) -> TypeValue {
                match v {
                    TypeValue::Identifier(id) => evaluate_past_identifiers(
                        type_typevalue_table[id].last().unwrap().clone(),
                        type_typevalue_table,
                    ),
                    x => x,
                }
            }
            let generic_val = evaluate_past_identifiers(
                evaluate_hir_type(&genericty, dlogger, type_name_table, type_typevalue_table),
                type_typevalue_table,
            );
            match generic_val {
                TypeValue::Error => TypeValue::Error,
                TypeValue::RefConstructorTy => {
                    assert!(tyargs.len() == 1, "wrong number of arguments");
                    TypeValue::RefTy(Box::new(evaluate_hir_type(
                        &tyargs[0],
                        dlogger,
                        type_name_table,
                        type_typevalue_table,
                    )))
                }
                TypeValue::ArrayConstructorTy => {
                    assert!(tyargs.len() == 2, "wrong number of arguments");
                    let elem_type = evaluate_hir_type(
                        &tyargs[0],
                        dlogger,
                        type_name_table,
                        type_typevalue_table,
                    );
                    let elem_num_tyval = evaluate_hir_type(
                        &tyargs[1],
                        dlogger,
                        type_name_table,
                        type_typevalue_table,
                    );
                    let elem_num = match elem_num_tyval {
                        TypeValue::Int(x) => x.try_into().unwrap_or_else(|_| {
                            dlogger.log_type_error(v.range, &format!("array size invalid: {}", x));
                            0
                        }),
                        // should have been kindchecked
                        _ => unreachable!(),
                    };
                    TypeValue::ArrayTy(Box::new(elem_type), elem_num)
                }
                TypeValue::SliceConstructorTy => {
                    assert!(tyargs.len() == 1, "wrong number of arguments");
                    TypeValue::SliceTy(Box::new(evaluate_hir_type(
                        &tyargs[0],
                        dlogger,
                        type_name_table,
                        type_typevalue_table,
                    )))
                }
                TypeValue::IntConstructorTy => {
                    assert!(tyargs.len() == 1, "wrong number of arguments");
                    let bits_num_tyval = evaluate_hir_type(
                        &tyargs[0],
                        dlogger,
                        type_name_table,
                        type_typevalue_table,
                    );
                    let bits_num = match bits_num_tyval {
                        TypeValue::Int(x) => x.try_into().unwrap_or_else(|_| {
                            dlogger.log_type_error(v.range, &format!("num bits invalid: {}", x));
                            0
                        }),
                        // should have been kindchecked
                        _ => unreachable!(),
                    };
                    TypeValue::IntTy(bits_num)
                }
                TypeValue::UIntConstructorTy => {
                    assert!(tyargs.len() == 1, "wrong number of arguments");
                    let bits_num_tyval = evaluate_hir_type(
                        &tyargs[0],
                        dlogger,
                        type_name_table,
                        type_typevalue_table,
                    );
                    let bits_num = match bits_num_tyval {
                        TypeValue::Int(x) => x.try_into().unwrap_or_else(|_| {
                            dlogger.log_type_error(v.range, &format!("num bits invalid: {}", x));
                            0
                        }),
                        // should have been kindchecked
                        _ => unreachable!(),
                    };
                    TypeValue::UIntTy(bits_num)
                }
                TypeValue::FloatConstructorTy => {
                    assert!(tyargs.len() == 1, "wrong number of arguments");
                    let bits_num_tyval = evaluate_hir_type(
                        &tyargs[0],
                        dlogger,
                        type_name_table,
                        type_typevalue_table,
                    );
                    let bits_num = match bits_num_tyval {
                        TypeValue::Int(x) => x.try_into().unwrap_or_else(|_| {
                            dlogger.log_type_error(v.range, &format!("num bits invalid: {}", x));
                            0
                        }),
                        // should have been kindchecked
                        _ => unreachable!(),
                    };
                    TypeValue::FloatTy(bits_num)
                }
                TypeValue::Constructor { typarams, body, .. } => {
                    assert!(typarams.len() == tyargs.len(), "wrong number of arguments");
                    let tyvalues = tyargs
                        .iter()
                        .map(|x| {
                            evaluate_hir_type(x, dlogger, type_name_table, type_typevalue_table)
                        })
                        .collect();
                    for (typat, tyval) in std::iter::zip(typarams.iter(), tyvalues.iter()) {
                        intro_typepat(typat, tyval, type_typevalue_table)
                    }
                    let bodytype =
                        evaluate_hir_type(&body, dlogger, type_name_table, type_typevalue_table);
                    for (typat, tyval) in std::iter::zip(typarams.iter(), tyvalues.iter()) {
                        elim_typepat(typat, type_typevalue_table)
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

pub fn typecheck_hir_value_infermode(
    v: &mut Augmented<hir::ValExpr>,
    dlogger: &mut DiagnosticLogger,
    val_name_table: &mut Vec<String>,
    val_type_table: &mut Vec<Option<TypeValue>>,
) -> TypeValue {
    match &mut v.value {
        hir::ValExpr::Error => TypeValue::Error,
        hir::ValExpr::Unit => TypeValue::UnitTy,
        hir::ValExpr::Int(_) => TypeValue::IntTy(64),
        hir::ValExpr::Bool(_) => TypeValue::BoolTy,
        hir::ValExpr::Float(_) => TypeValue::FloatTy(64),
        hir::ValExpr::String(_) => TypeValue::SliceTy(Box::new(TypeValue::IntTy(8))),
        hir::ValExpr::Ref(v) => TypeValue::RefTy(Box::new(typecheck_hir_value_infermode(
            v,
            dlogger,
            val_name_table,
            val_type_table,
        ))),
        hir::ValExpr::Deref(v) => {
            let vtype = typecheck_hir_value_infermode(v, dlogger, val_name_table, val_type_table);
            match vtype {
                TypeValue::RefTy(x) => *x,
                _ => {
                    dlogger.log_deref_of_non_ref(v.range);
                    TypeValue::Error
                }
            }
        }
        hir::ValExpr::StructLiteral(fields) => {
            let mut fieldtypes = Vec::new();
            for (name, field) in fields.iter_mut() {
                fieldtypes.push(typecheck_hir_value_infermode(
                    field,
                    dlogger,
                    val_name_table,
                    val_type_table,
                ));
            }
            TypeValue::StructTy(fieldtypes)
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
                typecheck_hir_value_checkmode(
                    right_operand,
                    dlogger,
                    val_name_table,
                    val_type_table,
                    &left_type,
                );

                match left_type {
                    k @ (TypeValue::IntTy(_) | TypeValue::UIntTy(_) | TypeValue::FloatTy(_)) => k,
                    _ => {
                        dlogger.log_type_error(left_operand.range, "expected int, uint, or float");
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
                typecheck_hir_value_checkmode(
                    right_operand,
                    dlogger,
                    val_name_table,
                    val_type_table,
                    &left_type,
                );

                match left_type {
                    TypeValue::IntTy(_) | TypeValue::UIntTy(_) | TypeValue::FloatTy(_) => {
                        TypeValue::BoolTy
                    }
                    _ => {
                        dlogger.log_type_error(left_operand.range, "expected int, uint, or float");
                        TypeValue::BoolTy
                    }
                }
            }
            hir::ValBinaryOpKind::And | hir::ValBinaryOpKind::Or => {
                let _ = typecheck_hir_value_checkmode(
                    left_operand,
                    dlogger,
                    val_name_table,
                    val_type_table,
                    &TypeValue::BoolTy,
                );
                typecheck_hir_value_checkmode(
                    right_operand,
                    dlogger,
                    val_name_table,
                    val_type_table,
                    &TypeValue::BoolTy,
                );
                TypeValue::BoolTy
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
                    TypeValue::IntTy(_) | TypeValue::UIntTy(_) | TypeValue::FloatTy(_) => {
                        TypeValue::BoolTy
                    }
                    _ => {
                        dlogger.log_type_error(
                            left_operand.range,
                            "expected int, uint, float, or bool",
                        );
                        TypeValue::BoolTy
                    }
                }

                TypeValue::BoolTy
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
                &TypeValue::BoolTy,
            );
            let if_type =
                typecheck_hir_value_infermode(then_branch, dlogger, val_name_table, val_type_table);
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
        hir::ValExpr::CaseOf { expr, cases } => {
            let expr_type = typecheck_hir_value_infermode(expr, dlogger, val_name_table, val_type_table);
            let mut case_types = Vec::new();
            for case in cases.iter_mut() {
                let case_type = typecheck_hir_case_checkmode(
                    case,
                    dlogger,
                    val_name_table,
                    val_type_table,
                    &expr_type,
                );
                case_types.push(case_type);
            }
            let mut case_types_iter = case_types.iter();
            let first_case_type = case_types_iter.next().unwrap();
            for case_type in case_types_iter {
                if case_type != first_case_type {
                    dlogger.log_type_error(
                        v.range,
                        "all cases must have the same type",
                    );
                    return TypeValue::Error;
                }
            }
            first_case_type.clone()
        }
        hir::ValExpr::Block(block) => {
            for statement in block.val.statements {
                match statement.val {
                    hir::BlockStatement::NoOp => {},
                    hir::BlockStatement::TypeDef { .. } => {},
                    hir::BlockStatement::Use { .. } => unreachable!("should have been resolved"),
                    hir::BlockStatement::Let { pat, value, .. } => {
                        
                    }
                }
            }
        }
    }
}
