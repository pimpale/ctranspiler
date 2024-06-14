use std::collections::HashMap;

use indexmap::IndexMap;
use num_bigint::{BigInt, TryFromBigIntError};
use num_rational::{BigRational, Ratio};
use num_traits::ToPrimitive;

use crate::dlogger::DiagnosticLogger;
use crate::hir::Augmented;
use crate::typecheck::{evaluate_hir_kind, TypeChecker};
use crate::types::{KindValue, TypeValue, TypeValueConstructor};
use crate::{hir, typecheck};

// gets a hint but doesn't bind any free variables yet
// doesn't thoroughly traverse the tree, so make sure to call typecheck_type_expr_and_patch on this node afterwards
pub fn typehint_of_val_pat_and_patch(
    v: &mut Augmented<hir::ValPatExpr>,
    dlogger: &mut DiagnosticLogger,
    checker: &mut TypeChecker,
) -> TypeValue {
    match &mut v.val {
        hir::ValPatExpr::Error => TypeValue::Unknown,
        hir::ValPatExpr::Ignore => TypeValue::Unknown,
        hir::ValPatExpr::Identifier { mutable, id } => TypeValue::Unknown,
        hir::ValPatExpr::StructLiteral(fields) => TypeValue::Struct(
            fields
                .iter_mut()
                .map(|(name, expr)| {
                    (
                        name.val.clone(),
                        typehint_of_val_pat_and_patch(expr, dlogger, checker),
                    )
                })
                .collect(),
        ),
        hir::ValPatExpr::New { ty, .. } => typecheck_type_expr_and_patch(ty, dlogger, checker),
        hir::ValPatExpr::Typed { ty, .. } => typecheck_type_expr_and_patch(ty, dlogger, checker),
    }
}

// helper function to make reporting expected kind errors simpler
fn expect_type<T>(
    v: &mut Augmented<T>,
    expected_hint: &TypeValue,
    actual: TypeValue,
    dlogger: &mut DiagnosticLogger,
) -> TypeValue
where
    T: std::default::Default,
{
    if expected_hint.supports_assign(&actual) {
        actual
    } else {
        dlogger.log_type_mismatch(v.range, &expected_hint.to_string(), &actual.to_string());
        v.val = T::default();
        TypeValue::Unknown
    }
}

pub fn typecheck_type_pat_expr_and_patch(
    v: &mut Augmented<hir::TypePatExpr>,
    expected_type: &TypeValue,
    _dlogger: &mut DiagnosticLogger,
    checker: &mut TypeChecker,
) -> TypeValue {
    match &mut v.val {
        hir::TypePatExpr::Error => TypeValue::Unknown,
        hir::TypePatExpr::Identifier(id) => {
            checker.type_type_table[*id] = Some(expected_type.clone());
            expected_type.clone()
        }
        hir::TypePatExpr::Typed { id, .. } => {
            checker.type_type_table[*id] = Some(expected_type.clone());
            expected_type.clone()
        }
    }
}

fn subst(val: &TypeValue, bindings: &HashMap<usize, TypeValue>) -> TypeValue {
    match val {
        TypeValue::SymbolicVariable(id) => match bindings.get(id) {
            Some(ty) => ty.clone(),
            None => TypeValue::SymbolicVariable(*id),
        },
        TypeValue::Fn {
            paramtys,
            returntype,
        } => TypeValue::Fn {
            paramtys: paramtys.iter().map(|ty| subst(ty, bindings)).collect(),
            returntype: Box::new(subst(returntype, bindings)),
        },
        TypeValue::Struct(fields) => TypeValue::Struct(
            fields
                .iter()
                .map(|(name, ty)| (name.clone(), subst(ty, bindings)))
                .collect(),
        ),
        TypeValue::Enum(fields) => TypeValue::Enum(
            fields
                .iter()
                .map(|(name, ty)| (name.clone(), subst(ty, bindings)))
                .collect(),
        ),
        TypeValue::Union(fields) => TypeValue::Union(
            fields
                .iter()
                .map(|(name, ty)| (name.clone(), subst(ty, bindings)))
                .collect(),
        ),
        TypeValue::Generic { typarams, body } => TypeValue::Generic {
            typarams: typarams.clone(),
            body: Box::new(subst(body, bindings)),
        },
        TypeValue::Concretization {
            constructor,
            tyargs,
        } => {
            // substitute in the arguments
            let tyargs = tyargs
                .iter()
                .map(|ty| subst(ty, bindings))
                .collect::<Vec<_>>();

            match constructor {
                TypeValueConstructor::SymbolicVariable(id) => match bindings.get(id) {
                    // if we replaced the constructor, then we can evaluate
                    Some(constructor) => concretize_type_expr(constructor, tyargs),
                    // otherwise we just keep the concretization
                    None => TypeValue::Concretization {
                        constructor: TypeValueConstructor::SymbolicVariable(*id),
                        tyargs,
                    },
                },
                TypeValueConstructor::RefConstructor => TypeValue::Concretization {
                    constructor: TypeValueConstructor::RefConstructor,
                    tyargs,
                },
                TypeValueConstructor::ArrayConstructor => TypeValue::Concretization {
                    constructor: TypeValueConstructor::ArrayConstructor,
                    tyargs,
                },
                TypeValueConstructor::SliceConstructor => TypeValue::Concretization {
                    constructor: TypeValueConstructor::SliceConstructor,
                    tyargs,
                },
                TypeValueConstructor::IntConstructor => TypeValue::Concretization {
                    constructor: TypeValueConstructor::IntConstructor,
                    tyargs,
                },
                TypeValueConstructor::FloatConstructor => TypeValue::Concretization {
                    constructor: TypeValueConstructor::FloatConstructor,
                    tyargs,
                },
            }
        }
        _ => val.clone(),
    }
}

fn concretize_type_expr(constructor: &TypeValue, mut tyargs: Vec<TypeValue>) -> TypeValue {
    match constructor {
        TypeValue::Unknown => TypeValue::Unknown,
        TypeValue::SymbolicVariable(id) => TypeValue::Concretization {
            constructor: TypeValueConstructor::SymbolicVariable(*id),
            tyargs,
        },
        TypeValue::RefConstructor => TypeValue::Concretization {
            constructor: TypeValueConstructor::RefConstructor,
            tyargs,
        },
        TypeValue::ArrayConstructor => TypeValue::Concretization {
            constructor: TypeValueConstructor::ArrayConstructor,
            tyargs,
        },
        TypeValue::SliceConstructor => TypeValue::Concretization {
            constructor: TypeValueConstructor::SliceConstructor,
            tyargs,
        },
        TypeValue::IntConstructor => TypeValue::Concretization {
            constructor: TypeValueConstructor::IntConstructor,
            tyargs,
        },
        TypeValue::FloatConstructor => TypeValue::Concretization {
            constructor: TypeValueConstructor::FloatConstructor,
            tyargs,
        },
        TypeValue::Generic { typarams, body } => {
            assert!(
                typarams.len() == tyargs.len(),
                "wrong number of arguments; should be kindchecked"
            );
            let mut bindings = HashMap::new();
            for (typat, tyarg) in std::iter::zip(typarams, tyargs) {
                match &typat.val {
                    hir::TypePatExpr::Typed { id, .. } => {
                        bindings.insert(*id, tyarg);
                    }
                    hir::TypePatExpr::Identifier(id) => {
                        bindings.insert(*id, tyarg);
                    }
                    hir::TypePatExpr::Error => {}
                }
            }
            subst(body, &bindings)
        }
        _ => {
            unreachable!("concretization of a non-generic; should have been kindchecked");
        }
    }
}

// if we encounter an error, we log it
// NOTE: we assume that all identifiers have already been resolved.
pub fn typecheck_type_expr_and_patch(
    v: &mut Augmented<hir::TypeExpr>,
    dlogger: &mut DiagnosticLogger,
    checker: &mut TypeChecker,
) -> TypeValue {
    match &mut v.val {
        hir::TypeExpr::Error => TypeValue::Unknown,
        hir::TypeExpr::Identifier(id) => TypeValue::SymbolicVariable(*id),
        hir::TypeExpr::BoolTy => TypeValue::Bool,
        hir::TypeExpr::RefConstructorTy => TypeValue::RefConstructor,
        hir::TypeExpr::ArrayConstructorTy => TypeValue::ArrayConstructor,
        hir::TypeExpr::SliceConstructorTy => TypeValue::SliceConstructor,
        hir::TypeExpr::IntConstructorTy => TypeValue::IntConstructor,
        hir::TypeExpr::FloatConstructorTy => TypeValue::FloatConstructor,
        hir::TypeExpr::Int(ref i) => match i64::try_from(i) {
            Ok(i) => TypeValue::IntLit(i),
            Err(e) => {
                dlogger.log_int_too_large(v.range, 64);
                TypeValue::Unknown
            }
        },
        hir::TypeExpr::Bool(i) => TypeValue::BoolLit(*i),
        hir::TypeExpr::Float(ref i) => {
            let (n, d) = i.reduced().into_raw();
            let n = ToPrimitive::to_f64(&n).unwrap();
            let d = ToPrimitive::to_f64(&d).unwrap();
            TypeValue::FloatLit(n / d)
        }
        hir::TypeExpr::Fn { paramtys, returnty } => TypeValue::Fn {
            paramtys: paramtys
                .iter_mut()
                .map(|ty| typecheck_type_expr_and_patch(ty, dlogger, checker))
                .collect(),
            returntype: Box::new(typecheck_type_expr_and_patch(returnty, dlogger, checker)),
        },
        hir::TypeExpr::Struct(fields) => TypeValue::Struct(
            fields
                .iter_mut()
                .map(|(name, expr)| {
                    (
                        name.val.clone(),
                        typecheck_type_expr_and_patch(expr, dlogger, checker),
                    )
                })
                .collect(),
        ),
        hir::TypeExpr::Enum(fields) => TypeValue::Enum(
            fields
                .iter_mut()
                .map(|(name, expr)| {
                    (
                        name.val.clone(),
                        typecheck_type_expr_and_patch(expr, dlogger, checker),
                    )
                })
                .collect(),
        ),
        hir::TypeExpr::Union(fields) => TypeValue::Union(
            fields
                .iter_mut()
                .map(|(name, expr)| {
                    (
                        name.val.clone(),
                        typecheck_type_expr_and_patch(expr, dlogger, checker),
                    )
                })
                .collect(),
        ),
        hir::TypeExpr::Concretization { genericty, tyargs } => {
            // first we evaluate the type of the generic function
            let generic_val = typecheck_type_expr_and_patch(genericty, dlogger, checker);
            let tyargs = tyargs
                .iter_mut()
                .map(|x| typecheck_type_expr_and_patch(x, dlogger, checker))
                .collect::<Vec<_>>();
            // attempt to concretize the type (will only work if there are no symbolic variables in the type)
            concretize_type_expr(&generic_val, tyargs)
        }
        hir::TypeExpr::Generic {
            params,
            returnkind: _,
            body,
        } => {
            let body = Box::new(typecheck_type_expr_and_patch(body, dlogger, checker));
            TypeValue::Generic {
                typarams: params.clone(),
                body,
            }
        }
    }
}

pub fn typecheck_val_pat_and_patch(
    v: &mut Augmented<hir::ValPatExpr>,
    expected_type: &TypeValue,
    dlogger: &mut DiagnosticLogger,
    checker: &mut TypeChecker,
) -> TypeValue {
    match &mut v.val {
        hir::ValPatExpr::Error => TypeValue::Unknown,
        hir::ValPatExpr::Ignore => expected_type.clone(),
        hir::ValPatExpr::Identifier { id, .. } => {
            checker.val_type_table[*id] = Some(expected_type.clone());
            expected_type.clone()
        }
        hir::ValPatExpr::StructLiteral(fields) => {
            let mut expected_fields = match expected_type {
                TypeValue::Struct(fields) => fields.clone(),
                _ => fields
                    .iter()
                    .map(|(name, _)| (name.val.clone(), TypeValue::Unknown))
                    .collect(),
            };

            let mut actual_fields = HashMap::new();

            for (name, expr) in fields {
                match expected_fields.remove(&name.val) {
                    Some(expected_type) => {
                        actual_fields.insert(
                            name.val.clone(),
                            typecheck_val_pat_and_patch(expr, &expected_type, dlogger, checker),
                        );
                    }
                    None => {
                        dlogger.log_unexpected_field(v.range, &name.val);
                    }
                }
            }

            for (name, _) in expected_fields {
                dlogger.log_missing_field(v.range, &name);
            }

            expect_type(v, expected_type, TypeValue::Struct(actual_fields), dlogger)
        }
        hir::ValPatExpr::New { pat, ty } => {
            // the symbolic type we are deconstructing
            let symbolic_type_to_construct = typecheck_type_expr_and_patch(ty, dlogger, checker);
            let dereferenced_type = match symbolic_type_to_construct {
                TypeValue::SymbolicVariable(id) => checker.type_type_table[id]
                    .clone()
                    .expect("type not initialized yet (should be kindchecked)"),
                _ => {
                    dlogger.log_cannot_destructure_non_struct(
                        v.range,
                        &symbolic_type_to_construct.to_string(),
                    );
                    v.val = hir::ValPatExpr::Error;
                    return TypeValue::Unknown;
                }
            };
            // check inner pattern
            typecheck_val_pat_and_patch(pat, &dereferenced_type, dlogger, checker);
            // overall type of the pattern
            expect_type(v, expected_type, symbolic_type_to_construct, dlogger)
        }
        hir::ValPatExpr::Typed { pat, ty } => {
            let asserted_type = typecheck_type_expr_and_patch(ty, dlogger, checker);
            typecheck_val_pat_and_patch(pat, &asserted_type, dlogger, checker);
            expect_type(ty, expected_type, asserted_type, dlogger)
        }
    }
}

pub fn typecheck_case_target_expr_and_patch(
    v: &mut Augmented<hir::CaseTargetExpr>,
    expected_hint: &TypeValue,
    dlogger: &mut DiagnosticLogger,
    checker: &mut TypeChecker,
) -> TypeValue {
    match &mut v.val {
        hir::CaseTargetExpr::Error => TypeValue::Unknown,
        hir::CaseTargetExpr::Bool(_) => expect_type(v, expected_hint, TypeValue::Bool, dlogger),
        hir::CaseTargetExpr::Int(_) => match expected_hint {
            w @ TypeValue::Concretization {
                constructor: TypeValueConstructor::IntConstructor,
                ..
            } => w.clone(),
            _ => expect_type(
                v,
                expected_hint,
                TypeValue::Concretization {
                    constructor: TypeValueConstructor::IntConstructor,
                    tyargs: vec![TypeValue::Unknown, TypeValue::Unknown],
                },
                dlogger,
            ),
        },
        hir::CaseTargetExpr::PatExpr(pat) => {
            typecheck_val_pat_and_patch(pat, expected_hint, dlogger, checker)
        }
    }
}

pub fn typecheck_val_expr_and_patch(
    v: &mut Augmented<hir::ValExpr>,
    expected_type: &TypeValue,
    dlogger: &mut DiagnosticLogger,
    checker: &mut TypeChecker,
) -> TypeValue {
    match &mut v.val {
        hir::ValExpr::Error => TypeValue::Unknown,
        hir::ValExpr::Identifier(id) => {
            let actual_type = checker.val_type_table[*id]
                .clone()
                .expect("type not initialized yet");
            expect_type(v, expected_type, actual_type, dlogger)
        }
        hir::ValExpr::Bool(_) => expect_type(v, expected_type, TypeValue::Bool, dlogger),
        hir::ValExpr::Int(_) => match expected_type {
            // takes the shape of the expected type
            w @ TypeValue::Concretization {
                constructor: TypeValueConstructor::IntConstructor,
                tyargs,
            } => w.clone(),
            _ => expect_type(
                v,
                expected_type,
                TypeValue::Concretization {
                    constructor: TypeValueConstructor::IntConstructor,
                    tyargs: vec![TypeValue::Unknown, TypeValue::Unknown],
                },
                dlogger,
            ),
        },
        hir::ValExpr::Float(_) => match expected_type {
            // takes the shape of the expected type
            w @ TypeValue::Concretization {
                constructor: TypeValueConstructor::FloatConstructor,
                tyargs,
            } => w.clone(),
            _ => expect_type(
                v,
                expected_type,
                TypeValue::Concretization {
                    constructor: TypeValueConstructor::FloatConstructor,
                    tyargs: vec![TypeValue::Unknown],
                },
                dlogger,
            ),
        },
        hir::ValExpr::String(s) => expect_type(
            v,
            expected_type,
            TypeValue::Concretization {
                constructor: TypeValueConstructor::ArrayConstructor,
                tyargs: vec![
                    TypeValue::Concretization {
                        constructor: TypeValueConstructor::IntConstructor,
                        tyargs: vec![TypeValue::Bool, TypeValue::Unknown],
                    },
                    TypeValue::IntLit(s.len() as i64),
                ],
            },
            dlogger,
        ),
        hir::ValExpr::StructLiteral(fields) => {
            let expected_fields = match expected_type {
                TypeValue::Struct(fields) => fields.clone(),
                _ => fields
                    .iter()
                    .map(|(name, _)| (name.val.clone(), TypeValue::Unknown))
                    .collect(),
            };

            let actual_fields = HashMap::new();
            for (name, val) in fields {
                match expected_fields.get(&name.val) {
                    Some(expected_type) => {
                        actual_fields.insert(
                            name.val.clone(),
                            typecheck_val_expr_and_patch(val, expected_type, dlogger, checker),
                        );
                    }
                    None => {
                        dlogger.log_unexpected_field(v.range, &name.val.clone());
                        v.val = hir::ValExpr::Error;
                    }
                }
            }

            for (name, expected_type) in expected_fields {
                dlogger.log_missing_field(v.range, &name);
            }

            expect_type(v, expected_type, TypeValue::Struct(actual_fields), dlogger)
        }
        hir::ValExpr::New { ty, val } => {
            // the symbolic type we are constructing
            let symbolic_type_to_construct = typecheck_type_expr_and_patch(ty, dlogger, checker);
            let dereferenced_type = match symbolic_type_to_construct {
                TypeValue::SymbolicVariable(id) => checker.type_type_table[id]
                    .clone()
                    .expect("type not initialized yet (should be kindchecked)"),
                _ => {
                    dlogger.log_cannot_new_type(v.range, &symbolic_type_to_construct.to_string());
                    v.val = hir::ValExpr::Error;
                    return TypeValue::Unknown;
                }
            };
            // check inner pattern
            typecheck_val_expr_and_patch(val, &dereferenced_type, dlogger, checker);
            // overall type of the expression
            expect_type(v, expected_type, symbolic_type_to_construct, dlogger)
        }
        hir::ValExpr::Ref(inner) => {
            let expected_inner = match expected_type {
                TypeValue::Concretization {
                    constructor: TypeValueConstructor::RefConstructor,
                    tyargs,
                } => &tyargs[0],
                _ => &TypeValue::Unknown,
            };

            let actual_inner_type =
                typecheck_val_expr_and_patch(inner, expected_inner, dlogger, checker);

            expect_type(
                v,
                expected_type,
                TypeValue::Concretization {
                    constructor: TypeValueConstructor::IntConstructor,
                    tyargs: vec![actual_inner_type],
                },
                dlogger,
            )
        }
        hir::ValExpr::Deref(inner) => {
            // the inner type should be a reference
            let expected_inner = TypeValue::Concretization {
                constructor: TypeValueConstructor::RefConstructor,
                tyargs: vec![expected_type.clone()],
            };
            let actual_type =
                typecheck_val_expr_and_patch(inner, &expected_inner, dlogger, checker);
            expect_type(v, expected_type, actual_type, dlogger)
        }
        hir::ValExpr::Generic { params, body } => match expected_type {
            TypeValue::Generic {
                typarams: expected_params,
                body: expected_body,
            } => {
                // we have already kindchecked, but make sure that the number of arguments is the same (as a sanity check)
                assert_eq!(
                    params.len(),
                    expected_params.len(),
                    "wrong number of arguments"
                );
                // we can actually check the body though
                let body = typecheck_val_expr_and_patch(body, expected_body, dlogger, checker);
                TypeValue::Generic {
                    typarams: params.clone(),
                    body: Box::new(body),
                }
            }
            _ => {
                // we don't know what the expected type is, so we just typecheck the body
                let body =
                    typecheck_val_expr_and_patch(body, &TypeValue::Unknown, dlogger, checker);
                let actual = TypeValue::Generic {
                    typarams: params.clone(),
                    body: Box::new(body),
                };
                expect_type(v, expected_type, actual, dlogger)
            }
        },
        hir::ValExpr::FnDef {
            params,
            returnty,
            body,
        } => {
            let (expected_paramtys, expected_returnty) = match expected_type {
                TypeValue::Fn {
                    paramtys,
                    returntype,
                } => (paramtys, returntype.as_ref()),
                _ => (
                    &params
                        .iter()
                        .map(|_| TypeValue::Unknown)
                        .collect::<Vec<_>>(),
                    &TypeValue::Unknown,
                ),
            };

            let actual_paramtys = std::iter::zip(params.iter_mut(), expected_paramtys)
                .map(|(param, expected_paramty)| {
                    typecheck_val_pat_and_patch(param, expected_paramty, dlogger, checker)
                })
                .collect::<Vec<_>>();

            let actual_returnty = match returnty {
                Some(returnty) => {
                    let actual_returnty = typecheck_type_expr_and_patch(returnty, dlogger, checker);
                    // typecheck the returnty with the expected returnty
                    expect_type(returnty, expected_returnty, actual_returnty, dlogger)
                }
                None => expected_returnty.clone(),
            };

            // typecheck the body
            typecheck_val_expr_and_patch(body, &actual_returnty, dlogger, checker);

            expect_type(
                v,
                expected_type,
                TypeValue::Fn {
                    paramtys: actual_paramtys,
                    returntype: Box::new(actual_returnty),
                },
                dlogger,
            )
        }
        hir::ValExpr::CaseOf { expr, cases } => {
            let expr_type =
                typecheck_val_expr_and_patch(expr, &TypeValue::Unknown, dlogger, checker);

            match cases.split_first_mut() {
                Some(((first_target, first_body), rest)) => {
                    typecheck_case_target_expr_and_patch(
                        first_target,
                        &expr_type,
                        dlogger,
                        checker,
                    );

                    let first_body_type =
                        typecheck_val_expr_and_patch(first_body, expected_type, dlogger, checker);

                    for (target, body) in rest {
                        typecheck_case_target_expr_and_patch(target, &expr_type, dlogger, checker);
                        typecheck_val_expr_and_patch(body, &first_body_type, dlogger, checker);
                    }

                    expect_type(v, expected_type, first_body_type, dlogger)
                }
                None => expect_type(v, expected_type, TypeValue::Unknown, dlogger),
            }
        }
        hir::ValExpr::Block {
            statements,
            last_expression,
        } => {
            // check all statements
            for statement in statements {
                typecheck_block_statement_and_patch(statement, dlogger, checker);
            }

            // the type of the last expression is the type of the block
            match last_expression {
                Some(expr) => typecheck_val_expr_and_patch(expr, expected_type, dlogger, checker),
                None => expect_type(v, expected_type, TypeValue::Struct(HashMap::new()), dlogger),
            }
        }
        hir::ValExpr::ArrayLiteral(vals) => {
            let expected_inner_ty = match expected_type {
                TypeValue::Concretization {
                    constructor: TypeValueConstructor::ArrayConstructor,
                    tyargs,
                } => &tyargs[0],
                _ => &TypeValue::Unknown,
            };

            match vals.split_first_mut() {
                Some((first, rest)) => {
                    let first_val_type =
                        typecheck_val_expr_and_patch(first, expected_inner_ty, dlogger, checker);

                    for val in rest {
                        typecheck_val_expr_and_patch(val, &first_val_type, dlogger, checker);
                    }

                    expect_type(
                        v,
                        expected_type,
                        TypeValue::Concretization {
                            constructor: TypeValueConstructor::ArrayConstructor,
                            tyargs: vec![first_val_type, TypeValue::IntLit(1 + rest.len() as i64)],
                        },
                        dlogger,
                    )
                }
                None => expect_type(
                    v,
                    expected_type,
                    TypeValue::Concretization {
                        constructor: TypeValueConstructor::ArrayConstructor,
                        tyargs: vec![TypeValue::Unknown, TypeValue::IntLit(0)],
                    },
                    dlogger,
                ),
            }
        }
        hir::ValExpr::BinaryOp {
            op,
            left_operand,
            right_operand,
        } => match op {
            // number x number -> number
            hir::ValBinaryOpKind::Add
            | hir::ValBinaryOpKind::Sub
            | hir::ValBinaryOpKind::Mul
            | hir::ValBinaryOpKind::Div
            | hir::ValBinaryOpKind::Rem => {
                // left type
                let left_type =
                    typecheck_val_expr_and_patch(left_operand, &expected_type, dlogger, checker);
                let right_type =
                    typecheck_val_expr_and_patch(right_operand, &left_type, dlogger, checker);
                match (left_type, right_type) {
                    (
                        TypeValue::Int(Some(lsgn), Some(lsz)),
                        TypeValue::Int(Some(rsgn), Some(rsz)),
                    ) if lsgn == rsgn && lsz == rsz => {}
                    (TypeValue::Float(Some(lsz)), TypeValue::Float(Some(rsz))) if lsz == rsz => {}
                    _ => {
                        dlogger.log_invalid_binary_op(
                            v.range,
                            op.as_ref(),
                            &left_type.to_string(),
                            &right_type.to_string(),
                        );
                        v.val = hir::ValExpr::Error;
                    }
                }
                expect_type(v, expected_type, left_type, dlogger)
            }
            // number x number -> bool
            hir::ValBinaryOpKind::Lt
            | hir::ValBinaryOpKind::Leq
            | hir::ValBinaryOpKind::Gt
            | hir::ValBinaryOpKind::Geq => {
                // left type
                let left_type = typecheck_val_expr_and_patch(
                    left_operand,
                    &TypeValue::Unknown,
                    dlogger,
                    checker,
                );
                let right_type =
                    typecheck_val_expr_and_patch(right_operand, &left_type, dlogger, checker);

                match (left_type, right_type) {
                    (
                        TypeValue::Int(Some(lsgn), Some(lsz)),
                        TypeValue::Int(Some(rsgn), Some(rsz)),
                    ) if lsgn == rsgn && lsz == rsz => {}
                    (TypeValue::Float(Some(lsz)), TypeValue::Float(Some(rsz))) if lsz == rsz => {}
                    _ => {
                        dlogger.log_invalid_binary_op(
                            v.range,
                            op.as_ref(),
                            &left_type.to_string(),
                            &right_type.to_string(),
                        );
                        v.val = hir::ValExpr::Error;
                    }
                }

                expect_type(v, expected_type, TypeValue::Bool, dlogger)
            }
            // bool x bool -> bool
            hir::ValBinaryOpKind::And | hir::ValBinaryOpKind::Or => {
                typecheck_val_expr_and_patch(left_operand, &TypeValue::Bool, dlogger, checker);
                typecheck_val_expr_and_patch(right_operand, &TypeValue::Bool, dlogger, checker);
                expect_type(v, expected_type, TypeValue::Bool, dlogger)
            }
            // (number | bool) x (number | bool) -> bool
            hir::ValBinaryOpKind::Eq | hir::ValBinaryOpKind::Neq => {
                // left type
                let left_type = typecheck_val_expr_and_patch(
                    left_operand,
                    &TypeValue::Unknown,
                    dlogger,
                    checker,
                );

                let right_type =
                    typecheck_val_expr_and_patch(right_operand, &left_type, dlogger, checker);

                match (left_type, right_type) {
                    (
                        TypeValue::Int(Some(lsgn), Some(lsz)),
                        TypeValue::Int(Some(rsgn), Some(rsz)),
                    ) if lsgn == rsgn && lsz == rsz => {}
                    (TypeValue::Float(Some(lsz)), TypeValue::Float(Some(rsz))) if lsz == rsz => {}
                    _ => {
                        dlogger.log_invalid_binary_op(
                            v.range,
                            op.as_ref(),
                            &left_type.to_string(),
                            &right_type.to_string(),
                        );
                        v.val = hir::ValExpr::Error;
                    }
                }

                expect_type(v, expected_type, TypeValue::Bool, dlogger)
            }
        },

        hir::ValExpr::ArrayAccess { root, index } => {
            // let expected_inner = Array

            typecheck_val_expr_and_patch(root, &KindValue::Type, dlogger, checker);
            typecheck_val_expr_and_patch(index, &KindValue::Int, dlogger, checker);
            expect_type(v, expected_type, KindValue::Type, dlogger)
        }
        hir::ValExpr::FieldAccess { root, .. } => {
            typecheck_val_expr_and_patch(root, &KindValue::Type, dlogger, checker);
            expect_type(v, expected_type, KindValue::Type, dlogger)
        }
        hir::ValExpr::Concretization { generic, tyargs } => {
            let kind_of_generic =
                typecheck_val_expr_and_patch(generic, &KindValue::Unknown, dlogger, checker);
            match kind_of_generic {
                KindValue::Unknown => TypeValue::Unknown,
                KindValue::Generic {
                    paramkinds: expected_kinds,
                    returnkind,
                } => {
                    if expected_kinds.len() != tyargs.len() {
                        dlogger.log_wrong_number_type_args(
                            v.range,
                            expected_kinds.len(),
                            tyargs.len(),
                        );
                        v.val = hir::ValExpr::Error;
                        KindValue::Unknown
                    } else {
                        for (tyarg, ref argkind) in std::iter::zip(tyargs, expected_kinds) {
                            typecheck_type_expr_and_patch(tyarg, argkind, dlogger, checker);
                        }
                        expect_type(v, expected_type, *returnkind, dlogger)
                    }
                }
                _ => {
                    dlogger.log_cannot_be_concretized(v.range);
                    v.val = hir::ValExpr::Error;
                    KindValue::Unknown
                }
            }
        }
        hir::ValExpr::App { fun, args } => {
            typecheck_val_expr_and_patch(fun, &KindValue::Type, dlogger, checker);
            for arg in args {
                typecheck_val_expr_and_patch(arg, &KindValue::Type, dlogger, checker);
            }
            expect_type(v, expected_type, KindValue::Type, dlogger)
        }
    }
}

pub fn typecheck_block_statement_and_patch(
    v: &mut Augmented<hir::BlockStatement>,
    dlogger: &mut DiagnosticLogger,
    checker: &mut TypeChecker,
) {
    match &mut v.val {
        hir::BlockStatement::Error => {}
        hir::BlockStatement::TypeDef { value, typat } => {
            // try calculating the kind of the type expression
            let ty = typecheck_type_expr_and_patch(value, dlogger, checker);
            // bind the resolved kind to all the identifiers in type pattern
            typecheck_type_pat_expr_and_patch(typat, &ty, dlogger, checker);
        }
        hir::BlockStatement::ValDef { pat, value } => {
            // get hint from the value pattern
            let type_hint = typehint_of_val_pat_and_patch(pat, dlogger, checker);
            let ty = typecheck_val_expr_and_patch(value, &type_hint, dlogger, checker);
            typecheck_val_pat_and_patch(pat, &ty, dlogger, checker);
        }
        hir::BlockStatement::Set { place, value } => {
            typecheck_val_expr_and_patch(place, &KindValue::Type, dlogger, checker);
            typecheck_val_expr_and_patch(value, &KindValue::Type, dlogger, checker);
        }
        hir::BlockStatement::IfThen {
            cond,
            then_branch,
            else_branch,
        } => {
            typecheck_val_expr_and_patch(cond, &TypeValue::Bool, dlogger, checker);
            for statement in then_branch {
                typecheck_block_statement_and_patch(statement, dlogger, checker);
            }
            for statement in else_branch {
                typecheck_block_statement_and_patch(statement, dlogger, checker);
            }
        }
        hir::BlockStatement::While { cond, body } => {
            typecheck_val_expr_and_patch(cond, &TypeValue::Bool, dlogger, checker);
            for statement in body {
                typecheck_block_statement_and_patch(statement, dlogger, checker);
            }
        }
        hir::BlockStatement::For {
            pattern,
            start,
            end,
            inclusive: _,
            by,
            body,
        } => {
            typecheck_val_pat_and_patch(pattern, &KindValue::Type, dlogger, checker);
            typecheck_val_expr_and_patch(start, &KindValue::Type, dlogger, checker);
            typecheck_val_expr_and_patch(end, &KindValue::Type, dlogger, checker);
            if let Some(by) = by {
                typecheck_val_expr_and_patch(by, &KindValue::Type, dlogger, checker);
            }
            for statement in body {
                typecheck_block_statement_and_patch(statement, dlogger, checker);
            }
        }
        hir::BlockStatement::Do(val) => {
            typecheck_val_expr_and_patch(val, &KindValue::Type, dlogger, checker);
        }
    }
}

pub fn typecheck_file_statement_and_patch(
    v: &mut Augmented<hir::FileStatement>,
    dlogger: &mut DiagnosticLogger,
    checker: &mut TypeChecker,
) {
    match &mut v.val {
        hir::FileStatement::Error => {}
        hir::FileStatement::TypeDef { value, typat } => {
            // try calculating the type of the type expression
            let ty = typecheck_type_expr_and_patch(value, dlogger, checker);
            // bind the resolved type to all the identifiers in type pattern
            typecheck_type_pat_expr_and_patch(typat, &ty, dlogger, checker);
        }
        hir::FileStatement::ValDef { pat, value } => {
            // get hint from the value pattern
            let ty_hint = typehint_of_val_pat_and_patch(pat, dlogger, checker);
            let ty = typecheck_val_expr_and_patch(value, &ty_hint, dlogger, checker);
            typecheck_val_pat_and_patch(pat, &ty, dlogger, checker);
        }
    }
}
