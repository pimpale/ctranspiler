use std::collections::HashMap;

use indexmap::IndexMap;
use num_bigint::{BigInt, TryFromBigIntError};
use num_rational::{BigRational, Ratio};
use num_traits::ToPrimitive;

use crate::dlogger::DiagnosticLogger;
use crate::hir::Augmented;
use crate::typecheck::{evaluate_hir_kind, TypeChecker};
use crate::types::{KindValue, TypeValue};
use crate::{hir, typecheck};

const DEFAULT_INT_SIZE: u64 = 64;
const DEFAULT_FLOAT_SIZE: u64 = 64;

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
        TypeValue::Ref(v) => TypeValue::Ref(Box::new(subst(v, bindings))),
        TypeValue::Array(v, s) => TypeValue::Array(Box::new(subst(v, bindings)), *s),
        TypeValue::Slice(v) => TypeValue::Slice(Box::new(subst(v, bindings))),
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
            symbolic_constructor,
            tyargs,
        } => {
            // substitute in the arguments
            let tyargs = tyargs
                .iter()
                .map(|ty| subst(ty, bindings))
                .collect::<Vec<_>>();
            match bindings.get(symbolic_constructor) {
                // if we replaced the constructor, then we can evaluate
                Some(constructor) => concretize_type_expr(constructor, tyargs),
                // otherwise we just keep the concretization
                None => TypeValue::Concretization {
                    symbolic_constructor: *symbolic_constructor,
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
            symbolic_constructor: *id,
            tyargs,
        },
        TypeValue::RefConstructor => {
            assert!(tyargs.len() == 1, "wrong number of arguments");
            let arg0 = tyargs.remove(0);
            TypeValue::Ref(Box::new(arg0))
        }
        TypeValue::ArrayConstructor => {
            assert!(tyargs.len() == 2, "wrong number of arguments");
            let arg1 = tyargs.remove(1);
            let arg0 = tyargs.remove(0);
            let arg1_as_int = match arg1 {
                TypeValue::Int(i) => i,
                _ => {
                    unreachable!("array size should be an integer; should have been kindchecked")
                }
            };
            TypeValue::Array(Box::new(arg0), arg1_as_int)
        }
        TypeValue::SliceConstructor => {
            assert!(tyargs.len() == 1, "wrong number of arguments");
            let arg0 = tyargs.remove(0);
            TypeValue::Ref(Box::new(arg0))
        }
        TypeValue::IntConstructor => {
            assert!(tyargs.len() == 1, "wrong number of arguments");
            let arg0 = tyargs.remove(0);
            let arg0_as_int = match arg0 {
                TypeValue::Int(i) => i,
                _ => {
                    unreachable!("integer size should be an integer; should have been kindchecked")
                }
            };
            TypeValue::Int(arg0_as_int)
        }
        TypeValue::UIntConstructor => {
            assert!(tyargs.len() == 1, "wrong number of arguments");
            let arg0 = tyargs.remove(0);
            let arg0_as_int = match arg0 {
                TypeValue::Int(i) => i,
                _ => {
                    unreachable!("integer size should be an integer; should have been kindchecked")
                }
            };
            TypeValue::UInt(arg0_as_int)
        }
        TypeValue::FloatConstructor => {
            assert!(tyargs.len() == 1, "wrong number of arguments");
            let arg0 = tyargs.remove(0);
            let arg0_as_int = match arg0 {
                TypeValue::Int(i) => i,
                _ => {
                    unreachable!("float size should be an integer; should have been kindchecked")
                }
            };
            TypeValue::Float(arg0_as_int)
        }
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
        hir::TypeExpr::UIntConstructorTy => TypeValue::UIntConstructor,
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
        hir::ValPatExpr::Ignore => {
            // if expected kind is unknown, we throw an error
            if expected_type == &TypeValue::Unknown {
                dlogger.log_cannot_infer_valpat_kind(v.range);
                v.val = hir::ValPatExpr::Error;
                TypeValue::Unknown
            } else {
                expected_type.clone()
            }
        }
        hir::ValPatExpr::Identifier { id, .. } => {
            // if expected kind is unknown, we throw an error
            if expected_type == &TypeValue::Unknown {
                dlogger.log_cannot_infer_valpat_kind(v.range);
                v.val = hir::ValPatExpr::Error;
                TypeValue::Unknown
            } else {
                checker.val_type_table[*id] = Some(expected_type.clone());
                expected_type.clone()
            }
        }
        hir::ValPatExpr::StructLiteral(fields) => {
            let mut expected_fields = match expected_type {
                TypeValue::Struct(fields) => fields.clone(),
                _ => fields
                    .iter()
                    .map(|(name, _)| (name.val.clone(), TypeValue::Unknown))
                    .collect(),
            };

            let actual_fields = HashMap::new();

            for (name, expr) in fields {
                match expected_fields.remove(&name.val) {
                    Some(expected_type) => {
                        actual_fields.insert(
                            name.val.clone(),
                            typecheck_val_pat_and_patch(expr, &expected_type, dlogger, checker),
                        );
                    }
                    None => {
                        dlogger.log_unexpected_field(v.range, name.val.clone());
                        v.val = hir::ValPatExpr::Error;
                    }
                }
            }

            for (name, expected_type) in expected_fields {
                dlogger.log_missing_field(v.range, name);
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
                    dlogger
                        .log_cannot_destructure(v.range, &symbolic_type_to_construct.to_string());
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
            TypeValue::Int(size) => TypeValue::Int(*size),
            TypeValue::UInt(size) => TypeValue::UInt(*size),
            _ => expect_type(v, expected_hint, TypeValue::Int(DEFAULT_INT_SIZE), dlogger),
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
            TypeValue::Int(size) => TypeValue::Int(size.clone()),
            TypeValue::UInt(size) => TypeValue::UInt(size.clone()),
            _ => expect_type(v, expected_type, TypeValue::Int(DEFAULT_INT_SIZE), dlogger),
        },
        hir::ValExpr::Float(_) => match expected_type {
            TypeValue::Float(size) => TypeValue::Float(size.clone()),
            _ => expect_type(
                v,
                expected_type,
                TypeValue::Float(DEFAULT_FLOAT_SIZE),
                dlogger,
            ),
        },
        hir::ValExpr::String(s) => expect_type(
            v,
            expected_type,
            TypeValue::Array(Box::new(TypeValue::UInt(8)), s.len() as u64),
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
                        dlogger.log_unexpected_field(v.range, name.val.clone());
                        v.val = hir::ValExpr::Error;
                    }
                }
            }

            for (name, expected_type) in expected_fields {
                dlogger.log_missing_field(v.range, name);
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
                    dlogger.log_cannot_construct(v.range, &symbolic_type_to_construct.to_string());
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
                TypeValue::Ref(inner) => inner,
                _ => &TypeValue::Unknown,
            };
            let actual_type = typecheck_val_expr_and_patch(inner, expected_inner, dlogger, checker);

            expect_type(
                v,
                expected_type,
                TypeValue::Ref(Box::new(actual_type)),
                dlogger,
            )
        }
        hir::ValExpr::Deref(inner) => {
            let expected_inner = match expected_type {
                TypeValue::Ref(inner) => inner,
                _ => &TypeValue::Unknown,
            };
            let actual_type = typecheck_val_expr_and_patch(inner, expected_inner, dlogger, checker);
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
                None => match expected_returnty {
                    TypeValue::Unknown => {
                        dlogger.log_cannot_infer_return_ty(v.range);
                        expected_returnty.clone()
                    }
                    _ => expected_returnty.clone(),
                },
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
            for (target, body) in cases {
                typecheck_case_target_expr_and_patch(target, &expr_type, dlogger, checker);
                typecheck_val_expr_and_patch(body, expected_type, dlogger, checker);
            }
            expect_type(v, expected_type, KindValue::Type, dlogger)
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
            let (inner_ty, len) = match expected_type {
                TypeValue::Array(inner_ty, len) => (inner_ty.as_ref(), *len),
                _ => (&TypeValue::Unknown, vals.len() as u64),
            };

            // if inner ty is unknown then attempt to infer it from the first element
            let inner_ty = match inner_ty {
                TypeValue::Unknown => {
                    if let Some(first_val) = vals.first_mut() {
                        typecheck_val_expr_and_patch(
                            first_val,
                            &TypeValue::Unknown,
                            dlogger,
                            checker,
                        )
                    } else {
                        dlogger.log_cannot_infer_array_ty(v.range);
                        v.val = hir::ValExpr::Error;
                        TypeValue::Unknown
                    }
                }
                _ => inner_ty.clone(),
            };

            // check all elements in the array
            for val in vals {
                typecheck_val_expr_and_patch(val, &inner_ty, dlogger, checker);
            }

            // return the type of the array
            expect_type(
                v,
                expected_type,
                TypeValue::Array(Box::new(inner_ty), len),
                dlogger,
            )
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
                match left_type {
                    TypeValue::Int(_) | TypeValue::UInt(_) | TypeValue::Float(_) => {}
                    _ => {
                        dlogger.log_invalid_binary_op(v.range, op, &left_type.to_string());
                        v.val = hir::ValExpr::Error;
                    }
                }
                typecheck_val_expr_and_patch(right_operand, &left_type, dlogger, checker);

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
                match left_type {
                    TypeValue::Int(_) | TypeValue::UInt(_) | TypeValue::Float(_) => {}
                    _ => {
                        dlogger.log_invalid_binary_op(v.range, op, &left_type.to_string());
                        v.val = hir::ValExpr::Error;
                    }
                }
                typecheck_val_expr_and_patch(right_operand, &left_type, dlogger, checker);

                expect_type(v, expected_type, TypeValue::Bool, dlogger)
            }
            // bool x bool -> bool
            hir::ValBinaryOpKind::And |
            hir::ValBinaryOpKind::Or => {
                typecheck_val_expr_and_patch(left_operand, &TypeValue::Bool, dlogger, checker);
                typecheck_val_expr_and_patch(right_operand, &TypeValue::Bool, dlogger, checker);
                expect_type(v, expected_type, TypeValue::Bool, dlogger)
            },
            // (number | bool) x (number | bool) -> bool
            hir::ValBinaryOpKind::Eq => todo!(),
            hir::ValBinaryOpKind::Neq => todo!(),
        },

        hir::ValExpr::ArrayAccess { root, index } => {
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
