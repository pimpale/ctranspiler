use std::collections::HashMap;

use num_traits::ToPrimitive;

use crate::ast::IdentifierModifier;
use crate::dlogger::DiagnosticLogger;
use crate::hint::Hint;
use crate::hir;
use crate::hir::{Augmented, ResolutionEnvironment};
use crate::values::Value;


// gets a hint but doesn't bind any free variables yet
// doesn't thoroughly traverse the tree, so make sure to call typecheck_val_expr_and_patch on this node afterwards
fn typehint_of_pat_and_patch(
    v: &mut Augmented<hir::PatExpr>,
    dlogger: &mut DiagnosticLogger,
    checker: &mut ResolutionEnvironment,
) -> Hint {
    match &mut v.val {
        hir::PatExpr::Error => Hint::Unknown,
        hir::PatExpr::Ignore => Value::Unknown,
        hir::PatExpr::Identifier { .. } => Value::Unknown,
        hir::PatExpr::StructLiteral(fields) => Value::Struct(
            fields
                .iter_mut()
                .map(|(name, expr)| {
                    (
                        name.val.clone(),
                        typehint_of_pat_and_patch(expr, dlogger, checker),
                    )
                })
                .collect(),
        ),
        hir::PatExpr::New { ty, .. } => {
            typecheck_val_expr_and_patch(ty, &Value::Unknown, dlogger, checker)
        }
        hir::PatExpr::Typed { ty, .. } => {
            typecheck_val_expr_and_patch(ty, &Value::Unknown, dlogger, checker)
        }
    }
}



// helper function to make reporting expected kind errors simpler
fn expect_type<T>(
    v: &mut Augmented<T>,
    expected: &Hint,
    actual: Value,
    dlogger: &mut DiagnosticLogger,
) -> Value
where
    T: std::default::Default,
{
    match (actual, expected) {
        (actual @ Value::Never, _) => actual,
        (actual, Value::Never) => actual,
        (actual @ Value::Unknown, _) => actual,
        (actual, Value::Unknown) => actual,
        (
            Value::Concretization {
                constructor: actual_constructor,
                tyargs: actual_tyargs,
            },
            Value::Concretization {
                constructor,
                tyargs,
            },
        ) => {
            if actual_constructor == *constructor {
                let mut new_tyargs = Vec::new();
                for (actual_tyarg, expected_tyarg) in actual_tyargs.into_iter().zip(tyargs) {
                    new_tyargs.push(expect_type(v, expected_tyarg, actual_tyarg, dlogger));
                }
                Value::Concretization {
                    constructor: actual_constructor,
                    tyargs: new_tyargs,
                }
            } else {
                dlogger.log_type_mismatch(
                    v.range,
                    &constructor.to_string(),
                    &actual_constructor.to_string(),
                );
                v.val = T::default();
                Value::Unknown
            }
        }
        (actual, expected) if &actual == expected => actual,
        (actual, expected) => {
            dlogger.log_type_mismatch(v.range, &expected.to_string(), &actual.to_string());
            v.val = T::default();
            Value::Unknown
        }
    }
}

fn subst(val: &Value, bindings: &HashMap<usize, Value>) -> Value {
    match val {
        Value::SymbolicVariable(id) => match bindings.get(id) {
            Some(ty) => ty.clone(),
            None => Value::SymbolicVariable(*id),
        },
        Value::Fn {
            paramtys,
            returntype,
        } => Value::Fn {
            paramtys: paramtys.iter().map(|ty| subst(ty, bindings)).collect(),
            returntype: Box::new(subst(returntype, bindings)),
        },
        Value::Struct(fields) => Value::Struct(
            fields
                .iter()
                .map(|(name, ty)| (name.clone(), subst(ty, bindings)))
                .collect(),
        ),
        Value::Enum(fields) => Value::Enum(
            fields
                .iter()
                .map(|(name, ty)| (name.clone(), subst(ty, bindings)))
                .collect(),
        ),
        Value::Union(fields) => Value::Union(
            fields
                .iter()
                .map(|(name, ty)| (name.clone(), subst(ty, bindings)))
                .collect(),
        ),
        Value::Generic { typarams, body } => Value::Generic {
            typarams: typarams.clone(),
            body: Box::new(subst(body, bindings)),
        },
        Value::Concretization {
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
                    None => Value::Concretization {
                        constructor: TypeValueConstructor::SymbolicVariable(*id),
                        tyargs,
                    },
                },
                TypeValueConstructor::RefConstructor => Value::Concretization {
                    constructor: TypeValueConstructor::RefConstructor,
                    tyargs,
                },
                TypeValueConstructor::ArrayConstructor => Value::Concretization {
                    constructor: TypeValueConstructor::ArrayConstructor,
                    tyargs,
                },
                TypeValueConstructor::SliceConstructor => Value::Concretization {
                    constructor: TypeValueConstructor::SliceConstructor,
                    tyargs,
                },
                TypeValueConstructor::IntConstructor => Value::Concretization {
                    constructor: TypeValueConstructor::IntConstructor,
                    tyargs,
                },
                TypeValueConstructor::FloatConstructor => Value::Concretization {
                    constructor: TypeValueConstructor::FloatConstructor,
                    tyargs,
                },
            }
        }
        _ => val.clone(),
    }
}

fn concretize_type_expr(constructor: &Value, tyargs: Vec<Value>) -> Value {
    match constructor {
        Value::Unknown => Value::Unknown,
        Value::SymbolicVariable(id) => Value::Concretization {
            constructor: TypeValueConstructor::SymbolicVariable(*id),
            tyargs,
        },
        Value::RefTyConstructor => Value::Concretization {
            constructor: TypeValueConstructor::RefConstructor,
            tyargs,
        },
        Value::ArrayTyConstructor => Value::Concretization {
            constructor: TypeValueConstructor::ArrayConstructor,
            tyargs,
        },
        Value::SliceTyConstructor => Value::Concretization {
            constructor: TypeValueConstructor::SliceConstructor,
            tyargs,
        },
        Value::IntTyConstructor => Value::Concretization {
            constructor: TypeValueConstructor::IntConstructor,
            tyargs,
        },
        Value::FloatTyConstructor => Value::Concretization {
            constructor: TypeValueConstructor::FloatConstructor,
            tyargs,
        },
        Value::Generic { typarams, body } => {
            assert!(
                typarams.len() == tyargs.len(),
                "wrong number of arguments; should be kindchecked"
            );
            let mut bindings = HashMap::new();
            for (typaram, tyarg) in std::iter::zip(typarams, tyargs) {
                match typaram {
                    Some(TypeParam { id, .. }) => {
                        bindings.insert(*id, tyarg);
                    }
                    None => {}
                }
            }
            subst(body, &bindings)
        }
        _ => {
            unreachable!("concretization of a non-generic; should have been kindchecked");
        }
    }
}

pub fn typecheck_val_pat_and_patch(
    v: &mut Augmented<hir::PatExpr>,
    expected_type: &Value,
    dlogger: &mut DiagnosticLogger,
    checker: &mut ResolutionEnvironment,
) -> Value {
    match &mut v.val {
        hir::PatExpr::Error => Value::Unknown,
        hir::PatExpr::Ignore => expected_type.clone(),
        hir::PatExpr::Identifier { id, .. } => match expected_type {
            Value::Unknown => {
                dlogger.log_cannot_infer_pattern_type(v.range);
                Value::Unknown
            }
            expected_type => {
                checker.id_type_table[*id] = expected_type.clone();
                expected_type.clone()
            }
        },
        hir::PatExpr::StructLiteral(fields) => {
            let mut expected_fields = match expected_type {
                Value::Struct(fields) => fields.clone(),
                _ => fields
                    .iter()
                    .map(|(name, _)| (name.val.clone(), Value::Unknown))
                    .collect(),
            };

            let mut actual_fields = HashMap::new();

            let mut has_error = false;
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
                        has_error = true;
                    }
                }
            }

            for (name, _) in expected_fields {
                dlogger.log_missing_field(v.range, &name);
                has_error = true;
            }

            if has_error {
                v.val = hir::PatExpr::Error;
            }

            expect_type(v, expected_type, Value::Struct(actual_fields), dlogger)
        }
        hir::PatExpr::New { pat, ty } => {
            // the symbolic type we are deconstructing
            let symbolic_type_to_construct =
                typecheck_val_expr_and_patch(ty, &Value::Unknown, dlogger, checker);
            let dereferenced_type = match symbolic_type_to_construct {
                Value::SymbolicVariable(id) => checker.id_type_table[id].clone(),
                _ => {
                    dlogger.log_cannot_destructure_non_struct(
                        v.range,
                        &symbolic_type_to_construct.to_string(),
                    );
                    v.val = hir::PatExpr::Error;
                    return Value::Unknown;
                }
            };
            // check inner pattern
            typecheck_val_pat_and_patch(pat, &dereferenced_type, dlogger, checker);
            // overall type of the pattern
            expect_type(v, expected_type, symbolic_type_to_construct, dlogger)
        }
        hir::PatExpr::Typed { pat, ty } => {
            let asserted_type =
                typecheck_val_expr_and_patch(ty, &Value::Unknown, dlogger, checker);
            typecheck_val_pat_and_patch(pat, &asserted_type, dlogger, checker);
            expect_type(ty, expected_type, asserted_type, dlogger)
        }
        hir::PatExpr::Kinded { pat, .. } => {
            typecheck_val_pat_and_patch(pat, expected_type, dlogger, checker)
        }
    }
}

pub fn typecheck_case_target_expr_and_patch(
    v: &mut Augmented<hir::CaseTargetExpr>,
    expected_hint: &Hint,
    dlogger: &mut DiagnosticLogger,
    checker: &mut ResolutionEnvironment,
) -> Value {
    match &mut v.val {
        hir::CaseTargetExpr::Error => Value::Unknown,
        hir::CaseTargetExpr::Bool(_) => expect_type(v, expected_hint, Value::Bool, dlogger),
        hir::CaseTargetExpr::Int(_) => match expected_hint {
            w @ Value::Concretization {
                constructor: TypeValueConstructor::IntConstructor,
                ..
            } => w.clone(),
            _ => expect_type(
                v,
                expected_hint,
                Value::Concretization {
                    constructor: TypeValueConstructor::IntConstructor,
                    tyargs: vec![Value::Unknown, Value::Unknown],
                },
                dlogger,
            ),
        },
        hir::CaseTargetExpr::PatExpr(pat) => {
            typecheck_val_pat_and_patch(pat, expected_hint, dlogger, checker)
        }
    }
}

// We assume that the val pat is already kindchecked
pub fn val_pat_expr_to_typeparam(v: &Augmented<hir::PatExpr>) -> Option<TypeParam> {
    match &v.val {
        hir::PatExpr::Identifier { id, .. } => Some(TypeParam {
            range: v.range.clone(),
            id: *id,
        }),
        hir::PatExpr::Kinded { pat, .. } => val_pat_expr_to_typeparam(pat),
        hir::PatExpr::Error => None,
        _ => {
            unreachable!("should have been kindchecked")
        }
    }
}

pub fn typecheck_val_expr_and_patch(
    v: &mut Augmented<hir::ValExpr>,
    expected: &Hint,
    dlogger: &mut DiagnosticLogger,
    checker: &mut ResolutionEnvironment,
) -> Value {
    match &mut v.val {
        hir::ValExpr::Error => Value::Unknown,
        hir::ValExpr::Identifier(id) => {
            let actual_type = match checker.id_modifier_table[*id] {
                IdentifierModifier::Nominal => Value::SymbolicVariable(*id),
                _ => checker.id_type_table[*id].clone(),
            };
            expect_type(v, expected, actual_type, dlogger)
        }
        hir::ValExpr::Bool { value, kind } => match kind {
            Some(KindValue::Bool) => {
                let value = *value;
                expect_type(v, expected, Value::BoolLit(value), dlogger)
            }
            Some(KindValue::Val) => expect_type(v, expected, Value::Bool, dlogger),
            _ => unreachable!("should have been kindchecked"),
        },
        hir::ValExpr::Int { value, kind, data } => match kind {
            Some(KindValue::Int) => {
                // type variable
                match i64::try_from(value.clone()) {
                    Ok(i) => expect_type(v, expected, Value::IntLit(i), dlogger),
                    Err(_) => {
                        dlogger.log_int_too_large(v.range, 64);
                        v.val = hir::ValExpr::Error;
                        Value::Unknown
                    }
                }
            }
            Some(KindValue::Val) => match expected {
                // takes the shape of the expected type
                w @ Value::Concretization {
                    constructor: TypeValueConstructor::IntConstructor,
                    tyargs,
                } => match tyargs[..] {
                    [Value::BoolLit(sgn), Value::IntLit(sz)] => {
                        *data = Some((sgn, sz as usize));
                        w.clone()
                    }
                    _ => unreachable!("should have been kindchecked"),
                },
                // otherwise error
                expected_type => {
                    dlogger.log_type_mismatch(
                        v.range,
                        &expected_type.to_string(),
                        Value::Concretization {
                            constructor: TypeValueConstructor::IntConstructor,
                            tyargs: vec![Value::Unknown, Value::Unknown],
                        }
                        .to_string()
                        .as_str(),
                    );
                    v.val = hir::ValExpr::Error;
                    Value::Unknown
                }
            },
            _ => unreachable!("should have been kindchecked"),
        },
        hir::ValExpr::Float { value, kind, data } => match kind {
            Some(KindValue::Float) => {
                let (n, d) = value.reduced().into_raw();
                let n = ToPrimitive::to_f64(&n).unwrap();
                let d = ToPrimitive::to_f64(&d).unwrap();
                expect_type(v, expected, Value::FloatLit(n / d), dlogger)
            }
            Some(KindValue::Val) => match expected {
                // takes the shape of the expected type
                w @ Value::Concretization {
                    constructor: TypeValueConstructor::FloatConstructor,
                    tyargs,
                } => match tyargs[..] {
                    [Value::IntLit(sz)] => {
                        *data = Some(sz as usize);
                        w.clone()
                    }
                    _ => unreachable!("should have been kindchecked"),
                },
                // otherwise error
                expected_type => {
                    dlogger.log_type_mismatch(
                        v.range,
                        &expected_type.to_string(),
                        Value::Concretization {
                            constructor: TypeValueConstructor::FloatConstructor,
                            tyargs: vec![Value::Unknown],
                        }
                        .to_string()
                        .as_str(),
                    );
                    v.val = hir::ValExpr::Error;
                    Value::Unknown
                }
            },
            _ => unreachable!("should have been kindchecked"),
        },
        hir::ValExpr::String(s) => {
            let l = s.len();
            expect_type(
                v,
                expected,
                Value::Concretization {
                    constructor: TypeValueConstructor::ArrayConstructor,
                    tyargs: vec![
                        Value::Concretization {
                            constructor: TypeValueConstructor::IntConstructor,
                            tyargs: vec![Value::BoolLit(false), Value::Unknown],
                        },
                        Value::IntLit(l as i64),
                    ],
                },
                dlogger,
            )
        }
        hir::ValExpr::StructLiteral(fields) => {
            let expected_fields = match expected {
                Value::Struct(fields) => fields.clone(),
                _ => fields
                    .iter()
                    .map(|(name, _)| (name.val.clone(), Value::Unknown))
                    .collect(),
            };

            let mut has_error = false;

            let mut actual_fields = HashMap::new();
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
                        has_error = true;
                    }
                }
            }

            for (name, _) in expected_fields {
                dlogger.log_missing_field(v.range, &name);
                has_error = true;
            }

            if has_error {
                v.val = hir::ValExpr::Error;
            }

            expect_type(v, expected, Value::Struct(actual_fields), dlogger)
        }
        hir::ValExpr::New { ty, val } => {
            // the symbolic type we are constructing
            let symbolic_type_to_construct =
                typecheck_val_expr_and_patch(ty, &Value::Unknown, dlogger, checker);
            let dereferenced_type = match symbolic_type_to_construct {
                Value::SymbolicVariable(id) => checker.id_type_table[id].clone(),
                _ => {
                    dlogger.log_cannot_new_type(v.range, &symbolic_type_to_construct.to_string());
                    v.val = hir::ValExpr::Error;
                    return Value::Unknown;
                }
            };
            // check inner pattern
            typecheck_val_expr_and_patch(val, &dereferenced_type, dlogger, checker);
            // overall type of the expression
            expect_type(v, expected, symbolic_type_to_construct, dlogger)
        }
        hir::ValExpr::Ref(inner) => {
            let expected_inner = match expected {
                Value::Concretization {
                    constructor: TypeValueConstructor::RefConstructor,
                    tyargs,
                } => &tyargs[0],
                _ => &Value::Unknown,
            };

            let actual_inner_type =
                typecheck_val_expr_and_patch(inner, expected_inner, dlogger, checker);

            expect_type(
                v,
                expected,
                Value::Concretization {
                    constructor: TypeValueConstructor::IntConstructor,
                    tyargs: vec![actual_inner_type],
                },
                dlogger,
            )
        }
        hir::ValExpr::Deref(inner) => {
            // the inner type should be a reference
            let expected_inner = Value::Concretization {
                constructor: TypeValueConstructor::RefConstructor,
                tyargs: vec![expected.clone()],
            };
            let actual_type =
                typecheck_val_expr_and_patch(inner, &expected_inner, dlogger, checker);
            expect_type(v, expected, actual_type, dlogger)
        }
        hir::ValExpr::Generic { params, body, .. } => match expected {
            Value::Generic {
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
                Value::Generic {
                    typarams: params.iter().map(val_pat_expr_to_typeparam).collect(),
                    body: Box::new(body),
                }
            }
            _ => {
                // we don't know what the expected type is, so we just typecheck the body
                let body =
                    typecheck_val_expr_and_patch(body, &Value::Unknown, dlogger, checker);
                let actual = Value::Generic {
                    typarams: params.iter().map(val_pat_expr_to_typeparam).collect(),
                    body: Box::new(body),
                };
                expect_type(v, expected, actual, dlogger)
            }
        },
        hir::ValExpr::Lam {
            params,
            returnty,
            body,
        } => {
            let (expected_paramtys, expected_returnty) = match expected {
                Value::Fn {
                    paramtys,
                    returntype,
                } => (paramtys, returntype.as_ref()),
                _ => (
                    &params
                        .iter()
                        .map(|_| Value::Unknown)
                        .collect::<Vec<_>>(),
                    &Value::Unknown,
                ),
            };

            let actual_paramtys = std::iter::zip(params.iter_mut(), expected_paramtys)
                .map(|(param, expected_paramty)| {
                    typecheck_val_pat_and_patch(param, expected_paramty, dlogger, checker)
                })
                .collect::<Vec<_>>();

            let actual_returnty = match returnty {
                Some(returnty) => {
                    let actual_returnty = typecheck_val_expr_and_patch(
                        returnty,
                        &Value::Unknown,
                        dlogger,
                        checker,
                    );
                    // typecheck the returnty with the expected returnty
                    expect_type(returnty, expected_returnty, actual_returnty, dlogger)
                }
                None => expected_returnty.clone(),
            };

            // typecheck the body
            typecheck_val_expr_and_patch(body, &actual_returnty, dlogger, checker);

            expect_type(
                v,
                expected,
                Value::Fn {
                    paramtys: actual_paramtys,
                    returntype: Box::new(actual_returnty),
                },
                dlogger,
            )
        }
        hir::ValExpr::CaseOf { expr, cases } => {
            let expr_type =
                typecheck_val_expr_and_patch(expr, &Value::Unknown, dlogger, checker);

            match cases.split_first_mut() {
                Some(((first_target, first_body), rest)) => {
                    typecheck_case_target_expr_and_patch(
                        first_target,
                        &expr_type,
                        dlogger,
                        checker,
                    );

                    let first_body_type =
                        typecheck_val_expr_and_patch(first_body, expected, dlogger, checker);

                    for (target, body) in rest {
                        typecheck_case_target_expr_and_patch(target, &expr_type, dlogger, checker);
                        typecheck_val_expr_and_patch(body, &first_body_type, dlogger, checker);
                    }

                    expect_type(v, expected, first_body_type, dlogger)
                }
                None => expect_type(v, expected, Value::Unknown, dlogger),
            }
        }
        hir::ValExpr::Block { statements, label } => {
            // add the type hint to the label
            checker.lb_type_hint_table[*label] = expected.clone();

            // check all statements with this context
            for statement in statements {
                typecheck_block_statement_and_patch(statement, dlogger, checker);
            }

            // the type of the block is the type of the label, if it exists
            // if it doesn't exist, then it's unit
            let actual_type = match checker.lb_type_table[*label] {
                Value::Unknown => Value::Struct(HashMap::new()),
                ref ty => ty.clone(),
            };

            expect_type(v, expected, actual_type, dlogger)
        }
        hir::ValExpr::Loop { label, body } => {
            // add the type hint to the label
            checker.lb_type_hint_table[*label] = expected.clone();

            // body must have unit type
            typecheck_val_expr_and_patch(
                body,
                &Value::Struct(HashMap::new()),
                dlogger,
                checker,
            );

            // the type of the loop is the type of the label, if it exists
            // if it doesn't exist, then it's Never (since it never returns)
            let actual_type = match checker.lb_type_table[*label] {
                Value::Unknown => Value::Never,
                ref ty => ty.clone(),
            };

            expect_type(v, expected, actual_type, dlogger)
        }
        hir::ValExpr::If {
            cond,
            then_branch,
            else_branch: None,
        } => {
            typecheck_val_expr_and_patch(cond, &Value::Bool, dlogger, checker);
            typecheck_val_expr_and_patch(
                then_branch,
                &Value::Struct(HashMap::new()),
                dlogger,
                checker,
            );
            expect_type(v, expected, Value::Struct(HashMap::new()), dlogger)
        }
        hir::ValExpr::If {
            cond,
            then_branch,
            else_branch: Some(else_branch),
        } => {
            typecheck_val_expr_and_patch(cond, &Value::Bool, dlogger, checker);
            let then_type =
                typecheck_val_expr_and_patch(then_branch, expected, dlogger, checker);
            typecheck_val_expr_and_patch(else_branch, &then_type, dlogger, checker);
            expect_type(v, expected, then_type, dlogger)
        }
        hir::ValExpr::Ret { label, value } => {
            // get label type or label type hint
            let expected_ret_type = match checker.lb_type_table[*label] {
                Value::Unknown => checker.lb_type_hint_table[*label].clone(),
                ref ty => ty.clone(),
            };
            let actual_type =
                typecheck_val_expr_and_patch(value, &expected_ret_type, dlogger, checker);
            // set the label type
            if actual_type != Value::Unknown {
                checker.lb_type_table[*label] = actual_type.clone();
            }
            // ret never returns
            expect_type(v, expected, Value::Never, dlogger)
        }
        hir::ValExpr::ArrayLiteral(vals) => {
            let expected_inner_ty = match expected {
                Value::Concretization {
                    constructor: TypeValueConstructor::ArrayConstructor,
                    tyargs,
                } => &tyargs[0],
                _ => &Value::Unknown,
            };

            let vals_len = vals.len();

            match vals.split_first_mut() {
                Some((first, rest)) => {
                    let first_val_type =
                        typecheck_val_expr_and_patch(first, expected_inner_ty, dlogger, checker);

                    for val in rest {
                        typecheck_val_expr_and_patch(val, &first_val_type, dlogger, checker);
                    }

                    expect_type(
                        v,
                        expected,
                        Value::Concretization {
                            constructor: TypeValueConstructor::ArrayConstructor,
                            tyargs: vec![first_val_type, Value::IntLit(vals_len as i64)],
                        },
                        dlogger,
                    )
                }
                None => expect_type(
                    v,
                    expected,
                    Value::Concretization {
                        constructor: TypeValueConstructor::ArrayConstructor,
                        tyargs: vec![Value::Unknown, Value::IntLit(0)],
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
            // T x T -> unit
            hir::ValBinaryOpKind::Assign => {
                let left_type = typecheck_val_expr_and_patch(
                    left_operand,
                    &Value::Unknown,
                    dlogger,
                    checker,
                );
                // check right type
                typecheck_val_expr_and_patch(right_operand, &left_type, dlogger, checker);
                expect_type(v, expected, Value::Struct(HashMap::new()), dlogger)
            }
            // number x number -> number
            hir::ValBinaryOpKind::Add
            | hir::ValBinaryOpKind::Sub
            | hir::ValBinaryOpKind::Mul
            | hir::ValBinaryOpKind::Div
            | hir::ValBinaryOpKind::Rem => {
                // left type
                let left_type =
                    typecheck_val_expr_and_patch(left_operand, &expected, dlogger, checker);
                let right_type =
                    typecheck_val_expr_and_patch(right_operand, &left_type, dlogger, checker);
                match (&left_type, &right_type) {
                    (
                        Value::Concretization {
                            constructor: TypeValueConstructor::IntConstructor,
                            tyargs: l_tyargs,
                        },
                        Value::Concretization {
                            constructor: TypeValueConstructor::IntConstructor,
                            tyargs: r_tyargs,
                        },
                    ) if l_tyargs == r_tyargs => {}
                    (
                        Value::Concretization {
                            constructor: TypeValueConstructor::FloatConstructor,
                            tyargs: l_tyargs,
                        },
                        Value::Concretization {
                            constructor: TypeValueConstructor::FloatConstructor,
                            tyargs: r_tyargs,
                        },
                    ) if l_tyargs == r_tyargs => {}
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
                expect_type(v, expected, left_type, dlogger)
            }
            // nummber x number -> unit
            hir::ValBinaryOpKind::AssignAdd
            | hir::ValBinaryOpKind::AssignSub
            | hir::ValBinaryOpKind::AssignMul
            | hir::ValBinaryOpKind::AssignDiv => {
                let left_type = typecheck_val_expr_and_patch(
                    left_operand,
                    &Value::Unknown,
                    dlogger,
                    checker,
                );
                let right_type =
                    typecheck_val_expr_and_patch(right_operand, &left_type, dlogger, checker);
                match (&left_type, &right_type) {
                    (
                        Value::Concretization {
                            constructor: TypeValueConstructor::IntConstructor,
                            tyargs: l_tyargs,
                        },
                        Value::Concretization {
                            constructor: TypeValueConstructor::IntConstructor,
                            tyargs: r_tyargs,
                        },
                    ) if l_tyargs == r_tyargs => {}
                    (
                        Value::Concretization {
                            constructor: TypeValueConstructor::FloatConstructor,
                            tyargs: l_tyargs,
                        },
                        Value::Concretization {
                            constructor: TypeValueConstructor::FloatConstructor,
                            tyargs: r_tyargs,
                        },
                    ) if l_tyargs == r_tyargs => {}
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

                expect_type(v, expected, Value::Struct(HashMap::new()), dlogger)
            }
            // number x number -> bool
            hir::ValBinaryOpKind::Lt
            | hir::ValBinaryOpKind::Leq
            | hir::ValBinaryOpKind::Gt
            | hir::ValBinaryOpKind::Geq => {
                // left type
                let left_type = typecheck_val_expr_and_patch(
                    left_operand,
                    &Value::Unknown,
                    dlogger,
                    checker,
                );
                let right_type =
                    typecheck_val_expr_and_patch(right_operand, &left_type, dlogger, checker);

                match (&left_type, &right_type) {
                    (
                        Value::Concretization {
                            constructor: TypeValueConstructor::IntConstructor,
                            tyargs: l_tyargs,
                        },
                        Value::Concretization {
                            constructor: TypeValueConstructor::IntConstructor,
                            tyargs: r_tyargs,
                        },
                    ) if l_tyargs == r_tyargs => {}
                    (
                        Value::Concretization {
                            constructor: TypeValueConstructor::FloatConstructor,
                            tyargs: l_tyargs,
                        },
                        Value::Concretization {
                            constructor: TypeValueConstructor::FloatConstructor,
                            tyargs: r_tyargs,
                        },
                    ) if l_tyargs == r_tyargs => {}
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

                expect_type(v, expected, Value::Bool, dlogger)
            }
            // bool x bool -> bool
            hir::ValBinaryOpKind::And | hir::ValBinaryOpKind::Or => {
                typecheck_val_expr_and_patch(left_operand, &Value::Bool, dlogger, checker);
                typecheck_val_expr_and_patch(right_operand, &Value::Bool, dlogger, checker);
                expect_type(v, expected, Value::Bool, dlogger)
            }
            // (number | bool) x (number | bool) -> bool
            hir::ValBinaryOpKind::Eq | hir::ValBinaryOpKind::Neq => {
                // left type
                let left_type = typecheck_val_expr_and_patch(
                    left_operand,
                    &Value::Unknown,
                    dlogger,
                    checker,
                );

                let right_type =
                    typecheck_val_expr_and_patch(right_operand, &left_type, dlogger, checker);

                match (&left_type, &right_type) {
                    (
                        Value::Concretization {
                            constructor: TypeValueConstructor::IntConstructor,
                            tyargs: l_tyargs,
                        },
                        Value::Concretization {
                            constructor: TypeValueConstructor::IntConstructor,
                            tyargs: r_tyargs,
                        },
                    ) if l_tyargs == r_tyargs => {}
                    (
                        Value::Concretization {
                            constructor: TypeValueConstructor::FloatConstructor,
                            tyargs: l_tyargs,
                        },
                        Value::Concretization {
                            constructor: TypeValueConstructor::FloatConstructor,
                            tyargs: r_tyargs,
                        },
                    ) if l_tyargs == r_tyargs => {}
                    (Value::Bool, Value::Bool) => {}
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

                expect_type(v, expected, Value::Bool, dlogger)
            }
        },

        hir::ValExpr::ArrayAccess { root, index } => {
            let actual_inner_type =
                typecheck_val_expr_and_patch(root, &Value::Unknown, dlogger, checker);

            let usize_type = Value::Concretization {
                constructor: TypeValueConstructor::IntConstructor,
                tyargs: vec![Value::BoolLit(false), Value::IntLit(64)],
            };
            typecheck_val_expr_and_patch(index, &usize_type, dlogger, checker);

            let actual_type = match actual_inner_type {
                Value::Concretization {
                    constructor: TypeValueConstructor::ArrayConstructor,
                    tyargs,
                } => tyargs[0].clone(),
                Value::Concretization {
                    constructor: TypeValueConstructor::SliceConstructor,
                    tyargs,
                } => tyargs[0].clone(),
                _ => {
                    dlogger.log_cannot_access_index_of_non_array(
                        v.range,
                        &actual_inner_type.to_string(),
                    );
                    v.val = hir::ValExpr::Error;
                    Value::Unknown
                }
            };

            expect_type(v, expected, actual_type, dlogger)
        }
        hir::ValExpr::FieldAccess { root, field } => {
            let inner_type =
                typecheck_val_expr_and_patch(root, &Value::Unknown, dlogger, checker);

            match inner_type {
                Value::Struct(fields) => match fields.get(field) {
                    Some(field_type) => expect_type(v, expected, field_type.clone(), dlogger),
                    None => {
                        dlogger.log_missing_field(v.range, field);
                        v.val = hir::ValExpr::Error;
                        Value::Unknown
                    }
                },
                _ => {
                    dlogger.log_cannot_access_field_of_non_struct(v.range, &inner_type.to_string());
                    v.val = hir::ValExpr::Error;
                    Value::Unknown
                }
            }
        }
        hir::ValExpr::Concretization { generic, tyargs } => {
            // first we evaluate the type of the generic function
            let generic_val =
                typecheck_val_expr_and_patch(generic, &Value::Unknown, dlogger, checker);
            let tyargs = tyargs
                .iter_mut()
                .map(|x| typecheck_val_expr_and_patch(x, &Value::Unknown, dlogger, checker))
                .collect::<Vec<_>>();
            // attempt to concretize the type (will only work if there are no symbolic variables in the type)
            concretize_type_expr(&generic_val, tyargs)
        }
        hir::ValExpr::App { fun, args } => {
            let fun_ty = typecheck_val_expr_and_patch(fun, &Value::Unknown, dlogger, checker);

            let (paramtys, returnty) = match fun_ty {
                Value::Fn {
                    paramtys,
                    returntype,
                } => (paramtys, returntype),
                _ => {
                    dlogger.log_not_callable(v.range, &fun_ty.to_string());
                    v.val = hir::ValExpr::Error;
                    return Value::Unknown;
                }
            };

            let args_len = args.len();
            let paramtys_len = paramtys.len();

            for (arg, paramty) in std::iter::zip(args, paramtys) {
                typecheck_val_expr_and_patch(arg, &paramty, dlogger, checker);
            }

            // mark wrong number of args as errors
            if args_len != paramtys_len {
                dlogger.log_wrong_number_args(v.range, paramtys_len, args_len);
                v.val = hir::ValExpr::Error;
            }

            // type of the function application
            expect_type(v, expected, *returnty, dlogger)
        }
        hir::ValExpr::PiTy { param_tys: paramtys, dep_ty: returnty } => Value::Fn {
            paramtys: paramtys
                .iter_mut()
                .map(|ty| typecheck_val_expr_and_patch(ty, &Value::Unknown, dlogger, checker))
                .collect(),
            returntype: Box::new(typecheck_val_expr_and_patch(
                returnty,
                &Value::Unknown,
                dlogger,
                checker,
            )),
        },
        hir::ValExpr::Struct(fields) => Value::Struct(
            fields
                .iter_mut()
                .map(|(name, expr)| {
                    (
                        name.val.clone(),
                        typecheck_val_expr_and_patch(expr, &Value::Unknown, dlogger, checker),
                    )
                })
                .collect(),
        ),
        hir::ValExpr::Enum(fields) => Value::Enum(
            fields
                .iter_mut()
                .map(|(name, expr)| {
                    (
                        name.val.clone(),
                        typecheck_val_expr_and_patch(expr, &Value::Unknown, dlogger, checker),
                    )
                })
                .collect(),
        ),
        hir::ValExpr::Union(fields) => Value::Union(
            fields
                .iter_mut()
                .map(|(name, expr)| {
                    (
                        name.val.clone(),
                        typecheck_val_expr_and_patch(expr, &Value::Unknown, dlogger, checker),
                    )
                })
                .collect(),
        ),
        hir::ValExpr::Extern { ty, .. } => {
            typecheck_val_expr_and_patch(ty, expected, dlogger, checker)
        }
        hir::ValExpr::Hole => todo!(),
        hir::ValExpr::Typed { val, ty } => {
            // get the expected type's type
            let expected_ty =
                typecheck_val_expr_and_patch(ty, &Value::Unknown, dlogger, checker);
            // check the value
            let actual_ty = typecheck_val_expr_and_patch(val, &expected_ty, dlogger, checker);
            // check the type
            expect_type(ty, expected, actual_ty, dlogger)
        }
    }
}

pub fn typecheck_block_statement_and_patch(
    v: &mut Augmented<hir::BlockStatement>,
    dlogger: &mut DiagnosticLogger,
    checker: &mut ResolutionEnvironment,
) {
    match &mut v.val {
        hir::BlockStatement::Error => {}
        hir::BlockStatement::NoOp => {}
        hir::BlockStatement::Let { pat, value } => {
            // get hint from the value pattern
            let type_hint = typehint_of_pat_and_patch(pat, dlogger, checker);
            let ty = typecheck_val_expr_and_patch(value, &type_hint, dlogger, checker);
            typecheck_val_pat_and_patch(pat, &ty, dlogger, checker);
        }
        hir::BlockStatement::Do(val) => {
            typecheck_val_expr_and_patch(val, &Value::Struct(HashMap::new()), dlogger, checker);
        }
    }
}

pub fn typecheck_file_statement_and_patch(
    v: &mut Augmented<hir::FileStatement>,
    dlogger: &mut DiagnosticLogger,
    checker: &mut ResolutionEnvironment,
) {
    match &mut v.val {
        hir::FileStatement::Error => {}
        hir::FileStatement::Let { pat, value } => {
            // get hint from the value pattern
            let ty_hint = typehint_of_pat_and_patch(pat, dlogger, checker);
            let ty = typecheck_val_expr_and_patch(value, &ty_hint, dlogger, checker);
            typecheck_val_pat_and_patch(pat, &ty, dlogger, checker);
        }
    }
}
