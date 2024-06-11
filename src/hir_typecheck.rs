use crate::dlogger::DiagnosticLogger;
use crate::hir::Augmented;
use crate::typecheck::{evaluate_hir_kind, TypeChecker};
use crate::types::{KindValue, TypeValue};
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
                        name.clone(),
                        typehint_of_val_pat_and_patch(expr, dlogger, checker),
                    )
                })
                .collect(),
        ),
        hir::ValPatExpr::New { ty, .. } => {
            typecheck_type_expr_and_patch(ty, &TypeValue::Unknown, dlogger, checker)
        }
        hir::ValPatExpr::Typed { ty, .. } => {
            typecheck_type_expr_and_patch(ty, &TypeValue::Unknown, dlogger, checker)
        }
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

// if we encounter an error, we log it
// NOTE: we assume that all identifiers have already been resolved.
pub fn typecheck_type_expr_and_patch(
    v: &mut Augmented<hir::TypeExpr>,
    expected_type: &TypeValue,
    dlogger: &mut DiagnosticLogger,
    checker: &mut TypeChecker,
) -> TypeValue {
    match &mut v.val {
        hir::TypeExpr::Error => TypeValue::Unknown,
        hir::TypeExpr::Identifier(id) => {
            let actual_type = checker.type_type_table[*id]
                .clone()
                .expect("kind not initialized yet");
            expect_type(v, expected_type, actual_type, dlogger)
        }
        hir::TypeExpr::BoolTy => expect_type(v, expected_type, TypeValue::Bool, dlogger),
        hir::TypeExpr::RefConstructorTy => {
            expect_type(v, expected_type, TypeValue::RefConstructor, dlogger)
        }
        hir::TypeExpr::ArrayConstructorTy => {
            expect_type(v, expected_type, TypeValue::ArrayConstructor, dlogger)
        }
        hir::TypeExpr::SliceConstructorTy => {
            expect_type(v, expected_type, TypeValue::SliceConstructor, dlogger)
        }
        hir::TypeExpr::IntConstructorTy => {
            expect_type(v, expected_type, TypeValue::IntConstructor, dlogger)
        }
        hir::TypeExpr::UIntConstructorTy => {
            expect_type(v, expected_type, TypeValue::UIntConstructor, dlogger)
        }
        hir::TypeExpr::FloatConstructorTy => {
            expect_type(v, expected_type, TypeValue::FloatConstructor, dlogger)
        }

        hir::TypeExpr::Int(i) => {
            expect_type(v, expected_type, TypeValue::IntLit(i.clone()), dlogger)
        }
        hir::TypeExpr::Bool(i) => expect_type(v, expected_type, TypeValue::BoolLit(*i), dlogger),
        hir::TypeExpr::Float(i) => {
            expect_type(v, expected_type, TypeValue::FloatLit(i.clone()), dlogger)
        }
        hir::TypeExpr::Fn { paramtys, returnty } => {
            // we can actually use the type hints here:
            // for each param, we evaluate the paramty with the expected type
            // we evaluate the returnty with the expected returntype

            let (expected_params_type, expected_return_type) = match expected_type {
                TypeValue::Unknown => (
                    &std::iter::repeat(TypeValue::Unknown)
                        .take(paramtys.len())
                        .collect(),
                    TypeValue::Unknown,
                ),
                TypeValue::Fn {
                    paramtys,
                    returntype,
                } => (paramtys, *returntype.clone()),
                _ => {
                    // the expected type is not compatible
                    dlogger.log_unexpected_fn(v.range, &expected_type.to_string());
                    v.val = hir::TypeExpr::Error;
                    return TypeValue::Unknown;
                }
            };

            if expected_params_type.len() != paramtys.len() {
                dlogger.log_wrong_number_fn_params(
                    v.range,
                    expected_params_type.len(),
                    paramtys.len(),
                );
                v.val = hir::TypeExpr::Error;
                return TypeValue::Unknown;
            }

            let paramtys_actual = std::iter::zip(paramtys, expected_params_type)
                .map(|(paramty, expected_type)| {
                    typecheck_type_expr_and_patch(paramty, expected_type, dlogger, checker)
                })
                .collect();

            let returnty_actual =
                typecheck_type_expr_and_patch(returnty, &expected_return_type, dlogger, checker);

            TypeValue::Fn {
                paramtys: paramtys_actual,
                returntype: Box::new(returnty_actual),
            }
        }
        hir::TypeExpr::Struct(fields) => {
            for (_, expr) in fields {
                typecheck_type_expr_and_patch(expr, &KindValue::Type, dlogger, checker);
            }
            expect_type(v, expected_type, KindValue::Type, dlogger)
        }
        hir::TypeExpr::Enum(fields) => {
            for (_, expr) in fields {
                typecheck_type_expr_and_patch(expr, &KindValue::Type, dlogger, checker);
            }
            expect_type(v, expected_type, KindValue::Type, dlogger)
        }
        hir::TypeExpr::Union(fields) => {
            for (_, expr) in fields {
                typecheck_type_expr_and_patch(expr, &KindValue::Type, dlogger, checker);
            }
            expect_type(v, expected_type, KindValue::Type, dlogger)
        }
        hir::TypeExpr::Concretization {
            genericty,
            tyargs: provided_args,
        } => {
            let kind_of_generic =
                typecheck_type_expr_and_patch(genericty, &KindValue::Unknown, dlogger, checker);
            match kind_of_generic {
                KindValue::Generic {
                    paramkinds: expected_kinds,
                    returnkind,
                } => {
                    if expected_kinds.len() != provided_args.len() {
                        dlogger.log_wrong_number_type_args(
                            v.range,
                            expected_kinds.len(),
                            provided_args.len(),
                        );
                        KindValue::Unknown
                    } else {
                        for (ref argkind, ref mut tyarg) in
                            std::iter::zip(expected_kinds, provided_args)
                        {
                            typecheck_type_expr_and_patch(tyarg, argkind, dlogger, checker);
                        }
                        *returnkind
                    }
                }
                _ => {
                    dlogger.log_cannot_be_concretized(v.range);
                    v.val = hir::TypeExpr::Error;
                    KindValue::Unknown
                }
            }
        }
        hir::TypeExpr::Generic {
            params,
            returnkind,
            body,
        } => {
            // we can actually use the type hints here:
            // for each param, we evaluate the type_pattern with the expected kind
            // if the returnkind exists we evaluate it with the expected kind, otherwise we take the expected kind
            // we then evaluate the body with the returnkind

            let (expected_params_kind, expected_returnkind) = match expected_type {
                KindValue::Unknown => (
                    &std::iter::repeat(KindValue::Unknown)
                        .take(params.len())
                        .collect(),
                    KindValue::Unknown,
                ),
                KindValue::Generic {
                    paramkinds,
                    returnkind,
                } => (paramkinds, *returnkind.clone()),
                _ => {
                    // the expected type is not compatible
                    dlogger.log_unexpected_generic(v.range, &expected_type.to_string());
                    v.val = hir::TypeExpr::Error;
                    return KindValue::Unknown;
                }
            };

            // assert we have the right number of type arguments
            if expected_params_kind.len() != params.len() {
                dlogger.log_wrong_number_type_args(
                    v.range,
                    expected_params_kind.len(),
                    params.len(),
                );
                v.val = hir::TypeExpr::Error;
                return KindValue::Unknown;
            }

            // check each type parameter
            let paramkinds = std::iter::zip(params.iter_mut(), expected_params_kind)
                .map(|(param, expected_param_kind)| {
                    typecheck_type_pat_expr_and_patch(param, expected_param_kind, dlogger, checker)
                })
                .collect();

            // ensure return kind matches
            let returnkind = if let Some(rk) = returnkind {
                evaluate_hir_kind(&rk)
            } else {
                KindValue::Unknown
            };

            if expected_returnkind.supports_assign(&returnkind) {
                // check the body
                typecheck_type_expr_and_patch(body, &returnkind, dlogger, checker);
                KindValue::Generic {
                    paramkinds,
                    returnkind: Box::new(returnkind),
                }
            } else {
                dlogger.log_kind_mismatch(
                    v.range,
                    &expected_returnkind.to_string(),
                    &returnkind.to_string(),
                );
                v.val = hir::TypeExpr::Error;
                KindValue::Unknown
            }
        }
    }
}

pub fn typecheck_val_pat_and_patch(
    v: &mut Augmented<hir::ValPatExpr>,
    expected_kind: &TypeValue,
    dlogger: &mut DiagnosticLogger,
    checker: &mut TypeChecker,
) -> TypeValue {
    match &mut v.val {
        hir::ValPatExpr::Error => TypeValue::Unknown,
        hir::ValPatExpr::Ignore => {
            // if expected kind is unknown, we throw an error
            if expected_kind == &KindValue::Unknown {
                dlogger.log_cannot_infer_valpat_kind(v.range);
                v.val = hir::ValPatExpr::Error;
                KindValue::Unknown
            } else {
                expected_kind.clone()
            }
        }
        hir::ValPatExpr::Identifier { id, .. } => {
            // if expected kind is unknown, we throw an error
            if expected_kind == &KindValue::Unknown {
                dlogger.log_cannot_infer_val_kind(v.range);
                v.val = hir::ValPatExpr::Error;
                KindValue::Unknown
            } else {
                // otherwise we assign the expected kind to the identifier
                checker.val_kind_table[*id] = Some(expected_kind.clone());
                expected_kind.clone()
            }
        }
        hir::ValPatExpr::StructLiteral(fields) => {
            for (_, expr) in fields {
                typecheck_val_pat_and_patch(expr, &KindValue::Type, dlogger, checker);
            }
            expect_type(v, expected_kind, KindValue::Type, dlogger)
        }
        hir::ValPatExpr::New { pat, ty } => {
            typecheck_type_expr_and_patch(ty, &KindValue::Type, dlogger, checker);
            typecheck_val_pat_and_patch(pat, &KindValue::Type, dlogger, checker);
            expect_type(v, expected_kind, KindValue::Type, dlogger)
        }
        hir::ValPatExpr::Typed { pat, ty } => {
            let kind = typecheck_type_expr_and_patch(ty, expected_kind, dlogger, checker);
            typecheck_val_pat_and_patch(pat, &kind, dlogger, checker)
        }
    }
}

pub fn typecheck_case_target_expr_and_patch(
    v: &mut Augmented<hir::CaseTargetExpr>,
    expected_kind: &TypeValue,
    dlogger: &mut DiagnosticLogger,
    checker: &mut TypeChecker,
) -> TypeValue {
    match &mut v.val {
        hir::CaseTargetExpr::Error => TypeValue::Unknown,
        hir::CaseTargetExpr::Bool(_) => expect_type(v, expected_kind, KindValue::Type, dlogger),
        hir::CaseTargetExpr::Int(_) => expect_type(v, expected_kind, KindValue::Type, dlogger),
        hir::CaseTargetExpr::PatExpr(pat) => {
            typecheck_val_pat_and_patch(pat, expected_kind, dlogger, checker)
        }
    }
}

pub fn typecheck_val_expr_and_patch(
    v: &mut Augmented<hir::ValExpr>,
    expected_kind: &TypeValue,
    dlogger: &mut DiagnosticLogger,
    checker: &mut TypeChecker,
) -> TypeValue {
    match &mut v.val {
        hir::ValExpr::Error => TypeValue::Unknown,
        hir::ValExpr::Identifier(id) => {
            let actual_kind = checker.val_kind_table[*id]
                .clone()
                .expect("kind not initialized yet");
            expect_type(v, expected_kind, actual_kind, dlogger)
        }
        hir::ValExpr::Bool(_) => expect_type(v, expected_kind, KindValue::Type, dlogger),
        hir::ValExpr::Int(_) => expect_type(v, expected_kind, KindValue::Type, dlogger),
        hir::ValExpr::Float(_) => expect_type(v, expected_kind, KindValue::Type, dlogger),
        hir::ValExpr::String(_) => expect_type(v, expected_kind, KindValue::Type, dlogger),
        hir::ValExpr::StructLiteral(fields) => {
            for (_, expr) in fields {
                typecheck_val_expr_and_patch(expr, &KindValue::Type, dlogger, checker);
            }
            expect_type(v, expected_kind, KindValue::Type, dlogger)
        }
        hir::ValExpr::New { ty, val } => {
            typecheck_type_expr_and_patch(ty, &KindValue::Type, dlogger, checker);
            typecheck_val_expr_and_patch(val, &KindValue::Type, dlogger, checker);
            expect_type(v, expected_kind, KindValue::Type, dlogger)
        }
        hir::ValExpr::Ref(inner) => {
            let actual_kind =
                typecheck_val_expr_and_patch(inner, &KindValue::Unknown, dlogger, checker);
            // can only take reference of a value of kind TYPE
            expect_type(v, expected_kind, actual_kind, dlogger)
        }
        hir::ValExpr::Deref(inner) => {
            let actual_kind =
                typecheck_val_expr_and_patch(inner, &KindValue::Type, dlogger, checker);
            expect_type(v, expected_kind, actual_kind, dlogger)
        }
        hir::ValExpr::Generic { params, body } => {
            let (expected_params_kind, expected_returnkind) = match expected_kind {
                KindValue::Unknown => (
                    &std::iter::repeat(KindValue::Unknown)
                        .take(params.len())
                        .collect(),
                    KindValue::Unknown,
                ),
                KindValue::Generic {
                    paramkinds,
                    returnkind,
                } => (paramkinds, *returnkind.clone()),
                _ => {
                    // the expected type is not compatible
                    dlogger.log_unexpected_generic(v.range, &expected_kind.to_string());
                    v.val = hir::ValExpr::Error;
                    return KindValue::Unknown;
                }
            };

            // assert we have the right number of type arguments
            if expected_params_kind.len() != params.len() {
                dlogger.log_wrong_number_type_args(
                    v.range,
                    expected_params_kind.len(),
                    params.len(),
                );
                v.val = hir::ValExpr::Error;
                return KindValue::Unknown;
            }

            // check each type parameter
            let paramkinds = std::iter::zip(params.iter_mut(), expected_params_kind)
                .map(|(param, expected_param_kind)| {
                    typecheck_type_pat_expr_and_patch(param, expected_param_kind, dlogger, checker)
                })
                .collect();

            // ensure return kind matches
            if expected_returnkind.supports_assign(&KindValue::Type) {
                // check the body
                typecheck_val_expr_and_patch(body, &KindValue::Type, dlogger, checker);
                KindValue::Generic {
                    paramkinds,
                    returnkind: Box::new(KindValue::Type),
                }
            } else {
                dlogger.log_kind_mismatch(
                    v.range,
                    &expected_returnkind.to_string(),
                    &KindValue::Type.to_string(),
                );
                v.val = hir::ValExpr::Error;
                KindValue::Unknown
            }
        }
        hir::ValExpr::FnDef {
            params,
            returnty,
            body,
        } => {
            for param in params {
                typecheck_val_pat_and_patch(param, &KindValue::Type, dlogger, checker);
            }
            if let Some(returnty) = returnty {
                typecheck_type_expr_and_patch(returnty, &KindValue::Type, dlogger, checker);
            }
            typecheck_val_expr_and_patch(body, &KindValue::Type, dlogger, checker);
            expect_type(v, expected_kind, KindValue::Type, dlogger)
        }
        hir::ValExpr::IfThen {
            cond,
            then_branch,
            else_branch,
        } => {
            typecheck_val_expr_and_patch(cond, &KindValue::Type, dlogger, checker);
            typecheck_val_expr_and_patch(then_branch, &KindValue::Type, dlogger, checker);
            if let Some(else_branch) = else_branch {
                typecheck_val_expr_and_patch(else_branch, &KindValue::Type, dlogger, checker);
            }
            expect_type(v, expected_kind, KindValue::Type, dlogger)
        }
        hir::ValExpr::CaseOf { expr, cases } => {
            typecheck_val_expr_and_patch(expr, &KindValue::Type, dlogger, checker);
            for (target, body) in cases {
                typecheck_case_target_expr_and_patch(target, &KindValue::Type, dlogger, checker);
                typecheck_val_expr_and_patch(body, &KindValue::Type, dlogger, checker);
            }
            expect_type(v, expected_kind, KindValue::Type, dlogger)
        }
        hir::ValExpr::Block {
            statements,
            last_expression,
        } => {
            for statement in statements {
                typecheck_block_statement_and_patch(statement, dlogger, checker);
            }
            if let Some(expr) = last_expression {
                typecheck_val_expr_and_patch(expr, &KindValue::Type, dlogger, checker);
            }
            expect_type(v, expected_kind, KindValue::Type, dlogger)
        }
        hir::ValExpr::ArrayLiteral(vals) => {
            for val in vals {
                typecheck_val_expr_and_patch(val, &KindValue::Type, dlogger, checker);
            }
            expect_type(v, expected_kind, KindValue::Type, dlogger)
        }
        hir::ValExpr::BinaryOp {
            op: _,
            left_operand,
            right_operand,
        } => {
            typecheck_val_expr_and_patch(left_operand, &KindValue::Type, dlogger, checker);
            typecheck_val_expr_and_patch(right_operand, &KindValue::Type, dlogger, checker);
            expect_type(v, expected_kind, KindValue::Type, dlogger)
        }
        hir::ValExpr::ArrayAccess { root, index } => {
            typecheck_val_expr_and_patch(root, &KindValue::Type, dlogger, checker);
            typecheck_val_expr_and_patch(index, &KindValue::Int, dlogger, checker);
            expect_type(v, expected_kind, KindValue::Type, dlogger)
        }
        hir::ValExpr::FieldAccess { root, .. } => {
            typecheck_val_expr_and_patch(root, &KindValue::Type, dlogger, checker);
            expect_type(v, expected_kind, KindValue::Type, dlogger)
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
                        expect_type(v, expected_kind, *returnkind, dlogger)
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
            expect_type(v, expected_kind, KindValue::Type, dlogger)
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
            // get hint from the type pattern
            let kind_hint = typehint_of_type_pat(typat);
            // try calculating the kind of the type expression
            let kind = typecheck_type_expr_and_patch(value, &kind_hint, dlogger, checker);
            // bind the resolved kind to all the identifiers in type pattern
            typecheck_type_pat_expr_and_patch(typat, &kind, dlogger, checker);
        }
        hir::BlockStatement::ValDef { pat, value } => {
            // get hint from the value pattern
            let kind_hint = typehint_of_val_pat_and_patch(pat, dlogger, checker);
            let kind = typecheck_val_expr_and_patch(value, &kind_hint, dlogger, checker);
            typecheck_val_pat_and_patch(pat, &kind, dlogger, checker);
        }
        hir::BlockStatement::Set { place, value } => {
            typecheck_val_expr_and_patch(place, &KindValue::Type, dlogger, checker);
            typecheck_val_expr_and_patch(value, &KindValue::Type, dlogger, checker);
        }
        hir::BlockStatement::While { cond, body } => {
            typecheck_val_expr_and_patch(cond, &KindValue::Type, dlogger, checker);
            typecheck_val_expr_and_patch(body, &KindValue::Type, dlogger, checker);
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
            typecheck_val_expr_and_patch(body, &KindValue::Type, dlogger, checker);
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
            // get hint from the type pattern
            let kind_hint = typehint_of_type_pat(typat);
            // try calculating the kind of the type expression
            let kind = typecheck_type_expr_and_patch(value, &kind_hint, dlogger, checker);
            // bind the resolved kind to all the identifiers in type pattern
            typecheck_type_pat_expr_and_patch(typat, &kind, dlogger, checker);
        }
        hir::FileStatement::ValDef { pat, value } => {
            // get hint from the value pattern
            let kind_hint = typehint_of_val_pat_and_patch(pat, dlogger, checker);
            let kind = typecheck_val_expr_and_patch(value, &kind_hint, dlogger, checker);
            typecheck_val_pat_and_patch(pat, &kind, dlogger, checker);
        }
    }
}
