use lsp_types::Range;

use crate::dlogger::DiagnosticLogger;
use crate::hir;
use crate::hir::Augmented;
use crate::typecheck::{evaluate_hir_kind, TypeChecker};
use crate::types::{KindValue, TypeValue};

pub fn kindhint_of_type_pat(v: &Augmented<hir::TypePatExpr>) -> KindValue {
    match &v.val {
        hir::TypePatExpr::Error => KindValue::Unknown,
        hir::TypePatExpr::Identifier(_) => KindValue::Unknown,
        hir::TypePatExpr::Typed { kind, .. } => evaluate_hir_kind(kind),
    }
}

pub fn kindhint_of_val_pat_and_patch(
    v: &mut Augmented<hir::ValPatExpr>,
    dlogger: &mut DiagnosticLogger,
    checker: &TypeChecker,
) -> KindValue {
    match &mut v.val {
        hir::ValPatExpr::Error => KindValue::Unknown,
        hir::ValPatExpr::Ignore => KindValue::Unknown,
        hir::ValPatExpr::Identifier { mutable, id } => KindValue::Unknown,
        hir::ValPatExpr::StructLiteral(_) => KindValue::Type,
        hir::ValPatExpr::New { .. } => KindValue::Type,
        hir::ValPatExpr::Typed { ty, .. } => {
            kindcheck_type_expr_and_patch(ty, &KindValue::Unknown, dlogger, checker)
        }
    }
}

// helper function to make reporting expected kind errors simpler
fn expect_kind<T>(
    v: &mut Augmented<T>,
    dlogger: &mut DiagnosticLogger,
    expected: &KindValue,
    actual: KindValue,
) -> KindValue
where
    T: std::default::Default,
{
    if expected.supports_assign(&actual) {
        actual
    } else {
        dlogger.log_kind_mismatch(v.range, expected, &actual);
        v.val = T::default();
        KindValue::Error
    }
}

pub fn kindcheck_type_pat_and_patch(
    v: &mut Augmented<hir::TypePatExpr>,
    expected_kind: &KindValue,
    dlogger: &mut DiagnosticLogger,
    checker: &TypeChecker,
) -> KindValue {
    match &mut v.val {
        hir::TypePatExpr::Error => KindValue::Unknown,
        hir::TypePatExpr::Identifier(id) => {
            // if expected kind is unknown, we throw an error
            if expected_kind == &KindValue::Unknown {
                dlogger.log_cannot_infer_pattern_kind(v.range);
                v.val = hir::TypePatExpr::Error;
                KindValue::Error
            } else {
                // otherwise we assign the expected kind to the identifier
                checker.type_kind_table[*id] = Some(expected_kind.clone());
                expected_kind.clone()
            }
        }
        hir::TypePatExpr::Typed { kind, id } => {
            let kind = evaluate_hir_kind(kind);
            checker.type_kind_table[*id] = Some(kind.clone());
            expect_kind(v, dlogger, expected_kind, kind)
        }
    }
}

// if we encounter an error, we log it
// NOTE: we assume that all identifiers have already been resolved.
pub fn kindcheck_type_expr_and_patch(
    v: &mut Augmented<hir::TypeExpr>,
    expected_kind: &KindValue,
    dlogger: &mut DiagnosticLogger,
    checker: &TypeChecker,
) -> KindValue {
    match &mut v.val {
        hir::TypeExpr::Error => KindValue::Unknown,
        hir::TypeExpr::Identifier(id) => expect_kind(
            v,
            dlogger,
            expected_kind,
            checker.type_kind_table[*id]
                .clone()
                .expect("kind not initialized yet"),
        ),
        hir::TypeExpr::BoolTy => expect_kind(v, dlogger, expected_kind, KindValue::Type),
        hir::TypeExpr::RefConstructorTy => expect_kind(
            v,
            dlogger,
            expected_kind,
            KindValue::Generic {
                paramkinds: vec![KindValue::Type],
                returnkind: Box::new(KindValue::Type),
            },
        ),
        hir::TypeExpr::ArrayConstructorTy => expect_kind(
            v,
            dlogger,
            expected_kind,
            KindValue::Generic {
                paramkinds: vec![KindValue::Type, KindValue::Int],
                returnkind: Box::new(KindValue::Type),
            },
        ),
        hir::TypeExpr::SliceConstructorTy => expect_kind(
            v,
            dlogger,
            expected_kind,
            KindValue::Generic {
                paramkinds: vec![KindValue::Type],
                returnkind: Box::new(KindValue::Type),
            },
        ),
        hir::TypeExpr::IntConstructorTy => expect_kind(
            v,
            dlogger,
            expected_kind,
            KindValue::Generic {
                paramkinds: vec![KindValue::Int],
                returnkind: Box::new(KindValue::Type),
            },
        ),
        hir::TypeExpr::UIntConstructorTy => expect_kind(
            v,
            dlogger,
            expected_kind,
            KindValue::Generic {
                paramkinds: vec![KindValue::Int],
                returnkind: Box::new(KindValue::Type),
            },
        ),
        hir::TypeExpr::FloatConstructorTy => expect_kind(
            v,
            dlogger,
            expected_kind,
            KindValue::Generic {
                paramkinds: vec![KindValue::Int],
                returnkind: Box::new(KindValue::Type),
            },
        ),

        hir::TypeExpr::Int(_) => expect_kind(v, dlogger, expected_kind, KindValue::Int),
        hir::TypeExpr::Bool(_) => expect_kind(v, dlogger, expected_kind, KindValue::Bool),
        hir::TypeExpr::Float(_) => expect_kind(v, dlogger, expected_kind, KindValue::Float),
        hir::TypeExpr::Fn { paramtys, returnty } => {
            for arg in paramtys {
                kindcheck_type_expr_and_patch(arg, &KindValue::Type, dlogger, checker);
            }
            kindcheck_type_expr_and_patch(returnty, &KindValue::Type, dlogger, checker);

            expect_kind(v, dlogger, expected_kind, KindValue::Type)
        }
        hir::TypeExpr::Struct(fields) => {
            for (_, expr) in fields {
                kindcheck_type_expr_and_patch(expr, &KindValue::Type, dlogger, checker);
            }
            expect_kind(v, dlogger, expected_kind, KindValue::Type)
        }
        hir::TypeExpr::Enum(fields) => {
            for (_, expr) in fields {
                kindcheck_type_expr_and_patch(expr, &KindValue::Type, dlogger, checker);
            }
            expect_kind(v, dlogger, expected_kind, KindValue::Type)
        }
        hir::TypeExpr::Union(fields) => {
            for (_, expr) in fields {
                kindcheck_type_expr_and_patch(expr, &KindValue::Type, dlogger, checker);
            }
            expect_kind(v, dlogger, expected_kind, KindValue::Type)
        }
        hir::TypeExpr::Concretization {
            genericty,
            tyargs: provided_args,
        } => {
            let kind_of_generic =
                kindcheck_type_expr_and_patch(genericty, &KindValue::Unknown, dlogger, checker);
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
                        KindValue::Error
                    } else {
                        for (ref argkind, ref mut tyarg) in
                            std::iter::zip(expected_kinds, provided_args)
                        {
                            kindcheck_type_expr_and_patch(tyarg, argkind, dlogger, checker);
                        }
                        *returnkind
                    }
                }
                _ => {
                    dlogger.log_cannot_be_concretized(v.range);
                    v.val = hir::TypeExpr::Error;
                    KindValue::Error
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

            let (expected_params_kind, expected_returnkind) = match expected_kind {
                KindValue::Unknown => (
                    std::iter::repeat(KindValue::Unknown)
                        .take(params.len())
                        .collect(),
                    KindValue::Unknown,
                ),
                KindValue::Generic {
                    paramkinds,
                    returnkind,
                } => (paramkinds, returnkind.clone()),
                _ => {
                    // the expected type is not compatible
                    dlogger.log_unexpected_generic(v.range, expected_kind);
                    v.val = hir::TypeExpr::Error;
                    return KindValue::Error;
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
                return KindValue::Error;
            }

            // check each type parameter
            let paramkinds = std::iter::zip(params.iter_mut(), expected_params_kind)
                .map(|(param, expected_param_kind)| {
                    kindcheck_type_pat_and_patch(param, expected_param_kind, dlogger, checker)
                })
                .collect();

            // ensure return kind matches
            let returnkind = Box::new(evaluate_hir_kind(&returnkind));
            if expected_returnkind.supports_assign(&returnkind) {
                // check the body
                kindcheck_type_expr_and_patch(body, &returnkind, dlogger, checker);
                KindValue::Generic {
                    paramkinds,
                    returnkind,
                }
            } else {
                dlogger.log_kind_mismatch(v.range, &expected_returnkind, &returnkind);
                v.val = hir::TypeExpr::Error;
                KindValue::Error
            }
        }
    }
}

pub fn kindcheck_val_expr_and_patch(
    v: &mut Augmented<hir::ValExpr>,
    expected_kind: &KindValue,
    dlogger: &mut DiagnosticLogger,
    checker: &TypeChecker,
) -> KindValue {
    match &mut v.val {
        hir::ValExpr::Error => KindValue::Unknown,
        hir::ValExpr::Identifier(id) => expect_kind(
            v,
            dlogger,
            expected_kind,
            checker.val_kind_table[*id]
                .clone()
                .expect("kind not initialized yet"),
        ),
        hir::ValExpr::Bool(_) => expect_kind(v, dlogger, expected_kind, KindValue::Type),
        hir::ValExpr::Int(_) => expect_kind(v, dlogger, expected_kind, KindValue::Type),
        hir::ValExpr::Float(_) => expect_kind(v, dlogger, expected_kind, KindValue::Type),
        hir::ValExpr::String(_) => expect_kind(v, dlogger, expected_kind, KindValue::Type),
        hir::ValExpr::StructLiteral(fields) => {
            for (_, expr) in fields {
                kindcheck_val_expr_and_patch(expr, &KindValue::Type, dlogger, checker);
            }
            expect_kind(v, dlogger, expected_kind, KindValue::Type)
        }
        hir::ValExpr::New { ty, val } => {
            kindcheck_type_expr_and_patch(ty, &KindValue::Type, dlogger, checker);
            kindcheck_val_expr_and_patch(val, &KindValue::Type, dlogger, checker);
            expect_kind(v, dlogger, expected_kind, KindValue::Type)
        }
        hir::ValExpr::Ref(inner) => expect_kind(
            v,
            dlogger,
            expected_kind,
            // can only take reference of a value of kind TYPE
            kindcheck_val_expr_and_patch(inner, &KindValue::Type, dlogger, checker),
        ),
        hir::ValExpr::Deref(inner) => expect_kind(
            v,
            dlogger,
            expected_kind,
            // can only dereference a reference (which has kind TYPE)
            kindcheck_val_expr_and_patch(
                inner,
                &KindValue::Ref(Box::new(KindValue::Type)),
                dlogger,
                checker,
            ),
        ),
        hir::ValExpr::Generic { params, body } => {
            let (expected_params_kind, expected_returnkind) = match expected_kind {
                KindValue::Unknown => (
                    std::iter::repeat(KindValue::Unknown)
                        .take(params.len())
                        .collect(),
                    KindValue::Unknown,
                ),
                KindValue::Generic {
                    paramkinds,
                    returnkind,
                } => (paramkinds, returnkind.clone()),
                _ => {
                    // the expected type is not compatible
                    dlogger.log_unexpected_generic(v.range, expected_kind);
                    v.val = hir::ValExpr::Error;
                    return KindValue::Error;
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
                return KindValue::Error;
            }

            // check each type parameter
            let paramkinds = std::iter::zip(params.iter_mut(), expected_params_kind)
                .map(|(param, expected_param_kind)| {
                    kindcheck_type_expr_and_patch(param, expected_param_kind, dlogger, checker)
                })
                .collect();

            // ensure return kind matches
            let returnkind = Box::new(evaluate_hir_kind(&expected_returnkind));
            if expected_returnkind.supports_assign(&returnkind) {
                // check the body
                kindcheck_val_expr_and_patch(body, &returnkind, dlogger, checker);
                KindValue::Generic {
                    paramkinds,
                    returnkind,
                }
            } else {
                dlogger.log_kind_mismatch(v.range, &expected_returnkind, &returnkind);
                v.val = hir::ValExpr::Error;
                KindValue::Error
            }
        }
        hir::ValExpr::FnDef {
            params,
            returnty,
            body,
        } => {
            for param in params {
                kindcheck_val_pat_and_patch(param, &KindValue::Type, dlogger, checker);
            }
            kindcheck_type_expr_and_patch(returnty, &KindValue::Type, dlogger, checker);
            kindcheck_val_expr_and_patch(body, &KindValue::Type, dlogger, checker);
            expect_kind(v, dlogger, expected_kind, KindValue::Type)
        }
        hir::ValExpr::IfThen {
            cond,
            then_branch,
            else_branch,
        } => {
            kindcheck_val_expr_and_patch(cond, &KindValue::Type, dlogger, checker);
            kindcheck_val_expr_and_patch(then_branch, &KindValue::Type, dlogger, checker);
            if let Some(else_branch) = else_branch {
                kindcheck_val_expr_and_patch(else_branch, &KindValue::Type, dlogger, checker);
            }
            expect_kind(v, dlogger, expected_kind, KindValue::Type)
        }
        hir::ValExpr::CaseOf {
            expr,
            first_case,
            rest_cases,
        } => {
            kindcheck_val_expr_and_patch(expr, &KindValue::Type, dlogger, checker);
            kindcheck_case_expr_and_patch(first_case, &KindValue::Type, dlogger, checker);
            for case in rest_cases {
                kindcheck_case_expr_and_patch(case, &KindValue::Type, dlogger, checker);
            }
            expect_kind(v, dlogger, expected_kind, KindValue::Type)
        }
        hir::ValExpr::Block {
            statements,
            last_expression,
        } => {
            for statement in statements {
                kindcheck_file_statement_and_patch(statement, dlogger, checker);
            }
            if let Some(expr) = last_expression {
                kindcheck_val_expr_and_patch(expr, &KindValue::Type, dlogger, checker);
            }
            expect_kind(v, dlogger, expected_kind, KindValue::Type)
        }
        hir::ValExpr::ArrayLiteral(vals) => {
            for val in vals {
                kindcheck_val_expr_and_patch(val, &KindValue::Type, dlogger, checker);
            }
            expect_kind(v, dlogger, expected_kind, KindValue::Type)
        }
        hir::ValExpr::BinaryOp {
            op,
            left_operand,
            right_operand,
        } => {
            kindcheck_val_expr_and_patch(left_operand, &KindValue::Type, dlogger, checker);
            kindcheck_val_expr_and_patch(right_operand, &KindValue::Type, dlogger, checker);
            expect_kind(v, dlogger, expected_kind, KindValue::Type)
        }
        hir::ValExpr::ArrayAccess { root, index } => {
            kindcheck_val_expr_and_patch(root, &KindValue::Type, dlogger, checker);
            kindcheck_val_expr_and_patch(index, &KindValue::Int, dlogger, checker);
            expect_kind(v, dlogger, expected_kind, KindValue::Type)
        }
        hir::ValExpr::FieldAccess { root, field } => {
            kindcheck_val_expr_and_patch(root, &KindValue::Type, dlogger, checker);
            expect_kind(v, dlogger, expected_kind, KindValue::Type)
        }
        hir::ValExpr::Concretization { generic, tyargs } => {
            let kind_of_generic =
                kindcheck_val_expr_and_patch(generic, &KindValue::Unknown, dlogger, checker);
            match kind_of_generic {
                KindValue::Unknown => {}
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
                        KindValue::Error
                    } else {
                        for (ref argkind, ref mut tyarg) in std::iter::zip(expected_kinds, tyargs) {
                            kindcheck_type_expr_and_patch(tyarg, argkind, dlogger, checker);
                        }
                        expect_kind(v, dlogger, expected_kind, returnkind)
                    }
                }
                _ => {
                    dlogger.log_cannot_be_concretized(v.range);
                    v.val = hir::ValExpr::Error;
                    KindValue::Error
                }
            }
        }
        hir::ValExpr::App { fun, args } => {
            kindcheck_val_expr_and_patch(fun, &KindValue::Type, dlogger, checker);
            for arg in args {
                kindcheck_val_expr_and_patch(arg, &KindValue::Type, dlogger, checker);
            }
            expect_kind(v, dlogger, expected_kind, KindValue::Type)
        }
    }
}

pub fn kindcheck_file_statement_and_patch(
    v: &mut Augmented<hir::FileStatement>,
    dlogger: &mut DiagnosticLogger,
    checker: &TypeChecker,
) {
    match &mut v.val {
        hir::FileStatement::Error => {}
        hir::FileStatement::TypeDef { value, typat } => {
            // get hint from the type pattern
            let kind_hint = kindhint_of_type_pat(typat);
            // try calculating the kind of the type expression
            let kind = kindcheck_type_expr_and_patch(value, &kind_hint, dlogger, checker);
            // bind the resolved kind to all the identifiers in type pattern
            kindcheck_type_pat_and_patch(typat, &kind, dlogger, checker);
        }
        hir::FileStatement::ValDef { pat, value } => {
            // get hint from the value pattern
            let kind_hint = kindhint_of_val_pat_and_patch(pat, dlogger, checker);
            let kind = kindcheck_val_expr_and_patch(value, &kind_hint, dlogger, checker);
            kindcheck_val_pat_and_patch(pat, &kind, dlogger, checker);
        }
    }
}
