use lsp_types::Range;

use crate::ast;
use crate::dlogger::DiagnosticLogger;
use crate::hir::Augmented;
use crate::hir::{self, Environment};
use crate::types::{get_kind_of_member, get_kind_of_type, kind_is_val, KindValue, TypeValue};

pub fn evaluate_hir_kind(kind: &Augmented<hir::KindExpr>) -> KindValue {
    match &kind.val {
        hir::KindExpr::Error => KindValue::Unknown,
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
                paramkinds_out.push(evaluate_hir_kind(arg));
            }
            KindValue::Generic {
                paramkinds: paramkinds_out,
                returnkind: Box::new(evaluate_hir_kind(&returnkind)),
            }
        }
    }
}

pub fn kindhint_of_val_pat_and_patch(
    v: &mut Augmented<hir::ValPatExpr>,
    dlogger: &mut DiagnosticLogger,
    checker: &mut Environment,
) -> KindValue {
    match &mut v.val {
        hir::ValPatExpr::Error => KindValue::Unknown,
        hir::ValPatExpr::Ignore => KindValue::Unknown,
        hir::ValPatExpr::Identifier { .. } => KindValue::Unknown,
        hir::ValPatExpr::StructLiteral(_) => KindValue::Val,
        hir::ValPatExpr::New { .. } => KindValue::Val,
        hir::ValPatExpr::Typed { ty, .. } => {
            let kind = kindcheck_val_expr_and_patch(ty, &KindValue::Unknown, dlogger, checker);
            // the kind of what could be assigned to this variable
            get_kind_of_member(kind, ty.range, dlogger)
        }
        hir::ValPatExpr::Kinded { kind, .. } => evaluate_hir_kind(&kind),
    }
}

// helper function to make reporting expected kind errors simpler
fn expect_kind<T>(
    v: &mut Augmented<T>,
    expected: &KindValue,
    actual: KindValue,
    dlogger: &mut DiagnosticLogger,
) -> KindValue
where
    T: std::default::Default,
{
    if expected.supports_assign(&actual) {
        actual
    } else {
        dlogger.log_kind_mismatch(v.range, &expected.to_string(), &actual.to_string());
        v.val = T::default();
        KindValue::Unknown
    }
}

pub fn kindcheck_valpatexpr_and_patch(
    v: &mut Augmented<hir::ValPatExpr>,
    expected_kind: &KindValue,
    dlogger: &mut DiagnosticLogger,
    checker: &mut Environment,
) -> KindValue {
    match &mut v.val {
        hir::ValPatExpr::Error => KindValue::Unknown,
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
        hir::ValPatExpr::Identifier {
            id,
            modifier,
            original,
        } => {
            let id = *id;
            // if expected kind is unknown, we throw an error
            if expected_kind == &KindValue::Unknown {
                dlogger.log_cannot_infer_val_kind(v.range);
                v.val = hir::ValPatExpr::Error;
                KindValue::Unknown
            } else {
                match (modifier, kind_is_val(expected_kind)) {
                    (ast::IdentifierModifier::Mutable, Some(false)) => {
                        dlogger.log_mutable_type(v.range, &original);
                        v.val = hir::ValPatExpr::Error;
                    }
                    (ast::IdentifierModifier::Nominal, Some(true)) => {
                        dlogger.log_nominal_value(v.range, &original);
                        v.val = hir::ValPatExpr::Error;
                    }
                    _ => {}
                }
                // otherwise we assign the expected kind to the identifier
                checker.id_kind_table[id] = expected_kind.clone();
                expected_kind.clone()
            }
        }
        hir::ValPatExpr::StructLiteral(fields) => {
            for (_, expr) in fields {
                kindcheck_valpatexpr_and_patch(expr, &KindValue::Type, dlogger, checker);
            }
            expect_kind(v, expected_kind, KindValue::Type, dlogger)
        }
        hir::ValPatExpr::New { pat, ty } => {
            kindcheck_val_expr_and_patch(ty, &KindValue::Type, dlogger, checker);
            kindcheck_valpatexpr_and_patch(pat, &KindValue::Type, dlogger, checker);
            expect_kind(v, expected_kind, KindValue::Type, dlogger)
        }
        hir::ValPatExpr::Typed { pat, ty } => {
            // gets the expected kind of the type
            let expected_type_kind = get_kind_of_type(expected_kind.clone(), ty.range, dlogger);
            // get the actual kind of the type
            let actual_type_kind =
                kindcheck_val_expr_and_patch(ty, &expected_type_kind, dlogger, checker);
            // get the actual kind of the pattern
            let actual_pattern_kind = get_kind_of_member(actual_type_kind, ty.range, dlogger);
            kindcheck_valpatexpr_and_patch(pat, &actual_pattern_kind, dlogger, checker)
        }
        hir::ValPatExpr::Kinded { pat, kind } => {
            let kind = evaluate_hir_kind(kind);
            kindcheck_valpatexpr_and_patch(pat, &kind, dlogger, checker)
        }
    }
}

pub fn kindcheck_case_target_expr_and_patch(
    v: &mut Augmented<hir::CaseTargetExpr>,
    expected_kind: &KindValue,
    dlogger: &mut DiagnosticLogger,
    checker: &mut Environment,
) -> KindValue {
    match &mut v.val {
        hir::CaseTargetExpr::Error => KindValue::Unknown,
        hir::CaseTargetExpr::Bool(_) => expect_kind(v, expected_kind, KindValue::Val, dlogger),
        hir::CaseTargetExpr::Int(_) => expect_kind(v, expected_kind, KindValue::Val, dlogger),
        hir::CaseTargetExpr::PatExpr(pat) => {
            kindcheck_valpatexpr_and_patch(pat, expected_kind, dlogger, checker)
        }
    }
}

pub fn kindcheck_val_expr_and_patch(
    v: &mut Augmented<hir::ValExpr>,
    expected_kind: &KindValue,
    dlogger: &mut DiagnosticLogger,
    checker: &mut Environment,
) -> KindValue {
    match &mut v.val {
        hir::ValExpr::Error => KindValue::Unknown,
        hir::ValExpr::Identifier(id) => {
            let actual_kind = checker.id_kind_table[*id].clone();
            expect_kind(v, expected_kind, actual_kind, dlogger)
        }
        // depending on context can either be a type or a value
        hir::ValExpr::Bool { kind, .. } => match expected_kind {
            KindValue::Val => {
                *kind = Some(KindValue::Val);
                KindValue::Val
            }
            _ => {
                *kind = Some(KindValue::Bool);
                expect_kind(v, expected_kind, KindValue::Bool, dlogger)
            }
        },
        hir::ValExpr::Int { kind, .. } => match expected_kind {
            KindValue::Val => {
                *kind = Some(KindValue::Val);
                KindValue::Val
            }
            _ => {
                *kind = Some(KindValue::Int);
                expect_kind(v, expected_kind, KindValue::Int, dlogger)
            }
        },
        hir::ValExpr::Float { kind, .. } => match expected_kind {
            KindValue::Val => {
                *kind = Some(KindValue::Val);
                KindValue::Val
            }
            _ => {
                *kind = Some(KindValue::Float);
                expect_kind(v, expected_kind, KindValue::Float, dlogger)
            }
        },
        hir::ValExpr::String(_) => expect_kind(v, expected_kind, KindValue::Val, dlogger),
        hir::ValExpr::StructLiteral(fields) => {
            for (_, expr) in fields {
                kindcheck_val_expr_and_patch(expr, &KindValue::Val, dlogger, checker);
            }
            expect_kind(v, expected_kind, KindValue::Val, dlogger)
        }
        hir::ValExpr::New { ty, val } => {
            kindcheck_val_expr_and_patch(ty, &KindValue::Type, dlogger, checker);
            kindcheck_val_expr_and_patch(val, &KindValue::Val, dlogger, checker);
            expect_kind(v, expected_kind, KindValue::Val, dlogger)
        }
        hir::ValExpr::Ref(inner) => {
            let actual_kind =
                kindcheck_val_expr_and_patch(inner, &KindValue::Val, dlogger, checker);
            expect_kind(v, expected_kind, actual_kind, dlogger)
        }
        hir::ValExpr::Deref(inner) => {
            let actual_kind =
                kindcheck_val_expr_and_patch(inner, &KindValue::Val, dlogger, checker);
            expect_kind(v, expected_kind, actual_kind, dlogger)
        }
        hir::ValExpr::FnDef {
            params,
            returnty,
            body,
        } => {
            for param in params {
                kindcheck_valpatexpr_and_patch(param, &KindValue::Val, dlogger, checker);
            }
            if let Some(returnty) = returnty {
                kindcheck_val_expr_and_patch(returnty, &KindValue::Type, dlogger, checker);
            }
            kindcheck_val_expr_and_patch(body, &KindValue::Val, dlogger, checker);
            expect_kind(v, expected_kind, KindValue::Val, dlogger)
        }
        hir::ValExpr::CaseOf { expr, cases } => {
            kindcheck_val_expr_and_patch(expr, &KindValue::Val, dlogger, checker);
            for (target, body) in cases {
                kindcheck_case_target_expr_and_patch(target, &KindValue::Val, dlogger, checker);
                kindcheck_val_expr_and_patch(body, &KindValue::Val, dlogger, checker);
            }
            expect_kind(v, expected_kind, KindValue::Type, dlogger)
        }
        hir::ValExpr::If {
            cond,
            then_branch,
            else_branch,
        } => {
            kindcheck_val_expr_and_patch(cond, &KindValue::Val, dlogger, checker);
            kindcheck_val_expr_and_patch(then_branch, &KindValue::Val, dlogger, checker);
            if let Some(else_branch) = else_branch {
                kindcheck_val_expr_and_patch(else_branch, &KindValue::Val, dlogger, checker);
            }
            expect_kind(v, expected_kind, KindValue::Val, dlogger)
        }
        hir::ValExpr::Loop { label, body } => {
            checker.lb_kind_table[*label] = KindValue::Val;
            kindcheck_val_expr_and_patch(body, &KindValue::Val, dlogger, checker);
            expect_kind(v, expected_kind, KindValue::Val, dlogger)
        }
        hir::ValExpr::Ret { label, value } => {
            let expected_val_kind = checker.lb_kind_table[*label].clone();
            kindcheck_val_expr_and_patch(value, &expected_val_kind, dlogger, checker);
            expect_kind(v, expected_kind, KindValue::Never, dlogger)
        }
        hir::ValExpr::Block { statements, label } => {
            checker.lb_kind_table[*label] = KindValue::Val;
            for statement in statements {
                kindcheck_block_statement_and_patch(statement, dlogger, checker);
            }
            expect_kind(v, expected_kind, KindValue::Val, dlogger)
        }
        hir::ValExpr::ArrayLiteral(vals) => {
            for val in vals {
                kindcheck_val_expr_and_patch(val, &KindValue::Val, dlogger, checker);
            }
            expect_kind(v, expected_kind, KindValue::Val, dlogger)
        }
        hir::ValExpr::BinaryOp {
            op: _,
            left_operand,
            right_operand,
        } => {
            kindcheck_val_expr_and_patch(left_operand, &KindValue::Val, dlogger, checker);
            kindcheck_val_expr_and_patch(right_operand, &KindValue::Val, dlogger, checker);
            expect_kind(v, expected_kind, KindValue::Val, dlogger)
        }
        hir::ValExpr::ArrayAccess { root, index } => {
            kindcheck_val_expr_and_patch(root, &KindValue::Val, dlogger, checker);
            kindcheck_val_expr_and_patch(index, &KindValue::Val, dlogger, checker);
            expect_kind(v, expected_kind, KindValue::Val, dlogger)
        }
        hir::ValExpr::FieldAccess { root, .. } => {
            kindcheck_val_expr_and_patch(root, &KindValue::Val, dlogger, checker);
            expect_kind(v, expected_kind, KindValue::Val, dlogger)
        }
        hir::ValExpr::App { fun, args } => {
            kindcheck_val_expr_and_patch(fun, &KindValue::Val, dlogger, checker);
            for arg in args {
                kindcheck_val_expr_and_patch(arg, &KindValue::Val, dlogger, checker);
            }
            expect_kind(v, expected_kind, KindValue::Val, dlogger)
        }
        hir::ValExpr::FnTy { paramtys, returnty } => {
            for arg in paramtys {
                kindcheck_val_expr_and_patch(arg, &KindValue::Type, dlogger, checker);
            }
            kindcheck_val_expr_and_patch(returnty, &KindValue::Type, dlogger, checker);

            expect_kind(v, expected_kind, KindValue::Type, dlogger)
        }
        hir::ValExpr::Struct(fields) => {
            for (_, expr) in fields {
                kindcheck_val_expr_and_patch(expr, &KindValue::Type, dlogger, checker);
            }
            expect_kind(v, expected_kind, KindValue::Type, dlogger)
        }
        hir::ValExpr::Enum(fields) => {
            for (_, expr) in fields {
                kindcheck_val_expr_and_patch(expr, &KindValue::Type, dlogger, checker);
            }
            expect_kind(v, expected_kind, KindValue::Type, dlogger)
        }
        hir::ValExpr::Union(fields) => {
            for (_, expr) in fields {
                kindcheck_val_expr_and_patch(expr, &KindValue::Type, dlogger, checker);
            }
            expect_kind(v, expected_kind, KindValue::Type, dlogger)
        }
        hir::ValExpr::Concretization {
            generic,
            tyargs: provided_args,
        } => {
            let kind_of_generic =
                kindcheck_val_expr_and_patch(generic, &KindValue::Unknown, dlogger, checker);
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
                            kindcheck_val_expr_and_patch(tyarg, argkind, dlogger, checker);
                        }
                        *returnkind
                    }
                }
                _ => {
                    dlogger.log_cannot_be_concretized(v.range);
                    v.val = hir::ValExpr::Error;
                    KindValue::Unknown
                }
            }
        }
        hir::ValExpr::Generic {
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
                    kindcheck_valpatexpr_and_patch(param, expected_param_kind, dlogger, checker)
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
                kindcheck_val_expr_and_patch(body, &returnkind, dlogger, checker);
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
                v.val = hir::ValExpr::Error;
                KindValue::Unknown
            }
        }
        hir::ValExpr::Extern { ty, .. } => {
            kindcheck_val_expr_and_patch(ty, &KindValue::Type, dlogger, checker);
            expect_kind(v, expected_kind, KindValue::Val, dlogger)
        }
        hir::ValExpr::Hole => match expected_kind {
            KindValue::Unknown => {
                dlogger.log_could_not_infer_hole_kind(v.range);
                v.val = hir::ValExpr::Error;
                KindValue::Unknown
            },
            ek => ek.clone()
        },
        hir::ValExpr::Typed { val, ty } => {
            let expected_type_kind = get_kind_of_type(expected_kind.clone(), ty.range, dlogger);
            let actual_type_kind =
                kindcheck_val_expr_and_patch(ty, &expected_type_kind, dlogger, checker);
            let expected_val_kind_from_type = get_kind_of_member(actual_type_kind, ty.range, dlogger);
            let actual_val_kind = kindcheck_val_expr_and_patch(val, &expected_val_kind_from_type, dlogger, checker);
            expect_kind(v, expected_kind, actual_val_kind, dlogger)
        }
        hir::ValExpr::Kinded { val, kind } => {
            // compute the kind of the kind
            let kind = evaluate_hir_kind(kind);
            // check the value with the kind
            let actual_val_kind = kindcheck_val_expr_and_patch(val, &kind, dlogger, checker);
            expect_kind(v, expected_kind, actual_val_kind, dlogger)
        }
    }
}

pub fn kindcheck_block_statement_and_patch(
    v: &mut Augmented<hir::BlockStatement>,
    dlogger: &mut DiagnosticLogger,
    checker: &mut Environment,
) {
    match &mut v.val {
        hir::BlockStatement::Error => {}
        hir::BlockStatement::NoOp => {}
        hir::BlockStatement::Let { pat, value } => {
            // get hint from the value pattern
            let kind_hint = kindhint_of_val_pat_and_patch(pat, dlogger, checker);
            let kind = kindcheck_val_expr_and_patch(value, &kind_hint, dlogger, checker);
            kindcheck_valpatexpr_and_patch(pat, &kind, dlogger, checker);
        }
        hir::BlockStatement::Do(val) => {
            kindcheck_val_expr_and_patch(val, &KindValue::Unknown, dlogger, checker);
        }
    }
}

pub fn kindcheck_file_statement_and_patch(
    v: &mut Augmented<hir::FileStatement>,
    dlogger: &mut DiagnosticLogger,
    checker: &mut Environment,
) {
    match &mut v.val {
        hir::FileStatement::Error => {}
        hir::FileStatement::Let { pat, value } => {
            // get hint from the value pattern
            let kind_hint = kindhint_of_val_pat_and_patch(pat, dlogger, checker);
            let kind = kindcheck_val_expr_and_patch(value, &kind_hint, dlogger, checker);
            kindcheck_valpatexpr_and_patch(pat, &kind, dlogger, checker);
        }
    }
}
