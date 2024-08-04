use std::collections::{HashMap, HashSet};

use indexmap::IndexMap;
use lsp_types::Range;

use crate::ast::{self};
use crate::builtin::Builtin;
use crate::dlogger::DiagnosticLogger;
use crate::environment::Environment;
use crate::hir::*;

fn translate_augstructitemexpr<U>(
    mut lower: impl FnMut(
        ast::Augmented<ast::Expr>,
        &mut VariableResolutionEnvironment,
        &mut Environment,
        &mut DiagnosticLogger,
    ) -> Augmented<U>,
    mut replace_eponymous: impl FnMut(
        ast::Identifier,
        &mut VariableResolutionEnvironment,
        &mut Environment,
        &mut DiagnosticLogger,
    ) -> U,
    env: &mut VariableResolutionEnvironment,
    env2: &mut Environment,
    dlogger: &mut DiagnosticLogger,
    items: Vec<ast::Augmented<ast::StructItemExpr>>,
) -> Vec<(Augmented<String>, Augmented<U>)> {
    let mut identifier_ranges: IndexMap<String, Range> = IndexMap::new();
    let mut out_items: Vec<(Augmented<String>, Augmented<U>)> = Vec::new();
    for ast::Augmented { range, val, .. } in items {
        match val {
            ast::StructItemExpr::Error => {}
            ast::StructItemExpr::Eponymous(ast::Identifier {
                identifier: Some(identifier),
                range: identifier_range,
            }) => {
                if let Some(preexisting_range) = identifier_ranges.get(&identifier) {
                    dlogger.log_duplicate_field_name(range, *preexisting_range, &identifier);
                } else {
                    out_items.push((
                        Augmented {
                            range: identifier_range,
                            val: identifier.clone(),
                        },
                        Augmented {
                            range,
                            val: replace_eponymous(
                                ast::Identifier {
                                    identifier: Some(identifier),
                                    range: identifier_range,
                                },
                                env,
                                env2,
                                dlogger,
                            ),
                        },
                    ));
                }
            }
            ast::StructItemExpr::Eponymous(_) => {}
            ast::StructItemExpr::Identified {
                identifier:
                    ast::Identifier {
                        range: identifier_range,
                        identifier: Some(identifier),
                    },
                expr,
            } => {
                if let Some(preexisting_range) = identifier_ranges.get(&identifier) {
                    dlogger.log_duplicate_field_name(range, *preexisting_range, &identifier);
                } else {
                    identifier_ranges.insert(identifier.clone(), identifier_range);
                    out_items.push((
                        Augmented {
                            range: identifier_range,
                            val: identifier,
                        },
                        lower(*expr, env, env2, dlogger),
                    ));
                }
            }
            ast::StructItemExpr::Identified { .. } => {}
        }
    }
    out_items
}

fn translate_augpatexpr(
    ast::Augmented { range, val, .. }: ast::Augmented<ast::Expr>,
    env: &mut VariableResolutionEnvironment,
    env2: &mut Environment,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<PatExpr> {
    match val {
        ast::Expr::Error => Augmented {
            range,
            val: PatExpr::Error,
        },
        ast::Expr::Ignore => Augmented {
            range,
            val: PatExpr::Ignore,
        },
        ast::Expr::Identifier {
            identifier,
            modifier,
        } => Augmented {
            range,
            val: match env.introduce_identifier(identifier, modifier, env2) {
                Some((id, _)) => PatExpr::Identifier(id),
                None => PatExpr::Error,
            },
        },
        ast::Expr::StructLiteral(items) => Augmented {
            range,
            val: PatExpr::StructLiteral(translate_augstructitemexpr(
                |x, env, env2, dlogger| translate_augpatexpr(x, env, env2, dlogger),
                |id, env, env2, _| match env.introduce_identifier(
                    id,
                    ast::IdentifierModifier::None,
                    env2,
                ) {
                    Some((id, _)) => PatExpr::Identifier(id),
                    None => PatExpr::Error,
                },
                env,
                env2,
                dlogger,
                items,
            )),
        },
        ast::Expr::BinaryOp {
            op,
            left_operand,
            right_operand,
        } => match op {
            ast::ValBinaryOpKind::Ascribe => {
                let pat = Box::new(translate_augpatexpr(*left_operand, env, env2, dlogger));
                let ty = Box::new(translate_augvalexpr(*right_operand, env, env2, dlogger));
                Augmented {
                    range,
                    val: PatExpr::Typed { pat, ty },
                }
            }
            ast::ValBinaryOpKind::RevAscribe => {
                let ty = Box::new(translate_augvalexpr(*left_operand, env, env2, dlogger));
                let pat = Box::new(translate_augpatexpr(*right_operand, env, env2, dlogger));
                Augmented {
                    range,
                    val: PatExpr::Typed { pat, ty },
                }
            }
            op => {
                dlogger.log_unexpected_pattern_binop(range, op.as_ref());
                Augmented {
                    range,
                    val: PatExpr::Error,
                }
            }
        },
        ast::Expr::New { ty, val } => Augmented {
            range,
            val: PatExpr::New {
                ty: Box::new(translate_augvalexpr(*ty, env, env2, dlogger)),
                pat: Box::new(translate_augpatexpr(*val, env, env2, dlogger)),
            },
        },
        val => {
            dlogger.log_unexpected_pattern(range, val.as_ref());
            Augmented {
                range,
                val: PatExpr::Error,
            }
        }
    }
}

fn translate_caseexpr(
    c: ast::CaseExpr,
    env: &mut VariableResolutionEnvironment,
    env2: &mut Environment,
    dlogger: &mut DiagnosticLogger,
) -> (Augmented<PatExpr>, Augmented<ValExpr>) {
    (
        translate_augpatexpr(*c.target, env, env2, dlogger),
        translate_augvalexpr(*c.body, env, env2, dlogger),
    )
}

fn translate_augplaceexpr(
    ast::Augmented { range, val, .. }: ast::Augmented<ast::Expr>,
    env: &mut VariableResolutionEnvironment,
    env2: &mut Environment,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<PlaceExpr> {
    match val {
        ast::Expr::Identifier {
            modifier,
            identifier,
        } => Augmented {
            range,
            val: match modifier {
                ast::IdentifierModifier::None => env.lookup_identifier(identifier, dlogger),
                ast::IdentifierModifier::Mutable => {
                    dlogger.log_mutable_identifier_in_expression(range);
                    PlaceExpr::Error
                }
                ast::IdentifierModifier::Nominal => {
                    dlogger.log_nominal_identifier_in_expression(range);
                    PlaceExpr::Error
                }
            },
        },
        ast::Expr::FieldAccess { root, field } => Augmented {
            range,
            val: PlaceExpr::FieldAccess {
                root: Box::new(translate_augplaceexpr(*root, env, env2, dlogger)),
                field,
            },
        },
        _ => {
            dlogger.log_unexpected_place(range, val.as_ref());
            Augmented {
                range,
                val: PlaceExpr::Error,
            }
        }
    }
}

fn translate_augvalexpr(
    ast::Augmented { range, val }: ast::Augmented<ast::Expr>,
    env: &mut VariableResolutionEnvironment,
    env2: &mut Environment,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<ValExpr> {
    match val {
        ast::Expr::Error => Augmented {
            range,
            val: ValExpr::Error,
        },
        ast::Expr::Ignore => Augmented {
            range,
            val: ValExpr::Hole,
        },
        // ast::Expr::Kinded { pat, kind }
        ast::Expr::Annotated { value, .. } => translate_augvalexpr(*value, env, env2, dlogger),
        ast::Expr::Int(i) => Augmented {
            range,
            val: ValExpr::Int { value: i },
        },
        ast::Expr::Float(f) => Augmented {
            range,
            val: ValExpr::Float { value: f },
        },
        ast::Expr::String { value, .. } => Augmented {
            range,
            val: ValExpr::String(value),
        },
        ast::Expr::Builtin { builtin, level } => Augmented {
            range,
            val: ValExpr::Builtin { builtin, level },
        },
        ast::Expr::Ref(v) => Augmented {
            range,
            val: ValExpr::Use(
                Box::new(translate_augplaceexpr(*v, env, env2, dlogger)),
                UseKind::Borrow,
            ),
        },
        ast::Expr::Mutref(v) => Augmented {
            range,
            val: ValExpr::Use(
                Box::new(translate_augplaceexpr(*v, env, env2, dlogger)),
                UseKind::MutBorrow,
            ),
        },
        ast::Expr::Copy(v) => Augmented {
            range,
            val: ValExpr::App {
                fun: Box::new(Augmented {
                    range,
                    val: ValExpr::App {
                        fun: Box::new(Augmented {
                            range,
                            val: ValExpr::Builtin {
                                builtin: Builtin::Copy,
                                level: 0,
                            },
                        }),
                        args: vec![Augmented {
                            range,
                            val: ValExpr::Hole,
                        }],
                    },
                }),
                args: vec![Augmented {
                    range,
                    val: ValExpr::Use(
                        Box::new(translate_augplaceexpr(*v, env, env2, dlogger)),
                        UseKind::Borrow,
                    ),
                }],
            },
        },
        ast::Expr::Reborrow(v) => Augmented {
            range,
            val: ValExpr::App {
                fun: Box::new(Augmented {
                    range,
                    val: ValExpr::App {
                        fun: Box::new(Augmented {
                            range,
                            val: ValExpr::Builtin {
                                builtin: Builtin::Reborrow,
                                level: 0,
                            },
                        }),
                        args: vec![Augmented {
                            range,
                            val: ValExpr::Hole,
                        }],
                    },
                }),
                args: vec![Augmented {
                    range,
                    val: ValExpr::Use(
                        Box::new(translate_augplaceexpr(*v, env, env2, dlogger)),
                        UseKind::MutBorrow,
                    ),
                }],
            },
        },
        ast::Expr::Deref(v) => Augmented {
            range,
            val: ValExpr::Use(
                Box::new(translate_augplaceexpr(
                    ast::Augmented {
                        range,
                        val: ast::Expr::Deref(v),
                    },
                    env,
                    env2,
                    dlogger,
                )),
                UseKind::Move,
            ),
        },
        ast::Expr::StructLiteral(items) => Augmented {
            range,
            val: ValExpr::StructLiteral(translate_augstructitemexpr(
                |x, env, env2, dlogger| translate_augvalexpr(x, env, env2, dlogger),
                |id, env, _, dlogger| {
                    ValExpr::Use(
                        Box::new(Augmented {
                            range,
                            val: env.lookup_identifier(id, dlogger),
                        }),
                        UseKind::Move,
                    )
                },
                env,
                env2,
                dlogger,
                items,
            )),
        },
        ast::Expr::BinaryOp {
            op,
            left_operand,
            right_operand,
        } => {
            let val = match op {
                ast::ValBinaryOpKind::Pipe => ValExpr::App {
                    fun: Box::new(translate_augvalexpr(*right_operand, env, env2, dlogger)),
                    args: vec![translate_augvalexpr(*left_operand, env, env2, dlogger)],
                },
                ast::ValBinaryOpKind::Assign => ValExpr::App {
                    fun: Box::new(Augmented {
                        range,
                        val: ValExpr::App {
                            fun: Box::new(Augmented {
                                range,
                                val: ValExpr::Builtin {
                                    builtin: Builtin::Assign,
                                    level: 0,
                                },
                            }),
                            args: vec![Augmented {
                                range,
                                val: ValExpr::Hole,
                            }],
                        },
                    }),
                    args: vec![
                        translate_augvalexpr(*left_operand, env, env2, dlogger),
                        translate_augvalexpr(*right_operand, env, env2, dlogger),
                    ],
                },
                ast::ValBinaryOpKind::And => ValExpr::And {
                    left: Box::new(translate_augvalexpr(*left_operand, env, env2, dlogger)),
                    right: Box::new(translate_augvalexpr(*right_operand, env, env2, dlogger)),
                },
                ast::ValBinaryOpKind::Or => ValExpr::Or {
                    left: Box::new(translate_augvalexpr(*left_operand, env, env2, dlogger)),
                    right: Box::new(translate_augvalexpr(*right_operand, env, env2, dlogger)),
                },
                ast::ValBinaryOpKind::Range | ast::ValBinaryOpKind::RangeInclusive => {
                    dlogger.log_range_in_expression(range);
                    ValExpr::Error
                }
                ast::ValBinaryOpKind::AssignAdd
                | ast::ValBinaryOpKind::AssignSub
                | ast::ValBinaryOpKind::AssignMul
                | ast::ValBinaryOpKind::AssignDiv
                | ast::ValBinaryOpKind::AssignRem => {
                    let (builtin, field) = match op {
                        ast::ValBinaryOpKind::AssignAdd => (Builtin::AddAssignTrait, "add_assign"),
                        ast::ValBinaryOpKind::AssignSub => (Builtin::SubAssignTrait, "sub_assign"),
                        ast::ValBinaryOpKind::AssignMul => (Builtin::MulAssignTrait, "mul_assign"),
                        ast::ValBinaryOpKind::AssignDiv => (Builtin::DivAssignTrait, "div_assign"),
                        ast::ValBinaryOpKind::AssignRem => (Builtin::RemAssignTrait, "rem_assign"),
                        _ => unreachable!(),
                    };

                    let place = Augmented {
                        range,
                        val: ValExpr::Use(
                            Box::new(translate_augplaceexpr(*left_operand, env, env2, dlogger)),
                            UseKind::Move,
                        ),
                    };
                    let value = translate_augvalexpr(*right_operand, env, env2, dlogger);
                    ValExpr::App {
                        fun: Box::new(Augmented {
                            range,
                            val: ValExpr::FieldAccess {
                                root: Box::new(Augmented {
                                    range,
                                    val: ValExpr::Typed {
                                        value: Box::new(Augmented {
                                            range,
                                            val: ValExpr::Hole,
                                        }),
                                        ty: Box::new(Augmented {
                                            range,
                                            val: ValExpr::App {
                                                fun: Box::new(Augmented {
                                                    range,
                                                    val: ValExpr::Builtin { builtin, level: 0 },
                                                }),
                                                args: vec![Augmented {
                                                    range,
                                                    val: ValExpr::Hole,
                                                }],
                                            },
                                        }),
                                    },
                                }),
                                field: field.to_string(),
                            },
                        }),
                        args: vec![place, value],
                    }
                }
                ast::ValBinaryOpKind::Add
                | ast::ValBinaryOpKind::Sub
                | ast::ValBinaryOpKind::Mul
                | ast::ValBinaryOpKind::Div
                | ast::ValBinaryOpKind::Rem
                | ast::ValBinaryOpKind::Equal
                | ast::ValBinaryOpKind::Less
                | ast::ValBinaryOpKind::LessEqual
                | ast::ValBinaryOpKind::Greater
                | ast::ValBinaryOpKind::GreaterEqual => {
                    let (builtin, field) = match op {
                        ast::ValBinaryOpKind::Add => (Builtin::AddTrait, "add"),
                        ast::ValBinaryOpKind::Sub => (Builtin::SubTrait, "sub"),
                        ast::ValBinaryOpKind::Mul => (Builtin::MulTrait, "mul"),
                        ast::ValBinaryOpKind::Div => (Builtin::DivTrait, "div"),
                        ast::ValBinaryOpKind::Rem => (Builtin::RemTrait, "rem"),
                        ast::ValBinaryOpKind::Equal => (Builtin::EqTrait, "eq"),
                        ast::ValBinaryOpKind::Less => (Builtin::LtTrait, "lt"),
                        ast::ValBinaryOpKind::LessEqual => (Builtin::LteTrait, "lte"),
                        ast::ValBinaryOpKind::Greater => (Builtin::GtTrait, "gt"),
                        ast::ValBinaryOpKind::GreaterEqual => (Builtin::GteTrait, "gte"),
                        _ => unreachable!(),
                    };

                    let left = translate_augvalexpr(*left_operand, env, env2, dlogger);
                    let right = translate_augvalexpr(*right_operand, env, env2, dlogger);

                    ValExpr::App {
                        fun: Box::new(Augmented {
                            range,
                            val: ValExpr::FieldAccess {
                                root: Box::new(Augmented {
                                    range,
                                    val: ValExpr::Typed {
                                        value: Box::new(Augmented {
                                            range,
                                            val: ValExpr::Hole,
                                        }),
                                        ty: Box::new(Augmented {
                                            range,
                                            val: ValExpr::App {
                                                fun: Box::new(Augmented {
                                                    range,
                                                    val: ValExpr::Builtin { builtin, level: 0 },
                                                }),
                                                args: vec![Augmented {
                                                    range,
                                                    val: ValExpr::Hole,
                                                }],
                                            },
                                        }),
                                    },
                                }),
                                field: field.to_string(),
                            },
                        }),
                        args: vec![left, right],
                    }
                }
                ast::ValBinaryOpKind::Ascribe => {
                    let value = Box::new(translate_augvalexpr(*left_operand, env, env2, dlogger));
                    let ty = Box::new(translate_augvalexpr(*right_operand, env, env2, dlogger));
                    ValExpr::Typed { value, ty }
                }
                ast::ValBinaryOpKind::RevAscribe => {
                    let ty = Box::new(translate_augvalexpr(*left_operand, env, env2, dlogger));
                    let value = Box::new(translate_augvalexpr(*right_operand, env, env2, dlogger));
                    ValExpr::Typed { value, ty }
                }
                ast::ValBinaryOpKind::Lambda => {
                    // introduce new type and val scope
                    env.push_fn_scope();

                    // insert params into scope
                    let mut params: Vec<Augmented<PatExpr>> = match left_operand.val {
                        ast::Expr::GroupOrParams { items, .. } => items
                            .into_iter()
                            .map(|x| translate_augpatexpr(x, env, env2, dlogger))
                            .collect(),
                        _ => {
                            dlogger.log_expected_args(range, left_operand.val.as_ref());
                            vec![]
                        }
                    };

                    let mut body =
                        Box::new(translate_augvalexpr(*right_operand, env, env2, dlogger));

                    // end type and val scope
                    env.pop_fn_scope();

                    let mut find_env = FindCapturedVarEnvironment::new();

                    // find captured variables and their use kind
                    for param in params.iter() {
                        find_captured_patexpr(param, &mut find_env);
                    }
                    find_captured_valexpr(&body, &mut find_env);

                    // allocate new ids for captured variables
                    let rewrite_env = RewriteCapturedVarEnvironment::new(find_env, env, env2);

                    // rewrite to use captured variables
                    for param in params.iter_mut() {
                        rewrite_captured_patexpr(param, &rewrite_env);
                    }
                    rewrite_captured_valexpr(&mut body, &rewrite_env);

                    ValExpr::Lam {
                        params,
                        body,
                        captures: rewrite_env.to_captured(),
                    }
                }
                ast::ValBinaryOpKind::PiType => {
                    // introduce new type and val scope
                    env.push_fn_scope();

                    // insert params into scope
                    let mut params: Vec<Augmented<PatExpr>> = match left_operand.val {
                        ast::Expr::GroupOrParams { items, .. } => items
                            .into_iter()
                            .map(|x| translate_augpatexpr(x, env, env2, dlogger))
                            .collect(),
                        _ => {
                            dlogger.log_expected_args(range, left_operand.val.as_ref());
                            vec![]
                        }
                    };

                    let mut body =
                        Box::new(translate_augvalexpr(*right_operand, env, env2, dlogger));

                    // end type and val scope
                    env.pop_fn_scope();

                    let mut find_env = FindCapturedVarEnvironment::new();

                    // find captured variables and their use kind
                    for param in params.iter() {
                        find_captured_patexpr(param, &mut find_env);
                    }
                    find_captured_valexpr(&body, &mut find_env);

                    // allocate new ids for captured variables
                    let rewrite_env = RewriteCapturedVarEnvironment::new(find_env, env, env2);

                    // rewrite to use captured variables
                    for param in params.iter_mut() {
                        rewrite_captured_patexpr(param, &rewrite_env);
                    }
                    rewrite_captured_valexpr(&mut body, &rewrite_env);

                    ValExpr::PiTy {
                        params,
                        dep_ty: body,
                        captures: rewrite_env.to_captured(),
                    }
                }
            };

            Augmented { range, val }
        }
        ast::Expr::CaseOf { expr, cases } => {
            let expr = Box::new(translate_augvalexpr(*expr, env, env2, dlogger));
            let cases = cases
                .into_iter()
                .map(|x| translate_caseexpr(x.val, env, env2, dlogger))
                .collect();
            Augmented {
                range,
                val: ValExpr::CaseOf { expr, cases },
            }
        }
        ast::Expr::Ret { label, value } => {
            let label = match env.lookup_label(label, dlogger) {
                Some(i) => i,
                None => {
                    return Augmented {
                        range,
                        val: ValExpr::Error,
                    }
                }
            };

            Augmented {
                range,
                val: ValExpr::Ret {
                    value: Box::new(translate_augvalexpr(*value, env, env2, dlogger)),
                    label,
                },
            }
        }
        ast::Expr::Loop { body } => {
            let body = Box::new(translate_augvalexpr(*body, env, env2, dlogger));
            Augmented {
                range,
                val: ValExpr::Loop { body },
            }
        }
        ast::Expr::Block {
            statements,
            trailing_semicolon,
        } => {
            // introduce new scope
            env.push_block_scope();
            let mut statements: Vec<Augmented<BlockStatement>> = statements
                .into_iter()
                .map(|x| translate_augblockstatement(x, env, env2, dlogger))
                .collect();
            // end scope
            env.pop_block_scope();

            // get last expr
            let last_expr = match statements.pop() {
                Some(Augmented {
                    val: BlockStatement::Do(v),
                    ..
                }) if trailing_semicolon => v,
                Some(Augmented { range, val }) => {
                    statements.push(Augmented { range, val });
                    Box::new(Augmented {
                        range: Range::default(),
                        val: ValExpr::StructLiteral(vec![]),
                    })
                }
                None => Box::new(Augmented {
                    range: Range::default(),
                    val: ValExpr::StructLiteral(vec![]),
                }),
            };

            Augmented {
                range,
                val: ValExpr::Block {
                    statements,
                    last_expr,
                },
            }
        }
        ast::Expr::Labeled { label, value } => Augmented {
            range,
            val: match env.introduce_label(label, env2) {
                Some((i, _)) => {
                    let value = Box::new(translate_augvalexpr(*value, env, env2, dlogger));
                    env.unintroduce_label();
                    ValExpr::Label { label: i, value }
                }
                None => ValExpr::Error,
            },
        },
        ast::Expr::GroupOrParams {
            mut items,
            trailing_comma,
        } => match items.len() {
            1 => {
                if trailing_comma {
                    dlogger.log_trailing_comma_in_group(range);
                    Augmented {
                        range,
                        val: ValExpr::Error,
                    }
                } else {
                    translate_augvalexpr(items.remove(0), env, env2, dlogger)
                }
            }
            n => {
                dlogger.log_wrong_items_in_group(range, n);
                Augmented {
                    range,
                    val: ValExpr::Error,
                }
            }
        },
        ast::Expr::Array(items) => Augmented {
            range,
            val: ValExpr::ArrayLiteral(
                items
                    .into_iter()
                    .map(|x| translate_augvalexpr(x, env, env2, dlogger))
                    .collect(),
            ),
        },
        ast::Expr::Identifier {
            modifier,
            identifier,
        } => Augmented {
            range,
            val: ValExpr::Use(
                Box::new(translate_augplaceexpr(
                    ast::Augmented {
                        range,
                        val: ast::Expr::Identifier {
                            modifier,
                            identifier,
                        },
                    },
                    env,
                    env2,
                    dlogger,
                )),
                UseKind::Move,
            ),
        },
        ast::Expr::App { root, args } => Augmented {
            range,
            val: ValExpr::App {
                fun: Box::new(translate_augvalexpr(*root, env, env2, dlogger)),
                args: args
                    .into_iter()
                    .map(|x| translate_augvalexpr(x, env, env2, dlogger))
                    .collect(),
            },
        },
        ast::Expr::ArrayAccess { root, index } => Augmented {
            range,
            val: ValExpr::App {
                fun: Box::new(Augmented {
                    range,
                    val: ValExpr::FieldAccess {
                        root: Box::new(Augmented {
                            range,
                            val: ValExpr::Typed {
                                value: Box::new(Augmented {
                                    range,
                                    val: ValExpr::Hole,
                                }),
                                ty: Box::new(Augmented {
                                    range,
                                    val: ValExpr::App {
                                        fun: Box::new(Augmented {
                                            range,
                                            val: ValExpr::Builtin {
                                                builtin: Builtin::IndexTrait,
                                                level: 0,
                                            },
                                        }),
                                        args: vec![Augmented {
                                            range,
                                            val: ValExpr::Hole,
                                        }],
                                    },
                                }),
                            },
                        }),
                        field: "get".to_string(),
                    },
                }),
                args: vec![
                    translate_augvalexpr(*root, env, env2, dlogger),
                    translate_augvalexpr(*index, env, env2, dlogger),
                ],
            },
        },
        ast::Expr::FieldAccess { root, field } => Augmented {
            range,
            val: ValExpr::FieldAccess {
                root: Box::new(translate_augvalexpr(*root, env, env2, dlogger)),
                field,
            },
        },
        ast::Expr::New { ty, val } => {
            let ty = Box::new(translate_augvalexpr(*ty, env, env2, dlogger));
            let val = Box::new(translate_augvalexpr(*val, env, env2, dlogger));
            Augmented {
                range,
                val: ValExpr::New { ty, val },
            }
        }
        ast::Expr::StructTy(items) => Augmented {
            range,
            val: ValExpr::Struct(translate_augstructitemexpr(
                translate_augvalexpr,
                |id, env, _, dlogger| {
                    ValExpr::Use(
                        Box::new(Augmented {
                            range,
                            val: env.lookup_identifier(id, dlogger),
                        }),
                        UseKind::Move,
                    )
                },
                env,
                env2,
                dlogger,
                items,
            )),
        },
        ast::Expr::EnumTy(items) => Augmented {
            range,
            val: ValExpr::Enum(translate_augstructitemexpr(
                translate_augvalexpr,
                |id, env, _, dlogger| {
                    ValExpr::Use(
                        Box::new(Augmented {
                            range,
                            val: env.lookup_identifier(id, dlogger),
                        }),
                        UseKind::Move,
                    )
                },
                env,
                env2,
                dlogger,
                items,
            )),
        },
        ast::Expr::UnionTy(items) => Augmented {
            range,
            val: ValExpr::Union(translate_augstructitemexpr(
                translate_augvalexpr,
                |id, env, _, dlogger| {
                    ValExpr::Use(
                        Box::new(Augmented {
                            range,
                            val: env.lookup_identifier(id, dlogger),
                        }),
                        UseKind::Move,
                    )
                },
                env,
                env2,
                dlogger,
                items,
            )),
        },
        ast::Expr::Extern { name, ty } => Augmented {
            range,
            val: ValExpr::Extern {
                name: name.clone(),
                ty: Box::new(translate_augvalexpr(*ty, env, env2, dlogger)),
            },
        },
    }
}

struct FindCapturedVarEnvironment {
    // variables that we need to capture, and their use kind
    captured: HashMap<usize, UseKind>,
    // contains variables that were bound inside the body, and so don't need to be captured
    // note: we don't need to worry about removing variables from this set,
    // because the previous variable resolution pass will have already caught any unresolved variables
    bound: HashSet<usize>,
}

impl FindCapturedVarEnvironment {
    fn new() -> Self {
        FindCapturedVarEnvironment {
            captured: HashMap::new(),
            bound: HashSet::new(),
        }
    }

    fn bind_var(&mut self, id: usize) {
        self.bound.insert(id);
    }

    fn capture_var_if_needed(&mut self, id: usize, new_use_kind: UseKind) {
        if !self.bound.contains(&id) {
            let current_use_level = self.captured.get(&id).cloned();
            let new_use_level = match current_use_level {
                Some(old) => std::cmp::max(old, new_use_kind),
                None => new_use_kind,
            };
            self.captured.insert(id, new_use_level);
        }
    }
}

fn find_captured_patexpr(pat: &Augmented<PatExpr>, env: &mut FindCapturedVarEnvironment) {
    match &pat.val {
        PatExpr::Identifier(id) => env.bind_var(*id),
        PatExpr::Ignore => {}
        PatExpr::Typed { pat, ty } => {
            find_captured_patexpr(pat, env);
            find_captured_valexpr(ty, env);
        }
        PatExpr::Error => {}
        PatExpr::StructLiteral(entries) => {
            for (_, pat) in entries {
                find_captured_patexpr(pat, env);
            }
        }
        PatExpr::New { pat, ty } => {
            find_captured_patexpr(pat, env);
            find_captured_valexpr(ty, env);
        }
        PatExpr::Literal(val) => {
            find_captured_valexpr(val, env);
        }
    }
}

fn find_captured_valexpr(val: &Augmented<ValExpr>, env: &mut FindCapturedVarEnvironment) {
    match val.val {
        ValExpr::Use(ref place, ref role) => {
            find_captured_placeexpr(place, env, role.clone());
        }
        ValExpr::Block { ref statements, .. } => {
            for statement in statements {
                match statement.val {
                    BlockStatement::Let { ref pat, .. } => {
                        find_captured_patexpr(pat, env);
                    }
                    BlockStatement::Do(ref val) => {
                        find_captured_valexpr(val, env);
                    }
                    _ => {}
                }
            }
        }
        ValExpr::Lam { ref captures, .. } => {
            for (capture_pat, capture_val) in captures {
                find_captured_patexpr(capture_pat, env);
                find_captured_valexpr(capture_val, env);
            }
        }
        ValExpr::PiTy { ref captures, .. } => {
            for (capture_pat, capture_val) in captures {
                find_captured_patexpr(capture_pat, env);
                find_captured_valexpr(capture_val, env);
            }
        }
        ValExpr::CaseOf {
            ref expr,
            ref cases,
        } => {
            find_captured_valexpr(expr, env);
            for (pat, val) in cases {
                find_captured_patexpr(pat, env);
                find_captured_valexpr(val, env);
            }
        }
        ValExpr::App { ref fun, ref args } => {
            find_captured_valexpr(fun, env);
            for arg in args {
                find_captured_valexpr(arg, env);
            }
        }
        ValExpr::ArrayLiteral(ref items) => {
            for item in items {
                find_captured_valexpr(item, env);
            }
        }
        ValExpr::StructLiteral(ref items) => {
            for (_, val) in items {
                find_captured_valexpr(val, env);
            }
        }
        ValExpr::Enum(ref items) => {
            for (_, val) in items {
                find_captured_valexpr(val, env);
            }
        }
        ValExpr::Union(ref items) => {
            for (_, val) in items {
                find_captured_valexpr(val, env);
            }
        }
        ValExpr::Extern { ref ty, .. } => {
            find_captured_valexpr(ty, env);
        }
        ValExpr::Int { .. } => {}
        ValExpr::Float { .. } => {}
        ValExpr::String(_) => {}
        ValExpr::Builtin { .. } => {}
        ValExpr::Ret { ref value, .. } => {
            find_captured_valexpr(value, env);
        }
        ValExpr::Loop { ref body, .. } => {
            find_captured_valexpr(body, env);
        }
        ValExpr::And {
            ref left,
            ref right,
        } => {
            find_captured_valexpr(left, env);
            find_captured_valexpr(right, env);
        }
        ValExpr::Or {
            ref left,
            ref right,
        } => {
            find_captured_valexpr(left, env);
            find_captured_valexpr(right, env);
        }
        ValExpr::FieldAccess { ref root, .. } => {
            find_captured_valexpr(root, env);
        }
        ValExpr::Error => {}
        ValExpr::Hole => {}
        ValExpr::Bool { .. } => {}
        ValExpr::New { ref ty, ref val } => {
            find_captured_valexpr(ty, env);
            find_captured_valexpr(val, env);
        }
        ValExpr::Struct(ref fields) => {
            for (_, val) in fields {
                find_captured_valexpr(val, env);
            }
        }
        ValExpr::Typed { ref value, ref ty } => {
            find_captured_valexpr(value, env);
            find_captured_valexpr(ty, env);
        }
        ValExpr::Label { ref value, .. } => find_captured_valexpr(value, env),
    }
}

fn find_captured_placeexpr(
    place: &Augmented<PlaceExpr>,
    env: &mut FindCapturedVarEnvironment,
    role: UseKind,
) {
    match place.val {
        PlaceExpr::Var(id) => {
            env.capture_var_if_needed(id, role);
        }
        PlaceExpr::DeBrujinVar { .. } => {
            unreachable!("should not have debruijn vars at this point")
        }
        PlaceExpr::FieldAccess { ref root, .. } => {
            find_captured_placeexpr(root, env, role);
        }
        PlaceExpr::Error => {}
    }
}

struct RewriteCapturedVarEnvironment {
    captured: HashMap<usize, (usize, UseKind)>,
}

enum Action {
    Copy,
}

impl RewriteCapturedVarEnvironment {
    fn new(
        FindCapturedVarEnvironment { captured, .. }: FindCapturedVarEnvironment,
        env: &mut VariableResolutionEnvironment,
        env2: &mut Environment,
    ) -> Self {
        RewriteCapturedVarEnvironment {
            captured: captured
                .into_iter()
                .map(|(id, kind)| {
                    let new_id = env.introduce_captured_var(id, env2);
                    (id, (new_id, kind))
                })
                .collect(),
        }
    }

    fn to_captured(self) -> Vec<(Augmented<PatExpr>, Augmented<ValExpr>)> {
        self.captured
            .into_iter()
            .map(|(id, (new_id, kind))| {
                let pat = Augmented {
                    range: Range::default(),
                    val: PatExpr::Identifier(id),
                };
                let val = Augmented {
                    range: Range::default(),
                    val: ValExpr::Use(
                        Box::new(Augmented {
                            range: Range::default(),
                            val: PlaceExpr::Var(new_id),
                        }),
                        kind,
                    ),
                };
                (pat, val)
            })
            .collect()
    }
}

fn rewrite_captured_patexpr(pat: &mut Augmented<PatExpr>, env: &RewriteCapturedVarEnvironment) {
    match &mut pat.val {
        PatExpr::Identifier { .. } => {}
        PatExpr::Ignore => {}
        PatExpr::Typed { pat, ty } => {
            rewrite_captured_patexpr(pat, env);
            rewrite_captured_valexpr(ty, env);
        }
        PatExpr::Error => {}
        PatExpr::StructLiteral(entries) => {
            for (_, pat) in entries {
                rewrite_captured_patexpr(pat, env);
            }
        }
        PatExpr::New { pat, ty } => {
            rewrite_captured_patexpr(pat, env);
            rewrite_captured_valexpr(ty, env);
        }
        PatExpr::Literal(val) => {
            rewrite_captured_valexpr(val, env);
        }
    }
}

fn rewrite_captured_valexpr(
    Augmented { range, val }: &mut Augmented<ValExpr>,
    env: &RewriteCapturedVarEnvironment,
) {
    let range = range.clone();
    match val {
        ValExpr::Use(place, ref role) => match rewrite_captured_placeexpr(place, env) {
            // make no changes if not captured
            None => {}
            // when moving, everything stays the same
            Some(UseKind::Move) => {}
            // when mutably borrowing, convert mutable borrows to reborrows, and borrows to reborrows + downcast
            Some(UseKind::MutBorrow) => {
                let role = role.clone();
                let place = place.clone();
                *val = match role {
                    UseKind::Move => unreachable!(
                        "move is not permitted when the capture is only mutably borrowed"
                    ),
                    // x! -> @reborrow(_)(x_captured!)
                    UseKind::MutBorrow => ValExpr::App {
                        fun: Box::new(Augmented {
                            range,
                            val: ValExpr::App {
                                fun: Box::new(Augmented {
                                    range,
                                    val: ValExpr::Builtin {
                                        builtin: Builtin::Reborrow,
                                        level: 0,
                                    },
                                }),
                                args: vec![Augmented {
                                    range,
                                    val: ValExpr::Hole,
                                }],
                            },
                        }),
                        args: vec![Augmented {
                            range,
                            val: ValExpr::Use(place, UseKind::MutBorrow),
                        }],
                    },
                    //x& -> @readonly(_)(@reborrow(_)(x_captured!))
                    UseKind::Borrow => ValExpr::App {
                        fun: Box::new(Augmented {
                            range,
                            val: ValExpr::App {
                                fun: Box::new(Augmented {
                                    range,
                                    val: ValExpr::Builtin {
                                        builtin: Builtin::Readonly,
                                        level: 0,
                                    },
                                }),
                                args: vec![Augmented {
                                    range,
                                    val: ValExpr::Hole,
                                }],
                            },
                        }),
                        args: vec![Augmented {
                            range,
                            val: ValExpr::App {
                                fun: Box::new(Augmented {
                                    range,
                                    val: ValExpr::App {
                                        fun: Box::new(Augmented {
                                            range,
                                            val: ValExpr::Builtin {
                                                builtin: Builtin::Reborrow,
                                                level: 0,
                                            },
                                        }),
                                        args: vec![Augmented {
                                            range,
                                            val: ValExpr::Hole,
                                        }],
                                    },
                                }),
                                args: vec![Augmented {
                                    range,
                                    val: ValExpr::Use(place, UseKind::MutBorrow),
                                }],
                            },
                        }],
                    },
                }
            }
            // when borrowing, convert borrows to copies
            Some(UseKind::Borrow) => {
                let role = role.clone();
                let place = place.clone();
                *val = match role {
                    UseKind::Move => {
                        unreachable!("move is not permitted when the capture is only borrowed")
                    }
                    UseKind::MutBorrow => unreachable!(
                        "mutably borrowing is not permitted when the capture is only borrowed"
                    ),
                    // x& -> @copy(_)(x_captured&)
                    UseKind::Borrow => ValExpr::App {
                        fun: Box::new(Augmented {
                            range,
                            val: ValExpr::App {
                                fun: Box::new(Augmented {
                                    range,
                                    val: ValExpr::Builtin {
                                        builtin: Builtin::Copy,
                                        level: 0,
                                    },
                                }),
                                args: vec![Augmented {
                                    range,
                                    val: ValExpr::Hole,
                                }],
                            },
                        }),
                        args: vec![Augmented {
                            range,
                            val: ValExpr::Use(place, UseKind::Borrow),
                        }],
                    },
                }
            }
        },
        ValExpr::Block { statements, .. } => {
            for statement in statements {
                match &mut statement.val {
                    BlockStatement::Let { pat, .. } => {
                        rewrite_captured_patexpr(pat, env);
                    }
                    BlockStatement::Do(val) => {
                        rewrite_captured_valexpr(val, env);
                    }
                    _ => {}
                }
            }
        }
        ValExpr::Lam { captures, .. } => {
            for (pat, val) in captures {
                rewrite_captured_patexpr(pat, env);
                rewrite_captured_valexpr(val, env);
            }
        }
        ValExpr::PiTy { captures, .. } => {
            for (pat, val) in captures {
                rewrite_captured_patexpr(pat, env);
                rewrite_captured_valexpr(val, env);
            }
        }
        ValExpr::CaseOf { expr, cases } => {
            rewrite_captured_valexpr(expr, env);
            for (pat, val) in cases {
                rewrite_captured_patexpr(pat, env);
                rewrite_captured_valexpr(val, env);
            }
        }
        ValExpr::App { fun, ref mut args } => {
            rewrite_captured_valexpr(fun, env);
            for arg in args {
                rewrite_captured_valexpr(arg, env);
            }
        }
        ValExpr::ArrayLiteral(items) => {
            for item in items {
                rewrite_captured_valexpr(item, env);
            }
        }
        ValExpr::StructLiteral(items) => {
            for (_, val) in items {
                rewrite_captured_valexpr(val, env);
            }
        }
        ValExpr::Enum(items) => {
            for (_, val) in items {
                rewrite_captured_valexpr(val, env);
            }
        }
        ValExpr::Union(items) => {
            for (_, val) in items {
                rewrite_captured_valexpr(val, env);
            }
        }
        ValExpr::Extern { ty, .. } => {
            rewrite_captured_valexpr(ty, env);
        }
        ValExpr::Int { .. } => {}
        ValExpr::Float { .. } => {}
        ValExpr::String(_) => {}
        ValExpr::Builtin { .. } => {}
        ValExpr::Ret { value, .. } => {
            rewrite_captured_valexpr(value, env);
        }
        ValExpr::Loop { body, .. } => {
            rewrite_captured_valexpr(body, env);
        }
        ValExpr::And { left, right } => {
            rewrite_captured_valexpr(left, env);
            rewrite_captured_valexpr(right, env);
        }
        ValExpr::Or { left, right } => {
            rewrite_captured_valexpr(left, env);
            rewrite_captured_valexpr(right, env);
        }
        ValExpr::FieldAccess { root, .. } => {
            rewrite_captured_valexpr(root, env);
        }
        ValExpr::Error => {}
        ValExpr::Hole => {}
        ValExpr::Bool { .. } => {}
        ValExpr::New { ty, val } => {
            rewrite_captured_valexpr(ty, env);
            rewrite_captured_valexpr(val, env);
        }

        ValExpr::Struct(fields) => {
            for (_, val) in fields {
                rewrite_captured_valexpr(val, env);
            }
        }
        ValExpr::Typed { value, ty } => {
            rewrite_captured_valexpr(value, env);
            rewrite_captured_valexpr(ty, env);
        }
        ValExpr::Label { value, .. } => rewrite_captured_valexpr(value, env),
    }
}

fn rewrite_captured_placeexpr(
    place: &mut Augmented<PlaceExpr>,
    env: &RewriteCapturedVarEnvironment,
) -> Option<UseKind> {
    match &mut place.val {
        PlaceExpr::Var(id) => match env.captured.get(&id) {
            Some((new_id, role)) => {
                *id = *new_id;
                Some(role.clone())
            }
            None => None,
        },
        PlaceExpr::DeBrujinVar { .. } => {
            unreachable!("should not have debruijn vars at this point")
        }
        PlaceExpr::FieldAccess { root, .. } => rewrite_captured_placeexpr(root, env),
        PlaceExpr::Error => None,
    }
}

fn translate_augblockstatement(
    ast::Augmented { range, val }: ast::Augmented<ast::BlockStatement>,
    env: &mut VariableResolutionEnvironment,
    env2: &mut Environment,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<BlockStatement> {
    match val {
        ast::BlockStatement::Error => Augmented {
            range,
            val: BlockStatement::Error,
        },
        ast::BlockStatement::Let { pat, value } => {
            // first parse value so that we don't accidentally introduce the name of the val before the value
            let value = Box::new(translate_augvalexpr(*value, env, env2, dlogger));

            // now introduce name
            let pat = Box::new(translate_augpatexpr(*pat, env, env2, dlogger));

            Augmented {
                range,
                val: BlockStatement::Let { pat, value },
            }
        }
        ast::BlockStatement::Use { namespace } => {
            env.use_namespace(namespace, dlogger);
            Augmented {
                range,
                val: BlockStatement::NoOp,
            }
        }
        ast::BlockStatement::Do(v) => Augmented {
            range,
            val: BlockStatement::Do(Box::new(translate_augvalexpr(*v, env, env2, dlogger))),
        },
        ast::BlockStatement::Annotated { value, .. } => {
            translate_augblockstatement(*value, env, env2, dlogger)
        }
    }
}

pub fn translate_augfilestatement(
    ast::Augmented { range, val, .. }: ast::Augmented<ast::FileStatement>,
    env: &mut VariableResolutionEnvironment,
    env2: &mut Environment,
    dlogger: &mut DiagnosticLogger,
) -> Vec<Augmented<FileStatement>> {
    match val {
        ast::FileStatement::Error => vec![],
        ast::FileStatement::Let { pat, value } => {
            // first parse value so that we don't accidentally introduce the name of the val before the value
            let value = Box::new(translate_augvalexpr(*value, env, env2, dlogger));

            // now introduce name
            let pat = Box::new(translate_augpatexpr(*pat, env, env2, dlogger));

            vec![Augmented {
                val: FileStatement::Let { pat, value },
                range: range.clone(),
            }]
        }
        ast::FileStatement::Use { namespace } => {
            env.use_namespace(namespace, dlogger);
            vec![]
        }
        ast::FileStatement::Namespace {
            namespace: ast::Identifier { identifier, .. },
            items,
        } => {
            let mut out_items = vec![];
            if let Some(identifier) = identifier {
                // new scope
                env.names_in_scope.push(vec![]);
                // push prefix to the last prefix scope
                env.namespaces.last_mut().unwrap().push(identifier.clone());
                // translate items
                for item in items {
                    out_items.extend(translate_augfilestatement(item, env, env2, dlogger));
                }
                // pop prefix
                env.namespaces.last_mut().unwrap().pop();
                // pop scope and insert into the parent scope
                let namespace_scope = env.names_in_scope.pop().unwrap();
                env.names_in_scope
                    .last_mut()
                    .unwrap()
                    .push((identifier.clone(), Name::Namespace(namespace_scope)));
            }
            out_items
        }
        ast::FileStatement::Annotated { value, .. } => {
            translate_augfilestatement(*value, env, env2, dlogger)
        }
    }
}

#[derive(Clone, Debug)]
pub enum Name {
    Var(usize),
    Namespace(Vec<(String, Name)>),
}

pub struct VariableResolutionEnvironment {
    // namespaces that we are nested in
    namespaces: Vec<Vec<String>>,
    // these are the names that are in scope
    names_in_scope: Vec<Vec<(String, Name)>>,

    // these are the labels that are in scope
    labels_in_scope: Vec<Vec<(String, usize)>>,
}

impl VariableResolutionEnvironment {
    fn introduce_identifier(
        &mut self,
        ast::Identifier { identifier, range }: ast::Identifier,
        modifier: ast::IdentifierModifier,
        env: &mut Environment,
    ) -> Option<(usize, String)> {
        match identifier {
            Some(identifier) => {
                let id = env.id_name_table.len();
                let scope = self.names_in_scope.last_mut().unwrap();
                scope.push((identifier.clone(), Name::Var(id)));
                env.id_name_table.push(
                    [
                        self.namespaces.last().unwrap(),
                        [identifier.clone()].as_slice(),
                    ]
                    .concat(),
                );
                env.id_range_table.push(range);
                env.id_modifier_table.push(modifier);
                Some((id, identifier))
            }
            None => None,
        }
    }

    fn introduce_captured_var(&mut self, original_id: usize, env: &mut Environment) -> usize {
        let id = env.id_name_table.len();
        env.id_name_table
            .push(vec![format!("captured_var_{}", original_id)]);
        env.id_range_table.push(env.id_range_table[id].clone());
        env.id_modifier_table
            .push(env.id_modifier_table[id].clone());

        id
    }

    fn introduce_label(
        &mut self,
        ast::Label { label, range }: ast::Label,
        env: &mut Environment,
    ) -> Option<(usize, String)> {
        match label {
            Some(label) => {
                let id = env.lb_name_table.len();
                self.labels_in_scope
                    .last_mut()
                    .unwrap()
                    .push((label.clone(), id));
                env.lb_name_table.push(label.clone());
                env.lb_range_table.push(range);
                Some((id, label))
            }
            None => None,
        }
    }

    fn unintroduce_label(&mut self) {
        self.labels_in_scope.last_mut().unwrap().pop();
    }

    fn use_namespace(&mut self, identifier: ast::Identifier, dlogger: &mut DiagnosticLogger) {
        match &identifier.identifier {
            Some(identifier_name) => match self
                .names_in_scope
                .iter()
                .flatten()
                .rev()
                .filter_map(|(_, name)| match name {
                    Name::Namespace(v) => Some(v),
                    _ => None,
                })
                .next()
            {
                Some(id) => {
                    let id = id.clone();
                    self.names_in_scope.last_mut().unwrap().extend(id);
                }
                None => {
                    dlogger.log_unknown_identifier(identifier.range, identifier_name);
                }
            },
            None => {}
        }
    }

    fn lookup_identifier(
        &self,
        identifier: ast::Identifier,
        dlogger: &mut DiagnosticLogger,
    ) -> PlaceExpr {
        match &identifier.identifier {
            Some(identifier_name) => match self
                .names_in_scope
                .iter()
                .flatten()
                .rev()
                .filter_map(|(_, name)| match name {
                    Name::Var(id) => Some(PlaceExpr::Var(*id)),
                    _ => None,
                })
                .next()
            {
                Some(v) => v,
                None => {
                    dlogger.log_unknown_identifier(identifier.range, identifier_name);
                    PlaceExpr::Error
                }
            },
            None => PlaceExpr::Error,
        }
    }

    fn lookup_label(&self, label: ast::Label, dlogger: &mut DiagnosticLogger) -> Option<usize> {
        match &label.label {
            Some(label_name) => match self
                .labels_in_scope
                .last()
                .iter()
                .flat_map(|scope| scope.iter().rev())
                .filter(|(name, _)| name == label_name)
                .next()
            {
                Some((_, id)) => Some(*id),
                None => {
                    dlogger.log_unknown_label(label.range, label_name);
                    None
                }
            },
            None => None,
        }
    }

    fn push_block_scope(&mut self) {
        self.names_in_scope.push(vec![]);
    }

    fn pop_block_scope(&mut self) {
        self.names_in_scope.pop();
    }

    fn push_fn_scope(&mut self) {
        self.names_in_scope.push(vec![]);
        self.labels_in_scope.push(vec![]);
    }

    fn pop_fn_scope(&mut self) {
        self.names_in_scope.pop();
        assert_eq!(self.labels_in_scope.last().unwrap().len(), 0);
        self.labels_in_scope.pop();
    }

    pub fn new() -> Self {
        VariableResolutionEnvironment {
            namespaces: vec![vec![]],
            names_in_scope: vec![vec![]],
            labels_in_scope: vec![vec![]],
        }
    }
}
