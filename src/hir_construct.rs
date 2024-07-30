use std::collections::HashMap;

use indexmap::IndexMap;
use lsp_types::Range;

use crate::ast::{self};
use crate::builtin::Builtin;
use crate::dlogger::DiagnosticLogger;
use crate::hir::*;

fn translate_augstructitemexpr<U>(
    mut lower: impl FnMut(
        ast::Augmented<ast::Expr>,
        &mut Environment,
        &mut DiagnosticLogger,
    ) -> Augmented<U>,
    mut replace_eponymous: impl FnMut(ast::Identifier, &mut Environment, &mut DiagnosticLogger) -> U,
    env: &mut Environment,
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
                        lower(*expr, env, dlogger),
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
    env: &mut Environment,
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
            val: match env.introduce_identifier(identifier) {
                Some((id, original)) => PatExpr::Identifier {
                    id,
                    modifier,
                    original,
                },
                None => PatExpr::Error,
            },
        },
        ast::Expr::StructLiteral(items) => Augmented {
            range,
            val: PatExpr::StructLiteral(translate_augstructitemexpr(
                |x, env, dlogger| translate_augpatexpr(x, env, dlogger),
                |id, env, _| match env.introduce_identifier(id) {
                    Some((id, original)) => PatExpr::Identifier {
                        modifier: ast::IdentifierModifier::None,
                        id,
                        original,
                    },
                    None => PatExpr::Error,
                },
                env,
                dlogger,
                items,
            )),
        },
        ast::Expr::Typed { pat, ty } => Augmented {
            range,
            val: PatExpr::Typed {
                pat: Box::new(translate_augpatexpr(*pat, env, dlogger)),
                ty: Box::new(translate_augvalexpr(*ty, env, dlogger)),
            },
        },
        ast::Expr::ReverseTyped { pat, ty } => Augmented {
            range,
            val: PatExpr::Typed {
                pat: Box::new(translate_augpatexpr(*pat, env, dlogger)),
                ty: Box::new(translate_augvalexpr(*ty, env, dlogger)),
            },
        },
        ast::Expr::New { ty, val } => Augmented {
            range,
            val: PatExpr::New {
                ty: Box::new(translate_augvalexpr(*ty, env, dlogger)),
                pat: Box::new(translate_augpatexpr(*val, env, dlogger)),
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
    env: &mut Environment,
    dlogger: &mut DiagnosticLogger,
) -> (Augmented<PatExpr>, Augmented<ValExpr>) {
    (
        translate_augpatexpr(*c.target, env, dlogger),
        translate_augvalexpr(*c.body, env, dlogger),
    )
}

fn translate_augvalloopexpr(
    body: Box<ast::Augmented<ast::Expr>>,
    label: Option<ast::Label>,
    range: Range,
    env: &mut Environment,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<ValExpr> {
    let label = match label {
        Some(label) => match env.introduce_label(label) {
            Some((i, _)) => Some(i),
            _ => {
                return Augmented {
                    range: body.range,
                    val: ValExpr::Error,
                }
            }
        },
        None => None,
    };

    let body = Box::new(translate_augvalexpr(*body, env, dlogger));

    if label.is_some() {
        env.unintroduce_label();
    }

    Augmented {
        range,
        val: ValExpr::Loop { label, body },
    }
}

fn translate_augvalblockexpr(
    statements: Vec<ast::Augmented<ast::BlockStatement>>,
    trailing_semicolon: bool,
    range: Range,
    label: Option<ast::Label>,
    env: &mut Environment,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<ValExpr> {
    let label = match label {
        Some(label) => Some(match env.introduce_label(label) {
            Some((i, _)) => i,
            _ => {
                return Augmented {
                    range: Range::default(),
                    val: ValExpr::Error,
                }
            }
        }),
        None => None,
    };

    // introduce new scope
    env.push_block_scope();
    let mut statements: Vec<Augmented<BlockStatement>> = statements
        .into_iter()
        .map(|x| translate_augblockstatement(x, env, dlogger))
        .collect();
    // end scope
    env.pop_block_scope();

    if label.is_some() {
        env.unintroduce_label();
    }

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
            label,
            statements,
            last_expr,
        },
    }
}

fn translate_augplaceexpr(
    ast::Augmented { range, val, .. }: ast::Augmented<ast::Expr>,
    env: &mut Environment,
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
        ast::Expr::Deref(v) => Augmented {
            range,
            val: PlaceExpr::Deref(Box::new(translate_augvalexpr(
                ast::Augmented {
                    range,
                    val: ast::Expr::Deref(v),
                },
                env,
                dlogger,
            ))),
        },
        ast::Expr::ArrayAccess { root, index } => Augmented {
            range,
            val: PlaceExpr::ArrayAccess {
                root: Box::new(translate_augvalexpr(*root, env, dlogger)),
                index: Box::new(translate_augvalexpr(*index, env, dlogger)),
            },
        },
        ast::Expr::FieldAccess { root, field } => Augmented {
            range,
            val: PlaceExpr::FieldAccess {
                root: Box::new(translate_augplaceexpr(*root, env, dlogger)),
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
    env: &mut Environment,
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
        ast::Expr::Annotated { value, .. } => translate_augvalexpr(*value, env, dlogger),
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
                Box::new(translate_augplaceexpr(*v, env, dlogger)),
                UseKind::Borrow,
            ),
        },
        ast::Expr::Mutref(v) => Augmented {
            range,
            val: ValExpr::Use(
                Box::new(translate_augplaceexpr(*v, env, dlogger)),
                UseKind::MutBorrow,
            ),
        },
        ast::Expr::Copy(v) => Augmented {
            range,
            // TODO change this to a copy
            val: ValExpr::Use(
                Box::new(translate_augplaceexpr(*v, env, dlogger)),
                UseKind::Move,
            ),
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
                    dlogger,
                )),
                UseKind::Move,
            ),
        },
        ast::Expr::StructLiteral(items) => Augmented {
            range,
            val: ValExpr::StructLiteral(translate_augstructitemexpr(
                |x, env, dlogger| translate_augvalexpr(x, env, dlogger),
                |id, env, dlogger| {
                    ValExpr::Use(
                        Box::new(Augmented {
                            range,
                            val: env.lookup_identifier(id, dlogger),
                        }),
                        UseKind::Move,
                    )
                },
                env,
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
                    fun: Box::new(translate_augvalexpr(*right_operand, env, dlogger)),
                    args: vec![translate_augvalexpr(*left_operand, env, dlogger)],
                },
                ast::ValBinaryOpKind::Assign => ValExpr::Assign {
                    target: Box::new(translate_augplaceexpr(*left_operand, env, dlogger)),
                    value: Box::new(translate_augvalexpr(*right_operand, env, dlogger)),
                },
                ast::ValBinaryOpKind::And => ValExpr::And {
                    left: Box::new(translate_augvalexpr(*left_operand, env, dlogger)),
                    right: Box::new(translate_augvalexpr(*right_operand, env, dlogger)),
                },
                ast::ValBinaryOpKind::Or => ValExpr::Or {
                    left: Box::new(translate_augvalexpr(*left_operand, env, dlogger)),
                    right: Box::new(translate_augvalexpr(*right_operand, env, dlogger)),
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
                            Box::new(translate_augplaceexpr(*left_operand, env, dlogger)),
                            UseKind::Move,
                        ),
                    };
                    let value = translate_augvalexpr(*right_operand, env, dlogger);
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

                    let left = translate_augvalexpr(*left_operand, env, dlogger);
                    let right = translate_augvalexpr(*right_operand, env, dlogger);

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
            };

            Augmented { range, val }
        }
        ast::Expr::CaseOf { expr, cases } => {
            let expr = Box::new(translate_augvalexpr(*expr, env, dlogger));
            let cases = cases
                .into_iter()
                .map(|x| translate_caseexpr(x.val, env, dlogger))
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
                    value: Box::new(translate_augvalexpr(*value, env, dlogger)),
                    label,
                },
            }
        }
        ast::Expr::Loop { body } => translate_augvalloopexpr(body, None, range, env, dlogger),
        ast::Expr::Block {
            statements,
            trailing_semicolon,
        } => translate_augvalblockexpr(statements, trailing_semicolon, range, None, env, dlogger),
        ast::Expr::Labeled { label, value } => match value.val {
            ast::Expr::Loop { body } => {
                translate_augvalloopexpr(body, Some(label), range, env, dlogger)
            }
            ast::Expr::Block {
                statements,
                trailing_semicolon,
            } => translate_augvalblockexpr(
                statements,
                trailing_semicolon,
                range,
                Some(label),
                env,
                dlogger,
            ),
            _ => {
                let label_str = match label {
                    ast::Label {
                        label: Some(identifier),
                        ..
                    } => identifier,
                    _ => "{unknown}".to_string(),
                };
                dlogger.log_label_on_non_loop_or_block(range, &label_str);
                Augmented {
                    range,
                    val: ValExpr::Error,
                }
            }
        },
        ast::Expr::Group(v) => translate_augvalexpr(*v, env, dlogger),
        ast::Expr::Array(items) => Augmented {
            range,
            val: ValExpr::ArrayLiteral(
                items
                    .into_iter()
                    .map(|x| translate_augvalexpr(x, env, dlogger))
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
                    dlogger,
                )),
                UseKind::Move,
            ),
        },
        ast::Expr::FnDef { params, body } => {
            // introduce new type and val scope
            env.push_fn_scope();

            // insert params into scope
            let params = params
                .into_iter()
                .map(|x| translate_augpatexpr(x, env, dlogger))
                .collect();

            let body = Box::new(translate_augvalexpr(*body, env, dlogger));

            // end type and val scope
            env.pop_fn_scope();

            let mut captured = vec![];

            // find captured variables
            for param in params {
                find_captured_patexpr(param, &mut captured);
            }
            find_captured_valexpr(&body, &mut captured);

            // rewrite to use captured variables
            let params = params
                .into_iter()
                .map(|x| rewrite_captured_patexpr(x, &captured))
                .collect();
            let body = rewrite_captured_valexpr(body, &captured);

            Augmented {
                range,
                val: ValExpr::Lam {
                    params,
                    body,
                    captured,
                },
            }
        }
        ast::Expr::FnTy {
            param_tys: paramtys,
            dep_return_ty: returnty,
        } => Augmented {
            range,
            val: ValExpr::FnTy {
                param_tys: paramtys
                    .into_iter()
                    .map(|x| translate_augvalexpr(x, env, dlogger))
                    .collect(),
                dep_ty: Box::new(translate_augvalexpr(*returnty, env, dlogger)),
            },
        },
        ast::Expr::App { root, args } => Augmented {
            range,
            val: ValExpr::App {
                fun: Box::new(translate_augvalexpr(*root, env, dlogger)),
                args: args
                    .into_iter()
                    .map(|x| translate_augvalexpr(x, env, dlogger))
                    .collect(),
            },
        },
        ast::Expr::ArrayAccess { root, index } => Augmented {
            range,
            val: ValExpr::Use(
                Box::new(translate_augplaceexpr(
                    ast::Augmented {
                        range,
                        val: ast::Expr::ArrayAccess { root, index },
                    },
                    env,
                    dlogger,
                )),
                UseKind::Move,
            ),
        },
        ast::Expr::FieldAccess { root, field } => Augmented {
            range,
            val: ValExpr::FieldAccess {
                root: Box::new(translate_augvalexpr(*root, env, dlogger)),
                field,
            },
        },
        ast::Expr::New { ty, val } => {
            let ty = Box::new(translate_augvalexpr(*ty, env, dlogger));
            let val = Box::new(translate_augvalexpr(*val, env, dlogger));
            Augmented {
                range,
                val: ValExpr::New { ty, val },
            }
        }
        ast::Expr::StructTy(items) => Augmented {
            range,
            val: ValExpr::Struct(translate_augstructitemexpr(
                translate_augvalexpr,
                |id, env, dlogger| {
                    ValExpr::Use(
                        Box::new(Augmented {
                            range,
                            val: env.lookup_identifier(id, dlogger),
                        }),
                        UseKind::Move,
                    )
                },
                env,
                dlogger,
                items,
            )),
        },
        ast::Expr::EnumTy(items) => Augmented {
            range,
            val: ValExpr::Enum(translate_augstructitemexpr(
                translate_augvalexpr,
                |id, env, dlogger| {
                    ValExpr::Use(
                        Box::new(Augmented {
                            range,
                            val: env.lookup_identifier(id, dlogger),
                        }),
                        UseKind::Move,
                    )
                },
                env,
                dlogger,
                items,
            )),
        },
        ast::Expr::UnionTy(items) => Augmented {
            range,
            val: ValExpr::Union(translate_augstructitemexpr(
                translate_augvalexpr,
                |id, env, dlogger| {
                    ValExpr::Use(
                        Box::new(Augmented {
                            range,
                            val: env.lookup_identifier(id, dlogger),
                        }),
                        UseKind::Move,
                    )
                },
                env,
                dlogger,
                items,
            )),
        },
        ast::Expr::Extern { name, ty } => Augmented {
            range,
            val: ValExpr::Extern {
                name: name.clone(),
                ty: Box::new(translate_augvalexpr(*ty, env, dlogger)),
            },
        },
        ast::Expr::ReverseTyped { pat, ty } => {
            let value = Box::new(translate_augvalexpr(*pat, env, dlogger));
            let ty = Box::new(translate_augvalexpr(*ty, env, dlogger));
            Augmented {
                range,
                val: ValExpr::Typed { value, ty },
            }
        }
        ast::Expr::Typed { pat, ty } => {
            let value = Box::new(translate_augvalexpr(*pat, env, dlogger));
            let ty = Box::new(translate_augvalexpr(*ty, env, dlogger));
            Augmented {
                range,
                val: ValExpr::Typed { value, ty },
            }
        }
    }
}

fn find_captured_patexpr(pat: &Augmented<PatExpr>, captured: &mut Vec<(usize, UseKind)>) {
    match &pat.val {
        PatExpr::Identifier { .. } => {}
        PatExpr::Ignore => {}
        PatExpr::Typed { pat, ty } => {
            find_captured_patexpr(pat, captured);
            find_captured_valexpr(ty, captured);
        }
        PatExpr::Error => {}
        PatExpr::StructLiteral(entries) => {
            for (_, pat) in entries {
                find_captured_patexpr(pat, captured);
            }
        }
        PatExpr::New { pat, ty } => {
            find_captured_patexpr(pat, captured);
            find_captured_valexpr(ty, captured);
        }
        PatExpr::Literal(val) => {
            find_captured_valexpr(val, captured);
        }
    }
}

fn find_captured_valexpr(val: &Augmented<ValExpr>, captured: &mut Vec<(usize, UseKind)>) {
    match val.val {
        ValExpr::Use(ref place, ref role) => {
            find_captured_placeexpr(place, captured, role.clone());
        }
        ValExpr::Block { ref statements, .. } => {
            for statement in statements {
                match statement.val {
                    BlockStatement::Let { ref pat, .. } => {
                        find_captured_patexpr(pat, captured);
                    }
                    BlockStatement::Do(ref val) => {
                        find_captured_valexpr(val, captured);
                    }
                    _ => {}
                }
            }
        }
        ValExpr::Lam {
            ref params,
            ref body,
            ..
        } => {
            for param in params {
                find_captured_patexpr(param, captured);
            }
            find_captured_valexpr(body, captured);
        }
        ValExpr::CaseOf {
            ref expr,
            ref cases,
        } => {
            find_captured_valexpr(expr, captured);
            for (pat, val) in cases {
                find_captured_patexpr(pat, captured);
                find_captured_valexpr(val, captured);
            }
        }
        ValExpr::App { ref fun, ref args } => {
            find_captured_valexpr(fun, captured);
            for arg in args {
                find_captured_valexpr(arg, captured);
            }
        }
        ValExpr::ArrayLiteral(ref items) => {
            for item in items {
                find_captured_valexpr(item, captured);
            }
        }
        ValExpr::StructLiteral(ref items) => {
            for (_, val) in items {
                find_captured_valexpr(val, captured);
            }
        }
        ValExpr::Enum(ref items) => {
            for (_, val) in items {
                find_captured_valexpr(val, captured);
            }
        }
        ValExpr::Union(ref items) => {
            for (_, val) in items {
                find_captured_valexpr(val, captured);
            }
        }
        ValExpr::Extern { ref ty, .. } => {
            find_captured_valexpr(ty, captured);
        }
        ValExpr::Int { .. } => {}
        ValExpr::Float { .. } => {}
        ValExpr::String(_) => {}
        ValExpr::Builtin { .. } => {}
        ValExpr::Ret { ref value, .. } => {
            find_captured_valexpr(value, captured);
        }
        ValExpr::Loop { ref body, .. } => {
            find_captured_valexpr(body, captured);
        }
        ValExpr::And {
            ref left,
            ref right,
        } => {
            find_captured_valexpr(left, captured);
            find_captured_valexpr(right, captured);
        }
        ValExpr::Or {
            ref left,
            ref right,
        } => {
            find_captured_valexpr(left, captured);
            find_captured_valexpr(right, captured);
        }
        ValExpr::Assign {
            ref target,
            ref value,
        } => {
            find_captured_placeexpr(target, captured, UseKind::MutBorrow);
            find_captured_valexpr(value, captured);
        }
        ValExpr::FieldAccess { ref root, .. } => {
            find_captured_valexpr(root, captured);
        }
        ValExpr::Error => {}
        ValExpr::Hole => {}
        ValExpr::Bool { .. } => {}
        ValExpr::New { ref ty, ref val } => {
            find_captured_valexpr(ty, captured);
            find_captured_valexpr(val, captured);
        }
        ValExpr::FnTy {
            ref param_tys,
            ref dep_ty,
        } => {
            for param_ty in param_tys {
                find_captured_valexpr(param_ty, captured);
            }
            find_captured_valexpr(dep_ty, captured);
        }
        ValExpr::Struct(ref fields) => {
            for (_, val) in fields {
                find_captured_valexpr(val, captured);
            }
        }
        ValExpr::Typed { ref value, ref ty } => {
            find_captured_valexpr(value, captured);
            find_captured_valexpr(ty, captured);
        }
    }
}

fn find_captured_placeexpr(
    place: &Augmented<PlaceExpr>,
    captured: &mut Vec<(usize, UseKind)>,
    role: UseKind,
) {
    match place.val {
        PlaceExpr::Local { debrujin_idx, .. } => {
            if role == UseKind::MutBorrow {
                captured.push(id);
            }
        }
        PlaceExpr::Global(_) => {}
        PlaceExpr::Deref(ref v) => {
            find_captured_valexpr(v, captured);
        }
        PlaceExpr::ArrayAccess {
            ref root,
            ref index,
        } => {
            find_captured_valexpr(root, captured);
            find_captured_valexpr(index, captured);
        }
        PlaceExpr::FieldAccess {
            ref root,
            ref field,
        } => {
            find_captured_placeexpr(root, captured, role);
        }
        PlaceExpr::Error => {}
    }
}

fn translate_augblockstatement(
    ast::Augmented { range, val }: ast::Augmented<ast::BlockStatement>,
    env: &mut Environment,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<BlockStatement> {
    match val {
        ast::BlockStatement::Error => Augmented {
            range,
            val: BlockStatement::Error,
        },
        ast::BlockStatement::Let { pat, value } => {
            // first parse value so that we don't accidentally introduce the name of the val before the value
            let value = Box::new(translate_augvalexpr(*value, env, dlogger));

            // now introduce name
            let pat = Box::new(translate_augpatexpr(*pat, env, dlogger));

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
            val: BlockStatement::Do(Box::new(translate_augvalexpr(*v, env, dlogger))),
        },
        ast::BlockStatement::Annotated { value, .. } => {
            translate_augblockstatement(*value, env, dlogger)
        }
    }
}

pub fn translate_augfilestatement(
    ast::Augmented { range, val, .. }: ast::Augmented<ast::FileStatement>,
    env: &mut Environment,
    dlogger: &mut DiagnosticLogger,
) -> Vec<Augmented<FileStatement>> {
    match val {
        ast::FileStatement::Error => vec![],
        ast::FileStatement::Let { pat, value } => {
            // first parse value so that we don't accidentally introduce the name of the val before the value
            let value = Box::new(translate_augvalexpr(*value, env, dlogger));

            // now introduce name
            let pat = Box::new(translate_augpatexpr(*pat, env, dlogger));

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
                env.names_in_scope.push(HashMap::new());
                // push prefix to the last prefix scope
                env.namespaces.last_mut().unwrap().push(identifier.clone());
                // translate items
                for item in items {
                    out_items.extend(translate_augfilestatement(item, env, dlogger));
                }
                // pop prefix
                env.namespaces.last_mut().unwrap().pop();
                // pop scope and insert into the parent scope
                let namespace_scope = env.names_in_scope.pop().unwrap();
                env.names_in_scope
                    .last_mut()
                    .unwrap()
                    .insert(identifier.clone(), Name::Namespace(namespace_scope));
            }
            out_items
        }
        ast::FileStatement::Annotated { value, .. } => {
            translate_augfilestatement(*value, env, dlogger)
        }
    }
}
