use std::collections::HashMap;

use indexmap::IndexMap;
use lsp_types::Range;

use crate::ast;
use crate::dlogger::DiagnosticLogger;
use crate::hir::*;

fn tr_aug<T, U>(
    x: ast::Augmented<T>,
    env: &mut Environment,
    dlogger: &mut DiagnosticLogger,
    f: impl Fn(T, &mut Environment, &mut DiagnosticLogger) -> U,
) -> Augmented<U> {
    Augmented {
        range: x.range,
        val: f(x.val, env, dlogger),
    }
}

fn translate_augstructitemexpr<T, U>(
    mut lower: impl FnMut(ast::Augmented<T>, &mut Environment, &mut DiagnosticLogger) -> Augmented<U>,
    mut replace_eponymous: impl FnMut(ast::Identifier, &mut Environment, &mut DiagnosticLogger) -> U,
    env: &mut Environment,
    dlogger: &mut DiagnosticLogger,
    items: Vec<ast::Augmented<ast::StructItemExpr<T>>>,
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

fn translate_augkindexpr(
    k: ast::Augmented<ast::KindExpr>,
    env: &mut Environment,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<KindExpr> {
    tr_aug(k, env, dlogger, translate_kindexpr)
}

fn translate_kindexpr(
    k: ast::KindExpr,
    env: &mut Environment,
    dlogger: &mut DiagnosticLogger,
) -> KindExpr {
    match k {
        ast::KindExpr::Error => KindExpr::Error,
        ast::KindExpr::Type => KindExpr::Type,
        ast::KindExpr::Int => KindExpr::Int,
        ast::KindExpr::Float => KindExpr::Float,
        ast::KindExpr::Bool => KindExpr::Bool,
        ast::KindExpr::Generic { args, returnkind } => KindExpr::Constructor {
            paramkinds: args
                .into_iter()
                .map(|k| translate_augkindexpr(k, env, dlogger))
                .collect(),
            returnkind: Box::new(translate_augkindexpr(*returnkind, env, dlogger)),
        },
    }
}

fn translate_augpatexpr(
    ast::Augmented { range, val, .. }: ast::Augmented<ast::PatExpr>,
    env: &mut Environment,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<ValPatExpr> {
    match val {
        ast::PatExpr::Error => Augmented {
            range,
            val: ValPatExpr::Error,
        },
        ast::PatExpr::Ignore => Augmented {
            range,
            val: ValPatExpr::Ignore,
        },
        ast::PatExpr::Identifier {
            identifier,
            modifier,
        } => Augmented {
            range,
            val: match env.introduce_identifier(identifier, dlogger) {
                Some((id, original)) => ValPatExpr::Identifier {
                    id,
                    modifier,
                    original,
                },
                None => ValPatExpr::Error,
            },
        },
        ast::PatExpr::StructLiteral(items) => Augmented {
            range,
            val: ValPatExpr::StructLiteral(translate_augstructitemexpr(
                |x, env, dlogger| translate_augpatexpr(x, env, dlogger),
                |id, env, dlogger| match env.introduce_identifier(id, dlogger) {
                    Some((id, original)) => ValPatExpr::Identifier {
                        modifier: ast::IdentifierModifier::None,
                        id,
                        original,
                    },
                    None => ValPatExpr::Error,
                },
                env,
                dlogger,
                items,
            )),
        },
        ast::PatExpr::Typed { pat, ty } => Augmented {
            range,
            val: ValPatExpr::Typed {
                pat: Box::new(translate_augpatexpr(*pat, env, dlogger)),
                ty: Box::new(translate_augvalexpr(*ty, env, dlogger)),
            },
        },
        ast::PatExpr::Kinded { pat, kind } => Augmented {
            range,
            val: ValPatExpr::Kinded {
                pat: Box::new(translate_augpatexpr(*pat, env, dlogger)),
                kind: Box::new(translate_augkindexpr(*kind, env, dlogger)),
            },
        },
        ast::PatExpr::New { ty, pat } => Augmented {
            range,
            val: ValPatExpr::New {
                ty: Box::new(translate_augvalexpr(*ty, env, dlogger)),
                pat: Box::new(translate_augpatexpr(*pat, env, dlogger)),
            },
        },
    }
}

fn translate_casetargetexpr(
    c: ast::CaseTargetExpr,
    env: &mut Environment,
    dlogger: &mut DiagnosticLogger,
) -> CaseTargetExpr {
    match c {
        ast::CaseTargetExpr::Error => CaseTargetExpr::Error,
        ast::CaseTargetExpr::Bool(b) => CaseTargetExpr::Bool(b),
        ast::CaseTargetExpr::Int(i) => CaseTargetExpr::Int(i),
        ast::CaseTargetExpr::PatExpr(pat) => {
            CaseTargetExpr::PatExpr(Box::new(translate_augpatexpr(*pat, env, dlogger)))
        }
    }
}

fn translate_caseexpr(
    c: ast::CaseExpr,
    env: &mut Environment,
    dlogger: &mut DiagnosticLogger,
) -> (Augmented<CaseTargetExpr>, Augmented<ValExpr>) {
    (
        tr_aug(*c.target, env, dlogger, translate_casetargetexpr),
        translate_augvalexpr(*c.body, env, dlogger),
    )
}

fn translate_augvalexpr(
    ast::Augmented { range, val, .. }: ast::Augmented<ast::Expr>,
    env: &mut Environment,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<ValExpr> {
    match val {
        ast::Expr::Error => Augmented {
            range,
            val: ValExpr::Error,
        },
        ast::Expr::Int(i) => Augmented {
            range,
            val: ValExpr::Int(i),
        },
        ast::Expr::Bool(b) => Augmented {
            range,
            val: ValExpr::Bool(b),
        },
        ast::Expr::Float(f) => Augmented {
            range,
            val: ValExpr::Float(f),
        },
        ast::Expr::String { value, .. } => Augmented {
            range,
            val: ValExpr::String(value),
        },
        ast::Expr::Ref(v) => Augmented {
            range,
            val: ValExpr::Ref(Box::new(translate_augvalexpr(*v, env, dlogger))),
        },
        ast::Expr::Deref(v) => Augmented {
            range,
            val: ValExpr::Deref(Box::new(translate_augvalexpr(*v, env, dlogger))),
        },
        ast::Expr::StructLiteral(items) => Augmented {
            range,
            val: ValExpr::StructLiteral(translate_augstructitemexpr(
                |x, env, dlogger| translate_augvalexpr(x, env, dlogger),
                |id, env, dlogger| match env.lookup_identifier(id, dlogger) {
                    Some(id) => ValExpr::Identifier(id),
                    None => ValExpr::Error,
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
                ast::ValBinaryOpKind::Assign => ValExpr::BinaryOp {
                    op: ValBinaryOpKind::Assign,
                    left_operand: Box::new(translate_augvalexpr(*left_operand, env, dlogger)),
                    right_operand: Box::new(translate_augvalexpr(*right_operand, env, dlogger)),
                },
                ast::ValBinaryOpKind::AssignAdd => ValExpr::BinaryOp {
                    op: ValBinaryOpKind::AssignAdd,
                    left_operand: Box::new(translate_augvalexpr(*left_operand, env, dlogger)),
                    right_operand: Box::new(translate_augvalexpr(*right_operand, env, dlogger)),
                },
                ast::ValBinaryOpKind::AssignSub => ValExpr::BinaryOp {
                    op: ValBinaryOpKind::AssignSub,
                    left_operand: Box::new(translate_augvalexpr(*left_operand, env, dlogger)),
                    right_operand: Box::new(translate_augvalexpr(*right_operand, env, dlogger)),
                },
                ast::ValBinaryOpKind::AssignMul => ValExpr::BinaryOp {
                    op: ValBinaryOpKind::AssignMul,
                    left_operand: Box::new(translate_augvalexpr(*left_operand, env, dlogger)),
                    right_operand: Box::new(translate_augvalexpr(*right_operand, env, dlogger)),
                },
                ast::ValBinaryOpKind::AssignDiv => ValExpr::BinaryOp {
                    op: ValBinaryOpKind::AssignDiv,
                    left_operand: Box::new(translate_augvalexpr(*left_operand, env, dlogger)),
                    right_operand: Box::new(translate_augvalexpr(*right_operand, env, dlogger)),
                },
                ast::ValBinaryOpKind::Add => ValExpr::BinaryOp {
                    op: ValBinaryOpKind::Add,
                    left_operand: Box::new(translate_augvalexpr(*left_operand, env, dlogger)),
                    right_operand: Box::new(translate_augvalexpr(*right_operand, env, dlogger)),
                },
                ast::ValBinaryOpKind::Sub => ValExpr::BinaryOp {
                    op: ValBinaryOpKind::Sub,
                    left_operand: Box::new(translate_augvalexpr(*left_operand, env, dlogger)),
                    right_operand: Box::new(translate_augvalexpr(*right_operand, env, dlogger)),
                },
                ast::ValBinaryOpKind::Mul => ValExpr::BinaryOp {
                    op: ValBinaryOpKind::Mul,
                    left_operand: Box::new(translate_augvalexpr(*left_operand, env, dlogger)),
                    right_operand: Box::new(translate_augvalexpr(*right_operand, env, dlogger)),
                },
                ast::ValBinaryOpKind::Div => ValExpr::BinaryOp {
                    op: ValBinaryOpKind::Div,
                    left_operand: Box::new(translate_augvalexpr(*left_operand, env, dlogger)),
                    right_operand: Box::new(translate_augvalexpr(*right_operand, env, dlogger)),
                },
                ast::ValBinaryOpKind::Rem => ValExpr::BinaryOp {
                    op: ValBinaryOpKind::Rem,
                    left_operand: Box::new(translate_augvalexpr(*left_operand, env, dlogger)),
                    right_operand: Box::new(translate_augvalexpr(*right_operand, env, dlogger)),
                },
                ast::ValBinaryOpKind::And => ValExpr::BinaryOp {
                    op: ValBinaryOpKind::And,
                    left_operand: Box::new(translate_augvalexpr(*left_operand, env, dlogger)),
                    right_operand: Box::new(translate_augvalexpr(*right_operand, env, dlogger)),
                },
                ast::ValBinaryOpKind::Or => ValExpr::BinaryOp {
                    op: ValBinaryOpKind::Or,
                    left_operand: Box::new(translate_augvalexpr(*left_operand, env, dlogger)),
                    right_operand: Box::new(translate_augvalexpr(*right_operand, env, dlogger)),
                },
                ast::ValBinaryOpKind::Equal => ValExpr::BinaryOp {
                    op: ValBinaryOpKind::Eq,
                    left_operand: Box::new(translate_augvalexpr(*left_operand, env, dlogger)),
                    right_operand: Box::new(translate_augvalexpr(*right_operand, env, dlogger)),
                },
                ast::ValBinaryOpKind::NotEqual => ValExpr::BinaryOp {
                    op: ValBinaryOpKind::Neq,
                    left_operand: Box::new(translate_augvalexpr(*left_operand, env, dlogger)),
                    right_operand: Box::new(translate_augvalexpr(*right_operand, env, dlogger)),
                },
                ast::ValBinaryOpKind::Less => ValExpr::BinaryOp {
                    op: ValBinaryOpKind::Lt,
                    left_operand: Box::new(translate_augvalexpr(*left_operand, env, dlogger)),
                    right_operand: Box::new(translate_augvalexpr(*right_operand, env, dlogger)),
                },
                ast::ValBinaryOpKind::Greater => ValExpr::BinaryOp {
                    op: ValBinaryOpKind::Gt,
                    left_operand: Box::new(translate_augvalexpr(*left_operand, env, dlogger)),
                    right_operand: Box::new(translate_augvalexpr(*right_operand, env, dlogger)),
                },
                ast::ValBinaryOpKind::LessEqual => ValExpr::BinaryOp {
                    op: ValBinaryOpKind::Leq,
                    left_operand: Box::new(translate_augvalexpr(*left_operand, env, dlogger)),
                    right_operand: Box::new(translate_augvalexpr(*right_operand, env, dlogger)),
                },
                ast::ValBinaryOpKind::GreaterEqual => ValExpr::BinaryOp {
                    op: ValBinaryOpKind::Geq,
                    left_operand: Box::new(translate_augvalexpr(*left_operand, env, dlogger)),
                    right_operand: Box::new(translate_augvalexpr(*right_operand, env, dlogger)),
                },
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
        ast::Expr::If {
            cond,
            then_branch,
            else_branch,
        } => Augmented {
            range,
            val: ValExpr::If {
                cond: Box::new(translate_augvalexpr(*cond, env, dlogger)),
                then_branch: Box::new(translate_augvalexpr(*then_branch, env, dlogger)),
                else_branch: else_branch.map(|e| Box::new(translate_augvalexpr(*e, env, dlogger))),
            },
        },
        ast::Expr::Loop { label, body } => {
            let label = match env.introduce_label(label, dlogger) {
                Some((i, _)) => i,
                _ => {
                    return Augmented {
                        range,
                        val: ValExpr::Error,
                    }
                }
            };

            let body = Box::new(translate_augvalexpr(*body, env, dlogger));

            env.unintroduce_label();

            Augmented {
                range,
                val: ValExpr::Loop { label, body },
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
        ast::Expr::Block {
            label,
            statements,
            trailing_semicolon,
        } => {
            let label = match env.introduce_label(label, dlogger) {
                Some((i, _)) => i,
                _ => {
                    return Augmented {
                        range,
                        val: ValExpr::Error,
                    }
                }
            };

            // introduce new scope
            env.push_block_scope();
            let mut statements: Vec<Augmented<BlockStatement>> = statements
                .into_iter()
                .map(|x| tr_aug(x, env, dlogger, translate_blockstatement))
                .collect();
            // end scope
            env.pop_block_scope();

            env.unintroduce_label();

            // if the last statement is a do statement, and there is no trailing semicolon, then it is an implicit return
            // here we turn it into an explicit return
            if let Some(mut last_statement) = statements.pop() {
                if let Augmented {
                    range: do_range,
                    val: BlockStatement::Do(ref value),
                } = last_statement
                    && !trailing_semicolon
                {
                    last_statement = Augmented {
                        range: do_range.clone(),
                        val: BlockStatement::Do(Box::new(Augmented {
                            range: do_range,
                            val: ValExpr::Ret {
                                value: value.clone(),
                                label,
                            },
                        })),
                    };
                }
                statements.push(last_statement);
            }

            Augmented {
                range,
                val: ValExpr::Block { label, statements },
            }
        }
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
        ast::Expr::Identifier(i) => Augmented {
            range,
            val: match env.lookup_identifier(i, dlogger) {
                Some(id) => ValExpr::Identifier(id),
                None => ValExpr::Error,
            },
        },
        ast::Expr::FnDef {
            typarams: Some(typarams),
            params,
            returnty,
            body,
        } => {
            // introduce scope
            env.push_fn_scope();

            // insert typarams into scope
            let typarams = typarams
                .into_iter()
                .map(|x| translate_augpatexpr(x, env, dlogger))
                .collect();

            // insert params into scope
            let params = params
                .into_iter()
                .map(|x| translate_augpatexpr(x, env, dlogger))
                .collect();

            let returnty = returnty.map(|rt| Box::new(translate_augvalexpr(*rt, env, dlogger)));

            let body = Box::new(translate_augvalexpr(*body, env, dlogger));

            // end type and val scope
            env.pop_fn_scope();

            Augmented {
                range,
                val: ValExpr::Generic {
                    params: typarams,
                    returnkind: None,
                    body: Box::new(Augmented {
                        range,
                        val: ValExpr::FnDef {
                            params,
                            returnty,
                            body,
                        },
                    }),
                },
            }
        }
        ast::Expr::FnDef {
            typarams: None,
            params,
            returnty,
            body,
        } => {
            // introduce new type and val scope
            env.push_fn_scope();

            // insert params into scope
            let params = params
                .into_iter()
                .map(|x| translate_augpatexpr(x, env, dlogger))
                .collect();

            let returnty = returnty.map(|rt| Box::new(translate_augvalexpr(*rt, env, dlogger)));

            let body = Box::new(translate_augvalexpr(*body, env, dlogger));

            // end type and val scope
            env.pop_fn_scope();

            let val = ValExpr::FnDef {
                params,
                returnty,
                body,
            };

            Augmented { range, val }
        }
        ast::Expr::FnTy { paramtys, returnty } => Augmented {
            range,
            val: ValExpr::FnTy {
                paramtys: paramtys
                    .into_iter()
                    .map(|x| translate_augvalexpr(x, env, dlogger))
                    .collect(),
                returnty: Box::new(translate_augvalexpr(*returnty, env, dlogger)),
            },
        },
        ast::Expr::Concretization { root, tyargs } => Augmented {
            range,
            val: ValExpr::Concretization {
                generic: Box::new(translate_augvalexpr(*root, env, dlogger)),
                tyargs: tyargs
                    .into_iter()
                    .map(|x| translate_augvalexpr(x, env, dlogger))
                    .collect(),
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
            val: ValExpr::ArrayAccess {
                root: Box::new(translate_augvalexpr(*root, env, dlogger)),
                index: Box::new(translate_augvalexpr(*index, env, dlogger)),
            },
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
                |id, env, dlogger| match env.lookup_identifier(id, dlogger) {
                    Some(id) => ValExpr::Identifier(id),
                    None => ValExpr::Error,
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
                |id, env, dlogger| match env.lookup_identifier(id, dlogger) {
                    Some(id) => ValExpr::Identifier(id),
                    None => ValExpr::Error,
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
                |id, env, dlogger| match env.lookup_identifier(id, dlogger) {
                    Some(id) => ValExpr::Identifier(id),
                    None => ValExpr::Error,
                },
                env,
                dlogger,
                items,
            )),
        },
        ast::Expr::Generic {
            params,
            returnkind,
            body,
        } => Augmented {
            range,
            val: ValExpr::Generic {
                params: params
                    .into_iter()
                    .map(|x| translate_augpatexpr(x, env, dlogger))
                    .collect(),
                returnkind: returnkind.map(|rk| Box::new(translate_augkindexpr(*rk, env, dlogger))),
                body: Box::new(translate_augvalexpr(*body, env, dlogger)),
            },
        },
        ast::Expr::Extern { name, ty } => Augmented {
            range,
            val: ValExpr::Extern {
                name: name.clone(),
                ty: Box::new(translate_augvalexpr(*ty, env, dlogger)),
            },
        },
    }
}

fn translate_blockstatement(
    bs: ast::BlockStatement,
    env: &mut Environment,
    dlogger: &mut DiagnosticLogger,
) -> BlockStatement {
    match bs {
        ast::BlockStatement::Error => BlockStatement::Error,
        ast::BlockStatement::Let { pat, value } => {
            // first parse value so that we don't accidentally introduce the name of the val before the value
            let value = Box::new(translate_augvalexpr(*value, env, dlogger));

            // now introduce name
            let pat = Box::new(translate_augpatexpr(*pat, env, dlogger));

            BlockStatement::Let { pat, value }
        }
        ast::BlockStatement::Use { namespace } => {
            env.use_namespace(namespace, dlogger);
            BlockStatement::NoOp
        }
        ast::BlockStatement::Do(v) => {
            BlockStatement::Do(Box::new(translate_augvalexpr(*v, env, dlogger)))
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
    }
}
