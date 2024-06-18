use std::collections::HashMap;

use indexmap::IndexMap;
use lsp_types::Range;

use crate::ast;
use crate::dlogger::DiagnosticLogger;
use crate::hir::*;

impl Environment {
    fn introduce_identifier(
        &mut self,
        ast::Identifier { identifier, range }: ast::Identifier,
        dlogger: &mut DiagnosticLogger,
    ) -> Option<usize> {
        match identifier {
            Some(identifier) => {
                if let Some(previous_range) = self
                    .names_in_scope
                    .iter()
                    .rev()
                    .flat_map(|scope| match scope.get(&identifier) {
                        Some(Name::Value(id)) => Some(self.range_table[*id]),
                        _ => None,
                    })
                    .next()
                {
                    dlogger.log_duplicate_identifier(range, previous_range, &identifier);
                    None
                } else {
                    let id = self.name_table.len();
                    self.names_in_scope
                        .last_mut()
                        .unwrap()
                        .insert(identifier.clone(), Name::Value(id));
                    self.name_table.push(
                        [
                            self.namespaces.last().unwrap(),
                            [identifier.clone()].as_slice(),
                        ]
                        .concat(),
                    );
                    self.range_table.push(range);
                    self.kind_table.push(None);
                    self.type_table.push(None);
                    Some(id)
                }
            }
            None => None,
        }
    }

    fn use_namespace(&mut self, identifier: ast::Identifier, dlogger: &mut DiagnosticLogger) {
        match &identifier.identifier {
            Some(identifier_name) => match self
                .names_in_scope
                .iter()
                .rev()
                .flat_map(|scope| match scope.get(identifier_name).cloned() {
                    Some(Name::Namespace(v)) => Some(v),
                    _ => None,
                })
                .next()
            {
                Some(id) => self.names_in_scope.last_mut().unwrap().extend(id),
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
    ) -> Option<usize> {
        match &identifier.identifier {
            Some(identifier_name) => match self
                .names_in_scope
                .iter()
                .rev()
                .flat_map(|scope| match scope.get(identifier_name) {
                    Some(Name::Value(id)) => Some(id),
                    _ => None,
                })
                .next()
            {
                Some(id) => Some(*id),
                None => {
                    dlogger.log_unknown_identifier(identifier.range, identifier_name);
                    None
                }
            },
            None => None,
        }
    }
}

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
    ast::Augmented { range, val, .. }: ast::Augmented<ast::ValPatExpr>,
    env: &mut Environment,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<ValPatExpr> {
    match val {
        ast::ValPatExpr::Error => Augmented {
            range,
            val: ValPatExpr::Error,
        },
        ast::ValPatExpr::Ignore => Augmented {
            range,
            val: ValPatExpr::Ignore,
        },
        ast::ValPatExpr::Identifier {
            identifier,
            mutable,
        } => Augmented {
            range,
            val: match env.introduce_identifier(identifier, dlogger) {
                Some(id) => ValPatExpr::Identifier { id, mutable },
                None => ValPatExpr::Error,
            },
        },
        ast::ValPatExpr::StructLiteral(items) => Augmented {
            range,
            val: ValPatExpr::StructLiteral(translate_augstructitemexpr(
                |x, env, dlogger| translate_augpatexpr(x, env, dlogger),
                |id, env, dlogger| match env.introduce_identifier(id, dlogger) {
                    Some(id) => ValPatExpr::Identifier { mutable: false, id },
                    None => ValPatExpr::Error,
                },
                env,
                dlogger,
                items,
            )),
        },
        ast::ValPatExpr::Typed { pat, ty } => Augmented {
            range,
            val: ValPatExpr::Typed {
                pat: Box::new(translate_augpatexpr(*pat, env, dlogger)),
                ty: Box::new(translate_augvalexpr(*ty, env, dlogger)),
            },
        },
        ast::ValPatExpr::Kinded { pat, kind } => Augmented {
            range,
            val: ValPatExpr::Kinded {
                pat: Box::new(translate_augpatexpr(*pat, env, dlogger)),
                kind: Box::new(translate_augkindexpr(*kind, env, dlogger)),
            },
        },
        ast::ValPatExpr::New { ty, pat } => Augmented {
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

fn translate_augelseexpr(
    ast::Augmented { val, range, .. }: ast::Augmented<ast::ElseExpr>,
    env: &mut Environment,
    dlogger: &mut DiagnosticLogger,
) -> Vec<Augmented<BlockStatement>> {
    match val {
        ast::ElseExpr::Error => vec![],
        ast::ElseExpr::Else(body) => translate_blockexpr_statement(body.val, env, dlogger),
        ast::ElseExpr::Elif {
            cond,
            then_branch,
            else_branch,
        } => {
            let cond = Box::new(translate_augvalexpr(*cond, env, dlogger));
            let then_branch = translate_blockexpr_statement(then_branch.val, env, dlogger);
            let else_branch = match else_branch {
                Some(else_branch) => translate_augelseexpr(*else_branch, env, dlogger),
                None => vec![],
            };

            vec![Augmented {
                range,
                val: BlockStatement::IfThen {
                    cond,
                    then_branch,
                    else_branch,
                },
            }]
        }
    }
}

fn translate_augvalexpr(
    ast::Augmented { range, val, .. }: ast::Augmented<ast::ValExpr>,
    env: &mut Environment,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<ValExpr> {
    match val {
        ast::ValExpr::Error => Augmented {
            range,
            val: ValExpr::Error,
        },
        ast::ValExpr::Int(i) => Augmented {
            range,
            val: ValExpr::Int(i),
        },
        ast::ValExpr::Bool(b) => Augmented {
            range,
            val: ValExpr::Bool(b),
        },
        ast::ValExpr::Float(f) => Augmented {
            range,
            val: ValExpr::Float(f),
        },
        ast::ValExpr::String { value, .. } => Augmented {
            range,
            val: ValExpr::String(value),
        },
        ast::ValExpr::Ref(v) => Augmented {
            range,
            val: ValExpr::Ref(Box::new(translate_augvalexpr(*v, env, dlogger))),
        },
        ast::ValExpr::Deref(v) => Augmented {
            range,
            val: ValExpr::Deref(Box::new(translate_augvalexpr(*v, env, dlogger))),
        },
        ast::ValExpr::BoolTy => Augmented {
            range,
            val: ValExpr::BoolTy,
        },
        ast::ValExpr::RefConstructorTy => Augmented {
            range,
            val: ValExpr::RefConstructorTy,
        },
        ast::ValExpr::ArrayConstructorTy => Augmented {
            range,
            val: ValExpr::ArrayConstructorTy,
        },
        ast::ValExpr::SliceConstructorTy => Augmented {
            range,
            val: ValExpr::SliceConstructorTy,
        },
        ast::ValExpr::IntConstructorTy => Augmented {
            range,
            val: ValExpr::IntConstructorTy,
        },
        ast::ValExpr::FloatConstructorTy => Augmented {
            range,
            val: ValExpr::FloatConstructorTy,
        },
        ast::ValExpr::StructLiteral(items) => Augmented {
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
        ast::ValExpr::BinaryOp {
            op,
            left_operand,
            right_operand,
        } => {
            let left_operand = Box::new(translate_augvalexpr(*left_operand, env, dlogger));
            let right_operand = Box::new(translate_augvalexpr(*right_operand, env, dlogger));
            let val = match op {
                ast::ValBinaryOpKind::Pipe => ValExpr::App {
                    fun: right_operand,
                    args: vec![*left_operand],
                },
                ast::ValBinaryOpKind::Add => ValExpr::BinaryOp {
                    op: ValBinaryOpKind::Add,
                    left_operand,
                    right_operand,
                },
                ast::ValBinaryOpKind::Sub => ValExpr::BinaryOp {
                    op: ValBinaryOpKind::Sub,
                    left_operand,
                    right_operand,
                },
                ast::ValBinaryOpKind::Mul => ValExpr::BinaryOp {
                    op: ValBinaryOpKind::Mul,
                    left_operand,
                    right_operand,
                },
                ast::ValBinaryOpKind::Div => ValExpr::BinaryOp {
                    op: ValBinaryOpKind::Div,
                    left_operand,
                    right_operand,
                },
                ast::ValBinaryOpKind::Rem => ValExpr::BinaryOp {
                    op: ValBinaryOpKind::Rem,
                    left_operand,
                    right_operand,
                },
                ast::ValBinaryOpKind::And => ValExpr::BinaryOp {
                    op: ValBinaryOpKind::And,
                    left_operand,
                    right_operand,
                },
                ast::ValBinaryOpKind::Or => ValExpr::BinaryOp {
                    op: ValBinaryOpKind::Or,
                    left_operand,
                    right_operand,
                },
                ast::ValBinaryOpKind::Equal => ValExpr::BinaryOp {
                    op: ValBinaryOpKind::Eq,
                    left_operand,
                    right_operand,
                },
                ast::ValBinaryOpKind::NotEqual => ValExpr::BinaryOp {
                    op: ValBinaryOpKind::Neq,
                    left_operand,
                    right_operand,
                },
                ast::ValBinaryOpKind::Less => ValExpr::BinaryOp {
                    op: ValBinaryOpKind::Lt,
                    left_operand,
                    right_operand,
                },
                ast::ValBinaryOpKind::Greater => ValExpr::BinaryOp {
                    op: ValBinaryOpKind::Gt,
                    left_operand,
                    right_operand,
                },
                ast::ValBinaryOpKind::LessEqual => ValExpr::BinaryOp {
                    op: ValBinaryOpKind::Leq,
                    left_operand,
                    right_operand,
                },
                ast::ValBinaryOpKind::GreaterEqual => ValExpr::BinaryOp {
                    op: ValBinaryOpKind::Geq,
                    left_operand,
                    right_operand,
                },
            };

            Augmented { range, val }
        }
        ast::ValExpr::CaseOf { expr, cases } => {
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
        ast::ValExpr::Block(b) => Augmented {
            range,
            val: translate_blockexpr_val(b.val, env, dlogger),
        },
        ast::ValExpr::Group(v) => translate_augvalexpr(*v, env, dlogger),
        ast::ValExpr::Array(items) => Augmented {
            range,
            val: ValExpr::ArrayLiteral(
                items
                    .into_iter()
                    .map(|x| translate_augvalexpr(x, env, dlogger))
                    .collect(),
            ),
        },
        ast::ValExpr::Identifier(i) => Augmented {
            range,
            val: match env.lookup_identifier(i, dlogger) {
                Some(id) => ValExpr::Identifier(id),
                None => ValExpr::Error,
            },
        },
        ast::ValExpr::FnDef {
            typarams: Some(typarams),
            params,
            returnty,
            body,
        } => {
            // introduce new type and val scope
            env.names_in_scope.push(HashMap::new());

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
            env.names_in_scope.pop();

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
        ast::ValExpr::FnDef {
            typarams: None,
            params,
            returnty,
            body,
        } => {
            // introduce new type and val scope
            env.names_in_scope.push(HashMap::new());

            // insert params into scope
            let params = params
                .into_iter()
                .map(|x| translate_augpatexpr(x, env, dlogger))
                .collect();

            let returnty = returnty.map(|rt| Box::new(translate_augvalexpr(*rt, env, dlogger)));

            let body = Box::new(translate_augvalexpr(*body, env, dlogger));

            // end type and val scope
            env.names_in_scope.pop();

            let val = ValExpr::FnDef {
                params,
                returnty,
                body,
            };

            Augmented { range, val }
        }
        ast::ValExpr::FnTy { paramtys, returnty } => Augmented {
            range,
            val: ValExpr::FnTy {
                paramtys: paramtys
                    .into_iter()
                    .map(|x| translate_augvalexpr(x, env, dlogger))
                    .collect(),
                returnty: Box::new(translate_augvalexpr(*returnty, env, dlogger)),
            },
        },
        ast::ValExpr::Concretization { root, tyargs } => Augmented {
            range,
            val: ValExpr::Concretization {
                generic: Box::new(translate_augvalexpr(*root, env, dlogger)),
                tyargs: tyargs
                    .into_iter()
                    .map(|x| translate_augvalexpr(x, env, dlogger))
                    .collect(),
            },
        },
        ast::ValExpr::App { root, args } => Augmented {
            range,
            val: ValExpr::App {
                fun: Box::new(translate_augvalexpr(*root, env, dlogger)),
                args: args
                    .into_iter()
                    .map(|x| translate_augvalexpr(x, env, dlogger))
                    .collect(),
            },
        },
        ast::ValExpr::ArrayAccess { root, index } => Augmented {
            range,
            val: ValExpr::ArrayAccess {
                root: Box::new(translate_augvalexpr(*root, env, dlogger)),
                index: Box::new(translate_augvalexpr(*index, env, dlogger)),
            },
        },
        ast::ValExpr::FieldAccess { root, field } => Augmented {
            range,
            val: ValExpr::FieldAccess {
                root: Box::new(translate_augvalexpr(*root, env, dlogger)),
                field,
            },
        },
        ast::ValExpr::New { ty, val } => {
            let ty = Box::new(translate_augvalexpr(*ty, env, dlogger));
            let val = Box::new(translate_augvalexpr(*val, env, dlogger));
            Augmented {
                range,
                val: ValExpr::New { ty, val },
            }
        }
        ast::ValExpr::StructTy(items) => Augmented {
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
        ast::ValExpr::EnumTy(items) => Augmented {
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
        ast::ValExpr::UnionTy(items) => Augmented {
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
        ast::ValExpr::Generic {
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
    }
}

fn translate_blockexpr_statement(
    b: ast::BlockExpr,
    env: &mut Environment,
    dlogger: &mut DiagnosticLogger,
) -> Vec<Augmented<BlockStatement>> {
    // introduce new scope
    env.names_in_scope.push(HashMap::new());
    let statements: Vec<Augmented<BlockStatement>> = b
        .statements
        .into_iter()
        .map(|x| tr_aug(x, env, dlogger, translate_blockstatement))
        .collect();
    // end scope
    env.names_in_scope.pop();

    statements
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
        ast::BlockStatement::Set { place, value } => BlockStatement::Set {
            place: Box::new(translate_augvalexpr(*place, env, dlogger)),
            value: Box::new(translate_augvalexpr(*value, env, dlogger)),
        },
        ast::BlockStatement::IfThen {
            cond,
            then_branch,
            else_branch,
        } => BlockStatement::IfThen {
            cond: Box::new(translate_augvalexpr(*cond, env, dlogger)),
            then_branch: translate_blockexpr_statement(then_branch.val, env, dlogger),
            else_branch: match else_branch {
                Some(else_branch) => translate_augelseexpr(*else_branch, env, dlogger),
                None => vec![],
            },
        },
        ast::BlockStatement::While { cond, body } => BlockStatement::While {
            cond: Box::new(translate_augvalexpr(*cond, env, dlogger)),
            body: translate_blockexpr_statement(body.val, env, dlogger),
        },
        ast::BlockStatement::For {
            pattern,
            range,
            by,
            body,
        } => {
            // evaluate start, end, and by outside the scope
            let start = Box::new(translate_augvalexpr(*range.val.start, env, dlogger));
            let end = Box::new(translate_augvalexpr(*range.val.end, env, dlogger));
            let by = by.map(|x| Box::new(translate_augvalexpr(*x, env, dlogger)));
            // push val scope
            env.names_in_scope.push(HashMap::new());
            let pattern = Box::new(translate_augpatexpr(*pattern, env, dlogger));
            // parse body
            let body = translate_blockexpr_statement(body.val, env, dlogger);
            // pop val scope
            env.names_in_scope.pop();
            // return
            BlockStatement::For {
                pattern,
                body,
                start,
                end,
                by,
                inclusive: range.val.inclusive,
            }
        }
        ast::BlockStatement::Do(v) => {
            BlockStatement::Do(Box::new(translate_augvalexpr(*v, env, dlogger)))
        }
    }
}

fn translate_blockexpr_val(
    b: ast::BlockExpr,
    env: &mut Environment,
    dlogger: &mut DiagnosticLogger,
) -> ValExpr {
    // introduce new scope
    env.names_in_scope.push(HashMap::new());
    let mut statements: Vec<Augmented<BlockStatement>> = b
        .statements
        .into_iter()
        .map(|x| tr_aug(x, env, dlogger, translate_blockstatement))
        .collect();
    // end scope
    env.names_in_scope.pop();

    // if the last statement is a do statement, then we need to make it the last expression
    match statements.pop() {
        Some(Augmented {
            range,
            val: BlockStatement::Do(v),
        }) if !b.trailing_semicolon => ValExpr::Block {
            statements,
            last_expression: Some(v),
        },
        Some(s) => {
            statements.push(s);
            ValExpr::Block {
                statements,
                last_expression: None,
            }
        }
        None => ValExpr::Block {
            statements,
            last_expression: None,
        },
    }
}

fn translate_augfilestatement(
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
                val: FileStatement::ValDef { pat, value },
                range: range.clone(),
            }]
        }
        ast::FileStatement::Use { namespace } => {
            env.use_namespace(namespace, dlogger);
            vec![]
        }
        ast::FileStatement::Namespace {
            namespace:
                ast::Identifier {
                    identifier,
                    range: identifier_range,
                },
            items,
        } => {
            let mut out_items = vec![];
            if let Some(identifier) = identifier {
                // push prefix
                env.namespaces.push(identifier.clone());
                // new scope
                env.names_in_scope.push(HashMap::new());
                // translate items
                for item in items {
                    out_items.extend(translate_augfilestatement(item, env, dlogger));
                }
                // pop scope and insert into the parent scope
                let namespace_scope = env.names_in_scope.pop().unwrap();
                env.names_in_scope
                    .last_mut()
                    .unwrap()
                    .insert(identifier.clone(), Name::Namespace(namespace_scope));
                // pop prefix
                env.namespaces.pop();
            }
            out_items
        }
    }
}

pub fn init_env(dlogger: DiagnosticLogger) -> Environment {
    return Environment {
        type_names_in_scope: vec![HashMap::new()],
        val_names_in_scope: vec![HashMap::new()],
        prefixes: vec![],
        use_prefixes: vec![IndexMap::new()],
        dlogger,
        type_name_table: vec![],
        type_range_table: vec![],
        val_name_table: vec![],
        val_range_table: vec![],
    };
}
