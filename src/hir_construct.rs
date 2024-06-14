use std::collections::HashMap;

use indexmap::IndexMap;
use lsp_types::Range;

use crate::ast;
use crate::dlogger::DiagnosticLogger;
use crate::hir::*;

struct Environment {
    dlogger: DiagnosticLogger,
    prefixes: Vec<String>,
    use_prefixes: Vec<IndexMap<String, Range>>,
    // the global identifier tables
    type_range_table: Vec<Range>,
    type_name_table: Vec<String>,
    val_range_table: Vec<Range>,
    val_name_table: Vec<String>,
    // these are the names that are in scope
    type_names_in_scope: Vec<HashMap<String, usize>>,
    val_names_in_scope: Vec<HashMap<String, usize>>,
}

fn get_if_in_scope<'a, T>(
    name: &str,
    scopes: &'a Vec<HashMap<String, T>>,
    prefixes: &'a Vec<IndexMap<String, Range>>,
) -> Option<&'a T> {
    for (prefix, _) in prefixes.iter().flatten().rev() {
        let prefixed_name = [prefix.clone(), name.to_string()].concat();
        for scope in scopes.iter().rev() {
            if let Some(val) = scope.get(&prefixed_name) {
                return Some(val);
            }
        }
    }
    return None;
}

fn range_if_dupe<'a>(
    name: &str,
    current_scope: &Vec<HashMap<String, usize>>,
    range_table: &'a Vec<Range>,
) -> Option<&'a Range> {
    let id = current_scope.last().unwrap().get(name);
    match id {
        Some(id) => Some(&range_table[*id]),
        None => None,
    }
}

fn range_if_prefix_dupe<'a>(
    name: &str,
    scopes: &'a Vec<IndexMap<String, Range>>,
) -> Option<&'a Range> {
    for scope in scopes.iter().rev() {
        if let Some(val) = scope.get(name) {
            return Some(val);
        }
    }
    return None;
}

impl Environment {
    fn introduce_type_identifier(
        &mut self,
        ast::Identifier { identifier, range }: ast::Identifier,
        should_prefix: bool,
    ) -> Option<usize> {
        match identifier {
            Some(identifier) => {
                let identifier = if should_prefix {
                    [self.prefixes.concat(), identifier].concat()
                } else {
                    identifier
                };
                if let Some(previous_range) = range_if_dupe(
                    &identifier,
                    &self.type_names_in_scope,
                    &self.type_range_table,
                ) {
                    self.dlogger
                        .log_duplicate_identifier(range, *previous_range, &identifier);
                    None
                } else {
                    let id = self.type_name_table.len();
                    self.type_names_in_scope
                        .last_mut()
                        .unwrap()
                        .insert(identifier.clone(), id);
                    self.type_name_table.push(identifier.clone());
                    self.type_range_table.push(range);
                    Some(id)
                }
            }
            None => None,
        }
    }

    fn introduce_val_identifier(
        &mut self,
        ast::Identifier { identifier, range }: ast::Identifier,
        should_prefix: bool,
    ) -> Option<usize> {
        match identifier {
            Some(identifier) => {
                let identifier = if should_prefix {
                    [self.prefixes.concat(), identifier].concat()
                } else {
                    identifier
                };

                if let Some(previous_range) =
                    range_if_dupe(&identifier, &self.val_names_in_scope, &self.val_range_table)
                {
                    self.dlogger
                        .log_duplicate_identifier(range, *previous_range, &identifier);
                    None
                } else {
                    let id = self.val_name_table.len();
                    self.val_names_in_scope
                        .last_mut()
                        .unwrap()
                        .insert(identifier.clone(), id);
                    self.val_name_table.push(identifier.clone());
                    self.val_range_table.push(range);
                    Some(id)
                }
            }
            None => None,
        }
    }

    fn lookup_type_identifier(&mut self, identifier: ast::Identifier) -> Option<usize> {
        match &identifier.identifier {
            Some(identifier_name) => {
                if let Some(id) = get_if_in_scope(
                    identifier_name,
                    &self.type_names_in_scope,
                    &self.use_prefixes,
                ) {
                    Some(*id)
                } else {
                    self.dlogger
                        .log_unknown_identifier(identifier.range, identifier_name);
                    None
                }
            }
            None => None,
        }
    }

    fn lookup_val_identifier(&mut self, identifier: ast::Identifier) -> Option<usize> {
        match &identifier.identifier {
            Some(identifier_name) => {
                if let Some(id) = get_if_in_scope(
                    identifier_name,
                    &self.val_names_in_scope,
                    &self.use_prefixes,
                ) {
                    Some(*id)
                } else {
                    self.dlogger
                        .log_unknown_identifier(identifier.range, identifier_name);
                    None
                }
            }
            None => None,
        }
    }
}

fn tr_aug<T, U>(
    x: ast::Augmented<T>,
    env: &mut Environment,
    f: impl Fn(T, &mut Environment) -> U,
) -> Augmented<U> {
    Augmented {
        range: x.range,
        val: f(x.val, env),
    }
}

fn translate_augstructitemexpr<T, U>(
    lower: impl Fn(ast::Augmented<T>, &mut Environment) -> Augmented<U>,
    replace_eponymous: impl Fn(ast::Identifier, &mut Environment) -> U,
    env: &mut Environment,
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
                    env.dlogger
                        .log_duplicate_field_name(range, *preexisting_range, &identifier);
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
                    env.dlogger
                        .log_duplicate_field_name(range, *preexisting_range, &identifier);
                } else {
                    identifier_ranges.insert(identifier.clone(), identifier_range);
                    out_items.push((
                        Augmented {
                            range: identifier_range,
                            val: identifier,
                        },
                        lower(*expr, env),
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
) -> Augmented<KindExpr> {
    tr_aug(k, env, translate_kindexpr)
}

fn translate_kindexpr(k: ast::KindExpr, env: &mut Environment) -> KindExpr {
    match k {
        ast::KindExpr::Error => KindExpr::Error,
        ast::KindExpr::Type => KindExpr::Type,
        ast::KindExpr::Int => KindExpr::Int,
        ast::KindExpr::Float => KindExpr::Float,
        ast::KindExpr::Bool => KindExpr::Bool,
        ast::KindExpr::Generic { args, returnkind } => KindExpr::Constructor {
            paramkinds: args
                .into_iter()
                .map(|k| translate_augkindexpr(k, env))
                .collect(),
            returnkind: Box::new(translate_augkindexpr(*returnkind, env)),
        },
    }
}

fn translate_augtypepatexpr(
    ast::Augmented { range, val, .. }: ast::Augmented<ast::TypePatExpr>,
    env: &mut Environment,
    should_prefix: bool,
) -> Augmented<TypePatExpr> {
    match val {
        ast::TypePatExpr::Error => Augmented {
            range,
            val: TypePatExpr::Error,
        },
        ast::TypePatExpr::Typed { identifier, kind } => Augmented {
            range,
            val: match env.introduce_type_identifier(identifier, should_prefix) {
                Some(id) => TypePatExpr::Typed {
                    id,
                    kind: Box::new(tr_aug(*kind, env, translate_kindexpr)),
                },
                None => TypePatExpr::Error,
            },
        },
        ast::TypePatExpr::Identifier(identifier) => Augmented {
            range,
            val: match env.introduce_type_identifier(identifier, should_prefix) {
                Some(id) => TypePatExpr::Identifier(id),
                None => TypePatExpr::Error,
            },
        },
    }
}

fn translate_augtypeexpr(
    ast::Augmented {
        range,
        metadata,
        val,
    }: ast::Augmented<ast::TypeExpr>,
    env: &mut Environment,
) -> Augmented<TypeExpr> {
    match val {
        ast::TypeExpr::Error => Augmented {
            range,
            val: TypeExpr::Error,
        },
        ast::TypeExpr::Identifier(identifier) => Augmented {
            range,
            val: match env.lookup_type_identifier(identifier) {
                Some(id) => TypeExpr::Identifier(id),
                None => TypeExpr::Error,
            },
        },
        ast::TypeExpr::BoolTy => Augmented {
            range,
            val: TypeExpr::BoolTy,
        },
        ast::TypeExpr::RefConstructorTy => Augmented {
            range,
            val: TypeExpr::RefConstructorTy,
        },
        ast::TypeExpr::ArrayConstructorTy => Augmented {
            range,
            val: TypeExpr::ArrayConstructorTy,
        },
        ast::TypeExpr::SliceConstructorTy => Augmented {
            range,
            val: TypeExpr::SliceConstructorTy,
        },
        ast::TypeExpr::IntConstructorTy => Augmented {
            range,
            val: TypeExpr::IntConstructorTy,
        },
        ast::TypeExpr::FloatConstructorTy => Augmented {
            range,
            val: TypeExpr::FloatConstructorTy,
        },
        ast::TypeExpr::Int(i) => Augmented {
            range,
            val: TypeExpr::Int(i),
        },
        ast::TypeExpr::Bool(b) => Augmented {
            range,
            val: TypeExpr::Bool(b),
        },
        ast::TypeExpr::Float(f) => Augmented {
            range,
            val: TypeExpr::Float(f),
        },
        ast::TypeExpr::Ref(t) => Augmented {
            range,
            val: TypeExpr::Concretization {
                genericty: Box::new(Augmented {
                    range,
                    val: TypeExpr::RefConstructorTy,
                }),
                tyargs: vec![translate_augtypeexpr(*t, env)],
            },
        },
        ast::TypeExpr::Struct(items) => Augmented {
            range,
            val: TypeExpr::Struct(translate_augstructitemexpr(
                translate_augtypeexpr,
                |id, env| match env.lookup_type_identifier(id) {
                    Some(id) => TypeExpr::Identifier(id),
                    None => TypeExpr::Error,
                },
                env,
                items,
            )),
        },
        ast::TypeExpr::Enum(items) => Augmented {
            range,
            val: TypeExpr::Enum(translate_augstructitemexpr(
                translate_augtypeexpr,
                |id, env| match env.lookup_type_identifier(id) {
                    Some(id) => TypeExpr::Identifier(id),
                    None => TypeExpr::Error,
                },
                env,
                items,
            )),
        },
        ast::TypeExpr::Union(items) => Augmented {
            range,
            val: TypeExpr::Union(translate_augstructitemexpr(
                translate_augtypeexpr,
                |id, env| match env.lookup_type_identifier(id) {
                    Some(id) => TypeExpr::Identifier(id),
                    None => TypeExpr::Error,
                },
                env,
                items,
            )),
        },
        ast::TypeExpr::Group(t) => translate_augtypeexpr(*t, env),
        ast::TypeExpr::Generic {
            params,
            returnkind,
            body,
        } => Augmented {
            range,
            val: TypeExpr::Generic {
                params: params
                    .into_iter()
                    .map(|x| translate_augtypepatexpr(x, env, false))
                    .collect(),
                returnkind: returnkind.map(|rk| Box::new(translate_augkindexpr(*rk, env))),
                body: Box::new(translate_augtypeexpr(*body, env)),
            },
        },
        ast::TypeExpr::Concretization { root, tyargs } => Augmented {
            range,
            val: TypeExpr::Concretization {
                genericty: Box::new(translate_augtypeexpr(*root, env)),
                tyargs: tyargs
                    .into_iter()
                    .map(|x| translate_augtypeexpr(x, env))
                    .collect(),
            },
        },
        ast::TypeExpr::Fn { paramtys, returnty } => Augmented {
            range,
            val: TypeExpr::Fn {
                paramtys: paramtys
                    .into_iter()
                    .map(|x| translate_augtypeexpr(x, env))
                    .collect(),
                returnty: Box::new(translate_augtypeexpr(*returnty, env)),
            },
        },
    }
}

fn translate_augpatexpr(
    ast::Augmented { range, val, .. }: ast::Augmented<ast::ValPatExpr>,
    env: &mut Environment,
    should_prefix: bool,
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
            val: match env.introduce_val_identifier(identifier, should_prefix) {
                Some(id) => ValPatExpr::Identifier { id, mutable },
                None => ValPatExpr::Error,
            },
        },
        ast::ValPatExpr::StructLiteral(items) => Augmented {
            range,
            val: ValPatExpr::StructLiteral(translate_augstructitemexpr(
                |x, env| translate_augpatexpr(x, env, should_prefix),
                |id, env| match env.introduce_val_identifier(id, should_prefix) {
                    Some(id) => ValPatExpr::Identifier { mutable: false, id },
                    None => ValPatExpr::Error,
                },
                env,
                items,
            )),
        },
        ast::ValPatExpr::Typed { pat, ty } => Augmented {
            range,
            val: ValPatExpr::Typed {
                pat: Box::new(translate_augpatexpr(*pat, env, should_prefix)),
                ty: Box::new(translate_augtypeexpr(*ty, env)),
            },
        },
        ast::ValPatExpr::New { ty, pat } => Augmented {
            range,
            val: ValPatExpr::New {
                ty: Box::new(translate_augtypeexpr(*ty, env)),
                pat: Box::new(translate_augpatexpr(*pat, env, should_prefix)),
            },
        },
    }
}

fn translate_casetargetexpr(c: ast::CaseTargetExpr, env: &mut Environment) -> CaseTargetExpr {
    match c {
        ast::CaseTargetExpr::Error => CaseTargetExpr::Error,
        ast::CaseTargetExpr::Bool(b) => CaseTargetExpr::Bool(b),
        ast::CaseTargetExpr::Int(i) => CaseTargetExpr::Int(i),
        ast::CaseTargetExpr::PatExpr(pat) => {
            CaseTargetExpr::PatExpr(Box::new(translate_augpatexpr(*pat, env, false)))
        }
    }
}

fn translate_caseexpr(
    c: ast::CaseExpr,
    env: &mut Environment,
) -> (Augmented<CaseTargetExpr>, Augmented<ValExpr>) {
    (
        tr_aug(*c.target, env, translate_casetargetexpr),
        translate_augvalexpr(*c.body, env),
    )
}

fn translate_augelseexpr(
    ast::Augmented { val, range, .. }: ast::Augmented<ast::ElseExpr>,
    env: &mut Environment,
) -> Vec<Augmented<BlockStatement>> {
    match val {
        ast::ElseExpr::Error => vec![],
        ast::ElseExpr::Else(body) => translate_blockexpr_statement(body.val, env),
        ast::ElseExpr::Elif {
            cond,
            then_branch,
            else_branch,
        } => {
            let cond = Box::new(translate_augvalexpr(*cond, env));
            let then_branch = translate_blockexpr_statement(then_branch.val, env);
            let else_branch = match else_branch {
                Some(else_branch) => translate_augelseexpr(*else_branch, env),
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
            val: ValExpr::Ref(Box::new(translate_augvalexpr(*v, env))),
        },
        ast::ValExpr::Deref(v) => Augmented {
            range,
            val: ValExpr::Deref(Box::new(translate_augvalexpr(*v, env))),
        },
        ast::ValExpr::StructLiteral(items) => Augmented {
            range,
            val: ValExpr::StructLiteral(translate_augstructitemexpr(
                |x, env| translate_augvalexpr(x, env),
                |id, env| match env.lookup_val_identifier(id) {
                    Some(id) => ValExpr::Identifier(id),
                    None => ValExpr::Error,
                },
                env,
                items,
            )),
        },
        ast::ValExpr::BinaryOp {
            op,
            left_operand,
            right_operand,
        } => {
            let left_operand = Box::new(translate_augvalexpr(*left_operand, env));
            let right_operand = Box::new(translate_augvalexpr(*right_operand, env));
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
            let expr = Box::new(translate_augvalexpr(*expr, env));
            let cases = cases
                .into_iter()
                .map(|x| translate_caseexpr(x.val, env))
                .collect();
            Augmented {
                range,
                val: ValExpr::CaseOf { expr, cases },
            }
        }
        ast::ValExpr::Block(b) => Augmented {
            range,
            val: translate_blockexpr_val(b.val, env),
        },
        ast::ValExpr::Group(v) => translate_augvalexpr(*v, env),
        ast::ValExpr::Array(items) => Augmented {
            range,
            val: ValExpr::ArrayLiteral(
                items
                    .into_iter()
                    .map(|x| translate_augvalexpr(x, env))
                    .collect(),
            ),
        },
        ast::ValExpr::Identifier(i) => Augmented {
            range,
            val: match env.lookup_val_identifier(i) {
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
            env.type_names_in_scope.push(HashMap::new());
            env.val_names_in_scope.push(HashMap::new());

            // insert typarams into scope
            let typarams = typarams
                .into_iter()
                .map(|x| translate_augtypepatexpr(x, env, false))
                .collect();

            // insert params into scope
            let params = params
                .into_iter()
                .map(|x| translate_augpatexpr(x, env, false))
                .collect();

            let returnty = returnty.map(|rt| Box::new(translate_augtypeexpr(*rt, env)));

            let body = Box::new(translate_augvalexpr(*body, env));

            // end type and val scope
            env.val_names_in_scope.pop();
            env.type_names_in_scope.pop();

            Augmented {
                range,
                val: ValExpr::Generic {
                    params: typarams,
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
            env.type_names_in_scope.push(HashMap::new());
            env.val_names_in_scope.push(HashMap::new());

            // insert params into scope
            let params = params
                .into_iter()
                .map(|x| translate_augpatexpr(x, env, false))
                .collect();

            let returnty = returnty.map(|rt| Box::new(translate_augtypeexpr(*rt, env)));

            let body = Box::new(translate_augvalexpr(*body, env));

            // end type and val scope
            env.val_names_in_scope.pop();
            env.type_names_in_scope.pop();

            let val = ValExpr::FnDef {
                params,
                returnty,
                body,
            };

            Augmented { range, val }
        }
        ast::ValExpr::Concretize { root, tyargs } => Augmented {
            range,
            val: ValExpr::Concretization {
                generic: Box::new(translate_augvalexpr(*root, env)),
                tyargs: tyargs
                    .into_iter()
                    .map(|x| translate_augtypeexpr(x, env))
                    .collect(),
            },
        },
        ast::ValExpr::App { root, args } => Augmented {
            range,
            val: ValExpr::App {
                fun: Box::new(translate_augvalexpr(*root, env)),
                args: args
                    .into_iter()
                    .map(|x| translate_augvalexpr(x, env))
                    .collect(),
            },
        },
        ast::ValExpr::ArrayAccess { root, index } => Augmented {
            range,
            val: ValExpr::ArrayAccess {
                root: Box::new(translate_augvalexpr(*root, env)),
                index: Box::new(translate_augvalexpr(*index, env)),
            },
        },
        ast::ValExpr::FieldAccess { root, field } => Augmented {
            range,
            val: ValExpr::FieldAccess {
                root: Box::new(translate_augvalexpr(*root, env)),
                field,
            },
        },
        ast::ValExpr::New { ty, val } => {
            let ty = Box::new(translate_augtypeexpr(*ty, env));
            let val = Box::new(translate_augvalexpr(*val, env));
            Augmented {
                range,
                val: ValExpr::New { ty, val },
            }
        }
    }
}

fn translate_blockexpr_statement(
    b: ast::BlockExpr,
    env: &mut Environment,
) -> Vec<Augmented<BlockStatement>> {
    // introduce new scope
    env.type_names_in_scope.push(HashMap::new());
    env.val_names_in_scope.push(HashMap::new());
    env.use_prefixes.push(IndexMap::new());
    let statements: Vec<Augmented<BlockStatement>> = b
        .statements
        .into_iter()
        .map(|x| tr_aug(x, env, translate_blockstatement))
        .collect();
    // end scope
    env.type_names_in_scope.pop();
    env.val_names_in_scope.pop();
    env.use_prefixes.pop();

    statements
}

fn translate_blockstatement(bs: ast::BlockStatement, env: &mut Environment) -> BlockStatement {
    match bs {
        ast::BlockStatement::Error => BlockStatement::Error,
        ast::BlockStatement::TypeDef { typat, value } => {
            // first parse value so that we don't accidentally introduce the name of the type before the value
            let value = Box::new(translate_augtypeexpr(*value, env));

            // now introduce name
            let typat = Box::new(translate_augtypepatexpr(*typat, env, false));

            BlockStatement::TypeDef { typat, value }
        }
        ast::BlockStatement::ValDef { pat, value } => {
            // first parse value so that we don't accidentally introduce the name of the val before the value
            let value = Box::new(translate_augvalexpr(*value, env));

            // now introduce name
            let pat = Box::new(translate_augpatexpr(*pat, env, false));

            BlockStatement::ValDef { pat, value }
        }
        ast::BlockStatement::Use {
            prefix: ast::Identifier { range, identifier },
        } => {
            if let Some(prefix) = identifier {
                // duplicate use is a warning, so we can continue
                if let Some(previous_range) = range_if_prefix_dupe(&prefix, &env.use_prefixes) {
                    env.dlogger
                        .log_duplicate_use(range, *previous_range, &prefix);
                    // we need to remove the previous use (if it was in the same scope)
                    env.use_prefixes.last_mut().unwrap().shift_remove(&prefix);
                }
                env.use_prefixes
                    .last_mut()
                    .unwrap()
                    .insert(prefix.clone(), range);
            }
            BlockStatement::Error
        }
        ast::BlockStatement::Set { place, value } => BlockStatement::Set {
            place: Box::new(translate_augvalexpr(*place, env)),
            value: Box::new(translate_augvalexpr(*value, env)),
        },
        ast::BlockStatement::IfThen {
            cond,
            then_branch,
            else_branch,
        } => BlockStatement::IfThen {
            cond: Box::new(translate_augvalexpr(*cond, env)),
            then_branch: translate_blockexpr_statement(then_branch.val, env),
            else_branch: match else_branch {
                Some(else_branch) => translate_augelseexpr(*else_branch, env),
                None => vec![],
            },
        },
        ast::BlockStatement::While { cond, body } => BlockStatement::While {
            cond: Box::new(translate_augvalexpr(*cond, env)),
            body: translate_blockexpr_statement(body.val, env),
        },
        ast::BlockStatement::For {
            pattern,
            range,
            by,
            body,
        } => {
            // evaluate start, end, and by outside the scope
            let start = Box::new(translate_augvalexpr(*range.val.start, env));
            let end = Box::new(translate_augvalexpr(*range.val.end, env));
            let by = by.map(|x| Box::new(translate_augvalexpr(*x, env)));
            // push val scope
            env.val_names_in_scope.push(HashMap::new());
            let pattern = Box::new(translate_augpatexpr(*pattern, env, false));
            // parse body
            let body = translate_blockexpr_statement(body.val, env);
            // pop val scope
            env.val_names_in_scope.pop();
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
        ast::BlockStatement::Do(v) => BlockStatement::Do(Box::new(translate_augvalexpr(*v, env))),
    }
}

fn translate_blockexpr_val(b: ast::BlockExpr, env: &mut Environment) -> ValExpr {
    // introduce new scope
    env.type_names_in_scope.push(HashMap::new());
    env.val_names_in_scope.push(HashMap::new());
    env.use_prefixes.push(IndexMap::new());
    let mut statements: Vec<Augmented<BlockStatement>> = b
        .statements
        .into_iter()
        .map(|x| tr_aug(x, env, translate_blockstatement))
        .collect();
    // end scope
    env.type_names_in_scope.pop();
    env.val_names_in_scope.pop();
    env.use_prefixes.pop();

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
) -> Vec<Augmented<FileStatement>> {
    match val {
        ast::FileStatement::Error => vec![],
        ast::FileStatement::TypeDef { typat, value } => {
            // first parse value so that we don't accidentally introduce the name of the type before the value
            let value = Box::new(translate_augtypeexpr(*value, env));

            // now introduce name
            let typat = Box::new(translate_augtypepatexpr(*typat, env, false));

            vec![Augmented {
                val: FileStatement::TypeDef { typat, value },
                range: range.clone(),
            }]
        }
        ast::FileStatement::ValDef { pat, value } => {
            // first parse value so that we don't accidentally introduce the name of the val before the value
            let value = Box::new(translate_augvalexpr(*value, env));

            // now introduce name
            let pat = Box::new(translate_augpatexpr(*pat, env, false));

            vec![Augmented {
                val: FileStatement::ValDef { pat, value },
                range: range.clone(),
            }]
        }
        ast::FileStatement::Use {
            prefix: ast::Identifier { identifier, range },
        } => {
            if let Some(prefix) = identifier {
                // duplicate use is a warning, so we can continue
                if let Some(previous_range) = range_if_prefix_dupe(&prefix, &env.use_prefixes) {
                    env.dlogger
                        .log_duplicate_use(range, *previous_range, &prefix);
                    // we need to remove the previous use (if it was in the same scope)
                    env.use_prefixes.last_mut().unwrap().shift_remove(&prefix);
                }
                env.use_prefixes
                    .last_mut()
                    .unwrap()
                    .insert(prefix.clone(), range);
            }
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
                // file statements in this block will have this prepended to their names
                env.prefixes.push(identifier.clone());
                // also create new scope for uses, and insert this into it
                env.use_prefixes
                    .push(IndexMap::from([(identifier.clone(), identifier_range)]));
                // translate items
                for item in items {
                    out_items.extend(translate_augfilestatement(item, env));
                }
                // pop scope
                env.use_prefixes.pop();
                // pop prefix
                env.prefixes.pop();
            }
            out_items
        }
    }
}

pub fn construct_hir(
    tu: ast::TranslationUnit,
    dlogger: DiagnosticLogger,
) -> (
    TranslationUnit,
    Vec<String>,
    Vec<Range>,
    Vec<String>,
    Vec<Range>,
) {
    let mut env = Environment {
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
    (
        TranslationUnit {
            declarations: tu
                .declarations
                .into_iter()
                .flat_map(|x| translate_augfilestatement(x, &mut env))
                .collect(),
            phase: HirPhase::NameResolved,
        },
        env.type_name_table,
        env.type_range_table,
        env.val_name_table,
        env.val_range_table,
    )
}
