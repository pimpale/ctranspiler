use std::collections::{HashMap, VecDeque};

use indexmap::IndexMap;
use lsp_types::Range;

use crate::ast;
use crate::dlogger::DiagnosticLogger;
use crate::hir::{self, *};

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
) -> IndexMap<String, Augmented<U>> {
    let mut identifier_ranges: IndexMap<String, Range> = IndexMap::new();
    let mut out_items: IndexMap<String, Augmented<U>> = IndexMap::new();
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
                    identifier_ranges.insert(identifier.clone(), identifier_range);
                    out_items.insert(
                        identifier.clone(),
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
                    );
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
                    out_items.insert(identifier, lower(*expr, env));
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
        ast::KindExpr::Error => hir::KindExpr::Error,
        ast::KindExpr::Type => hir::KindExpr::Type,
        ast::KindExpr::Int => hir::KindExpr::Int,
        ast::KindExpr::Float => hir::KindExpr::Float,
        ast::KindExpr::Bool => hir::KindExpr::Bool,
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
            val: hir::TypePatExpr::Error,
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
            val: hir::TypeExpr::Error,
        },
        ast::TypeExpr::Identifier(identifier) => Augmented {
            range,
            val: match env.lookup_type_identifier(identifier) {
                Some(id) => TypeExpr::Identifier(id),
                None => TypeExpr::Error,
            },
        },
        ast::TypeExpr::UnitTy => Augmented {
            range,
            val: TypeExpr::UnitTy,
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
        ast::TypeExpr::UIntConstructorTy => Augmented {
            range,
            val: TypeExpr::UIntConstructorTy,
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

fn translate_caseexpr(c: ast::CaseExpr, env: &mut Environment) -> CaseExpr {
    CaseExpr {
        target: Box::new(tr_aug(*c.target, env, translate_casetargetexpr)),
        body: Box::new(tr_aug(*c.body, env, translate_valexpr)),
    }
}

fn translate_elseexpr(e: ast::ElseExpr, env: &mut Environment) -> ValExpr {
    match e {
        ast::ElseExpr::Error => ValExpr::Error,
        ast::ElseExpr::Else(body) => tr_aug(*body, env, translate_blockexpr),
        ast::ElseExpr::Elif {
            cond,
            then_branch,
            else_branch,
        } => ValExpr::IfThen {
            cond: Box::new(tr_aug(*cond, env, translate_valexpr)),
            then_branch: Box::new(tr_aug(*then_branch, env, translate_blockexpr)),
            else_branch: else_branch.map(|e| Box::new(tr_aug(*e, env, translate_elseexpr))),
        },
    }
}

fn translate_valexpr(v: ast::ValExpr, env: &mut Environment) -> ValExpr {
    match v {
        ast::ValExpr::Error => ValExpr::Error,
        ast::ValExpr::Unit => ValExpr::Unit,
        ast::ValExpr::Int(i) => ValExpr::Int(i),
        ast::ValExpr::Bool(b) => ValExpr::Bool(b),
        ast::ValExpr::Float(f) => ValExpr::Float(f),
        ast::ValExpr::String { value, .. } => ValExpr::String(value),
        ast::ValExpr::Ref(v) => ValExpr::Ref(Box::new(tr_aug(*v, env, translate_valexpr))),
        ast::ValExpr::Deref(v) => ValExpr::Deref(Box::new(tr_aug(*v, env, translate_valexpr))),
        ast::ValExpr::StructLiteral(items) => ValExpr::StructLiteral(translate_augstructitemexpr(
            |x, dlogger| tr_aug(x, dlogger, translate_valexpr),
            |id, env| match env.lookup_val_identifier(id) {
                Some(id) => ValExpr::Identifier(id),
                None => ValExpr::Error,
            },
            env,
            items,
        )),
        ast::ValExpr::BinaryOp {
            op,
            left_operand,
            right_operand,
        } => {
            let left_operand = Box::new(tr_aug(*left_operand, env, translate_valexpr));
            let right_operand = Box::new(tr_aug(*right_operand, env, translate_valexpr));
            match op {
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
            }
        }
        ast::ValExpr::IfThen {
            cond,
            then_branch,
            else_branch,
        } => ValExpr::IfThen {
            cond: Box::new(tr_aug(*cond, env, translate_valexpr)),
            then_branch: Box::new(tr_aug(*then_branch, env, translate_blockexpr)),
            else_branch: else_branch.map(|x| Box::new(tr_aug(*x, env, translate_elseexpr))),
        },
        ast::ValExpr::CaseOf { expr, cases } => {
            let mut cases = cases
                .into_iter()
                .map(|x| tr_aug(x, env, translate_caseexpr))
                .collect::<VecDeque<_>>();
            match cases.pop_front() {
                None => {
                    env.dlogger.log_empty_caseof(expr.range);
                    ValExpr::Error
                }
                Some(first_case) => ValExpr::CaseOf {
                    expr: Box::new(tr_aug(*expr, env, translate_valexpr)),
                    first_case,
                    rest_cases: cases.into(),
                },
            }
        }
        ast::ValExpr::Block(b) => ValExpr::Block(Box::new(tr_aug(*b, env, translate_blockexpr))),
        ast::ValExpr::Group(v) => translate_valexpr(v.val, env),
        ast::ValExpr::Array(items) => ValExpr::ArrayLiteral(
            items
                .into_iter()
                .map(|x| tr_aug(x, env, translate_valexpr))
                .collect(),
        ),
        ast::ValExpr::Identifier(i) => match env.lookup_val_identifier(i) {
            Some(id) => ValExpr::Identifier(id),
            None => ValExpr::Error,
        },
        ast::ValExpr::FnDef {
            typarams,
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

            let body = Box::new(tr_aug(*body, env, translate_valexpr));

            // end type and val scope
            env.val_names_in_scope.pop();
            env.type_names_in_scope.pop();

            ValExpr::FnDef {
                typarams,
                params,
                returnty,
                body,
            }
        }
        ast::ValExpr::Concretize { root, tyargs } => ValExpr::Concretization {
            generic: Box::new(tr_aug(*root, env, translate_valexpr)),
            tyargs: tyargs
                .into_iter()
                .map(|x| translate_augtypeexpr(x, env))
                .collect(),
        },
        ast::ValExpr::App { root, args } => ValExpr::App {
            fun: Box::new(tr_aug(*root, env, translate_valexpr)),
            args: args
                .into_iter()
                .map(|x| tr_aug(x, env, translate_valexpr))
                .collect(),
        },
        ast::ValExpr::ArrayAccess { root, index } => ValExpr::ArrayAccess {
            root: Box::new(tr_aug(*root, env, translate_valexpr)),
            index: Box::new(tr_aug(*index, env, translate_valexpr)),
        },
        ast::ValExpr::FieldAccess { root, field } => ValExpr::FieldAccess {
            root: Box::new(tr_aug(*root, env, translate_valexpr)),
            field,
        },
    }
}

fn translate_blockstatement(bs: ast::BlockStatement, env: &mut Environment) -> BlockStatement {
    match bs {
        ast::BlockStatement::Error => BlockStatement::NoOp,
        ast::BlockStatement::TypeDef { typat, value } => {
            // first parse value so that we don't accidentally introduce the name of the type before the value
            let value = Box::new(translate_augtypeexpr(*value, env));

            // now introduce name
            let typat = Box::new(translate_augtypepatexpr(*typat, env, false));

            BlockStatement::TypeDef { typat, value }
        }
        ast::BlockStatement::ValDef { pat, value } => {
            // first parse value so that we don't accidentally introduce the name of the val before the value
            let value = Box::new(tr_aug(*value, env, translate_valexpr));

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
            hir::BlockStatement::NoOp
        }
        ast::BlockStatement::Set { place, value } => BlockStatement::Set {
            place: Box::new(tr_aug(*place, env, translate_valexpr)),
            value: Box::new(tr_aug(*value, env, translate_valexpr)),
        },
        ast::BlockStatement::While { cond, body } => BlockStatement::While {
            cond: Box::new(tr_aug(*cond, env, translate_valexpr)),
            body: Box::new(tr_aug(*body, env, translate_blockexpr)),
        },
        ast::BlockStatement::For {
            pattern,
            range,
            by,
            body,
        } => {
            // evaluate start, end, and by outside the scope
            let start = Box::new(tr_aug(*range.val.start, env, translate_valexpr));
            let end = Box::new(tr_aug(*range.val.end, env, translate_valexpr));
            let by = by.map(|x| Box::new(tr_aug(*x, env, translate_valexpr)));
            // push val scope
            env.val_names_in_scope.push(HashMap::new());
            let pattern = Box::new(translate_augpatexpr(*pattern, env, false));
            // parse body
            let body = Box::new(tr_aug(*body, env, translate_blockexpr));
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
        ast::BlockStatement::Do(v) => {
            BlockStatement::Do(Box::new(tr_aug(*v, env, translate_valexpr)))
        }
    }
}

fn translate_blockexpr(b: ast::BlockExpr, env: &mut Environment) -> ValExpr {
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
        }) if !b.trailing_semicolon => ValExpr::BlockExpr {
            statements,
            last_expression: Some(*v),
        },
        Some(s) => {
            statements.push(s);
            ValExpr::BlockExpr {
                statements,
                last_expression: None,
            }
        }
        None => ValExpr::BlockExpr {
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
            let value = Box::new(tr_aug(*value, env, translate_valexpr));

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
