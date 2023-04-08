use indexmap::IndexMap;

use crate::ast;
use crate::ast::Augmented;
use crate::dlogger::DiagnosticLogger;
use crate::hir::{self, *};

fn tr_aug<T, U>(
    x: Augmented<T>,
    dlogger: &mut DiagnosticLogger,
    f: impl Fn(T, &mut DiagnosticLogger) -> U,
) -> Augmented<U> {
    Augmented {
        range: x.range,
        metadata: x.metadata,
        val: f(x.val, dlogger),
    }
}

fn translate_augstructitemexpr<T, U>(
    lower: fn(Augmented<T>, &mut DiagnosticLogger) -> Augmented<U>,
    replace_eponymous: impl Fn(String, &mut DiagnosticLogger) -> U,
    dlogger: &mut DiagnosticLogger,
    items: Vec<Augmented<ast::StructItemExpr<T>>>,
) -> IndexMap<String, Augmented<U>> {
    let mut out_items: IndexMap<String, Augmented<U>> = IndexMap::new();
    for Augmented {
        range,
        metadata,
        val,
    } in items
    {
        match val {
            ast::StructItemExpr::Error => {}
            ast::StructItemExpr::Eponymous(identifier) => {
                if let Some(preexisting) = out_items.get(&identifier) {
                    dlogger.log_duplicate_field_name(range, preexisting.range, &identifier);
                } else {
                    out_items.insert(
                        identifier.clone(),
                        Augmented {
                            metadata,
                            range,
                            val: replace_eponymous(identifier, dlogger),
                        },
                    );
                }
            }
            ast::StructItemExpr::Identified { identifier, expr } => {
                if let Some(preexisting) = out_items.get(&identifier) {
                    dlogger.log_duplicate_field_name(range, preexisting.range, &identifier);
                } else {
                    out_items.insert(identifier, lower(*expr, dlogger));
                }
            }
        }
    }
    out_items
}

fn translate_kindexpr(k: ast::KindExpr, dlogger: &mut DiagnosticLogger) -> KindExpr {
    match k {
        ast::KindExpr::Error => hir::KindExpr::Error,
        ast::KindExpr::Type => hir::KindExpr::Type,
        ast::KindExpr::Int => hir::KindExpr::Int,
        ast::KindExpr::Float => hir::KindExpr::Float,
        ast::KindExpr::Bool => hir::KindExpr::Bool,
        ast::KindExpr::GenericFn { args, returnkind } => KindExpr::TypeLevelFn {
            args: args
                .into_iter()
                .map(|x| tr_aug(x, dlogger, translate_kindexpr))
                .collect(),
            returnkind: Box::new(tr_aug(*returnkind, dlogger, translate_kindexpr)),
        },
    }
}

fn translate_augtypepatexpr(
    Augmented {
        range,
        metadata,
        val,
    }: Augmented<ast::TypePatExpr>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<TypePatExpr> {
    match val {
        ast::TypePatExpr::Error => Augmented {
            range,
            metadata,
            val: hir::TypePatExpr::Error,
        },
        ast::TypePatExpr::Identifier { identifier, kind } => Augmented {
            range,
            metadata,
            val: TypePatExpr::Identifier {
                identifier,
                kind: Box::new(match kind {
                    Some(kind) => tr_aug(*kind, dlogger, translate_kindexpr),
                    None => Augmented {
                        range,
                        metadata: vec![],
                        val: KindExpr::Type,
                    },
                }),
            },
        },
    }
}

fn translate_augtypeexpr(
    Augmented {
        range,
        metadata,
        val,
    }: Augmented<ast::TypeExpr>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<TypeExpr> {
    match val {
        ast::TypeExpr::Error => Augmented {
            range,
            metadata,
            val: hir::TypeExpr::Error,
        },
        ast::TypeExpr::Identifier(identifier) => Augmented {
            range,
            metadata,
            val: TypeExpr::Identifier(identifier),
        },
        ast::TypeExpr::UnitTy => Augmented {
            range,
            metadata,
            val: TypeExpr::UnitTy,
        },
        ast::TypeExpr::BoolTy => Augmented {
            range,
            metadata,
            val: TypeExpr::BoolTy,
        },
        ast::TypeExpr::ArrayTy => Augmented {
            range,
            metadata,
            val: TypeExpr::ArrayConstructorTy,
        },
        ast::TypeExpr::SliceTy => Augmented {
            range,
            metadata,
            val: TypeExpr::SliceConstructorTy,
        },
        ast::TypeExpr::IntTy => Augmented {
            range,
            metadata,
            val: TypeExpr::IntConstructorTy,
        },
        ast::TypeExpr::UIntTy => Augmented {
            range,
            metadata,
            val: TypeExpr::UIntConstructorTy,
        },
        ast::TypeExpr::FloatTy => Augmented {
            range,
            metadata,
            val: TypeExpr::FloatConstructorTy,
        },
        ast::TypeExpr::Int(i) => Augmented {
            range,
            metadata,
            val: TypeExpr::Int(i),
        },
        ast::TypeExpr::Bool(b) => Augmented {
            range,
            metadata,
            val: TypeExpr::Bool(b),
        },
        ast::TypeExpr::Float(f) => Augmented {
            range,
            metadata,
            val: TypeExpr::Float(f),
        },
        ast::TypeExpr::Ref(t) => Augmented {
            range,
            metadata,
            val: TypeExpr::Concretization {
                genericty: Box::new(Augmented {
                    range,
                    metadata: vec![],
                    val: TypeExpr::RefConstructorTy,
                }),
                tyargs: vec![translate_augtypeexpr(*t, dlogger)],
            },
        },
        ast::TypeExpr::Struct(items) => Augmented {
            range,
            metadata,
            val: TypeExpr::Struct(translate_augstructitemexpr(
                translate_augtypeexpr,
                |e, _| TypeExpr::Identifier(e),
                dlogger,
                items,
            )),
        },
        ast::TypeExpr::Enum(items) => Augmented {
            range,
            metadata,
            val: TypeExpr::Enum(translate_augstructitemexpr(
                translate_augtypeexpr,
                |e, _| TypeExpr::Identifier(e),
                dlogger,
                items,
            )),
        },
        ast::TypeExpr::Union(items) => Augmented {
            range,
            metadata,
            val: TypeExpr::Union(translate_augstructitemexpr(
                translate_augtypeexpr,
                |e, _| TypeExpr::Identifier(e),
                dlogger,
                items,
            )),
        },
        ast::TypeExpr::Group(t) => translate_augtypeexpr(*t, dlogger),
        ast::TypeExpr::Generic { fun, args } => Augmented {
            range,
            metadata,
            val: TypeExpr::Concretization {
                genericty: Box::new(translate_augtypeexpr(*fun, dlogger)),
                tyargs: args
                    .val
                    .args
                    .into_iter()
                    .map(|x| translate_augtypeexpr(x, dlogger))
                    .collect(),
            },
        },
        ast::TypeExpr::Fn { paramtys, tyreturn } => Augmented {
            range,
            metadata,
            val: TypeExpr::Fn {
                paramtys: paramtys
                    .val
                    .args
                    .into_iter()
                    .map(|x| translate_augtypeexpr(x, dlogger))
                    .collect(),
                tyreturn: Box::new(translate_augtypeexpr(*tyreturn, dlogger)),
            },
        },
    }
}

fn translate_patexpr(p: ast::PatExpr, dlogger: &mut DiagnosticLogger) -> PatExpr {
    match p {
        ast::PatExpr::Error => hir::PatExpr::Error,
        ast::PatExpr::Ignore => PatExpr::Ignore,
        ast::PatExpr::Identifier {
            identifier,
            mutable,
        } => PatExpr::Identifier {
            identifier,
            mutable,
        },
        ast::PatExpr::StructLiteral(items) => PatExpr::StructLiteral(translate_augstructitemexpr(
            |x, dlogger| tr_aug(x, dlogger, translate_patexpr),
            |e, _| PatExpr::Identifier {
                mutable: false,
                identifier: e,
            },
            dlogger,
            items,
        )),
        ast::PatExpr::Typed { pat, ty } => PatExpr::Typed {
            pat: Box::new(tr_aug(*pat, dlogger, translate_patexpr)),
            ty: Box::new(translate_augtypeexpr(*ty, dlogger)),
        },
    }
}

fn translate_casetargetexpr(
    c: ast::CaseTargetExpr,
    dlogger: &mut DiagnosticLogger,
) -> CaseTargetExpr {
    match c {
        ast::CaseTargetExpr::Error => CaseTargetExpr::Error,
        ast::CaseTargetExpr::Unit => CaseTargetExpr::Unit,
        ast::CaseTargetExpr::Bool(b) => CaseTargetExpr::Bool(b),
        ast::CaseTargetExpr::Int(i) => CaseTargetExpr::Int(i),
        ast::CaseTargetExpr::PatExpr(pat) => {
            CaseTargetExpr::PatExpr(Box::new(tr_aug(*pat, dlogger, translate_patexpr)))
        }
    }
}

fn translate_caseexpr(c: ast::CaseExpr, dlogger: &mut DiagnosticLogger) -> CaseExpr {
    CaseExpr {
        target: Box::new(tr_aug(*c.target, dlogger, translate_casetargetexpr)),
        body: Box::new(tr_aug(*c.body, dlogger, translate_valexpr)),
    }
}

fn translate_elseexpr(e: ast::ElseExpr, dlogger: &mut DiagnosticLogger) -> ElseExpr {
    match e {
        ast::ElseExpr::Error => ElseExpr::Error,
        ast::ElseExpr::Else(body) => {
            ElseExpr::Else(Box::new(tr_aug(*body, dlogger, translate_blockexpr)))
        }
        ast::ElseExpr::Elif {
            cond,
            then_branch,
            else_branch,
        } => ElseExpr::Elif {
            cond: Box::new(tr_aug(*cond, dlogger, translate_valexpr)),
            then_branch: Box::new(tr_aug(*then_branch, dlogger, translate_blockexpr)),
            else_branch: else_branch.map(|e| Box::new(tr_aug(*e, dlogger, translate_elseexpr))),
        },
    }
}

fn translate_valexpr(v: ast::ValExpr, dlogger: &mut DiagnosticLogger) -> ValExpr {
    match v {
        ast::ValExpr::Error => ValExpr::Error,
        ast::ValExpr::Unit => ValExpr::Unit,
        ast::ValExpr::Int(i) => ValExpr::Int(i),
        ast::ValExpr::Bool(b) => ValExpr::Bool(b),
        ast::ValExpr::Float(f) => ValExpr::Float(f),
        ast::ValExpr::String { value, .. } => ValExpr::String(value),
        ast::ValExpr::Ref(v) => ValExpr::Ref(Box::new(tr_aug(*v, dlogger, translate_valexpr))),
        ast::ValExpr::Deref(v) => ValExpr::Deref(Box::new(tr_aug(*v, dlogger, translate_valexpr))),
        ast::ValExpr::StructLiteral(items) => ValExpr::StructLiteral(translate_augstructitemexpr(
            |x, dlogger| tr_aug(x, dlogger, translate_valexpr),
            |e, _| ValExpr::Identifier(e),
            dlogger,
            items,
        )),
        ast::ValExpr::BinaryOp {
            op,
            left_operand,
            right_operand,
        } => ValExpr::BinaryOp {
            op: match op {
                ast::ValBinaryOpKind::Pipe => ValBinaryOpKind::Pipe,
                ast::ValBinaryOpKind::Add => ValBinaryOpKind::Add,
                ast::ValBinaryOpKind::Sub => ValBinaryOpKind::Sub,
                ast::ValBinaryOpKind::Mul => ValBinaryOpKind::Mul,
                ast::ValBinaryOpKind::Div => ValBinaryOpKind::Div,
                ast::ValBinaryOpKind::Rem => ValBinaryOpKind::Rem,
                ast::ValBinaryOpKind::And => ValBinaryOpKind::And,
                ast::ValBinaryOpKind::Or => ValBinaryOpKind::Or,
                ast::ValBinaryOpKind::Equal => ValBinaryOpKind::Eq,
                ast::ValBinaryOpKind::NotEqual => ValBinaryOpKind::Neq,
                ast::ValBinaryOpKind::Less => ValBinaryOpKind::Lt,
                ast::ValBinaryOpKind::Greater => ValBinaryOpKind::Gt,
                ast::ValBinaryOpKind::LessEqual => ValBinaryOpKind::Leq,
                ast::ValBinaryOpKind::GreaterEqual => ValBinaryOpKind::Geq,
            },
            left_operand: Box::new(tr_aug(*left_operand, dlogger, translate_valexpr)),
            right_operand: Box::new(tr_aug(*right_operand, dlogger, translate_valexpr)),
        },
        ast::ValExpr::IfThen {
            cond,
            then_branch,
            else_branch,
        } => ValExpr::IfThen {
            cond: Box::new(tr_aug(*cond, dlogger, translate_valexpr)),
            then_branch: Box::new(tr_aug(*then_branch, dlogger, translate_blockexpr)),
            else_branch: else_branch.map(|x| Box::new(tr_aug(*x, dlogger, translate_elseexpr))),
        },
        ast::ValExpr::CaseOf { expr, cases } => ValExpr::CaseOf {
            expr: Box::new(tr_aug(*expr, dlogger, translate_valexpr)),
            cases: cases
                .into_iter()
                .map(|x| tr_aug(x, dlogger, translate_caseexpr))
                .collect(),
        },
        ast::ValExpr::Block(b) => {
            ValExpr::Block(Box::new(tr_aug(*b, dlogger, translate_blockexpr)))
        }
        ast::ValExpr::Group(v) => translate_valexpr(v.val, dlogger),
        ast::ValExpr::Array(items) => ValExpr::ArrayLiteral(
            items
                .into_iter()
                .map(|x| tr_aug(x, dlogger, translate_valexpr))
                .collect(),
        ),
        ast::ValExpr::Identifier(i) => ValExpr::Identifier(i),
        ast::ValExpr::Concretize { root, tyargs } => ValExpr::Concretization {
            generic: Box::new(tr_aug(*root, dlogger, translate_valexpr)),
            tyargs: tyargs
                .val
                .args
                .into_iter()
                .map(|x| translate_augtypeexpr(x, dlogger))
                .collect(),
        },
        ast::ValExpr::App { root, args } => ValExpr::App {
            fun: Box::new(tr_aug(*root, dlogger, translate_valexpr)),
            args: args
                .val
                .args
                .into_iter()
                .map(|x| tr_aug(x, dlogger, translate_valexpr))
                .collect(),
        },
        ast::ValExpr::ArrayAccess { root, index } => ValExpr::ArrayAccess {
            root: Box::new(tr_aug(*root, dlogger, translate_valexpr)),
            index: Box::new(tr_aug(*index, dlogger, translate_valexpr)),
        },
        ast::ValExpr::FieldAccess { root, field } => ValExpr::FieldAccess {
            root: Box::new(tr_aug(*root, dlogger, translate_valexpr)),
            field,
        },
        ast::ValExpr::FnDef {
            params,
            returnty,
            body,
        } => ValExpr::FnLiteral {
            params: params
                .val
                .args
                .into_iter()
                .map(|x| tr_aug(x, dlogger, translate_patexpr))
                .collect(),
            returnty: Box::new(translate_augtypeexpr(*returnty, dlogger)),
            body: Box::new(tr_aug(*body, dlogger, translate_blockexpr)),
        },
    }
}

fn translate_blockstatement(
    bs: ast::BlockStatement,
    dlogger: &mut DiagnosticLogger,
) -> BlockStatement {
    match bs {
        ast::BlockStatement::Error => BlockStatement::NoOp,
        ast::BlockStatement::TypeDef {
            typarams,
            typat,
            value,
        } => BlockStatement::TypeDef {
            typarams: match typarams {
                Some(typarams) => typarams
                    .val
                    .args
                    .into_iter()
                    .map(|x| translate_augtypepatexpr(x, dlogger))
                    .collect(),
                None => vec![],
            },
            typat: Box::new(translate_augtypepatexpr(*typat, dlogger)),
            value: Box::new(translate_augtypeexpr(*value, dlogger)),
        },
        ast::BlockStatement::Use { prefix } => BlockStatement::Use { prefix },
        ast::BlockStatement::Let {
            typarams,
            pat,
            value,
        } => BlockStatement::Let {
            typarams: match typarams {
                Some(typarams) => typarams
                    .val
                    .args
                    .into_iter()
                    .map(|x| translate_augtypepatexpr(x, dlogger))
                    .collect(),
                None => vec![],
            },
            pat: Box::new(tr_aug(*pat, dlogger, translate_patexpr)),
            value: Box::new(tr_aug(*value, dlogger, translate_valexpr)),
        },
        ast::BlockStatement::Set { place, value } => BlockStatement::Set {
            place: Box::new(tr_aug(*place, dlogger, translate_valexpr)),
            value: Box::new(tr_aug(*value, dlogger, translate_valexpr)),
        },
        ast::BlockStatement::While { cond, body } => BlockStatement::While {
            cond: Box::new(tr_aug(*cond, dlogger, translate_valexpr)),
            body: Box::new(tr_aug(*body, dlogger, translate_blockexpr)),
        },
        ast::BlockStatement::For {
            pattern,
            range,
            by,
            body,
        } => BlockStatement::For {
            pattern: Box::new(tr_aug(*pattern, dlogger, translate_patexpr)),
            start: Box::new(tr_aug(*range.val.start, dlogger, translate_valexpr)),
            end: Box::new(tr_aug(*range.val.end, dlogger, translate_valexpr)),
            by: by.map(|x| Box::new(tr_aug(*x, dlogger, translate_valexpr))),
            inclusive: range.val.inclusive,
            body: Box::new(tr_aug(*body, dlogger, translate_blockexpr)),
        },
        ast::BlockStatement::Do(v) => {
            BlockStatement::Do(Box::new(tr_aug(*v, dlogger, translate_valexpr)))
        }
    }
}

fn translate_blockexpr(b: ast::BlockExpr, dlogger: &mut DiagnosticLogger) -> BlockExpr {
    BlockExpr {
        statements: b
            .statements
            .into_iter()
            .map(|x| tr_aug(x, dlogger, translate_blockstatement))
            .collect(),
        trailing_semicolon: b.trailing_semicolon,
    }
}

fn translate_filestatement(
    fs: ast::FileStatement,
    dlogger: &mut DiagnosticLogger,
) -> FileStatement {
    match fs {
        ast::FileStatement::Error => FileStatement::NoOp,
        ast::FileStatement::TypeDef {
            tyargs,
            typat,
            value,
        } => FileStatement::TypeDef {
            tyargs: match tyargs {
                Some(tyargs) => tyargs
                    .val
                    .args
                    .into_iter()
                    .map(|x| translate_augtypepatexpr(x, dlogger))
                    .collect(),
                None => vec![],
            },
            typat: Box::new(translate_augtypepatexpr(*typat, dlogger)),
            value: Box::new(translate_augtypeexpr(*value, dlogger)),
        },
        ast::FileStatement::Let { tyargs, pat, value } => FileStatement::Let {
            tyargs: match tyargs {
                Some(tyargs) => tyargs
                    .val
                    .args
                    .into_iter()
                    .map(|x| translate_augtypepatexpr(x, dlogger))
                    .collect(),
                None => vec![],
            },
            pat: Box::new(tr_aug(*pat, dlogger, translate_patexpr)),
            value: Box::new(tr_aug(*value, dlogger, translate_valexpr)),
        },
        ast::FileStatement::Prefix { prefix, items } => FileStatement::Prefix {
            prefix,
            items: items
                .into_iter()
                .map(|x| tr_aug(x, dlogger, translate_filestatement))
                .collect(),
        },
        ast::FileStatement::Use { prefix } => FileStatement::Use { prefix },
    }
}

pub fn construct_hir(
    tu: ast::TranslationUnit,
    ref mut dlogger: DiagnosticLogger,
) -> TranslationUnit {
    TranslationUnit {
        declarations: tu
            .declarations
            .into_iter()
            .map(|x| tr_aug(x, dlogger, translate_filestatement))
            .collect(),
        phase: HirPhase::Raw,
    }
}
