use crate::ast;
use crate::ast::Augmented;
use crate::dlogger::DiagnosticLogger;
use crate::hir::{self, *};

fn tr_aug<T, U>(
    x: Augmented<T>,
    dlogger: DiagnosticLogger,
    f: impl Fn(T, DiagnosticLogger) -> U,
) -> Augmented<U> {
    Augmented {
        range: x.range,
        metadata: x.metadata,
        val: f(x.val, dlogger),
    }
}

fn translate_augstructitemexpr<T, U>(
    lower: fn(T, DiagnosticLogger) -> U,
    replace_eponymous: impl Fn(String, DiagnosticLogger) -> U,
    dlogger: DiagnosticLogger,
    Augmented {
        range,
        metadata,
        val,
    }: Augmented<ast::StructItemExpr<T>>,
) -> Augmented<hir::StructItemExpr<U>> {
    match val {
        ast::StructItemExpr::Error => Augmented {
            metadata,
            range,
            val: StructItemExpr::Error,
        },
        ast::StructItemExpr::Eponymous(identifier) => Augmented {
            metadata,
            range,
            val: hir::StructItemExpr::Identified {
                identifier: identifier.clone(),
                expr: Box::new(Augmented {
                    metadata: vec![],
                    range,
                    val: replace_eponymous(identifier, dlogger),
                }),
            },
        },
        ast::StructItemExpr::Identified { identifier, expr } => Augmented {
            range,
            metadata,
            val: StructItemExpr::Identified {
                identifier,
                expr: Box::new(tr_aug(*expr, dlogger, lower)),
            },
        },
    }
}

fn translate_kindexpr(k: ast::KindExpr, dlogger: DiagnosticLogger) -> KindExpr {
    match k {
        ast::KindExpr::Error => hir::KindExpr::Error,
        ast::KindExpr::Type => hir::KindExpr::Type,
        ast::KindExpr::Int => hir::KindExpr::Int,
        ast::KindExpr::UInt => hir::KindExpr::UInt,
        ast::KindExpr::Float => hir::KindExpr::Float,
        ast::KindExpr::Bool => hir::KindExpr::Bool,
        ast::KindExpr::GenericFn { args, returnkind } => KindExpr::GenericFn {
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
    dlogger: DiagnosticLogger,
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

fn translate_typeexpr(t: ast::TypeExpr, dlogger: DiagnosticLogger) -> TypeExpr {
    match t {
        ast::TypeExpr::Error => hir::TypeExpr::Error,
        ast::TypeExpr::Identifier(identifier) => TypeExpr::Identifier(identifier),
        ast::TypeExpr::UnitTy => TypeExpr::UnitTy,
        ast::TypeExpr::ArrayTy => TypeExpr::ArrayTy,
        ast::TypeExpr::SliceTy => TypeExpr::SliceTy,
        ast::TypeExpr::IntTy => TypeExpr::IntTy,
        ast::TypeExpr::UIntTy => TypeExpr::UIntTy,
        ast::TypeExpr::FloatTy => TypeExpr::FloatTy,
        ast::TypeExpr::BoolTy => TypeExpr::BoolTy,
        ast::TypeExpr::Int(i) => TypeExpr::Int(i),
        ast::TypeExpr::Bool(b) => TypeExpr::Bool(b),
        ast::TypeExpr::Float(f) => TypeExpr::Float(f),
        ast::TypeExpr::Ref(t) => TypeExpr::Ref(Box::new(tr_aug(*t, dlogger, translate_typeexpr))),
        ast::TypeExpr::Struct(items) => TypeExpr::Struct(
            items
                .into_iter()
                .map(|x| {
                    translate_augstructitemexpr(
                        translate_typeexpr,
                        |e, _| TypeExpr::Identifier(e),
                        dlogger,
                        x,
                    )
                })
                .collect(),
        ),
        ast::TypeExpr::Enum(items) => TypeExpr::Enum(
            items
                .into_iter()
                .map(|x| {
                    translate_augstructitemexpr(
                        translate_typeexpr,
                        |e, _| TypeExpr::Identifier(e),
                        dlogger,
                        x,
                    )
                })
                .collect(),
        ),
        ast::TypeExpr::Union(items) => TypeExpr::Union(
            items
                .into_iter()
                .map(|x| {
                    translate_augstructitemexpr(
                        translate_typeexpr,
                        |e, _| TypeExpr::Identifier(e),
                        dlogger,
                        x,
                    )
                })
                .collect(),
        ),
        ast::TypeExpr::Group(t) => translate_typeexpr(t.val, dlogger),
        ast::TypeExpr::Generic { fun, args } => TypeExpr::Generic {
            fun: Box::new(tr_aug(*fun, dlogger, translate_typeexpr)),
            args: args
                .val
                .args
                .into_iter()
                .map(|x| tr_aug(x, dlogger, translate_typeexpr))
                .collect(),
        },
        ast::TypeExpr::Fn { args, returntype } => TypeExpr::Fn {
            args: args
                .val
                .args
                .into_iter()
                .map(|x| tr_aug(x, dlogger, translate_typeexpr))
                .collect(),
            returntype: Box::new(tr_aug(*returntype, dlogger, translate_typeexpr)),
        },
    }
}

fn translate_patexpr(p: ast::PatExpr, dlogger: DiagnosticLogger) -> PatExpr {
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
        ast::PatExpr::StructLiteral(items) => PatExpr::StructLiteral(
            items
                .into_iter()
                .map(|x| {
                    translate_augstructitemexpr(
                        translate_patexpr,
                        |e, _| PatExpr::Identifier {
                            mutable: false,
                            identifier: e,
                        },
                        dlogger,
                        x,
                    )
                })
                .collect(),
        ),
        ast::PatExpr::Typed { pat, ty } => PatExpr::Typed {
            pat: Box::new(tr_aug(*pat, dlogger, translate_patexpr)),
            ty: Box::new(tr_aug(*ty, dlogger, translate_typeexpr)),
        },
    }
}

fn translate_caseexpr(c: ast::CaseExpr, dlogger: DiagnosticLogger) -> CaseExpr {
    CaseExpr {
        target: c.target,
        body: Box::new(tr_aug(*c.body, dlogger, translate_valexpr)),
    }
}

fn translate_elseexpr(e: ast::ElseExpr, dlogger: DiagnosticLogger) -> ElseExpr {
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

fn translate_valexpr(v: ast::ValExpr, dlogger: DiagnosticLogger) -> ValExpr {
    match v {
        ast::ValExpr::Error => ValExpr::Error,
        ast::ValExpr::Unit => ValExpr::Unit,
        ast::ValExpr::Int(i) => ValExpr::Int(i),
        ast::ValExpr::Bool(b) => ValExpr::Bool(b),
        ast::ValExpr::Float(f) => ValExpr::Float(f),
        ast::ValExpr::String { value, .. } => ValExpr::String(value),
        ast::ValExpr::Ref(v) => ValExpr::Ref(Box::new(tr_aug(*v, dlogger, translate_valexpr))),
        ast::ValExpr::Deref(v) => ValExpr::Deref(Box::new(tr_aug(*v, dlogger, translate_valexpr))),
        ast::ValExpr::StructLiteral(items) => ValExpr::StructLiteral(
            items
                .into_iter()
                .map(|x| {
                    translate_augstructitemexpr(
                        translate_valexpr,
                        |e, _| ValExpr::Identifier(e),
                        dlogger,
                        x,
                    )
                })
                .collect(),
        ),
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
        ast::ValExpr::Concretize { root, tyargs } => ValExpr::GenericFnConcretization {
            fun: Box::new(tr_aug(*root, dlogger, translate_valexpr)),
            args: tyargs
                .val
                .args
                .into_iter()
                .map(|x| tr_aug(x, dlogger, translate_typeexpr))
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
            args,
            returntype,
            body,
        } => ValExpr::FnLiteral {
            args: args
                .val
                .args
                .into_iter()
                .map(|x| tr_aug(x, dlogger, translate_patexpr))
                .collect(),
            returntype: Box::new(tr_aug(*returntype, dlogger, translate_typeexpr)),
            body: Box::new(tr_aug(*body, dlogger, translate_valexpr)),
        },
    }
}

fn translate_blockstatement(bs: ast::BlockStatement, dlogger: DiagnosticLogger) -> BlockStatement {
    match bs {
        ast::BlockStatement::Error => BlockStatement::Error,
        ast::BlockStatement::TypeDef { typepat, value } => BlockStatement::TypeDef {
            typepat: Box::new(translate_augtypepatexpr(*typepat, dlogger)),
            value: Box::new(tr_aug(*value, dlogger, translate_typeexpr)),
        },
        ast::BlockStatement::Use { prefix } => BlockStatement::Use { prefix },
        ast::BlockStatement::Let { pattern, value } => BlockStatement::Let {
            pattern: Box::new(tr_aug(*pattern, dlogger, translate_patexpr)),
            value: Box::new(tr_aug(*value, dlogger, translate_valexpr)),
        },
        ast::BlockStatement::FnDef {
            identifier,
            tyargs,
            args,
            returntype,
            body,
        } => BlockStatement::FnDef {
            identifier,
            tyargs: match tyargs {
                Some(tyargs) => tyargs
                    .val
                    .args
                    .into_iter()
                    .map(|x| translate_augtypepatexpr(x, dlogger))
                    .collect(),
                None => vec![],
            },
            args: args
                .val
                .args
                .into_iter()
                .map(|x| tr_aug(x, dlogger, translate_patexpr))
                .collect(),
            returntype: Box::new(tr_aug(*returntype, dlogger, translate_typeexpr)),
            body: Box::new(tr_aug(*body, dlogger, translate_blockexpr)),
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

fn translate_blockexpr(b: ast::BlockExpr, dlogger: DiagnosticLogger) -> BlockExpr {
    BlockExpr {
        statements: b
            .statements
            .into_iter()
            .map(|x| tr_aug(x, dlogger, translate_blockstatement))
            .collect(),
        trailing_semicolon: b.trailing_semicolon,
    }
}

fn translate_filestatement(fs: ast::FileStatement, dlogger: DiagnosticLogger) -> FileStatement {
    match fs {
        ast::FileStatement::Error => FileStatement::Error,
        ast::FileStatement::TypeDef { typepat, value } => FileStatement::TypeDef {
            typepat: Box::new(translate_augtypepatexpr(*typepat, dlogger)),
            value: Box::new(tr_aug(*value, dlogger, translate_typeexpr)),
        },
        ast::FileStatement::Let { pattern, value } => FileStatement::Let {
            pattern: Box::new(tr_aug(*pattern, dlogger, translate_patexpr)),
            value: Box::new(tr_aug(*value, dlogger, translate_valexpr)),
        },
        ast::FileStatement::FnDef {
            identifier,
            tyargs,
            args,
            returntype,
            body,
        } => FileStatement::FnDef {
            identifier,
            tyargs: match tyargs {
                Some(tyargs) => tyargs
                    .val
                    .args
                    .into_iter()
                    .map(|x| translate_augtypepatexpr(x, dlogger))
                    .collect(),
                None => vec![],
            },
            args: args
                .val
                .args
                .into_iter()
                .map(|x| tr_aug(x, dlogger, translate_patexpr))
                .collect(),
            returntype: Box::new(tr_aug(*returntype, dlogger, translate_typeexpr)),
            body: Box::new(tr_aug(*body, dlogger, translate_blockexpr)),
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

pub fn construct_hir(tu: ast::TranslationUnit, dlogger: DiagnosticLogger) -> TranslationUnit {
    TranslationUnit {
        declarations: tu
            .declarations
            .into_iter()
            .map(|x| tr_aug(x, dlogger, translate_filestatement))
            .collect(),
        phase: HirPhase::Raw,
    }
}
