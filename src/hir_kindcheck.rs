use lsp_types::Range;

use crate::ast::Augmented;
use crate::dlogger::DiagnosticLogger;
use crate::hir;
use crate::types;
use crate::types::KindValue;

// checks kinds of global types
struct GlobalKindChecker<'d, 't> {
    dlogger: &'d mut DiagnosticLogger,
    // these are the global identifier tables
    type_range_table: &'t Vec<Range>,
    type_name_table: &'t Vec<String>,
    type_kind_table: Vec<Option<types::KindValue>>,
}

impl<'d, 't> GlobalKindChecker<'d, 't> {
    fn eval_type_pattern(&mut self, typat: &mut Augmented<hir::TypePatExpr>) -> KindValue {
        match typat.val {
            hir::TypePatExpr::Error => KindValue::Error,
            hir::TypePatExpr::Identifier { .. } => unreachable!("should have been resolved"),
            hir::TypePatExpr::Identifier2 { ref mut kind, .. } => {
                types::evaluate_hir_kind(kind, self.dlogger)
            }
        }
    }

    fn intro_provided_kind(&mut self, typat: &mut Augmented<hir::TypePatExpr>, kindval: KindValue) {
        match typat.val {
            hir::TypePatExpr::Error => unreachable!("should have been resolved"),
            hir::TypePatExpr::Identifier { .. } => unreachable!("should have been resolved"),
            hir::TypePatExpr::Identifier2 { identifier, .. } => {
                self.type_kind_table[identifier] = Some(kindval);
            }
        }
    }
}

impl<'d, 't> hir::HirVisitor for GlobalKindChecker<'d, 't> {
    fn visit_file_statement(&mut self, statement: &mut Augmented<hir::FileStatement>) {
        match statement.val {
            hir::FileStatement::TypeDef {
                ref mut typarams,
                ref mut typat,
                ..
            } => {
                let paramkinds = typarams
                    .iter_mut()
                    .map(|x| self.eval_type_pattern(x))
                    .collect();
                let typekind = self.eval_type_pattern(typat);
                // if it has free type variables, it's a type-level function
                if typarams.len() > 0 {
                    self.intro_provided_kind(
                        typat,
                        types::KindValue::Constructor {
                            paramkinds,
                            returnkind: Box::new(typekind),
                        },
                    );
                } else {
                    self.intro_provided_kind(typat, typekind);
                }
            }
            hir::FileStatement::Prefix { items, .. } => {
                for ref mut statement in items {
                    self.visit_file_statement(statement);
                }
            }
            _ => {}
        }
    }
}

// checks kinds of all type expressions
struct KindChecker<'d, 't> {
    dlogger: &'d mut DiagnosticLogger,
    // these are the global identifier tables
    type_range_table: &'t Vec<Range>,
    type_name_table: &'t Vec<String>,
    type_kind_table: Vec<Option<types::KindValue>>,
}

impl<'d, 't> KindChecker<'d, 't> {
    fn intro_provided_kind(&mut self, typat: &mut Augmented<hir::TypePatExpr>, kindval: KindValue) {
        match typat.val {
            hir::TypePatExpr::Error => {}
            hir::TypePatExpr::Identifier { .. } => unreachable!("should have been resolved"),
            hir::TypePatExpr::Identifier2 { identifier, .. } => {
                self.type_kind_table[identifier] = Some(kindval);
            }
        }
    }
    fn eval_type_pattern(&mut self, typat: &Augmented<hir::TypePatExpr>) -> KindValue {
        match typat.val {
            hir::TypePatExpr::Error => KindValue::Error,
            hir::TypePatExpr::Identifier { .. } => unreachable!("should have been resolved"),
            hir::TypePatExpr::Identifier2 { ref kind, .. } => {
                types::evaluate_hir_kind(kind, self.dlogger)
            }
        }
    }
}

impl<'d, 't> hir::HirVisitor for KindChecker<'d, 't> {
    fn visit_file_statement(&mut self, statement: &mut Augmented<hir::FileStatement>) {
        match statement.val {
            hir::FileStatement::Use { .. } => unreachable!("should have been resolved"),
            hir::FileStatement::TypeDef {
                typarams,
                ref mut value,
                typat,
            } => {
                for ref mut typaram in typarams {
                    self.visit_type_pat_expr(typaram);
                }
                match typat.val {
                    hir::TypePatExpr::Error => {}
                    hir::TypePatExpr::Identifier { .. } => {
                        unreachable!("should have been resolved")
                    }
                    hir::TypePatExpr::Identifier2 { identifier, .. } => {
                        let kind = if typarams.len() == 0 {
                            self.type_kind_table[identifier].unwrap()
                        } else {
                            match self.type_kind_table[identifier].unwrap() {
                                KindValue::Constructor { returnkind, .. } => *returnkind,
                                _ => unreachable!("this should be a type constructor"),
                            }
                        };
                        types::kindcheck_hir_type_checkmode(
                            value,
                            &kind,
                            self.dlogger,
                            &mut self.type_name_table,
                            &mut self.type_kind_table,
                        );
                    }
                }
            }
            _ => self.dfs_visit_file_statement(statement),
        }
    }

    fn visit_block_statement(&mut self, statement: &mut Augmented<hir::BlockStatement>) {
        match statement.val {
            hir::BlockStatement::Use { .. } => unreachable!("should have been resolved"),
            hir::BlockStatement::TypeDef {
                typarams,
                ref mut value,
                typat,
            } => {
                for ref mut typaram in typarams {
                    self.visit_type_pat_expr(typaram);
                }
                match typat.val {
                    hir::TypePatExpr::Error => {}
                    hir::TypePatExpr::Identifier { .. } => {
                        unreachable!("should have been resolved")
                    }
                    hir::TypePatExpr::Identifier2 { identifier, .. } => {
                        let kind = self.type_kind_table[identifier].unwrap();
                        types::kindcheck_hir_type_checkmode(
                            value,
                            &kind,
                            self.dlogger,
                            &mut self.type_name_table,
                            &mut self.type_kind_table,
                        );
                    }
                }
            }
            _ => self.dfs_visit_block_statement(statement),
        }
    }

    fn visit_type_expr(&mut self, expr: &mut Augmented<hir::TypeExpr>) {
        types::kindcheck_hir_type_checkmode(
            expr,
            &types::KindValue::Type,
            self.dlogger,
            &mut self.type_name_table,
            &mut self.type_kind_table,
        );
    }

    fn visit_type_pat_expr(&mut self, expr: &mut Augmented<hir::TypePatExpr>) {
        self.intro_provided_kind(expr, self.eval_type_pattern(expr));
    }
}

pub fn do_kindcheck(
    ast: &mut hir::TranslationUnit,
    type_range_table: &Vec<Range>,
    type_name_table: &Vec<String>,
    ref mut dlogger: DiagnosticLogger,
) -> Vec<types::KindValue> {
    let gkc = GlobalKindChecker {
        dlogger,
        type_range_table,
        type_name_table,
        type_kind_table: Vec::new(),
    };
    hir::HirVisitor::visit_translation_unit(&mut gkc, ast);
    let mut kc = KindChecker {
        dlogger,
        type_range_table,
        type_name_table,
        type_kind_table: gkc.type_kind_table,
    };
    hir::HirVisitor::visit_translation_unit(&mut kc, ast);
    kc.type_kind_table.into_iter().map(|x| x.unwrap()).collect()
}
