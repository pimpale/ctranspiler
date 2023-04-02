use std::collections::HashMap;

use crate::ast::Augmented;
use crate::dlogger::DiagnosticLogger;
use crate::hir;
use crate::hir::HirVisitor;

struct ValTableEntry {
    generic_variables: Vec<hir::KindExpr>,
    declared_type: hir::TypeExpr,
}

struct TypeTableEntry {
    declared_kind: hir::KindExpr,
    declared_type: hir::TypeExpr,
}

struct GlobalNameResolver {
    dlogger: DiagnosticLogger,
    val_table: Vec<ValTableEntry>,
    type_table: Vec<TypeTableEntry>,
    val_name_map: HashMap<String, usize>,
    type_name_map: HashMap<String, usize>,
    prefixes: Vec<String>,
}

impl HirVisitor for GlobalNameResolver {
    fn visit_file_statement(&mut self, statement: &mut Augmented<hir::FileStatement>) {
        match statement.val {
            hir::FileStatement::FnDef { identifier, .. } => {
                let identifier = [self.prefixes.concat(), identifier].concat();
                self.val_name_map.insert(identifier, self.val_table.len());
                self.val_table.push(ValTableEntry { declared_type: () });
            }
            hir::FileStatement::Let { ref mut pattern, .. } => {
                self.dfs_visit_pat_expr(pattern);
            } 
            hir::FileStatement::TypeDef { ref mut typepat, .. } => {
                self.visit_type_pat_expr(typepat)
            }
            hir::FileStatement::Prefix { prefix, ref mut items } => {
                self.prefixes.push(prefix.clone());
                for item in items.iter_mut() {
                    self.visit_file_statement(item);
                }
                self.prefixes.pop();
            }
            _ => {}
        }
    }

    fn visit_pat_expr(&mut self, expr: &mut Augmented<hir::PatExpr>) {
        match expr.val {
            hir::PatExpr::Ident { ref mut identifier, .. } => {
                let identifier = [self.prefixes.concat(), identifier.clone()].concat();
                self.val_name_table.push(NameTableEntry { identifier });
            }
            _ => {}
        }
    }
    
}