use std::collections::HashMap;

use crate::ast::Augmented;
use crate::dlogger::DiagnosticLogger;
use crate::hir;
use crate::types;
use crate::hir::HirVisitor;

struct Scope {
    val_table: HashMap<String, usize>,
    type_table: HashMap<String, usize>,
}

struct UnevaluatedTypeTableEntry {
    tyargs: Vec<Augmented<hir::TypePatExpr>>,
    value: Box<Augmented<hir::TypeExpr>>,
}

struct GlobalTypeNameResolver<'d> {
    dlogger: &'d mut DiagnosticLogger,
    type_name_map: HashMap<String, UnevaluatedTypeTableEntry>,
    prefixes: Vec<String>,
}

impl<'d> HirVisitor for GlobalTypeNameResolver<'d> {
    fn visit_file_statement(&mut self, statement: &mut Augmented<hir::FileStatement>) {
        match statement.val {
            hir::FileStatement::TypeDef {
                ref identifier,
                ref tyargs,
                ref value,
            } => {
                let identifier = [self.prefixes.concat(), identifier.clone()].concat();
                if self.type_name_map.contains_key(&identifier) {
                    self.dlogger
                        .log_duplicate_identifier(statement.range, &identifier);
                } else {
                    self.type_name_map.insert(identifier, UnevaluatedTypeTableEntry {
                        tyargs: tyargs.clone(),
                        value: value.clone(),
                    });
                }
            }
            hir::FileStatement::Prefix {
                prefix,
                ref mut items,
            } => {
                self.prefixes.push(prefix.clone());
                for item in items.iter_mut() {
                    self.visit_file_statement(item);
                }
                self.prefixes.pop();
            }
            _ => {}
        }
    }
}

struct GlobalTypeEvaluator<'d> {
    dlogger: &'d mut DiagnosticLogger,
    type_table: Vec<UnevaluatedTypeTableEntry>,
    type_name_map: HashMap<String, usize>,
    prefixes: Vec<String>,
}

pub fn resolve_kinds(ast: &mut hir::TranslationUnit, dlogger: &mut DiagnosticLogger) {
    // get the names and unevaluated type expressions of all the global types
    let mut resolver = GlobalTypeNameResolver {
        dlogger,
        type_name_map: HashMap::new(),
        prefixes: Vec::new(),
    };
    resolver.visit_translation_unit(ast);
    let type_name_map = resolver.type_name_map;
}
