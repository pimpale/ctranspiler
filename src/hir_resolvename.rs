use std::collections::HashMap;
use std::collections::HashSet;

use crate::ast::Augmented;
use crate::dlogger::DiagnosticLogger;
use crate::hir;
use crate::types;
use crate::hir::HirVisitor;

struct GlobalNameCollector<'d> {
    dlogger: &'d mut DiagnosticLogger,
    type_names: HashSet<String>,
    val_names: HashSet<String>,
    prefixes: Vec<String>,
}

// visits the AST and collects all the names of global types and values (all these values will be in scope when we start evaluating functions)
impl<'d> HirVisitor for GlobalNameCollector<'d> {
    fn visit_file_statement(&mut self, statement: &mut Augmented<hir::FileStatement>) {
        match statement.val {
            hir::FileStatement::Let {
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
