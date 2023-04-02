use crate::ast::Augmented;
use crate::dlogger::DiagnosticLogger;
use crate::hir;
use crate::hir::HirVisitor;

// first, identify the names of all the types
// then, resolve 

struct TypeNameTableEntry {
    identifier: String,
    type_name: String,
}

struct TypeNameResolver {
    dlogger: DiagnosticLogger,
    name_table: Vec<NameTableEntry>,
    prefixes: Vec<Vec<u8>>,
}

impl HirVisitor for GlobalNameResolver {
    fn visit_file_statement(&mut self, statement: &mut Augmented<hir::FileStatement>) {
        match  statement.val {
            hir::FileStatement::FnDef { identifier, args, returntype, body } => {
                self.name_table.push(NameTableEntry::ValDef { identifier: identifier.clone() });
            }
            hir::FileStatement::TypeDef { identifier, value } => {
                self.name_table.push(NameTableEntry::TypeDef { identifier: identifier.clone() });
            }
            hir::FileStatement::Prefix { prefix, items } => {
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