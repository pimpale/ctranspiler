use std::collections::HashMap;
use lsp_types::Range;
use crate::ast::Augmented;
use crate::dlogger::DiagnosticLogger;
use crate::hir;
use crate::hir::HirVisitor;

// visits the AST and collects all the names of global types and values (all these values will be in scope when we start evaluating functions)
struct GlobalIdentifierCollector<'d> {
    dlogger: &'d mut DiagnosticLogger,
    type_names: HashMap<String, Range>,
    val_names: HashMap<String, Range>,
    prefixes: Vec<String>,
}

impl<'d> HirVisitor for GlobalIdentifierCollector<'d> {
    fn visit_file_statement(&mut self, statement: &mut Augmented<hir::FileStatement>) {
        match statement.val {
            hir::FileStatement::Let {
                ref mut pattern, ..
            } => {
                self.visit_pat_expr(pattern);
            }
            hir::FileStatement::TypeDef {
                ref identifier,
                ref tyargs,
                ref value,
            } => {
                let identifier = [self.prefixes.concat(), identifier.clone()].concat();
                if self.type_names.contains_key(&identifier) {
                    self.dlogger
                        .log_duplicate_identifier(statement.range, &identifier);
                    statement.val = hir::FileStatement::Error;
                } else {
                    self.type_names.insert(identifier.clone());
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

    fn visit_pat_expr(&mut self, expr: &mut Augmented<hir::PatExpr>) {
        match expr.val {
            hir::PatExpr::Identifier { ref identifier, .. } => {
                let identifier = [self.prefixes.concat(), identifier.clone()].concat();
                if self.val_names.contains(&identifier) {
                    self.dlogger
                        .log_duplicate_identifier(expr.range, &identifier);
                    expr.val = hir::PatExpr::Error;
                } else {
                    self.val_names.insert(identifier.clone());
                }
            }
            _ => self.dfs_visit_pat_expr(expr),
        }
    }
}

fn get_if_in_scope<'a, T>(
    name: &str,
    scopes: &'a Vec<HashMap<String, T>>,
    prefixes: &'a Vec<Vec<String>>,
) -> Option<&'a T> {
    for prefix in prefixes.iter().flatten().rev() {
        let prefixed_name = [prefix.clone(), name.to_string()].concat();
        for scope in scopes.iter().rev() {
            if let Some(val) = scope.get(&prefixed_name) {
                return Some(val);
            }
        }
    }
    return None;
}

struct InvalidIdentifierRemover<'d> {
    dlogger: &'d mut DiagnosticLogger,
    use_prefixes: Vec<Vec<String>>,
    type_names: Vec<HashSet<String>>,
    val_names: Vec<HashSet<String>>,
}

impl<'d> HirVisitor for InvalidIdentifierRemover<'d> {
    fn visit_file_statement(&mut self, statement: &mut Augmented<hir::FileStatement>) {
        match statement.val {
            hir::FileStatement::Error => {}
            // check the body of the type, introducing the tyargs
            hir::FileStatement::TypeDef {
                ref mut tyargs,
                ref mut value,
                ..
            } => {
                let mut new_type_scope = HashSet::new();
                for tyarg in tyargs {
                    match tyarg.val {
                        hir::TypePatExpr::Error => {}
                        hir::TypePatExpr::Identifier { identifier, .. } => {
                            if new_type_scope.contains(&identifier) {
                                self.dlogger
                                    .log_duplicate_identifier(tyarg.range, &identifier);
                                tyarg.val = hir::TypePatExpr::Error;
                            } else {
                                new_type_scope.insert(identifier);
                            }
                        }
                    }
                }
                self.type_names.push(new_type_scope);
                self.visit_type_expr(value);
                self.type_names.pop();
            }
            // check the body of the let, introducing the tyargs
            hir::FileStatement::Let {
                ref mut tyargs,
                ref mut value,
                ..
            } => {
                let mut new_type_scope = HashSet::new();
                for tyarg in tyargs {
                    match tyarg.val {
                        hir::TypePatExpr::Error => {}
                        hir::TypePatExpr::Identifier { identifier, .. } => {
                            if new_type_scope.contains(&identifier) {
                                self.dlogger
                                    .log_duplicate_identifier(tyarg.range, &identifier);
                                tyarg.val = hir::TypePatExpr::Error;
                            } else {
                                new_type_scope.insert(identifier);
                            }
                        }
                    }
                }
                self.type_names.push(new_type_scope);
                self.visit_val_expr(value);
                self.type_names.pop();
            }
            // introduce a new scope for the prefix
            hir::FileStatement::Prefix {
                prefix,
                ref mut items,
            } => {
                self.use_prefixes.push(vec![prefix.clone()]);
                for item in items {
                    self.visit_file_statement(item);
                }
                self.use_prefixes.pop();
            }
            // push the use prefix onto the stack
            hir::FileStatement::Use { ref prefix } => {
                let last = self.use_prefixes.last_mut().unwrap();
                if last.contains(prefix) {
                    self.dlogger.log_duplicate_use(statement.range, prefix);
                    statement.val = hir::FileStatement::Error;
                } else {
                    last.push(prefix.clone());
                }
            }
        }
    }

    fn visit_type_expr(&mut self, expr: &mut Augmented<hir::TypeExpr>) {
        match expr.val {
            hir::TypeExpr::Error => {}
            hir::TypeExpr::Identifier(ref identifier) => {
                if !is_in_scope(identifier, &self.type_names, &self.use_prefixes) {
                    self.dlogger.log_unknown_identifier(expr.range, identifier);
                    expr.val = hir::TypeExpr::Error;
                }
            }
            _ => self.dfs_visit_type_expr(expr),
        }
    }

    fn visit_val_expr(&mut self, expr: &mut Augmented<hir::ValExpr>) {
        match expr.val {
            hir::ValExpr::Error => {}
            hir::ValExpr::Identifier(ref identifier) => {
                if !is_in_scope(identifier, &self.val_names, &self.use_prefixes) {
                    self.dlogger.log_unknown_identifier(expr.range, identifier);
                    expr.val = hir::ValExpr::Error;
                }
            }
            _ => self.dfs_visit_val_expr(expr),
        }
    }

    fn visit_block_expr(&mut self, block: &mut Augmented<hir::BlockExpr>) {
        self.type_names.push(HashSet::new());
        self.val_names.push(HashSet::new());
        self.use_prefixes.push(vec![]);
        for ref mut statement in block.val.statements {
            self.visit_block_statement(statement);
        }
        self.use_prefixes.pop();
        self.val_names.pop();
        self.type_names.pop();
    }

    fn visit_block_statement(&mut self, statement: &mut Augmented<hir::BlockStatement>) {
        match statement.val {
            hir::BlockStatement::Error => {}
            // check the body of the type, introducing the tyargs
            hir::BlockStatement::TypeDef {
                ref mut tyargs,
                ref mut value,
                ref identifier,
            } => {
                let mut new_type_scope = HashSet::new();
                new_type_scope.insert(identifier.clone());
                for tyarg in tyargs {
                    match tyarg.val {
                        hir::TypePatExpr::Error => {}
                        hir::TypePatExpr::Identifier { identifier, .. } => {
                            if new_type_scope.contains(&identifier) {
                                self.dlogger
                                    .log_duplicate_identifier(tyarg.range, &identifier);
                                tyarg.val = hir::TypePatExpr::Error;
                            } else {
                                new_type_scope.insert(identifier);
                            }
                        }
                    }
                }
                self.type_names.push(new_type_scope);
                self.visit_type_expr(value);
                self.type_names.pop();
            }
            // check the body of the let, introducing the tyargs
            hir::BlockStatement::Let {
                ref mut tyargs,
                ref mut value,
                ..
            } => {
                let mut new_type_scope = HashSet::new();
                for tyarg in tyargs {
                    match tyarg.val {
                        hir::TypePatExpr::Error => {}
                        hir::TypePatExpr::Identifier { identifier, .. } => {
                            if new_type_scope.contains(&identifier) {
                                self.dlogger
                                    .log_duplicate_identifier(tyarg.range, &identifier);
                                tyarg.val = hir::TypePatExpr::Error;
                            } else {
                                new_type_scope.insert(identifier);
                            }
                        }
                    }
                }
                self.type_names.push(new_type_scope);
                self.visit_val_expr(value);
                self.type_names.pop();
            }
            // push the use prefix onto the stack
            hir::BlockStatement::Use { prefix } => {
                self.use_prefixes.last().unwrap().push(prefix.clone());
            }
        }
    }

}

pub fn resolve_kinds(ast: &mut hir::TranslationUnit, dlogger: &mut DiagnosticLogger) {
    // get the names of all global variables
    let mut collector = GlobalIdentifierCollector {
        dlogger,
        type_names: HashSet::new(),
        val_names: HashSet::new(),
        prefixes: Vec::new(),
    };
    collector.visit_translation_unit(ast);
    // now remove all invalid identifiers
    let mut remover = InvalidIdentifierRemover {
        dlogger,
        use_prefixes: Vec::new(),
        type_names: vec![collector.type_names],
        val_names: vec![collector.val_names],
    };
    remover.visit_translation_unit(ast);
}
