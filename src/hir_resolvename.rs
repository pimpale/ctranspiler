use crate::ast::{self, Augmented};
use crate::dlogger::DiagnosticLogger;
use crate::hir;
use crate::hir::HirVisitor;
use indexmap::IndexMap;
use lsp_types::Range;
use std::collections::HashMap;

// visits the AST and resolves all global definitions (all these values will be in scope when we start evaluating functions)
struct GlobalIdenitiferResolver<'d> {
    dlogger: &'d mut DiagnosticLogger,
    prefixes: Vec<String>,

    // these are the global identifier tables
    type_range_table: Vec<Range>,
    type_name_table: Vec<String>,
    val_range_table: Vec<Range>,
    val_name_table: Vec<String>,
    // these are the names currently in scope
    type_names_in_scope: HashMap<String, usize>,
    val_names_in_scope: HashMap<String, usize>,
}

impl<'d> HirVisitor for GlobalIdenitiferResolver<'d> {
    fn visit_file_statement(&mut self, statement: &mut Augmented<hir::FileStatement>) {
        match statement.val {
            hir::FileStatement::Let { ref mut pat, .. } => {
                self.visit_pat_expr(pat);
            }
            hir::FileStatement::TypeDef {
                ref value,
                ref typat,
                ..
            } => match typat.val {
                hir::TypePatExpr::Error => {}
                hir::TypePatExpr::Identifier { identifier, kind } => {
                    let identifier = [self.prefixes.concat(), identifier.clone()].concat();
                    if let Some(id) = self.type_names_in_scope.get(&identifier) {
                        let previous_range = self.type_range_table[*id];
                        self.dlogger.log_duplicate_identifier(
                            typat.range,
                            previous_range,
                            &identifier,
                        );
                        typat.val = hir::TypePatExpr::Error;
                    } else {
                        let id = self.type_name_table.len();
                        self.type_names_in_scope.insert(identifier.clone(), id);
                        self.type_name_table.push(identifier);
                        self.type_range_table.push(typat.range);
                        typat.val = hir::TypePatExpr::Identifier2 {
                            identifier: id,
                            kind,
                        };
                    }
                }
                hir::TypePatExpr::Identifier2 { .. } => {
                    unreachable!("hir shouldn't have been lowered yet")
                }
            },
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
                if let Some(id) = self.val_names_in_scope.get(&identifier) {
                    let previous_range = self.val_range_table[*id];
                    self.dlogger
                        .log_duplicate_identifier(expr.range, previous_range, &identifier);
                    expr.val = hir::PatExpr::Error;
                } else {
                    let id = self.val_name_table.len();
                    self.val_names_in_scope.insert(identifier.clone(), id);
                    self.val_name_table.push(identifier);
                    self.val_range_table.push(expr.range);
                    expr.val = hir::PatExpr::Identifier2(id);
                }
            }
            _ => self.dfs_visit_pat_expr(expr),
        }
    }
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

struct IdentifierResolver<'d> {
    dlogger: &'d mut DiagnosticLogger,
    use_prefixes: Vec<IndexMap<String, Range>>,
    // these are the global identifier tables
    type_range_table: Vec<Range>,
    type_name_table: Vec<String>,
    val_range_table: Vec<Range>,
    val_name_table: Vec<String>,
    // these are the names that are in scope
    type_names_in_scope: Vec<HashMap<String, usize>>,
    val_names_in_scope: Vec<HashMap<String, usize>>,
}

impl<'d> IdentifierResolver<'d> {
    // introduce pattern vals (also delete any erroneous patterns)
    fn introduce_and_check_pattern_vals(&mut self, pat: &mut Augmented<hir::PatExpr>) {
        match pat.val {
            hir::PatExpr::Error => {}
            hir::PatExpr::Ignore => {}
            hir::PatExpr::Identifier2(_) => unreachable!("hir shouldn't have been lowered yet"),
            hir::PatExpr::Identifier { identifier, .. } => {
                if let Some(previous_range) =
                    range_if_dupe(&identifier, &self.val_names_in_scope, &self.val_range_table)
                {
                    self.dlogger
                        .log_duplicate_identifier(pat.range, *previous_range, &identifier);
                    pat.val = hir::PatExpr::Error;
                } else {
                    let id = self.val_name_table.len();
                    self.val_names_in_scope
                        .last()
                        .unwrap()
                        .insert(identifier.clone(), id);
                    self.val_name_table.push(identifier);
                    self.val_range_table.push(pat.range);
                    pat.val = hir::PatExpr::Identifier2(id);
                }
            }
            hir::PatExpr::StructLiteral(fields) => {
                for (_, ref mut field) in fields {
                    self.introduce_and_check_pattern_vals(field);
                }
            }
            hir::PatExpr::Typed {
                ref mut pat,
                ref mut ty,
            } => {
                self.introduce_and_check_pattern_vals(pat);
                self.visit_type_expr(ty);
            }
        }
    }

    // introduce type vals (also delete any erroneous types)
    fn introduce_and_check_type_vals(&mut self, typat: &mut Augmented<hir::TypePatExpr>) {
        match typat.val {
            hir::TypePatExpr::Error => {}
            hir::TypePatExpr::Identifier2 { .. } => {
                unreachable!("hir shouldn't have been lowered yet")
            }
            hir::TypePatExpr::Identifier { identifier, kind } => {
                if let Some(previous_range) = range_if_dupe(
                    &identifier,
                    &self.type_names_in_scope,
                    &self.type_range_table,
                ) {
                    self.dlogger.log_duplicate_identifier(
                        typat.range,
                        *previous_range,
                        &identifier,
                    );
                    typat.val = hir::TypePatExpr::Error;
                } else {
                    let id = self.type_name_table.len();
                    self.type_names_in_scope
                        .last()
                        .unwrap()
                        .insert(identifier.clone(), id);
                    self.type_name_table.push(identifier);
                    self.type_range_table.push(typat.range);
                    typat.val = hir::TypePatExpr::Identifier2 {
                        identifier: id,
                        kind,
                    }
                }
            }
        }
    }
}

impl<'d> HirVisitor for IdentifierResolver<'d> {
    fn visit_file_statement(&mut self, statement: &mut Augmented<hir::FileStatement>) {
        match statement.val {
            hir::FileStatement::NoOp => {}
            // check the body of the type, introducing the tyargs
            hir::FileStatement::TypeDef {
                ref mut typarams,
                ref mut value,
                ..
            } => {
                self.type_names_in_scope.push(HashMap::new());
                for typat in typarams {
                    self.introduce_and_check_type_vals(typat);
                }
                self.visit_type_expr(value);
                self.type_names_in_scope.pop();
            }
            // check the body of the let, introducing the tyargs
            hir::FileStatement::Let {
                ref mut typarams,
                ref mut value,
                ..
            } => {
                self.type_names_in_scope.push(HashMap::new());
                for typat in typarams {
                    self.introduce_and_check_type_vals(typat);
                }
                self.visit_val_expr(value);
                self.type_names_in_scope.pop();
            }
            // introduce a new scope for the prefix
            hir::FileStatement::Prefix {
                prefix,
                ref mut items,
            } => {
                self.use_prefixes
                    .push(IndexMap::from([(prefix.clone(), statement.range)]));
                for item in items {
                    self.visit_file_statement(item);
                }
                self.use_prefixes.pop();
            }
            // push the use prefix onto the stack
            hir::FileStatement::Use { ref prefix } => {
                // duplicate use is a warning, so we can continue
                if let Some(previous_range) = range_if_prefix_dupe(&prefix, &self.use_prefixes) {
                    self.dlogger
                        .log_duplicate_use(statement.range, *previous_range, prefix);
                    // we need to remove the previous use (if it was in the same scope)
                    self.use_prefixes.last_mut().unwrap().shift_remove(prefix);
                }
                self.use_prefixes
                    .last_mut()
                    .unwrap()
                    .insert(prefix.clone(), statement.range);
                statement.val = hir::FileStatement::NoOp;
            }
        }
    }

    fn visit_type_expr(&mut self, expr: &mut Augmented<hir::TypeExpr>) {
        match expr.val {
            hir::TypeExpr::Error => {}
            hir::TypeExpr::Identifier2(_) => unreachable!("hir shouldn't have been lowered yet"),
            hir::TypeExpr::Identifier(ref identifier) => {
                match get_if_in_scope(identifier, &self.type_names_in_scope, &self.use_prefixes) {
                    Some(id) => {
                        expr.val = hir::TypeExpr::Identifier2(*id);
                    }
                    None => {
                        self.dlogger.log_unknown_identifier(expr.range, identifier);
                        expr.val = hir::TypeExpr::Error;
                    }
                }
            }
            _ => self.dfs_visit_type_expr(expr),
        }
    }

    fn visit_val_expr(&mut self, expr: &mut Augmented<hir::ValExpr>) {
        match expr.val {
            hir::ValExpr::Error => {}
            hir::ValExpr::Identifier2(_) => unreachable!("hir shouldn't have been lowered yet"),
            hir::ValExpr::Identifier(ref identifier) => {
                match get_if_in_scope(identifier, &self.val_names_in_scope, &self.use_prefixes) {
                    Some(id) => {
                        expr.val = hir::ValExpr::Identifier2(*id);
                    }
                    None => {
                        self.dlogger.log_unknown_identifier(expr.range, identifier);
                        expr.val = hir::ValExpr::Error;
                    }
                }
            }
            _ => self.dfs_visit_val_expr(expr),
        }
    }

    fn visit_case_expr(&mut self, expr: &mut Augmented<hir::CaseExpr>) {
        match expr.val.target.val {
            hir::CaseTargetExpr::PatExpr(ref mut pat) => {
                self.val_names_in_scope.push(HashMap::new());
                self.introduce_and_check_pattern_vals(pat);
                self.visit_val_expr(&mut expr.val.body);
                self.val_names_in_scope.pop();
            }
            _ => self.visit_val_expr(&mut expr.val.body),
        }
    }

    fn visit_block_expr(&mut self, block: &mut Augmented<hir::BlockExpr>) {
        self.type_names_in_scope.push(HashMap::new());
        self.val_names_in_scope.push(HashMap::new());
        self.use_prefixes.push(IndexMap::new());
        for ref mut statement in block.val.statements {
            self.visit_block_statement(statement);
        }
        self.use_prefixes.pop();
        self.val_names_in_scope.pop();
        self.type_names_in_scope.pop();
    }

    // we manually handle all pattern expansion, so it's a bug if we hit this
    fn visit_pat_expr(&mut self, expr: &mut Augmented<hir::PatExpr>) {
        unreachable!()
    }

    fn visit_block_statement(&mut self, statement: &mut Augmented<hir::BlockStatement>) {
        match statement.val {
            hir::BlockStatement::NoOp => {}
            // check the body of the type, introducing the tyargs
            hir::BlockStatement::TypeDef {
                ref mut typarams,
                ref mut value,
                ref mut typat,
            } => {
                self.introduce_and_check_type_vals(typat);
                self.type_names_in_scope.push(HashMap::new());
                for typat in typarams {
                    self.introduce_and_check_type_vals(typat);
                }
                self.visit_type_expr(value);
                self.type_names_in_scope.pop();
            }
            // check the body of the let, introducing the tyargs
            hir::BlockStatement::Let {
                ref mut typarams,
                ref mut value,
                ref mut pat,
            } => {
                // introduce new type scope
                self.type_names_in_scope.push(HashMap::new());
                for typat in typarams {
                    self.introduce_and_check_type_vals(typat);
                }

                // we introduce the variables into the current scope before we check the value
                // note this means that the declared variables are accessible from the value
                // this is a bit weird, but it's the only way to allow recursive let bindings
                self.introduce_and_check_pattern_vals(pat);

                self.visit_val_expr(value);
                self.type_names_in_scope.pop();
            }
            // push the use prefix onto the stack
            hir::BlockStatement::Use { ref prefix } => {
                // duplicate use is a warning, so we can continue
                if let Some(previous_range) = range_if_prefix_dupe(&prefix, &self.use_prefixes) {
                    self.dlogger
                        .log_duplicate_use(statement.range, *previous_range, prefix);
                    // we need to remove the previous use (if it was in the same scope)
                    self.use_prefixes.last_mut().unwrap().shift_remove(prefix);
                }
                self.use_prefixes
                    .last_mut()
                    .unwrap()
                    .insert(prefix.clone(), statement.range);
                statement.val = hir::BlockStatement::NoOp;
            }
            hir::BlockStatement::For {
                ref mut pattern,
                ref mut start,
                ref mut end,
                ref mut by,
                ref mut body,
                ..
            } => {
                self.val_names_in_scope.push(HashMap::new());
                self.introduce_and_check_pattern_vals(pattern);
                self.visit_val_expr(start);
                self.visit_val_expr(end);
                if let Some(by) = by {
                    self.visit_val_expr(by);
                }
                self.visit_block_expr(body);
                self.val_names_in_scope.pop();
            }
            _ => self.dfs_visit_block_statement(statement),
        }
    }
}

pub fn fix_identifiers(
    ast: &mut hir::TranslationUnit,
    ref mut dlogger: DiagnosticLogger,
) -> (Vec<Range>, Vec<String>, Vec<Range>, Vec<String>) {
    // resolve all global variables
    let mut collector = GlobalIdenitiferResolver {
        dlogger,
        prefixes: Vec::new(),
        type_names_in_scope: HashMap::new(),
        val_names_in_scope: HashMap::new(),
        type_name_table: Vec::new(),
        type_range_table: Vec::new(),
        val_name_table: Vec::new(),
        val_range_table: Vec::new(),
    };
    collector.visit_translation_unit(ast);
    // resolve all local variables
    let mut remover = IdentifierResolver {
        dlogger,
        use_prefixes: vec![IndexMap::new()],
        type_names_in_scope: vec![HashMap::new()],
        val_names_in_scope: vec![HashMap::new()],
        type_name_table: collector.type_name_table,
        type_range_table: collector.type_range_table,
        val_name_table: collector.val_name_table,
        val_range_table: collector.val_range_table,
    };
    remover.visit_translation_unit(ast);
    (
        remover.type_range_table,
        remover.type_name_table,
        remover.val_range_table,
        remover.val_name_table,
    )
}
