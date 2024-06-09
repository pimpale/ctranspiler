use lsp_types::Range;

use crate::dlogger::DiagnosticLogger;
use crate::hir;
use crate::hir::Augmented;
use crate::types;

// finds kinds of all type variables in the HIR
// finds kinds of all val  variables in the HIR
struct KindChecker<'d, 't> {
    dlogger: &'d mut DiagnosticLogger,
    // these are the global identifier tables
    // type
    type_range_table: &'t Vec<Range>,
    type_name_table: &'t Vec<String>,
    type_kind_table: Vec<Option<types::KindValue>>,
    // val
    val_range_table: &'t Vec<Range>,
    val_name_table: &'t Vec<String>,
    val_kind_table: Vec<Option<types::KindValue>>,
}

impl<'d, 't> KindChecker<'d, 't> {
    fn unify_type_pat_expr(
        &mut self,
        pat: &mut Augmented<hir::TypePatExpr>,
        tyval: &mut Augmented<hir::TypeExpr>,
    ) {
        match &mut pat.val {
            hir::TypePatExpr::Error => {}
            hir::TypePatExpr::Identifier(id) => {
                let kind = types::kindcheck_hir_type_infermode_and_patch(
                    tyval,
                    self.dlogger,
                    &self.type_name_table,
                    &self.type_kind_table,
                );
                self.type_kind_table[*id] = Some(kind);
            }
            hir::TypePatExpr::Typed { id, kind } => {
                // evaluate the asserted kind
                let expected_kind = types::evaluate_hir_kind(&kind, self.dlogger);
                // ensure the value matches the kind
                types::kindcheck_hir_type_checkmode_and_patch(
                    tyval,
                    &expected_kind,
                    self.dlogger,
                    &self.type_name_table,
                    &self.type_kind_table,
                );
                // insert the asserted kind into the table
                self.type_kind_table[*id] = Some(expected_kind);
            }
        }
    }

    fn intro_type_param(&mut self, pat: &mut Augmented<hir::TypeParamExpr>) {
        match &mut pat.val {
            hir::TypeParamExpr::Error => {}
            hir::TypeParamExpr::Typed { id, kind } => {
                // evaluate the asserted kind
                let expected_kind = types::evaluate_hir_kind(&kind, self.dlogger);
                // insert the asserted kind into the table
                self.type_kind_table[*id] = Some(expected_kind);
            }
        }
    }

    // attempts to bind kinds to the introduced variables using a provided kind
    fn unify_val_pat_expr_val(
        &mut self,
        pat: &mut Augmented<hir::PatExpr>,
        kind: types::KindValue,
    ) {
        match &mut pat.val {
            hir::PatExpr::Error => {}
            hir::PatExpr::Ignore => {}
            hir::PatExpr::Identifier { id, .. } => {
                self.val_kind_table[*id] = Some(kind);
            }
            hir::PatExpr::StructLiteral { ty, fields } => {
                // struct ty must have a kind of TYPE
                types::kindcheck_hir_type_checkmode_and_patch(
                    ty,
                    &types::KindValue::Type,
                    self.dlogger,
                    &self.type_name_table,
                    &self.type_kind_table,
                );
                // all struct fields must have kind TYPE
                for (_, field) in fields {
                    self.unify_val_pat_expr_val(field, types::KindValue::Type);
                }
            }
            hir::PatExpr::Typed { pat, ty } => {
                // ty must have a kind equal to the provided kind
                types::kindcheck_hir_type_checkmode_and_patch(
                    ty,
                    &kind,
                    self.dlogger,
                    &self.type_name_table,
                    &self.type_kind_table,
                );
                // pat must have a kind equal to the provided kind
                self.unify_val_pat_expr_val(pat, kind);
            }
        }
    }

    // attempts to bind kinds to the introduced variables using a kind inferred from the value
    // also kindchecks the value
    fn unify_val_pat_expr_hir(
        &mut self,
        pat: &mut Augmented<hir::PatExpr>,
        val: &mut Augmented<hir::ValExpr>,
    ) {
        match &mut pat.val {
            hir::PatExpr::Error => {}
            hir::PatExpr::Ignore => {}
            hir::PatExpr::Identifier { mutable, id } => {
                // infer kind and then assign to pat
                let kind = types::kindcheck_hir_val_infermode_and_patch(
                    val,
                    self.dlogger,
                    &self.type_name_table,
                    &self.type_kind_table,
                    &self.val_name_table,
                    &self.val_kind_table,
                );
                self.val_kind_table[*id] = Some(kind);
            }
            hir::PatExpr::StructLiteral { ty, fields } => {
                // the pattern means that the value must be a struct literal, which has a kind of TYPE
                types::kindcheck_hir_val_checkmode_and_patch(
                    val,
                    &types::KindValue::Type,
                    self.dlogger,
                    &self.type_name_table,
                    &self.type_kind_table,
                    &self.val_name_table,
                    &self.val_kind_table,
                );
                for (_, field) in fields {
                    self.unify_val_pat_expr_hir(field, val);
                }
            }
            hir::PatExpr::Typed { pat, ty } => {
                // get the kind of the type
                let kind = types::kindcheck_hir_type_infermode_and_patch(
                    ty,
                    self.dlogger,
                    &self.type_name_table,
                    &self.type_kind_table,
                );
                // kindcheck the val with the type's kind
                types::kindcheck_hir_val_checkmode_and_patch(
                    val,
                    &kind,
                    self.dlogger,
                    &self.type_name_table,
                    &self.type_kind_table,
                    &self.val_name_table,
                    &self.val_kind_table,
                );
                // kindcheck the pat with the type's kind
                self.unify_val_pat_expr_hir(pat, val);
            }
        }
    }

    fn intro_val_param(&mut self, param: &mut Augmented<hir::ParamExpr>) {
        match &mut param.val {
            hir::ParamExpr::Error => {},
            hir::ParamExpr::Typed { pat, ty } => {
                // get the kind of the type
                let kind = types::kindcheck_hir_type_infermode_and_patch(
                    ty,
                    self.dlogger,
                    &self.type_name_table,
                    &self.type_kind_table,
                );
                // kindcheck the pat with the type's kind
                self.unify_val_pat_expr_val(pat, kind);
            }
        }
    }
}

impl<'d, 't> hir::HirVisitor for KindChecker<'d, 't> {
    fn visit_file_statement(&mut self, statement: &mut Augmented<hir::FileStatement>) {
        // the type pat must match the kind of the type expression
        match &mut statement.val {
            hir::FileStatement::TypeDef { value, typat } => {
                self.unify_type_pat_expr(typat, value);
            }
            _ => self.dfs_visit_file_statement(statement),
        }
    }

    fn visit_block_statement(&mut self, statement: &mut Augmented<hir::BlockStatement>) {
        match &mut statement.val {
            hir::BlockStatement::TypeDef { value, typat } => {
                self.unify_type_pat_expr(typat, value);
            }
            _ => self.dfs_visit_block_statement(statement),
        }
    }

    fn visit_val_expr(&mut self, expr: &mut Augmented<hir::ValExpr>) {
        match &mut expr.val {
            hir::ValExpr::FnDef {
                typarams,
                params,
                returnty,
                body,
            } => {
                // bind the typarams
                for typaram in typarams {
                    self.intro_type_param(typaram);
                }
                // all params types must have a kind of TYPE
                for param in params {
                    self.intro_val_param(param);
                }
                // the return type must have a kind of TYPE
                types::kindcheck_hir_type_checkmode_and_patch(
                    returnty,
                    &types::KindValue::Type,
                    self.dlogger,
                    &self.type_name_table,
                    &self.type_kind_table,
                );
                // the body must have a kind of TYPE
                types::kindcheck_hir_val_checkmode_and_patch(
                    body,
                    &types::KindValue::Type,
                    self.dlogger,
                    &self.type_name_table,
                    &self.type_kind_table,
                    &self.val_name_table,
                    &self.val_kind_table,
                );
            }
            
            _ => self.dfs_visit_val_expr(expr),
        }
    }

    // except for type declarations, all type expressions must have a kind of Type
    fn visit_type_expr(&mut self, _expr: &mut Augmented<hir::TypeExpr>) {
        unreachable!("should be handled by other methods before it gets here");
    }

    fn visit_type_pat_expr(&mut self, expr: &mut Augmented<hir::TypePatExpr>) {
        unreachable!("should be handled by other methods before it gets here");
    }
}

// visits all nodes, and deletes all parts of the HIR that do not comply with kind checking rules
// returns a table of kinds for all type variables
// binds kinds for all type variables
pub fn do_kindcheck(
    ast: &mut hir::TranslationUnit,
    type_range_table: &Vec<Range>,
    type_name_table: &Vec<String>,
    val_range_table: &Vec<Range>,
    val_name_table: &Vec<String>,
    ref mut dlogger: DiagnosticLogger,
) -> Vec<types::KindValue> {
    let mut kc = KindChecker {
        dlogger,
        // types
        type_range_table,
        type_name_table,
        // vec of nones with same length as the type range table
        type_kind_table: type_range_table.iter().map(|_| None).collect(),
        // values
        val_range_table,
        val_name_table,
        val_kind_table: val_range_table.iter().map(|_| None).collect(),
    };
    hir::HirVisitor::visit_translation_unit(&mut kc, ast);
    kc.type_kind_table.into_iter().map(|x| x.unwrap()).collect()
}
