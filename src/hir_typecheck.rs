use lsp_types::Range;
use crate::dlogger::DiagnosticLogger;
use crate::hir;
use crate::hir::HirVisitor;
use crate::types;

// this struct visits global definitions of values, and finds their type values
// these are used as roots when typechecking function bodies
struct GlobalTypeChecker<'d, 't> {
    dlogger: &'d mut DiagnosticLogger,
    type_range_table: &'t Vec<Range>,
    type_name_table: &'t Vec<String>,
    type_kind_table: &'t Vec<types::KindValue>,
    type_type_table: Vec<Option<types::TypeValue>>,
    val_range_table: &'t Vec<Range>,
    val_name_table: &'t Vec<String>,
    val_type_table: Vec<Option<types::TypeValue>>,
}

impl<'d, 't> GlobalTypeChecker<'d, 't> {

}

impl<'d, 't> HirVisitor for GlobalTypeChecker<'d, 't> {
    fn visit_file_statement(&mut self, statement: &mut crate::ast::Augmented<hir::FileStatement>) {
        match statement.val {
            hir::FileStatement::Let { ref mut typarams, ref mut pat, .. } => {
                
            }
        }
    }
}