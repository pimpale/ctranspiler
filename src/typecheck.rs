use lsp_types::Range;

use crate::{
    dlogger::DiagnosticLogger, hir::{self, Augmented}, types::{KindValue, TypeValue}
};

// finds kinds of all type variables in the HIR
// finds kinds of all val  variables in the HIR
pub struct TypeChecker<'t> {
    // these are the global identifier tables
    // type
    pub type_range_table: &'t Vec<Range>,
    pub type_name_table: &'t Vec<String>,
    pub type_kind_table: Vec<Option<KindValue>>,
    pub type_type_table: Vec<Option<TypeValue>>,
    // val
    pub val_range_table: &'t Vec<Range>,
    pub val_name_table: &'t Vec<String>,
    pub val_kind_table: Vec<Option<KindValue>>,
    pub val_type_table: Vec<Option<TypeValue>>,
}


pub fn evaluate_hir_kind(kind: &Augmented<hir::KindExpr>) -> KindValue {
    match &kind.val {
        hir::KindExpr::Error => KindValue::Error,
        hir::KindExpr::Type => KindValue::Type,
        hir::KindExpr::Int => KindValue::Int,
        hir::KindExpr::Float => KindValue::Float,
        hir::KindExpr::Bool => KindValue::Bool,
        hir::KindExpr::Constructor {
            paramkinds,
            returnkind,
        } => {
            let mut paramkinds_out = vec![];
            for arg in paramkinds.iter() {
                paramkinds_out.push(evaluate_hir_kind(arg));
            }
            KindValue::Generic {
                paramkinds: paramkinds_out,
                returnkind: Box::new(evaluate_hir_kind(&returnkind)),
            }
        }
    }
}