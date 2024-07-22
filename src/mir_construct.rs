use crate::hir;
use crate::mir::Environment;

use crate::dlogger::DiagnosticLogger;

pub fn lower_file_statement(
    hir::Augmented { val, .. }: hir::Augmented<hir::FileStatement>,
    dlogger: &mut DiagnosticLogger,
    mir_env: &mut Environment,
) {
    match val {
        // do nothing
        hir::FileStatement::Error => {}
        hir::FileStatement::Let { pat, value } => {
            // evaluate the value using hir eval
            // split the 
        }
    }
}
