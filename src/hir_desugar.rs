use crate::dlogger::DiagnosticLogger;
use crate::hir;
use crate::hir::HirVisitor;

struct DesugarVisitor {
    dlogger: DiagnosticLogger,
}

impl hir::HirVisitor for DesugarVisitor {
    fn visit_translation_unit(&mut self, tu: &mut crate::hir::TranslationUnit) {
        tu.phase = hir::HirPhase::Desugared;
        for item in tu.declarations.iter_mut() {
            self.visit_file_statement(item);
        }
    }
}

pub fn desugar_hir(tu: &mut hir::TranslationUnit, dlogger: DiagnosticLogger) {
    let mut visitor = DesugarVisitor { dlogger };
    visitor.visit_translation_unit(tu);
}
