#![feature(allocator_api)]
#![feature(let_chains)]
mod ast;
mod astbuilder;
mod codereader;
mod dlogger;
mod hir;
mod hir_construct;
mod thir_construct;
mod mir;
mod mir_construct;
mod environment;
mod token;
mod tokenize;
mod thir;
mod builtin;
mod values;
mod hint;

use std::io::stdin;
use std::io::Read;

use astbuilder::construct_ast;
use dlogger::DiagnosticLog;
use environment::Environment;
use tokenize::tokenize;

fn main() {
    let mut log = DiagnosticLog::new();
    // read input bytes
    let charstream = stdin().bytes().map_while(|x| x.ok());
    // lex input
    let tokenstream = tokenize(charstream, log.get_logger(Some(String::from("acnc-lex"))));
    // parse tokens
    let ast_filestatement_stream =
        construct_ast(tokenstream, log.get_logger(Some(String::from("acnc-ast"))));

    // create environments
    let mut env = Environment::new();

    let mut hir_env = hir_construct::VariableResolutionEnvironment::new();
    let mut mir_env = mir::Environment::new();

    // create diagnostic logger
    let mut dlogger = log.get_logger(Some(String::from("acnc-hir")));

    for filestatement in ast_filestatement_stream {
        // desugar
        let hir_filestatement =
            hir_construct::translate_augfilestatement(filestatement, &mut hir_env, &mut env, &mut dlogger);
        for mut filestatement in hir_filestatement {
            // kindcheck and typecheck
            thir_construct::typecheck_file_statement_and_patch(
                &mut filestatement,
                &mut dlogger,
                &mut hir_env,
            );
            // lower to mir
            mir_construct::lower_file_statement(filestatement, &mut dlogger, &mut mir_env);
        }
    }
}
