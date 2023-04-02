#![feature(allocator_api)]
mod ast;
mod astbuilder;
mod codereader;
mod dlogger;
mod hir;
mod hir_construct;
mod hir_desugar;
mod hir_resolvename;
mod token;
mod tokenize;
mod utils;

use std::io::stdin;
use std::io::Read;

use astbuilder::construct_ast;
use dlogger::DiagnosticLog;
use hir_construct::construct_hir;
use tokenize::tokenize;

fn main() {
    let mut log = DiagnosticLog::new();
    let charstream = stdin().bytes().map_while(|x| x.ok());
    let tokenstream = tokenize(charstream, log.get_logger(Some(String::from("acnc-lex"))));
    let ast = construct_ast(tokenstream, log.get_logger(Some(String::from("acnc-ast"))));

    // create and lower hir
    let hir = construct_hir(ast, log.get_logger(Some(String::from("acnc-hir (construct)"))));
    hir_desugar::desugar_hir(&mut hir, log.get_logger(Some(String::from("acnc-hir (desugar)"))));
    let global_vtable = hir_resolvename::resolve_global_names(&mut hir, log.get_logger(Some(String::from("acnc-hir (resolve global names)"))));


    // dbg!(thir);
}
