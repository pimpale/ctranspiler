#![feature(allocator_api)]
mod ast;
mod astbuilder;
mod codereader;
mod dlogger;
mod hir;
mod hir_construct;
mod hir_desugar;
mod hir_kindcheck;
mod hir_typecheck;
mod hir_resolvename;
mod token;
mod tokenize;
mod types;
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
    let hir = construct_hir(
        ast,
        log.get_logger(Some(String::from("acnc-hir (construct)"))),
    );
    hir_desugar::desugar_hir(
        &mut hir,
        log.get_logger(Some(String::from("acnc-hir (desugar)"))),
    );

    // resolve identifiers
    let (type_range_table, type_name_table, val_range_table, val_name_table) =
        hir_resolvename::fix_identifiers(
            &mut hir,
            log.get_logger(Some(String::from("acnc-hir (resolve identifiers)"))),
        );

    // kindcheck
    let type_kind_table = hir_kindcheck::do_kindcheck(
        &mut hir,
        &type_range_table,
        &type_name_table,
        log.get_logger(Some(String::from("acnc-hir (kindcheck)"))),
    );

    // typecheck
    let val_kind_table = hir_typecheck::do_typecheck(
        &mut hir,
        &type_range_table,
        &type_name_table,
        &type_kind_table,
        &val_range_table,
        &val_name_table,
        log.get_logger(Some(String::from("acnc-hir (typecheck)"))),
    );
}
