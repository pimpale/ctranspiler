#![feature(allocator_api)]
mod ast;
mod astbuilder;
mod codereader;
mod dlogger;
mod hir;
mod hir_construct;
mod hir_kindcheck;
mod hir_typecheck;
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
use typecheck::TypeChecker;


fn main() {
    let mut log = DiagnosticLog::new();
    // read input bytes
    let charstream = stdin().bytes().map_while(|x| x.ok());
    // lex input
    let tokenstream = tokenize(charstream, log.get_logger(Some(String::from("acnc-lex"))));
    // parse tokens
    let ast_filestatement_stream = construct_ast(tokenstream, log.get_logger(Some(String::from("acnc-ast"))));
    
    
    // resolve variables and desugar
    let (
        hir,
        type_name_table,
        type_range_table,
        val_name_table,
        val_range_table,
    ) = construct_hir(
        ast,
        log.get_logger(Some(String::from("acnc-hir (construct)"))),
    );

    // construct typechecking data
    let typechecker = TypeChecker {
        type_name_table: &type_name_table,
        type_range_table: &type_range_table,
        val_name_table: &val_name_table,
        val_range_table: &val_range_table,
        type_kind_table: todo!(),
        type_type_table: todo!(),
        val_kind_table: todo!(),
        val_type_table: todo!(),
    }

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
