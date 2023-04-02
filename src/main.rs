#![feature(allocator_api)]
mod ast;
mod astbuilder;
mod codereader;
mod dlogger;
mod hir;
mod hirbuilder;
mod desugar_hir;
mod token;
mod tokenize;
mod utils;

use std::io::stdin;
use std::io::Read;

use astbuilder::construct_ast;
use dlogger::DiagnosticLog;
use hirbuilder::translate_translationunit;
use tokenize::tokenize;

fn main() {
    let mut log = DiagnosticLog::new();
    let charstream = stdin().bytes().map_while(|x| x.ok());
    let tokenstream = tokenize(charstream, log.get_logger(Some(String::from("acnc-lex"))));
    let ast = construct_ast(tokenstream, log.get_logger(Some(String::from("acnc-ast"))));

    // create and lower hir
    let hir = translate_translationunit(ast, log.get_logger(Some(String::from("acnc-hir"))));


    // dbg!(thir);
}
