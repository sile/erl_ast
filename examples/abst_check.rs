extern crate erl_ast;

use erl_ast::ast2::ModuleDecl;
use std::env;
use std::thread;

fn main() {
    thread::Builder::new()
        .stack_size(32 * 1024 * 1024)
        .spawn(move || {
            for file in env::args().skip(1) {
                let _ = ModuleDecl::from_beam_file(file.clone()).map_err(|err| {
                    println!("FAILED");
                    println!("======");
                    println!("");
                    println!("FILE: {}", file);
                    println!("");
                    println!("REASON:");
                    println!("{}", err);
                    // std::process::exit(1);
                    println!("");
                });
            }
        })
        .unwrap()
        .join()
        .unwrap();
}
