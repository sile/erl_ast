//! A Rust representation of Abstract Syntax Trees of Erlang modules.
//!
//! Currently the library provide only a functionality that
//! loading ASTs from beam files which have debug infos.
//!
//! See also: [The Abstract Format](http://erlang.org/doc/apps/erts/absform.html)
//!
//! # Examples
//!
//! ```
//! use erl_ast::AST;
//!
//! let ast = AST::from_beam_file("src/testdata/test.beam").unwrap();
//! println!("{:?}", ast);
//! ```
extern crate beam_file;
extern crate eetf;
extern crate num;

pub mod ast;
pub mod error;
pub mod format;
pub mod result;

use std::path::Path;

/// Abstract Syntax Tree
#[derive(Debug)]
#[allow(clippy::upper_case_acronyms)]
pub struct AST {
    pub module: ast::ModuleDecl,
}
impl AST {
    /// Builds AST from the BEAM file
    pub fn from_beam_file<P: AsRef<Path>>(beam_file: P) -> result::FromBeamResult<Self> {
        let code = format::raw_abstract_v1::AbstractCode::from_beam_file(beam_file)?;
        let forms = code.to_forms()?;
        Ok(AST {
            module: ast::ModuleDecl { forms },
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    // To run the tests
    // cargo test --package erl_ast --lib -- tests --nocapture

    #[test]
    fn it_works_for_new_beam() {
        let ast = AST::from_beam_file("src/testdata/test2.beam")
            .map_err(|err| {
                println!("[ERROR] {}", err);
                "Failed"
            })
            .unwrap();
        assert_eq!(format!("{:?}", ast), "AST { module: ModuleDecl { forms: [File(FileAttr { line: 1, original_file: \"test2.erl\", original_line: 1 }), Module(ModuleAttr { line: 1, name: \"test2\" }), Eof(Eof { line: 2 })] } }");
    }

    #[test]
    fn it_works_for_old_beam() {
        AST::from_beam_file("src/testdata/test.beam")
            .map_err(|err| {
                println!("[ERROR] {}", err);
                "Failed"
            })
            .unwrap();
    }
}
