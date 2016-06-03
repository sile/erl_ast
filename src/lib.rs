//! A Rust representation of Abstract Syntax Trees of Erlang modules
//!
//! See: [The Abstract Format](http://erlang.org/doc/apps/erts/absform.html)
extern crate beam_file;
extern crate eetf;
extern crate num;

pub mod ast;
pub mod result;
pub mod error;
pub mod format;

use std::path::Path;

/// Abstract Syntax Tree
#[derive(Debug)]
pub struct AST {
    pub module: ast::form::ModuleDecl,
}
impl AST {
    /// Builds AST from the BEAM file
    pub fn from_beam_file<P: AsRef<Path>>(beam_file: P) -> result::BeamParseResult<Self> {
        let module = try!(ast::form::ModuleDecl::from_beam_file(beam_file));
        Ok(AST { module: module })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        AST::from_beam_file("src/testdata/test.beam")
            .map_err(|err| {
                println!("[ERROR] {}", err);
                "Failed"
            })
            .unwrap();
    }
}
