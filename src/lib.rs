extern crate beam_file;
extern crate eetf;
extern crate num;

pub mod ast;
pub mod result;
pub mod error;

use std::path::Path;

/// Abstract Syntax Tree
#[derive(Debug)]
pub struct AST {
    pub module: ast::ModuleDecl,
}
impl AST {
    pub fn from_beam_file<P: AsRef<Path>>(beam_file: P) -> result::BeamParseResult<Self> {
        let module = try!(ast::ModuleDecl::from_beam_file(beam_file));
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
