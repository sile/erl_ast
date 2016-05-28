use std::io;
use std::path::Path;
use beam_file;
use beam_file::chunk::Chunk;
use eetf;

use result::BeamParseResult;
use error::BeamParseError;
use ast::parser;
use ast::Node;
use ast::{Export, Import, Spec, Callback, TypeDef, RecordDecl, FunctionDecl};

#[derive(Debug)]
pub struct Module {
    pub name: Node<String>,
    pub compile_options: Vec<Node<eetf::Term>>,
    pub behaviours: Vec<Node<String>>,
    pub exports: Vec<Node<Export>>,
    pub export_types: Vec<Node<Export>>,
    pub imports: Vec<Node<Import>>,
    pub specs: Vec<Node<Spec>>,
    pub callbacks: Vec<Node<Callback>>,
    pub types: Vec<Node<TypeDef>>,
    pub record_types: Vec<Node<TypeDef>>,
    pub records: Vec<Node<RecordDecl>>,
    pub functions: Vec<Node<FunctionDecl>>,
    pub user_attributes: Vec<Node<(String, eetf::Term)>>,
}
impl Module {
    pub fn from_beam_file<P: AsRef<Path>>(path: P) -> BeamParseResult<Self> {
        let beam = try!(beam_file::RawBeamFile::from_file(path));
        let chunk = try!(beam.chunks
            .into_iter()
            .find(|c| c.id() == b"Abst")
            .ok_or(BeamParseError::NoDebugInfo));
        let abstract_code = try!(eetf::Term::decode(io::Cursor::new(&chunk.data)));
        parser::raw_abstract_v1::Parser::new().parse(abstract_code)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let _module = Module::from_beam_file("src/testdata/test.beam").unwrap();
    }
}
