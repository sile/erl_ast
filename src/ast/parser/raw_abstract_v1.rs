use eetf;

use ast::Module;
use ast::matcher::Matcher;
use result::BeamParseResult;

pub struct Parser {
}
impl Parser {
    pub fn new() -> Self {
        Parser {}
    }
    pub fn parse(self, abstract_code: eetf::Term) -> BeamParseResult<Module> {
        unimplemented!()
    }
}
