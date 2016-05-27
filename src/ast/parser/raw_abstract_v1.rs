use eetf;

use result::BeamParseResult;
use error::BeamParseError;
use ast;
use ast::matcher::Pattern;
use ast::matcher::List;

pub struct Parser {
}
impl Parser {
    pub fn new() -> Self {
        Parser {}
    }
    pub fn parse(self, abstract_code: eetf::Term) -> BeamParseResult<ast::Module> {
        let (_, forms) = try!(("raw_abstract_v1", List)
            .do_match(&abstract_code)
            .ok_or(BeamParseError::UnknownAbstractFormat));
        unimplemented!();
    }
}
