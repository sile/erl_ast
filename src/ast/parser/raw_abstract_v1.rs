use eetf;

use result::BeamParseResult;
use error::BeamParseError;
use ast;
use ast::matcher::Matcher;
use ast::matcher::Pattern as P;

pub struct Parser {
}
impl Parser {
    pub fn new() -> Self {
        Parser {}
    }
    pub fn parse(self, abstract_code: eetf::Term) -> BeamParseResult<ast::Module> {
        let (_, forms) = try!(P::tuple2(P::atom("raw_abstract_v1"), P::any())
            .do_match(abstract_code)
            .ok_or(BeamParseError::UnknownAbstractFormat));
        unimplemented!();
    }
}
