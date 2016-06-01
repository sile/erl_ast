//! Clauses
//!
//! See: [6.5 clauses](http://erlang.org/doc/apps/erts/absform.html#id88135)
use ast;
use ast::LineNum;

#[derive(Debug)]
pub struct Clause {
    pub line: LineNum,
    pub patterns: Vec<ast::pattern::Pattern>,
    pub guards: Vec<ast::OrGuard>,
    pub body: Vec<ast::expr::Expression>,
}
impl_node!(Clause);
impl Clause {
    pub fn new(line: LineNum,
               patterns: Vec<ast::pattern::Pattern>,
               guards: Vec<ast::OrGuard>,
               body: Vec<ast::expr::Expression>)
               -> Self {
        Clause {
            line: line,
            patterns: patterns,
            guards: guards,
            body: body,
        }
    }
}
