use eetf;

pub struct Matcher;
pub struct Matched;

pub enum Pattern {
    Tuple2(Box<Pattern>, Box<Pattern>),
    Atom(&'static str),
    Any,
}
impl Pattern {
    pub fn tuple2(p1: Pattern, p2: Pattern) -> Self {
        Pattern::Tuple2(Box::new(p1), Box::new(p2))
    }

    pub fn do_match(self, _: eetf::Term) -> Option<Matched> {
        unimplemented!()
    }
}
