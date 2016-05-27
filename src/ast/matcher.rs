use eetf;

pub trait Pattern<'a> {
    type Value;
    fn do_match(&self, term: &'a eetf::Term) -> Option<Self::Value>;
}

impl<'a, P1, P2> Pattern<'a> for (P1, P2)
    where P1: Pattern<'a>,
          P2: Pattern<'a>
{
    type Value = (P1::Value, P2::Value);
    fn do_match(&self, term: &'a eetf::Term) -> Option<Self::Value> {
        term.as_tuple().and_then(|t| if t.elements.len() == 2 {
            self.0
                .do_match(&t.elements[0])
                .and_then(|v1| self.1.do_match(&t.elements[1]).and_then(|v2| Some((v1, v2))))
        } else {
            None
        })
    }
}

impl<'a> Pattern<'a> for &'static str {
    type Value = &'a eetf::Atom;
    fn do_match(&self, term: &'a eetf::Term) -> Option<Self::Value> {
        term.as_atom().and_then(|a| if a.name == *self {
            Some(a)
        } else {
            None
        })
    }
}

pub struct List;
impl<'a> Pattern<'a> for List {
    type Value = &'a eetf::List;
    fn do_match(&self, term: &'a eetf::Term) -> Option<Self::Value> {
        term.as_list()
    }
}
