use eetf;

pub trait Pattern<'a> {
    type Value;
    fn do_match(&self, term: &'a eetf::Term) -> Option<Self::Value>;
}

macro_rules! try_match {
    ($pattern:expr, $value:expr) => (
        match $pattern.do_match($value) {
            None => return None,
            Some(v) => v
        }
    )
}

pub struct Or<'a, P: 'static>(pub &'a [P]);
impl<'a, P> Pattern<'a> for Or<'a, P>
    where P: Pattern<'a>
{
    type Value = P::Value;
    fn do_match(&self, term: &'a eetf::Term) -> Option<Self::Value> {
        for p in self.0 {
            if let Some(v) = p.do_match(term) {
                return Some(v);
            }
        }
        None
    }
}

impl<'a, P0, P1> Pattern<'a> for (P0, P1)
    where P0: Pattern<'a>,
          P1: Pattern<'a>
{
    type Value = (P0::Value, P1::Value);
    fn do_match(&self, term: &'a eetf::Term) -> Option<Self::Value> {
        term.as_tuple().and_then(|t| if t.elements.len() == 2 {
            Some((try_match!(self.0, &t.elements[0]), try_match!(self.1, &t.elements[1])))
        } else {
            None
        })
    }
}
impl<'a, P0, P1, P2> Pattern<'a> for (P0, P1, P2)
    where P0: Pattern<'a>,
          P1: Pattern<'a>,
          P2: Pattern<'a>
{
    type Value = (P0::Value, P1::Value, P2::Value);
    fn do_match(&self, term: &'a eetf::Term) -> Option<Self::Value> {
        term.as_tuple().and_then(|t| if t.elements.len() == 3 {
            Some((try_match!(self.0, &t.elements[0]),
                  try_match!(self.1, &t.elements[1]),
                  try_match!(self.2, &t.elements[2])))
        } else {
            None
        })
    }
}
impl<'a, P0, P1, P2, P3> Pattern<'a> for (P0, P1, P2, P3)
    where P0: Pattern<'a>,
          P1: Pattern<'a>,
          P2: Pattern<'a>,
          P3: Pattern<'a>
{
    type Value = (P0::Value, P1::Value, P2::Value, P3::Value);
    fn do_match(&self, term: &'a eetf::Term) -> Option<Self::Value> {
        term.as_tuple().and_then(|t| if t.elements.len() == 4 {
            Some((try_match!(self.0, &t.elements[0]),
                  try_match!(self.1, &t.elements[1]),
                  try_match!(self.2, &t.elements[2]),
                  try_match!(self.3, &t.elements[3])))
        } else {
            None
        })
    }
}
impl<'a, P0, P1, P2, P3, P4> Pattern<'a> for (P0, P1, P2, P3, P4)
    where P0: Pattern<'a>,
          P1: Pattern<'a>,
          P2: Pattern<'a>,
          P3: Pattern<'a>,
          P4: Pattern<'a>
{
    type Value = (P0::Value, P1::Value, P2::Value, P3::Value, P4::Value);
    fn do_match(&self, term: &'a eetf::Term) -> Option<Self::Value> {
        term.as_tuple().and_then(|t| if t.elements.len() == 5 {
            Some((try_match!(self.0, &t.elements[0]),
                  try_match!(self.1, &t.elements[1]),
                  try_match!(self.2, &t.elements[2]),
                  try_match!(self.3, &t.elements[3]),
                  try_match!(self.4, &t.elements[4])))
        } else {
            None
        })
    }
}

pub struct List2<P0, P1>(pub P0, pub P1);
impl<'a, P0, P1> Pattern<'a> for List2<P0, P1>
    where P0: Pattern<'a>,
          P1: Pattern<'a>
{
    type Value = (P0::Value, P1::Value);
    fn do_match(&self, term: &'a eetf::Term) -> Option<Self::Value> {
        term.as_list().and_then(|l| if l.elements.len() == 2 {
            Some((try_match!(self.0, &l.elements[0]), try_match!(self.1, &l.elements[1])))
        } else {
            None
        })
    }
}

pub struct Nil;
impl<'a> Pattern<'a> for Nil {
    type Value = &'a [eetf::Term];
    fn do_match(&self, term: &'a eetf::Term) -> Option<Self::Value> {
        term.as_list().and_then(|l| if l.elements.is_empty() {
            Some(&l.elements[..])
        } else {
            None
        })
    }
}

impl<'a> Pattern<'a> for &'static str {
    type Value = &'static str;
    fn do_match(&self, term: &'a eetf::Term) -> Option<Self::Value> {
        term.as_atom().and_then(|a| if a.name == *self {
            Some(*self)
        } else {
            None
        })
    }
}

pub struct AnyList;
impl<'a> Pattern<'a> for AnyList {
    type Value = &'a [eetf::Term];
    fn do_match(&self, term: &'a eetf::Term) -> Option<Self::Value> {
        term.as_list().map(|l| &l.elements[..])
    }
}

pub struct List<P: 'static>(pub P);
impl<'a, P> Pattern<'a> for List<P>
    where P: Pattern<'a>
{
    type Value = Vec<P::Value>;
    fn do_match(&self, term: &'a eetf::Term) -> Option<Self::Value> {
        term.as_list().and_then(|l| {
            let mut v = Vec::with_capacity(l.elements.len());
            for e in &l.elements {
                v.push(try_match!(self.0, e));
            }
            Some(v)
        })
    }
}

pub struct Str;
impl<'a> Pattern<'a> for Str {
    type Value = String;
    fn do_match(&self, term: &'a eetf::Term) -> Option<Self::Value> {
        term.as_list().and_then(|l| {
            let mut s = String::with_capacity(l.elements.len());
            for c in &l.elements {
                if let Some(c) = c.as_fix_integer()
                    .and_then(|i| if 0 <= i.value && i.value < 0x80 {
                        Some(i.value as u8 as char)
                    } else {
                        None
                    }) {
                    s.push(c);
                } else {
                    return None;
                }
            }
            Some(s)
        })
    }
}

pub struct U32;
impl<'a> Pattern<'a> for U32 {
    type Value = u32;
    fn do_match(&self, term: &'a eetf::Term) -> Option<Self::Value> {
        term.as_fix_integer().and_then(|i| if 0 <= i.value {
            Some(i.value as Self::Value)
        } else {
            None
        })
    }
}

pub struct F64;
impl<'a> Pattern<'a> for F64 {
    type Value = f64;
    fn do_match(&self, term: &'a eetf::Term) -> Option<Self::Value> {
        term.as_float().map(|f| f.value)
    }
}

pub struct Atom;
impl<'a> Pattern<'a> for Atom {
    type Value = &'a str;
    fn do_match(&self, term: &'a eetf::Term) -> Option<Self::Value> {
        term.as_atom().map(|a| a.name.as_str())
    }
}

pub struct Term;
impl<'a> Pattern<'a> for Term {
    type Value = &'a eetf::Term;
    fn do_match(&self, term: &'a eetf::Term) -> Option<Self::Value> {
        Some(term)
    }
}
