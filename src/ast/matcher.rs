use eetf;

pub trait Pattern<'a> {
    type Value;
    fn try_match(&self, term: &'a eetf::Term) -> Result<Self::Value, Option<&'a eetf::Term>> {
        self.do_match(term).ok_or(None)
    }
    // TODO: delete
    fn do_match(&self, term: &'a eetf::Term) -> Option<Self::Value> {
        self.try_match(term).ok()
    }
    fn map_match<F, V>(&self, term: &'a eetf::Term, fun: F) -> Option<V>
        where F: FnOnce(Self::Value) -> V
    {
        self.do_match(term).map(fun)
    }
    fn try_map_match<F, V>(&self, term: &'a eetf::Term, fun: F) -> Result<V, Option<&'a eetf::Term>>
        where F: FnOnce(Self::Value) -> V
    {
        self.try_match(term).map(fun)
    }
}

pub struct Either<L, R>(pub L, pub R);
impl<'a, L, R> Pattern<'a> for Either<L, R>
    where L: Pattern<'a>,
          R: Pattern<'a>
{
    type Value = Result<L::Value, R::Value>;
    fn do_match(&self, term: &'a eetf::Term) -> Option<Self::Value> {
        if let Some(v) = self.0.do_match(term) {
            Some(Ok(v))
        } else if let Some(v) = self.1.do_match(term) {
            Some(Err(v))
        } else {
            None
        }
    }
}

impl<'a, P0, P1> Pattern<'a> for (P0, P1)
    where P0: Pattern<'a>,
          P1: Pattern<'a>
{
    type Value = (P0::Value, P1::Value);
    fn try_match(&self, term: &'a eetf::Term) -> Result<Self::Value, Option<&'a eetf::Term>> {
        term.as_tuple().ok_or(None).and_then(|t| if t.elements.len() == 2 {
            Ok((try!(self.0.try_match(&t.elements[0])), try!(self.1.try_match(&t.elements[1]))))
        } else {
            Err(None)
        })
    }
}
impl<'a, P0, P1, P2> Pattern<'a> for (P0, P1, P2)
    where P0: Pattern<'a>,
          P1: Pattern<'a>,
          P2: Pattern<'a>
{
    type Value = (P0::Value, P1::Value, P2::Value);
    fn try_match(&self, term: &'a eetf::Term) -> Result<Self::Value, Option<&'a eetf::Term>> {
        term.as_tuple().ok_or(None).and_then(|t| if t.elements.len() == 3 {
            Ok((try!(self.0.try_match(&t.elements[0])),
                try!(self.1.try_match(&t.elements[1])),
                try!(self.2.try_match(&t.elements[2]))))
        } else {
            Err(None)
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
    fn try_match(&self, term: &'a eetf::Term) -> Result<Self::Value, Option<&'a eetf::Term>> {
        term.as_tuple().ok_or(None).and_then(|t| if t.elements.len() == 4 {
            Ok((try!(self.0.try_match(&t.elements[0])),
                try!(self.1.try_match(&t.elements[1])),
                try!(self.2.try_match(&t.elements[2])),
                try!(self.3.try_match(&t.elements[3]))))
        } else {
            Err(None)
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
    fn try_match(&self, term: &'a eetf::Term) -> Result<Self::Value, Option<&'a eetf::Term>> {
        term.as_tuple().ok_or(None).and_then(|t| if t.elements.len() == 5 {
            Ok((try!(self.0.try_match(&t.elements[0])),
                try!(self.1.try_match(&t.elements[1])),
                try!(self.2.try_match(&t.elements[2])),
                try!(self.3.try_match(&t.elements[3])),
                try!(self.4.try_match(&t.elements[4]))))
        } else {
            Err(None)
        })
    }
}
impl<'a, P0, P1, P2, P3, P4, P5> Pattern<'a> for (P0, P1, P2, P3, P4, P5)
    where P0: Pattern<'a>,
          P1: Pattern<'a>,
          P2: Pattern<'a>,
          P3: Pattern<'a>,
          P4: Pattern<'a>,
          P5: Pattern<'a>
{
    type Value = (P0::Value, P1::Value, P2::Value, P3::Value, P4::Value, P5::Value);
    fn try_match(&self, term: &'a eetf::Term) -> Result<Self::Value, Option<&'a eetf::Term>> {
        term.as_tuple().ok_or(None).and_then(|t| if t.elements.len() == 6 {
            Ok((try!(self.0.try_match(&t.elements[0])),
                try!(self.1.try_match(&t.elements[1])),
                try!(self.2.try_match(&t.elements[2])),
                try!(self.3.try_match(&t.elements[3])),
                try!(self.4.try_match(&t.elements[4])),
                try!(self.5.try_match(&t.elements[5]))))
        } else {
            Err(None)
        })
    }
}

pub struct List2<P0, P1>(pub P0, pub P1);
impl<'a, P0, P1> Pattern<'a> for List2<P0, P1>
    where P0: Pattern<'a>,
          P1: Pattern<'a>
{
    type Value = (P0::Value, P1::Value);
    fn try_match(&self, term: &'a eetf::Term) -> Result<Self::Value, Option<&'a eetf::Term>> {
        term.as_list().ok_or(None).and_then(|l| if l.elements.len() == 2 {
            Ok((try!(self.0.try_match(&l.elements[0])), try!(self.1.try_match(&l.elements[1]))))
        } else {
            Err(None)
        })
    }
}

pub struct List3<P0, P1, P2>(pub P0, pub P1, pub P2);
impl<'a, P0, P1, P2> Pattern<'a> for List3<P0, P1, P2>
    where P0: Pattern<'a>,
          P1: Pattern<'a>,
          P2: Pattern<'a>
{
    type Value = (P0::Value, P1::Value, P2::Value);
    fn try_match(&self, term: &'a eetf::Term) -> Result<Self::Value, Option<&'a eetf::Term>> {
        term.as_list().ok_or(None).and_then(|l| if l.elements.len() == 3 {
            Ok((try!(self.0.try_match(&l.elements[0])),
                try!(self.1.try_match(&l.elements[1])),
                try!(self.2.try_match(&l.elements[2]))))
        } else {
            Err(None)
        })
    }
}

pub struct Nil;
impl<'a> Pattern<'a> for Nil {
    type Value = &'a [eetf::Term];
    fn try_match(&self, term: &'a eetf::Term) -> Result<Self::Value, Option<&'a eetf::Term>> {
        term.as_list().ok_or(None).and_then(|l| if l.elements.is_empty() {
            Ok(&l.elements[..])
        } else {
            Err(None)
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

pub struct List<P>(pub P);
impl<'a, P> Pattern<'a> for List<P>
    where P: Pattern<'a>
{
    type Value = Vec<P::Value>;
    fn try_match(&self, term: &'a eetf::Term) -> Result<Self::Value, Option<&'a eetf::Term>> {
        term.as_list()
            .ok_or(None)
            .and_then(|l| {
                let mut v = Vec::with_capacity(l.elements.len());
                for e in &l.elements {
                    v.push(try!(self.0.try_match(e)));
                }
                Ok(v)
            })
    }
}

// XXX: Support latin-1 (?)
pub struct Str;
impl<'a> Pattern<'a> for Str {
    type Value = String;
    fn do_match(&self, term: &'a eetf::Term) -> Option<Self::Value> {
        term.as_list().and_then(|l| {
            let mut s = String::with_capacity(l.elements.len());
            for c in &l.elements {
                if let Some(c) = c.as_fix_integer()
                    .and_then(|i| ::std::char::from_u32(i.value as u32)) {
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

pub struct I32;
impl<'a> Pattern<'a> for I32 {
    type Value = i32;
    fn do_match(&self, term: &'a eetf::Term) -> Option<Self::Value> {
        term.as_fix_integer().map(|i| i.value)
    }
}

pub struct U64;
impl<'a> Pattern<'a> for U64 {
    type Value = u64;
    fn do_match(&self, term: &'a eetf::Term) -> Option<Self::Value> {
        use num::traits::ToPrimitive;
        None.or_else(|| term.as_fix_integer().map(|i| i.value as Self::Value))
            .or_else(|| term.as_big_integer().and_then(|i| i.value.to_u64()))
    }
}

pub struct Int; // TODO: UInt (?)
impl<'a> Pattern<'a> for Int {
    type Value = ::num::bigint::BigInt;
    fn do_match(&self, term: &'a eetf::Term) -> Option<Self::Value> {
        use num::traits::FromPrimitive;
        None.or_else(|| term.as_fix_integer().map(|i| FromPrimitive::from_i32(i.value).unwrap()))
            .or_else(|| term.as_big_integer().map(|i| i.value.clone()))
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
