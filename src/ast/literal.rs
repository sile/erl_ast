//! Atomic Literals
//!
//! See: [6.2 Atomic Literals](http://erlang.org/doc/apps/erts/absform.html#id87074)
use num::bigint::BigInt;
use ast::LineNum;

#[derive(Debug)]
pub struct Integer {
    pub line: LineNum,
    pub value: BigInt,
}
impl_node!(Integer);
impl Integer {
    pub fn new(line: LineNum, value: BigInt) -> Self {
        Integer {
            line: line,
            value: value,
        }
    }
    pub fn to_u64(&self) -> Option<u64> {
        use num::traits::ToPrimitive;
        self.value.to_u64()
    }
}

#[derive(Debug)]
pub struct Char {
    pub line: LineNum,
    pub value: char,
}
impl_node!(Char);
impl Char {
    pub fn new(line: LineNum, value: char) -> Self {
        Char {
            line: line,
            value: value,
        }
    }
}

#[derive(Debug)]
pub struct Float {
    pub line: LineNum,
    pub value: f64,
}
impl_node!(Float);
impl Float {
    pub fn new(line: LineNum, value: f64) -> Self {
        Float {
            line: line,
            value: value,
        }
    }
}

#[derive(Debug)]
pub struct Str {
    pub line: LineNum,
    pub value: String,
}
impl_node!(Str);
impl Str {
    pub fn new(line: LineNum, value: String) -> Self {
        Str {
            line: line,
            value: value,
        }
    }
}

#[derive(Debug)]
pub struct Atom {
    pub line: LineNum,
    pub value: String,
}
impl_node!(Atom);
impl Atom {
    pub fn new(line: LineNum, value: String) -> Self {
        Atom {
            line: line,
            value: value,
        }
    }
}
