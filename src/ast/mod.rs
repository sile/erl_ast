macro_rules! impl_from {
    ($to:ident :: $constructor:ident ($from:ty)) => {
        impl ::std::convert::From<$from> for $to {
            fn from(x: $from) -> Self {
                $to::$constructor(::std::convert::From::from(x))
            }
        }
    }
}

macro_rules! impl_node {
    ($x:ty) => {
        impl ::ast::Node for $x {
            fn line(&self) -> LineNum {
                self.line
            }
        }
    }
}

pub mod form;
pub mod literal;
pub mod pattern;
pub mod expr;
pub mod clause;
pub mod guard;
pub mod typ;
pub mod common;

pub type LineNum = i32;
pub type Arity = u32;

pub trait Node {
    fn line(&self) -> LineNum;
}
