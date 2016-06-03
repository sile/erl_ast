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

// TODO: delete
pub type AtomLit = literal::Atom;
pub type CharLit = literal::Char;
pub type StringLit = literal::Str;
pub type IntegerLit = literal::Integer;
pub type FloatLit = literal::Float;
pub type Pattern = pattern::Pattern;
pub type Expression = expr::Expression;
pub type Catch = expr::Catch;
pub type Case = expr::Case;
pub type Block = expr::Block;
pub type If = expr::If;
pub type Try = expr::Try;
pub type Receive = expr::Receive;
pub type AnonymousFun = expr::AnonymousFun;
pub type Qualifier = expr::Qualifier;
pub type Comprehension = expr::Comprehension;
pub type Clause = clause::Clause;
pub type OrGuard = guard::OrGuard;
pub type Guard = guard::Guard;
pub type Type = typ::Type;
pub type AnnotatedType = typ::Annotated;
pub type BitStringType = typ::BitString;
pub type AnyFunType = typ::AnyFun;
pub type FunctionType = typ::Fun;
pub type RangeType = typ::Range;
pub type MapType = typ::Map;
pub type TupleType = typ::Tuple;
pub type AnyTupleType = typ::AnyTuple;
pub type RecordType = typ::Record;
pub type RemoteType = typ::RemoteType;
pub type UserType = typ::UserType;
pub type BuiltInType = typ::BuiltInType;
pub type UnionType = typ::Union;
pub type RecordFieldType = typ::RecordField;
pub type MapPairType = typ::MapPair;
pub type FunctionConstraint = typ::Constraint;
pub type Variable = common::Variable;
pub type Nil = common::Nil;
pub type Match<T, F> = common::Match<T, F>;
pub type Cons<T> = common::Cons<T>;
pub type UnaryOp<T> = common::UnaryOp<T>;
pub type BinaryOp<T> = common::BinaryOp<T>;
pub type Record<T> = common::Record<T>;
pub type Binary<T> = common::Binary<T>;
pub type Tuple<T> = common::Tuple<T>;
pub type Map<T> = common::Map<T>;
pub use self::common::InternalFun;
pub use self::common::ExternalFun;
pub use self::common::RecordIndex;
pub use self::common::LocalCall;
pub use self::common::RemoteCall;
pub use self::common::BinElement;
pub use self::common::BinElementTypeSpec;
pub use self::common::RecordField;
pub use self::common::MapPair;
