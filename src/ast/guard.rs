//! Guards
//!
//! See: [6.6 Guards](http://erlang.org/doc/apps/erts/absform.html#id88356)
use ast;
use ast::literal;

pub type Var = ast::Variable;
pub type Nil = ast::Nil;
pub type Tuple = ast::Tuple<Guard>;
pub type Cons = ast::Cons<Guard>;
pub type Binary = ast::Binary<Guard>;
pub type UnaryOp = ast::UnaryOp<Guard>;
pub type BinaryOp = ast::BinaryOp<Guard>;
pub type Record = ast::Record<Guard>;
pub type RecordIndex = ast::RecordIndex<Guard>;
pub type Map = ast::Map<Guard>;
pub type LocalCall = ast::LocalCall<Guard>;
pub type RemoteCall = ast::RemoteCall<Guard>;

#[derive(Debug)]
pub struct OrGuard {
    and_guards: Vec<Guard>,
}
impl OrGuard {
    pub fn new(and_guards: Vec<Guard>) -> Self {
        OrGuard { and_guards: and_guards }
    }
}

#[derive(Debug)]
pub enum Guard {
    Integer(Box<literal::Integer>),
    Float(Box<literal::Float>),
    String(Box<literal::Str>),
    Char(Box<literal::Char>),
    Atom(Box<literal::Atom>),
    Var(Box<Var>),
    Tuple(Box<Tuple>),
    Nil(Box<Nil>),
    Cons(Box<Cons>),
    Binary(Box<Binary>),
    UnaryOp(Box<UnaryOp>),
    BinaryOp(Box<BinaryOp>),
    Record(Box<Record>),
    RecordIndex(Box<RecordIndex>),
    LocalCall(Box<LocalCall>),
    RemoteCall(Box<RemoteCall>),
}
impl_from!(Guard::Integer(literal::Integer));
impl_from!(Guard::Float(literal::Float));
impl_from!(Guard::String(literal::Str));
impl_from!(Guard::Char(literal::Char));
impl_from!(Guard::Atom(literal::Atom));
impl_from!(Guard::Var(Var));
impl_from!(Guard::Tuple(Tuple));
impl_from!(Guard::Nil(Nil));
impl_from!(Guard::Cons(Cons));
impl_from!(Guard::Binary(Binary));
impl_from!(Guard::UnaryOp(UnaryOp));
impl_from!(Guard::BinaryOp(BinaryOp));
impl_from!(Guard::Record(Record));
impl_from!(Guard::RecordIndex(RecordIndex));
impl_from!(Guard::LocalCall(LocalCall));
impl_from!(Guard::RemoteCall(RemoteCall));
