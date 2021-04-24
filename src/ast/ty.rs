//! Types
//!
//! See: [6.7 Types](http://erlang.org/doc/apps/erts/absform.html#id88630)
use ast;
use ast::common;
use ast::literal;

pub type UnaryOp = common::UnaryOp<Type>;
pub type BinaryOp = common::BinaryOp<Type>;

#[derive(Debug, Clone)]
pub enum Type {
    Atom(Box<literal::Atom>),
    Integer(Box<literal::Integer>),
    Var(Box<common::Var>),
    Annotated(Box<Annotated>),
    UnaryOp(Box<UnaryOp>),
    BinaryOp(Box<BinaryOp>),
    BitString(Box<BitString>),
    Nil(Box<common::Nil>),
    AnyFun(Box<AnyFun>),
    Function(Box<Fun>),
    Range(Box<Range>),
    Map(Box<Map>),
    BuiltIn(Box<BuiltInType>),
    Record(Box<Record>),
    Remote(Box<RemoteType>),
    AnyTuple(Box<AnyTuple>),
    Tuple(Box<Tuple>),
    Union(Box<Union>),
    User(Box<UserType>),
}
impl_from!(Type::Atom(literal::Atom));
impl_from!(Type::Integer(literal::Integer));
impl_from!(Type::Var(common::Var));
impl_from!(Type::Annotated(Annotated));
impl_from!(Type::UnaryOp(UnaryOp));
impl_from!(Type::BinaryOp(BinaryOp));
impl_from!(Type::BitString(BitString));
impl_from!(Type::Nil(common::Nil));
impl_from!(Type::AnyFun(AnyFun));
impl_from!(Type::Function(Fun));
impl_from!(Type::Range(Range));
impl_from!(Type::Map(Map));
impl_from!(Type::BuiltIn(BuiltInType));
impl_from!(Type::Record(Record));
impl_from!(Type::Remote(RemoteType));
impl_from!(Type::AnyTuple(AnyTuple));
impl_from!(Type::Tuple(Tuple));
impl_from!(Type::Union(Union));
impl_from!(Type::User(UserType));
impl ast::Node for Type {
    fn line(&self) -> ast::LineNum {
        match *self {
            Type::Integer(ref x) => x.line(),
            Type::Atom(ref x) => x.line(),
            Type::Var(ref x) => x.line(),
            Type::Annotated(ref x) => x.line(),
            Type::UnaryOp(ref x) => x.line(),
            Type::BinaryOp(ref x) => x.line(),
            Type::BitString(ref x) => x.line(),
            Type::Nil(ref x) => x.line(),
            Type::AnyFun(ref x) => x.line(),
            Type::Function(ref x) => x.line(),
            Type::Range(ref x) => x.line(),
            Type::Map(ref x) => x.line(),
            Type::BuiltIn(ref x) => x.line(),
            Type::Record(ref x) => x.line(),
            Type::Remote(ref x) => x.line(),
            Type::AnyTuple(ref x) => x.line(),
            Type::Tuple(ref x) => x.line(),
            Type::Union(ref x) => x.line(),
            Type::User(ref x) => x.line(),
        }
    }
}
impl Type {
    pub fn any(line: ast::LineNum) -> Self {
        Type::BuiltIn(Box::new(BuiltInType::new(
            line,
            "any".to_string(),
            Vec::new(),
        )))
    }
}

#[derive(Debug, Clone)]
pub struct UserType {
    pub line: ast::LineNum,
    pub name: String,
    pub args: Vec<Type>,
}
impl_node!(UserType);
impl UserType {
    pub fn new(line: ast::LineNum, name: String, args: Vec<Type>) -> Self {
        UserType { line, name, args }
    }
}

#[derive(Debug, Clone)]
pub struct Union {
    pub line: ast::LineNum,
    pub types: Vec<Type>,
}
impl_node!(Union);
impl Union {
    pub fn new(line: ast::LineNum, types: Vec<Type>) -> Self {
        Union { line, types }
    }
}

#[derive(Debug, Clone)]
pub struct AnyTuple {
    pub line: ast::LineNum,
}
impl_node!(AnyTuple);
impl AnyTuple {
    pub fn new(line: ast::LineNum) -> Self {
        AnyTuple { line }
    }
}

#[derive(Debug, Clone)]
pub struct Tuple {
    pub line: ast::LineNum,
    pub elements: Vec<Type>,
}
impl_node!(Tuple);
impl Tuple {
    pub fn new(line: ast::LineNum, elements: Vec<Type>) -> Self {
        Tuple { line, elements }
    }
}

#[derive(Debug, Clone)]
pub struct RemoteType {
    pub line: ast::LineNum,
    pub module: String,
    pub function: String,
    pub args: Vec<Type>,
}
impl_node!(RemoteType);
impl RemoteType {
    pub fn new(line: ast::LineNum, module: String, function: String, args: Vec<Type>) -> Self {
        RemoteType {
            line,
            module,
            function,
            args,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Record {
    pub line: ast::LineNum,
    pub name: String,
    pub fields: Vec<RecordField>,
}
impl_node!(Record);
impl Record {
    pub fn new(line: ast::LineNum, name: String, fields: Vec<RecordField>) -> Self {
        Record { line, name, fields }
    }
}

#[derive(Debug, Clone)]
pub struct RecordField {
    pub line: ast::LineNum,
    pub name: String,
    pub ty: Type,
}
impl_node!(RecordField);
impl RecordField {
    pub fn new(line: ast::LineNum, name: String, ty: Type) -> Self {
        RecordField { line, name, ty }
    }
}

#[derive(Debug, Clone)]
pub struct BuiltInType {
    pub line: ast::LineNum,
    pub name: String,
    pub args: Vec<Type>,
}
impl_node!(BuiltInType);
impl BuiltInType {
    pub fn new(line: ast::LineNum, name: String, args: Vec<Type>) -> Self {
        BuiltInType { line, name, args }
    }
}

#[derive(Debug, Clone)]
pub struct Map {
    pub line: ast::LineNum,
    pub pairs: Vec<MapPair>,
}
impl_node!(Map);
impl Map {
    pub fn new(line: ast::LineNum, pairs: Vec<MapPair>) -> Self {
        Map { line, pairs }
    }
}

#[derive(Debug, Clone)]
pub struct MapPair {
    pub line: ast::LineNum,
    pub key: Type,
    pub value: Type,
}
impl_node!(MapPair);
impl MapPair {
    pub fn new(line: ast::LineNum, key: Type, value: Type) -> Self {
        MapPair { line, key, value }
    }
}

#[derive(Debug, Clone)]
pub struct Annotated {
    pub line: ast::LineNum,
    pub name: common::Var,
    pub ty: Type,
}
impl_node!(Annotated);
impl Annotated {
    pub fn new(line: ast::LineNum, name: common::Var, ty: Type) -> Self {
        Annotated { line, name, ty }
    }
}

#[derive(Debug, Clone)]
pub struct BitString {
    pub line: ast::LineNum,
    pub bytes: u64,
    pub tail_bits: u64,
}
impl_node!(BitString);
impl BitString {
    pub fn new(line: ast::LineNum, bytes: u64, tail_bits: u64) -> Self {
        BitString {
            line,
            bytes,
            tail_bits,
        }
    }
}

#[derive(Debug, Clone)]
pub struct AnyFun {
    pub line: ast::LineNum,
    pub return_type: Option<Type>,
}
impl_node!(AnyFun);
impl AnyFun {
    pub fn new(line: ast::LineNum) -> Self {
        AnyFun {
            line,
            return_type: None,
        }
    }
    pub fn return_type(mut self, return_type: Type) -> Self {
        self.return_type = Some(return_type);
        self
    }
}

#[derive(Debug, Clone)]
pub struct Fun {
    pub line: ast::LineNum,
    pub args: Vec<Type>,
    pub return_type: Type,
    pub constraints: Vec<Constraint>,
}
impl_node!(Fun);
impl Fun {
    pub fn new(line: ast::LineNum, args: Vec<Type>, return_type: Type) -> Self {
        Fun {
            line,
            args,
            return_type,
            constraints: Vec::new(),
        }
    }
    pub fn constraints(mut self, constraints: Vec<Constraint>) -> Self {
        self.constraints = constraints;
        self
    }
}

#[derive(Debug, Clone)]
pub struct Constraint {
    pub line: ast::LineNum,
    pub var: common::Var,
    pub subtype: Type,
}
impl_node!(Constraint);
impl Constraint {
    pub fn new(line: ast::LineNum, var: common::Var, subtype: Type) -> Self {
        Constraint { line, var, subtype }
    }
}

#[derive(Debug, Clone)]
pub struct Range {
    pub line: ast::LineNum,
    pub low: Type,
    pub high: Type,
}
impl_node!(Range);
impl Range {
    pub fn new(line: ast::LineNum, low: Type, high: Type) -> Self {
        Range { line, low, high }
    }
}
