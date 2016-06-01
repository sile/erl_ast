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

pub mod type_;
pub mod codec;
pub mod matcher;

macro_rules! impl_node_1 {
    ($x:ty) => {
        impl<T> Node for $x {
            fn line(&self) -> LineNum {
                self.line
            }
        }
    }
}
macro_rules! impl_node_2 {
    ($x:ty) => {
        impl<T,U> Node for $x {
            fn line(&self) -> LineNum {
                self.line
            }
        }
    }
}

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

// TODO: Move to common module
#[derive(Debug)]
pub struct Match<L, R> {
    pub line: LineNum,
    pub left: L,
    pub right: R,
}
impl_node_2!(Match<T,U>);
impl<L, R> Match<L, R> {
    pub fn new(line: LineNum, left: L, right: R) -> Self {
        Match {
            line: line,
            left: left,
            right: right,
        }
    }
}

#[derive(Debug)]
pub struct Tuple<T> {
    pub line: LineNum,
    pub elements: Vec<T>,
}
impl_node_1!(Tuple<T>);
impl<T> Tuple<T> {
    pub fn new(line: LineNum, elements: Vec<T>) -> Self {
        Tuple {
            line: line,
            elements: elements,
        }
    }
}

#[derive(Debug)]
pub struct Nil {
    pub line: LineNum,
}
impl_node!(Nil);
impl Nil {
    pub fn new(line: LineNum) -> Self {
        Nil { line: line }
    }
}

#[derive(Debug)]
pub struct Cons<T> {
    pub line: LineNum,
    pub head: T,
    pub tail: T,
}
impl_node_1!(Cons<T>);
impl<T> Cons<T> {
    pub fn new(line: LineNum, head: T, tail: T) -> Self {
        Cons {
            line: line,
            head: head,
            tail: tail,
        }
    }
}

#[derive(Debug)]
pub struct Binary<T> {
    pub line: LineNum,
    pub elements: Vec<BinElement<T>>,
}
impl_node_1!(Binary<T>);
impl<T> Binary<T> {
    pub fn new(line: LineNum, elements: Vec<BinElement<T>>) -> Self {
        Binary {
            line: line,
            elements: elements,
        }
    }
}

#[derive(Debug)]
pub struct BinElement<T> {
    pub line: LineNum,
    pub element: T,
    pub size: Option<T>,
    pub tsl: Option<Vec<BinElementTypeSpec>>,
}
impl_node_1!(BinElement<T>);
impl<T> BinElement<T> {
    pub fn new(line: LineNum, element: T) -> Self {
        BinElement {
            line: line,
            element: element,
            size: None,
            tsl: None,
        }
    }
    pub fn size(mut self, size: T) -> Self {
        self.size = Some(size);
        self
    }
    pub fn tsl(mut self, tsl: Vec<BinElementTypeSpec>) -> Self {
        self.tsl = Some(tsl);
        self
    }
}

#[derive(Debug)]
pub struct BinElementTypeSpec {
    pub name: String,
    pub value: Option<u64>,
}
impl BinElementTypeSpec {
    pub fn new(name: String, value: Option<u64>) -> Self {
        BinElementTypeSpec {
            name: name,
            value: value,
        }
    }
}

#[derive(Debug)]
pub struct UnaryOp<T> {
    pub line: LineNum,
    pub operator: String,
    pub operand: T,
}
impl_node_1!(UnaryOp<T>);
impl<T> UnaryOp<T> {
    pub fn new(line: LineNum, operator: String, operand: T) -> Self {
        UnaryOp {
            line: line,
            operator: operator,
            operand: operand,
        }
    }
}

#[derive(Debug)]
pub struct BinaryOp<T> {
    pub line: LineNum,
    pub operator: String,
    pub left_operand: T,
    pub right_operand: T,
}
impl_node_1!(BinaryOp<T>);
impl<T> BinaryOp<T> {
    pub fn new(line: LineNum, operator: String, left_operand: T, right_operand: T) -> Self {
        BinaryOp {
            line: line,
            operator: operator,
            left_operand: left_operand,
            right_operand: right_operand,
        }
    }
}

#[derive(Debug)]
pub struct Record<T> {
    pub line: LineNum,
    pub base: Option<Expression>,
    pub name: String,
    pub fields: Vec<RecordField<T>>,
}
impl_node_1!(Record<T>);
impl<T> Record<T> {
    pub fn new(line: LineNum, name: String, fields: Vec<RecordField<T>>) -> Self {
        Record {
            line: line,
            base: None,
            name: name,
            fields: fields,
        }
    }
    pub fn base(mut self, base: Expression) -> Self {
        self.base = Some(base);
        self
    }
}

#[derive(Debug)]
pub struct RecordField<T> {
    pub line: LineNum,
    pub name: Option<String>, // `None` means `_` (i.e., default value)
    pub value: T,
}
impl_node_1!(RecordField<T>);
impl<T> RecordField<T> {
    pub fn new(line: LineNum, name: Option<String>, value: T) -> Self {
        RecordField {
            line: line,
            name: name,
            value: value,
        }
    }
}

// TODO: => RecordField<T>
#[derive(Debug)]
pub struct RecordIndex<T> {
    pub line: LineNum,
    pub base: Option<T>,
    pub record: String,
    pub field: String,
}
impl_node_1!(RecordIndex<T>);
impl<T> RecordIndex<T> {
    pub fn new(line: LineNum, record: String, field: String) -> Self {
        RecordIndex {
            line: line,
            record: record,
            field: field,
            base: None,
        }
    }
    pub fn base(mut self, base: T) -> Self {
        self.base = Some(base);
        self
    }
}

#[derive(Debug)]
pub struct Map<T> {
    pub line: LineNum,
    pub base: Option<Expression>,
    pub pairs: Vec<MapPair<T>>,
}
impl_node_1!(Map<T>);
impl<T> Map<T> {
    pub fn new(line: LineNum, pairs: Vec<MapPair<T>>) -> Self {
        Map {
            line: line,
            base: None,
            pairs: pairs,
        }
    }
    pub fn base(mut self, base: Expression) -> Self {
        self.base = Some(base);
        self
    }
}

#[derive(Debug)]
pub struct MapPair<T> {
    pub line: LineNum,
    pub is_assoc: bool,
    pub key: T,
    pub value: T,
}
impl_node_1!(MapPair<T>);
impl<T> MapPair<T> {
    pub fn new(line: LineNum, is_assoc: bool, key: T, value: T) -> Self {
        MapPair {
            line: line,
            is_assoc: is_assoc,
            key: key,
            value: value,
        }
    }
}

#[derive(Debug)]
pub struct LocalCall<T> {
    pub line: LineNum,
    pub function: T,
    pub args: Vec<T>,
}
impl_node_1!(LocalCall<T>);
impl<T> LocalCall<T> {
    pub fn new(line: LineNum, function: T, args: Vec<T>) -> Self {
        LocalCall {
            line: line,
            function: function,
            args: args,
        }
    }
}

#[derive(Debug)]
pub struct RemoteCall<T> {
    pub line: LineNum,
    pub module: T,
    pub function: T,
    pub args: Vec<T>,
}
impl_node_1!(RemoteCall<T>);
impl<T> RemoteCall<T> {
    pub fn new(line: LineNum, module: T, function: T, args: Vec<T>) -> Self {
        RemoteCall {
            line: line,
            module: module,
            function: function,
            args: args,
        }
    }
}

#[derive(Debug)]
pub struct InternalFun {
    pub line: LineNum,
    pub function: String,
    pub arity: Arity,
}
impl_node!(InternalFun);
impl InternalFun {
    pub fn new(line: LineNum, function: String, arity: Arity) -> Self {
        InternalFun {
            line: line,
            function: function,
            arity: arity,
        }
    }
}

#[derive(Debug)]
pub struct ExternalFun {
    pub line: LineNum,
    pub module: Expression,
    pub function: Expression,
    pub arity: Expression,
}
impl_node!(ExternalFun);
impl ExternalFun {
    pub fn new(line: LineNum, module: Expression, function: Expression, arity: Expression) -> Self {
        ExternalFun {
            line: line,
            module: module,
            function: function,
            arity: arity,
        }
    }
}

// 6.7 Types
#[derive(Debug)]
pub enum Type {
    Atom(Box<AtomLit>),
    Integer(Box<IntegerLit>),
    Var(Box<Variable>),
    Annotated(Box<AnnotatedType>),
    UnaryOp(Box<UnaryOp<Type>>),
    BinaryOp(Box<BinaryOp<Type>>),
    BitString(Box<BitStringType>),
    Nil(Box<Nil>),
    AnyFun(Box<AnyFunType>),
    Function(Box<FunctionType>),
    Range(Box<RangeType>),
    Map(Box<MapType>),
    BuiltIn(Box<BuiltInType>),
    Record(Box<RecordType>),
    Remote(Box<RemoteType>),
    AnyTuple(Box<AnyTupleType>),
    Tuple(Box<TupleType>),
    Union(Box<UnionType>),
    User(Box<UserType>),
}
impl_from!(Type::Atom(AtomLit));
impl_from!(Type::Integer(IntegerLit));
impl_from!(Type::Var(Variable));
impl_from!(Type::Annotated(AnnotatedType));
impl_from!(Type::UnaryOp(UnaryOp<Type>));
impl_from!(Type::BinaryOp(BinaryOp<Type>));
impl_from!(Type::BitString(BitStringType));
impl_from!(Type::Nil(Nil));
impl_from!(Type::AnyFun(AnyFunType));
impl_from!(Type::Function(FunctionType));
impl_from!(Type::Range(RangeType));
impl_from!(Type::Map(MapType));
impl_from!(Type::BuiltIn(BuiltInType));
impl_from!(Type::Record(RecordType));
impl_from!(Type::Remote(RemoteType));
impl_from!(Type::AnyTuple(AnyTupleType));
impl_from!(Type::Tuple(TupleType));
impl_from!(Type::Union(UnionType));
impl_from!(Type::User(UserType));
impl Type {
    pub fn any(line: LineNum) -> Self {
        Type::BuiltIn(Box::new(BuiltInType::new(line, "any".to_string(), Vec::new())))
    }
}

#[derive(Debug)]
pub struct UserType {
    pub line: LineNum,
    pub name: String,
    pub args: Vec<Type>,
}
impl_node!(UserType);
impl UserType {
    pub fn new(line: LineNum, name: String, args: Vec<Type>) -> Self {
        UserType {
            line: line,
            name: name,
            args: args,
        }
    }
}

#[derive(Debug)]
pub struct UnionType {
    pub line: LineNum,
    pub types: Vec<Type>,
}
impl_node!(UnionType);
impl UnionType {
    pub fn new(line: LineNum, types: Vec<Type>) -> Self {
        UnionType {
            line: line,
            types: types,
        }
    }
}

#[derive(Debug)]
pub struct AnyTupleType {
    pub line: LineNum,
}
impl_node!(AnyTupleType);
impl AnyTupleType {
    pub fn new(line: LineNum) -> Self {
        AnyTupleType { line: line }
    }
}

#[derive(Debug)]
pub struct TupleType {
    pub line: LineNum,
    pub elements: Vec<Type>,
}
impl_node!(TupleType);
impl TupleType {
    pub fn new(line: LineNum, elements: Vec<Type>) -> Self {
        TupleType {
            line: line,
            elements: elements,
        }
    }
}

#[derive(Debug)]
pub struct RemoteType {
    pub line: LineNum,
    pub module: String,
    pub function: String,
    pub args: Vec<Type>,
}
impl_node!(RemoteType);
impl RemoteType {
    pub fn new(line: LineNum, module: String, function: String, args: Vec<Type>) -> Self {
        RemoteType {
            line: line,
            module: module,
            function: function,
            args: args,
        }
    }
}

#[derive(Debug)]
pub struct RecordType {
    pub line: LineNum,
    pub name: String,
    pub fields: Vec<RecordFieldType>,
}
impl_node!(RecordType);
impl RecordType {
    pub fn new(line: LineNum, name: String, fields: Vec<RecordFieldType>) -> Self {
        RecordType {
            line: line,
            name: name,
            fields: fields,
        }
    }
}

#[derive(Debug)]
pub struct RecordFieldType {
    pub line: LineNum,
    pub name: String,
    pub type_: Type,
}
impl_node!(RecordFieldType);
impl RecordFieldType {
    pub fn new(line: LineNum, name: String, type_: Type) -> Self {
        RecordFieldType {
            line: line,
            name: name,
            type_: type_,
        }
    }
}

#[derive(Debug)]
pub struct BuiltInType {
    pub line: LineNum,
    pub name: String,
    pub args: Vec<Type>,
}
impl_node!(BuiltInType);
impl BuiltInType {
    pub fn new(line: LineNum, name: String, args: Vec<Type>) -> Self {
        BuiltInType {
            line: line,
            name: name,
            args: args,
        }
    }
}

#[derive(Debug)]
pub struct MapType {
    pub line: LineNum,
    pub pairs: Vec<MapPairType>,
}
impl_node!(MapType);
impl MapType {
    pub fn new(line: LineNum, pairs: Vec<MapPairType>) -> Self {
        MapType {
            line: line,
            pairs: pairs,
        }
    }
}

#[derive(Debug)]
pub struct MapPairType {
    pub line: LineNum,
    pub key: Type,
    pub value: Type,
}
impl_node!(MapPairType);
impl MapPairType {
    pub fn new(line: LineNum, key: Type, value: Type) -> Self {
        MapPairType {
            line: line,
            key: key,
            value: value,
        }
    }
}

#[derive(Debug)]
pub struct AnnotatedType {
    pub line: LineNum,
    pub name: Variable,
    pub type_: Type,
}
impl_node!(AnnotatedType);
impl AnnotatedType {
    pub fn new(line: LineNum, name: Variable, type_: Type) -> Self {
        AnnotatedType {
            line: line,
            name: name,
            type_: type_,
        }
    }
}

#[derive(Debug)]
pub struct BitStringType {
    pub line: LineNum,
    pub bytes: u64,
    pub tail_bits: u64,
}
impl_node!(BitStringType);
impl BitStringType {
    pub fn new(line: LineNum, bytes: u64, tail_bits: u64) -> Self {
        BitStringType {
            line: line,
            bytes: bytes,
            tail_bits: tail_bits,
        }
    }
}

#[derive(Debug)]
pub struct AnyFunType {
    pub line: LineNum,
    pub return_type: Option<Type>,
}
impl_node!(AnyFunType);
impl AnyFunType {
    pub fn new(line: LineNum) -> Self {
        AnyFunType {
            line: line,
            return_type: None,
        }
    }
    pub fn return_type(mut self, return_type: Type) -> Self {
        self.return_type = Some(return_type);
        self
    }
}

#[derive(Debug)]
pub struct FunctionType {
    pub line: LineNum,
    pub args: Vec<Type>,
    pub return_type: Type,
    pub constraints: Vec<FunctionConstraint>,
}
impl_node!(FunctionType);
impl FunctionType {
    pub fn new(line: LineNum, args: Vec<Type>, return_type: Type) -> Self {
        FunctionType {
            line: line,
            args: args,
            return_type: return_type,
            constraints: Vec::new(),
        }
    }
    pub fn constraints(mut self, constraints: Vec<FunctionConstraint>) -> Self {
        self.constraints = constraints;
        self
    }
}

#[derive(Debug)]
pub struct FunctionConstraint {
    pub line: LineNum,
    pub var: Variable,
    pub subtype: Type,
}
impl_node!(FunctionConstraint);
impl FunctionConstraint {
    pub fn new(line: LineNum, var: Variable, subtype: Type) -> Self {
        FunctionConstraint {
            line: line,
            var: var,
            subtype: subtype,
        }
    }
}

#[derive(Debug)]
pub struct RangeType {
    pub line: LineNum,
    pub low: Type,
    pub high: Type,
}
impl_node!(RangeType);
impl RangeType {
    pub fn new(line: LineNum, low: Type, high: Type) -> Self {
        RangeType {
            line: line,
            low: low,
            high: high,
        }
    }
}

// Others
#[derive(Debug)]
pub struct Variable {
    pub line: LineNum,
    pub name: String,
}
impl_node!(Variable);
impl Variable {
    pub fn new(line: LineNum, name: String) -> Self {
        Variable {
            line: line,
            name: name,
        }
    }
    pub fn is_anonymous(&self) -> bool {
        self.name == "_"
    }
}
