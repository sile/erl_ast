use ast::LineNum;
use ast::Arity;
use ast::expr::Expression;

macro_rules! impl_node_1 {
    ($x:ty) => {
        impl<T> ::ast::Node for $x {
            fn line(&self) -> LineNum {
                self.line
            }
        }
    }
}
macro_rules! impl_node_2 {
    ($x:ty) => {
        impl<T,U> ::ast::Node for $x {
            fn line(&self) -> LineNum {
                self.line
            }
        }
    }
}

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