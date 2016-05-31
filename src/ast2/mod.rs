use eetf;

pub mod codec;

use std::path::Path;
use std::convert::From;
use result::BeamParseResult;

macro_rules! impl_node {
    ($x:ty) => {
        impl Node for $x {
            fn line(&self) -> LineNum {
                self.line
            }
        }
    }
}
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

pub type LineNum = u32;
pub type Arity = u32;

pub trait Node {
    fn line(&self) -> LineNum;
}

// 6.1 Module Declarations and Forms
#[derive(Debug)]
pub struct ModuleDecl {
    pub forms: Vec<Form>,
}
impl ModuleDecl {
    pub fn from_beam_file<P: AsRef<Path>>(path: P) -> BeamParseResult<Self> {
        let code = try!(self::codec::raw_abstract_v1::AbstractCode::from_beam_file(path));
        code.to_module_decl()
    }
}

macro_rules! impl_from {
    ($to:ident :: $constructor:ident ($from:ident)) => {
        impl From<$from> for $to {
            fn from(x: $from) -> Self {
                $to::$constructor(x)
            }
        }
    }
}

#[derive(Debug)]
pub enum Form {
    Module(ModuleAttr),
    Behaviour(BehaviourAttr),
    Export(ExportAttr),
    Import(ImportAttr),
    ExportType(ExportTypeAttr),
    Compile(CompileOptionsAttr),
    File(FileAttr),
    Record(RecordDecl),
    Type(TypeDecl),
    Spec(FunctionSpec),
    Attr(WildAttr),
    Function(FunctionDecl),
}
impl_from!(Form::Module(ModuleAttr));
impl_from!(Form::Behaviour(BehaviourAttr));
impl_from!(Form::Export(ExportAttr));
impl_from!(Form::Import(ImportAttr));
impl_from!(Form::ExportType(ExportTypeAttr));
impl_from!(Form::Compile(CompileOptionsAttr));
impl_from!(Form::File(FileAttr));
impl_from!(Form::Record(RecordDecl));
impl_from!(Form::Type(TypeDecl));
impl_from!(Form::Spec(FunctionSpec));
impl_from!(Form::Attr(WildAttr));
impl_from!(Form::Function(FunctionDecl));

#[derive(Debug)]
pub struct ModuleAttr {
    pub line: LineNum,
    pub name: String,
}
impl_node!(ModuleAttr);
impl ModuleAttr {
    pub fn new(line: LineNum, name: String) -> Self {
        ModuleAttr {
            line: line,
            name: name,
        }
    }
}

#[derive(Debug)]
pub struct BehaviourAttr {
    pub line: LineNum,
    pub is_british: bool,
    pub name: String,
}
impl_node!(BehaviourAttr);
impl BehaviourAttr {
    pub fn new(line: LineNum, name: String) -> Self {
        BehaviourAttr {
            line: line,
            name: name,
            is_british: true,
        }
    }
    pub fn british(mut self, is_british: bool) -> Self {
        self.is_british = is_british;
        self
    }
}

#[derive(Debug)]
pub struct ExportAttr {
    pub line: LineNum,
    pub functions: Vec<Export>,
}
impl_node!(ExportAttr);
impl ExportAttr {
    pub fn new(line: LineNum, functions: Vec<Export>) -> Self {
        ExportAttr {
            line: line,
            functions: functions,
        }
    }
}

#[derive(Debug)]
pub struct ImportAttr {
    pub line: LineNum,
    pub module: String,
    pub functions: Vec<Import>,
}
impl_node!(ImportAttr);
impl ImportAttr {
    pub fn new(line: LineNum, module: String, functions: Vec<Import>) -> Self {
        ImportAttr {
            line: line,
            module: module,
            functions: functions,
        }
    }
}

#[derive(Debug)]
pub struct ExportTypeAttr {
    pub line: LineNum,
    pub types: Vec<ExportType>,
}
impl_node!(ExportTypeAttr);
impl ExportTypeAttr {
    pub fn new(line: LineNum, types: Vec<ExportType>) -> Self {
        ExportTypeAttr {
            line: line,
            types: types,
        }
    }
}

#[derive(Debug)]
pub struct CompileOptionsAttr {
    pub line: LineNum,
    pub options: eetf::Term,
}
impl_node!(CompileOptionsAttr);
impl CompileOptionsAttr {
    pub fn new(line: LineNum, options: eetf::Term) -> Self {
        CompileOptionsAttr {
            line: line,
            options: options,
        }
    }
}

#[derive(Debug)]
pub struct FileAttr {
    pub line: LineNum,
    pub original_file: String,
    pub original_line: LineNum,
}
impl_node!(FileAttr);
impl FileAttr {
    pub fn new(line: LineNum, original_file: String, original_line: LineNum) -> Self {
        FileAttr {
            line: line,
            original_file: original_file,
            original_line: original_line,
        }
    }
}

#[derive(Debug)]
pub struct RecordDecl {
    pub line: LineNum,
    pub name: String,
    pub fields: Vec<RecordFieldDecl>,
}
impl_node!(RecordDecl);

#[derive(Debug)]
pub struct TypeDecl {
    pub line: LineNum,
    pub is_opaque: bool,
    pub name: String,
    pub variables: Vec<Variable>,
    pub type_: Type,
}
impl_node!(TypeDecl);

#[derive(Debug)]
pub struct FunctionSpec {
    pub line: LineNum,
    pub module: Option<String>,
    pub name: String,
    pub types: Vec<FunctionType>,
}
impl_node!(FunctionSpec);

#[derive(Debug)]
pub struct WildAttr {
    pub line: LineNum,
    pub name: String,
    pub value: eetf::Term,
}
impl_node!(WildAttr);
impl WildAttr {
    pub fn new(line: LineNum, name: String, value: eetf::Term) -> Self {
        WildAttr {
            line: line,
            name: name,
            value: value,
        }
    }
}

#[derive(Debug)]
pub struct FunctionDecl {
    pub line: LineNum,
    pub name: String,
    pub arity: Arity,
    pub clauses: Vec<Clause>,
}
impl_node!(FunctionDecl);

#[derive(Debug)]
pub struct RecordFieldDecl {
    pub line: LineNum,
    pub name: String,
    pub type_: Type,
    pub default_value: Expression,
}
impl_node!(RecordFieldDecl);

// 6.2 Atomic Literals
#[derive(Debug)]
pub struct IntegerLit {
    pub line: LineNum,
    pub value: u64,
}
impl_node!(IntegerLit);

#[derive(Debug)]
pub struct FloatLit {
    pub line: LineNum,
    pub value: f64,
}
impl_node!(FloatLit);

#[derive(Debug)]
pub struct StringLit {
    pub line: LineNum,
    pub value: String,
}
impl_node!(StringLit);

#[derive(Debug)]
pub struct AtomLit {
    pub line: LineNum,
    pub value: String,
}
impl_node!(AtomLit);

// 6.3 Patterns
#[derive(Debug)]
pub enum Pattern {
    Integer(Box<IntegerLit>),
    Float(Box<FloatLit>),
    String(Box<StringLit>),
    Atom(Box<AtomLit>),
    Var(Box<Variable>),
    Match(Box<Match<Pattern, Pattern>>),
    Tuple(Box<Tuple<Pattern>>),
    Nil(Box<Nil>),
    Cons(Box<Cons<Pattern>>),
    Binary(Box<Binary<Pattern>>),
    UnaryOp(Box<UnaryOp<Pattern>>),
    BinaryOp(Box<BinaryOp<Pattern>>),
    Record(Box<Record<Pattern>>),
    RecordIndex(Box<RecordIndex<Pattern>>),
    Map(Box<Map<Pattern>>),
}

#[derive(Debug)]
pub struct Match<L, R> {
    pub line: LineNum,
    pub left: L,
    pub right: R,
}
impl_node_2!(Match<T,U>);

#[derive(Debug)]
pub struct Tuple<T> {
    pub line: LineNum,
    pub elements: Vec<T>,
}
impl_node_1!(Tuple<T>);

#[derive(Debug)]
pub struct Nil {
    pub line: LineNum,
}
impl_node!(Nil);

#[derive(Debug)]
pub struct Cons<T> {
    pub line: LineNum,
    pub head: T,
    pub tail: T,
}
impl_node_1!(Cons<T>);

#[derive(Debug)]
pub struct Binary<T> {
    pub line: LineNum,
    pub elements: Vec<BinElement<T>>,
}
impl_node_1!(Binary<T>);

#[derive(Debug)]
pub struct BinElement<T> {
    pub line: LineNum,
    pub element: T,
    pub size: Option<usize>,
    pub tsl: Option<Vec<BinElementTypeSpec>>,
}
impl_node_1!(BinElement<T>);

#[derive(Debug)]
pub struct BinElementTypeSpec {
    pub name: String,
    pub value: u64,
}

#[derive(Debug)]
pub struct UnaryOp<T> {
    pub line: LineNum,
    pub operator: String,
    pub operand: T,
}
impl_node_1!(UnaryOp<T>);

#[derive(Debug)]
pub struct BinaryOp<T> {
    pub line: LineNum,
    pub operator: String,
    pub left_operand: T,
    pub right_operand: T,
}
impl_node_1!(BinaryOp<T>);

#[derive(Debug)]
pub struct Record<T> {
    pub line: LineNum,
    pub base: Option<T>,
    pub name: String,
    pub fields: Vec<RecordField<T>>,
}
impl_node_1!(Record<T>);

#[derive(Debug)]
pub struct RecordField<T> {
    pub line: LineNum,
    pub name: String,
    pub value: T,
}
impl_node_1!(RecordField<T>);

#[derive(Debug)]
pub struct RecordIndex<T> {
    pub line: LineNum,
    pub base: Option<T>,
    pub record: String,
    pub field: String,
}
impl_node_1!(RecordIndex<T>);

#[derive(Debug)]
pub struct Map<T> {
    pub line: LineNum,
    pub base: Option<T>,
    pub pairs: Vec<MapPair<T>>,
}
impl_node_1!(Map<T>);

#[derive(Debug)]
pub struct MapPair<T> {
    pub line: LineNum,
    pub is_assoc: bool,
    pub key: T,
    pub value: T,
}
impl_node_1!(MapPair<T>);

// 6.4 Expressions
#[derive(Debug)]
pub enum Expression {
    Integer(Box<IntegerLit>),
    Float(Box<FloatLit>),
    String(Box<StringLit>),
    Atom(Box<AtomLit>),
    Match(Box<Match<Pattern, Expression>>),
    Var(Box<Variable>),
    Tuple(Box<Tuple<Expression>>),
    Nil(Box<Nil>),
    Cons(Box<Cons<Expression>>),
    Binary(Binary<Expression>),
    UnaryOp(Box<UnaryOp<Expression>>),
    BinaryOp(Box<BinaryOp<Expression>>),
    Record(Box<Record<Expression>>),
    RecordIndex(Box<RecordIndex<Expression>>),
    Map(Box<Map<Expression>>),
    Catch(Box<Catch>),
    LocalCall(Box<LocalCall<Expression>>),
    RemoteCall(Box<RemoteCall<Expression>>),
    Comprehension(Box<Comprehension>),
    Block(Box<Block>),
    If(Box<If>),
    Case(Box<Case>),
    Try(Box<Try>),
    Receive(Box<Receive>),
    InternalFun(Box<InternalFun>),
    ExternalFun(Box<ExternalFun>),
    AnonymousFun(Box<AnonymousFun>),
}

#[derive(Debug)]
pub struct Catch {
    pub line: LineNum,
    pub expr: Expression,
}
impl_node!(Catch);

#[derive(Debug)]
pub struct LocalCall<T> {
    pub line: LineNum,
    pub function: T,
    pub args: Vec<T>,
}
impl_node_1!(LocalCall<T>);

#[derive(Debug)]
pub struct RemoteCall<T> {
    pub line: LineNum,
    pub module: T,
    pub function: T,
    pub args: Vec<T>,
}
impl_node_1!(RemoteCall<T>);

#[derive(Debug)]
pub struct Comprehension {
    pub line: LineNum,
    pub is_list: bool,
    pub expr: Expression,
    pub qualifiers: Vec<Qualifier>,
}
impl_node!(Comprehension);

#[derive(Debug)]
pub enum Qualifier {
    Generator(Generator),
    BitStringGenerator(Generator),
    Filter(Expression),
}

#[derive(Debug)]
pub struct Generator {
    pub line: LineNum,
    pub pattern: Pattern,
    pub expr: Expression,
}
impl_node!(Generator);

#[derive(Debug)]
pub struct Block {
    pub line: LineNum,
    pub body: Vec<Expression>,
}
impl_node!(Block);

#[derive(Debug)]
pub struct If {
    pub line: LineNum,
    pub clauses: Vec<Clause>,
}
impl_node!(If);

#[derive(Debug)]
pub struct Case {
    pub line: LineNum,
    pub expr: Expression,
    pub clauses: Vec<Clause>,
}
impl_node!(Case);

#[derive(Debug)]
pub struct Try {
    pub line: LineNum,
    pub body: Vec<Expression>,
    pub case_clause: Vec<Clause>,
    pub catch_clause: Vec<Clause>,
    pub after: Vec<Expression>,
}
impl_node!(Try);

#[derive(Debug)]
pub struct Receive {
    pub line: LineNum,
    pub clause: Vec<Clause>,
    pub timeout: Option<Expression>,
    pub after: Vec<Expression>,
}
impl_node!(Receive);

#[derive(Debug)]
pub struct InternalFun {
    pub line: LineNum,
    pub function: String,
    pub airty: Arity,
}
impl_node!(InternalFun);

#[derive(Debug)]
pub struct ExternalFun {
    pub line: LineNum,
    pub module: String,
    pub function: String,
    pub arity: Arity,
}
impl_node!(ExternalFun);

#[derive(Debug)]
pub struct AnonymousFun {
    pub line: LineNum,
    pub name: Option<Variable>,
    pub clauses: Vec<Clause>,
}
impl_node!(AnonymousFun);

// 6.5 Clauses
#[derive(Debug)]
pub struct Clause {
    pub line: LineNum,
    pub patterns: Vec<Pattern>,
    pub guards: Vec<OrGuard>,
    pub body: Vec<Expression>,
}
impl_node!(Clause);

// 6.6 Guards
#[derive(Debug)]
pub struct OrGuard {
    and_guards: Vec<Guard>,
}

#[derive(Debug)]
pub enum Guard {
    Integer(Box<IntegerLit>),
    Float(Box<FloatLit>),
    String(Box<StringLit>),
    Atom(Box<AtomLit>),
    Var(Box<Variable>),
    Tuple(Box<Tuple<Guard>>),
    Nil(Box<Nil>),
    Cons(Box<Cons<Guard>>),
    Binary(Box<Binary<Guard>>),
    UnaryOp(Box<UnaryOp<Guard>>),
    BinaryOp(Box<BinaryOp<Guard>>),
    Record(Box<Record<Guard>>),
    RecordIndex(Box<RecordIndex<Guard>>),
    LocalCall(Box<LocalCall<Guard>>),
    RemoteCall(Box<RemoteCall<Guard>>),
}

// 6.7 Types
#[derive(Debug)]
pub enum Type {
    Atom(Box<AtomLit>),
    Integer(Box<IntegerLit>),
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

#[derive(Debug)]
pub struct UserType {
    pub line: LineNum,
    pub name: String,
    pub args: Vec<Type>,
}
impl_node!(UserType);

#[derive(Debug)]
pub struct UnionType {
    pub line: LineNum,
    pub types: Vec<Type>,
}
impl_node!(UnionType);

#[derive(Debug)]
pub struct AnyTupleType {
    pub line: LineNum,
}
impl_node!(AnyTupleType);

#[derive(Debug)]
pub struct TupleType {
    pub line: LineNum,
    pub elements: Vec<Type>,
}
impl_node!(TupleType);

#[derive(Debug)]
pub struct RemoteType {
    pub line: LineNum,
    pub module: String,
    pub function: String,
    pub args: Vec<Type>,
}
impl_node!(RemoteType);

#[derive(Debug)]
pub struct RecordType {
    pub line: LineNum,
    pub name: String,
    pub fields: Vec<RecordFieldType>,
}
impl_node!(RecordType);

#[derive(Debug)]
pub struct RecordFieldType {
    pub line: LineNum,
    pub name: String,
    pub type_: Type,
}
impl_node!(RecordFieldType);

#[derive(Debug)]
pub struct BuiltInType {
    pub line: LineNum,
    pub name: String,
    pub args: Vec<Type>,
}
impl_node!(BuiltInType);

#[derive(Debug)]
pub struct MapType {
    pub line: LineNum,
    pub pairs: Option<MapPairType>,
}
impl_node!(MapType);

#[derive(Debug)]
pub struct MapPairType {
    pub line: LineNum,
    pub key: Type,
    pub value: Type,
}
impl_node!(MapPairType);

#[derive(Debug)]
pub struct AnnotatedType {
    pub line: LineNum,
    pub name: Variable,
    pub type_: Type,
}
impl_node!(AnnotatedType);

#[derive(Debug)]
pub struct BitStringType {
    pub line: LineNum,
    pub bytes: u64,
    pub bits: u64,
}
impl_node!(BitStringType);

#[derive(Debug)]
pub struct AnyFunType {
    pub line: LineNum,
    pub body: Option<Type>,
}
impl_node!(AnyFunType);

#[derive(Debug)]
pub struct FunctionType {
    pub line: LineNum,
    pub args: Vec<Type>,
    pub body: Vec<Type>,
    pub contraints: Vec<FunctionConstraint>,
}
impl_node!(FunctionType);

#[derive(Debug)]
pub struct FunctionConstraint {
    pub line: LineNum,
    pub var: Variable,
    pub subtype: Type,
}
impl_node!(FunctionConstraint);

#[derive(Debug)]
pub struct RangeType {
    pub line: LineNum,
    pub low: i64,
    pub high: u64,
}
impl_node!(RangeType);

// Others
#[derive(Debug)]
pub struct Variable {
    pub line: LineNum,
    pub name: String,
}
impl_node!(Variable);
impl Variable {
    pub fn is_anonymous(&self) -> bool {
        self.name == "_"
    }
}

#[derive(Debug)]
pub struct Export {
    pub function: String,
    pub arity: Arity,
}
impl Export {
    pub fn new(function: String, arity: Arity) -> Self {
        Export {
            function: function,
            arity: arity,
        }
    }
}

#[derive(Debug)]
pub struct Import {
    pub function: String,
    pub arity: Arity,
}
impl Import {
    pub fn new(function: String, arity: Arity) -> Self {
        Import {
            function: function,
            arity: arity,
        }
    }
}

#[derive(Debug)]
pub struct ExportType {
    pub type_: String,
    pub arity: Arity,
}
impl ExportType {
    pub fn new(type_: String, arity: Arity) -> Self {
        ExportType {
            type_: type_,
            arity: arity,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let _module = ModuleDecl::from_beam_file("src/testdata/test.beam").unwrap();
    }
}
