use eetf;
use num::bigint::BigInt;

pub mod codec;
mod matcher;

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

pub type LineNum = i32;
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
    ($to:ident :: $constructor:ident ($from:ty)) => {
        impl From<$from> for $to {
            fn from(x: $from) -> Self {
                $to::$constructor(::std::convert::From::from(x))
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
    Eof(Eof),
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
impl_from!(Form::Eof(Eof));

#[derive(Debug)]
pub struct Eof {
    pub line: LineNum,
}
impl_node!(Eof);
impl Eof {
    pub fn new(line: LineNum) -> Self {
        Eof { line: line }
    }
}

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
impl RecordDecl {
    pub fn new(line: LineNum, name: String, fields: Vec<RecordFieldDecl>) -> Self {
        RecordDecl {
            line: line,
            name: name,
            fields: fields,
        }
    }
}

#[derive(Debug)]
pub struct TypeDecl {
    pub line: LineNum,
    pub is_opaque: bool,
    pub name: String,
    pub variables: Vec<Variable>,
    pub type_: Type,
}
impl_node!(TypeDecl);
impl TypeDecl {
    pub fn new(line: LineNum, name: String, variables: Vec<Variable>, type_: Type) -> Self {
        TypeDecl {
            line: line,
            name: name,
            variables: variables,
            type_: type_,
            is_opaque: false,
        }
    }
    pub fn opaque(mut self, is_opaque: bool) -> Self {
        self.is_opaque = is_opaque;
        self
    }
}

#[derive(Debug)]
pub struct FunctionSpec {
    pub line: LineNum,
    pub module: Option<String>,
    pub name: String,
    pub types: Vec<FunctionType>,
    pub is_callback: bool,
}
impl_node!(FunctionSpec);
impl FunctionSpec {
    pub fn new(line: LineNum, name: String, types: Vec<FunctionType>) -> Self {
        FunctionSpec {
            line: line,
            module: None,
            name: name,
            types: types,
            is_callback: false,
        }
    }
    pub fn module(mut self, module: String) -> Self {
        self.module = Some(module);
        self
    }
    pub fn callback(mut self, is_callback: bool) -> Self {
        self.is_callback = is_callback;
        self
    }
}

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
    pub clauses: Vec<Clause>,
}
impl_node!(FunctionDecl);
impl FunctionDecl {
    pub fn new(line: LineNum, name: String, clauses: Vec<Clause>) -> Self {
        FunctionDecl {
            line: line,
            name: name,
            clauses: clauses,
        }
    }
}

#[derive(Debug)]
pub struct RecordFieldDecl {
    pub line: LineNum,
    pub name: String,
    pub type_: Type,
    pub default_value: Expression,
}
impl_node!(RecordFieldDecl);
impl RecordFieldDecl {
    pub fn new(line: LineNum, name: String) -> Self {
        RecordFieldDecl {
            line: line,
            name: name,
            type_: Type::any(line),
            default_value: Expression::atom(line, "undefined".to_string()),
        }
    }
    pub fn type_(mut self, type_: Type) -> Self {
        self.type_ = type_;
        self
    }
    pub fn default_value(mut self, value: Expression) -> Self {
        self.default_value = value;
        self
    }
}

// 6.2 Atomic Literals
#[derive(Debug)]
pub struct IntegerLit {
    pub line: LineNum,
    // pub value: u64,
    pub value: BigInt,
}
impl_node!(IntegerLit);
impl IntegerLit {
    pub fn new(line: LineNum, value: BigInt) -> Self {
        IntegerLit {
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
pub struct CharLit {
    pub line: LineNum,
    pub value: char,
}
impl_node!(CharLit);
impl CharLit {
    pub fn new(line: LineNum, value: char) -> Self {
        CharLit {
            line: line,
            value: value,
        }
    }
}

#[derive(Debug)]
pub struct FloatLit {
    pub line: LineNum,
    pub value: f64,
}
impl_node!(FloatLit);
impl FloatLit {
    pub fn new(line: LineNum, value: f64) -> Self {
        FloatLit {
            line: line,
            value: value,
        }
    }
}

#[derive(Debug)]
pub struct StringLit {
    pub line: LineNum,
    pub value: String,
}
impl_node!(StringLit);
impl StringLit {
    pub fn new(line: LineNum, value: String) -> Self {
        StringLit {
            line: line,
            value: value,
        }
    }
}

#[derive(Debug)]
pub struct AtomLit {
    pub line: LineNum,
    pub value: String,
}
impl_node!(AtomLit);
impl AtomLit {
    pub fn new(line: LineNum, value: String) -> Self {
        AtomLit {
            line: line,
            value: value,
        }
    }
}

// 6.3 Patterns
#[derive(Debug)]
pub enum Pattern {
    Integer(Box<IntegerLit>),
    Float(Box<FloatLit>),
    String(Box<StringLit>),
    Char(Box<CharLit>),
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
impl_from!(Pattern::Integer(IntegerLit));
impl_from!(Pattern::Float(FloatLit));
impl_from!(Pattern::String(StringLit));
impl_from!(Pattern::Char(CharLit));
impl_from!(Pattern::Atom(AtomLit));
impl_from!(Pattern::Var(Variable));
impl_from!(Pattern::Match(Match<Pattern, Pattern>));
impl_from!(Pattern::Tuple(Tuple<Pattern>));
impl_from!(Pattern::Nil(Nil));
impl_from!(Pattern::Cons(Cons<Pattern>));
impl_from!(Pattern::Binary(Binary<Pattern>));
impl_from!(Pattern::UnaryOp(UnaryOp<Pattern>));
impl_from!(Pattern::BinaryOp(BinaryOp<Pattern>));
impl_from!(Pattern::Record(Record<Pattern>));
impl_from!(Pattern::RecordIndex(RecordIndex<Pattern>));
impl_from!(Pattern::Map(Map<Pattern>));

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

// 6.4 Expressions
#[derive(Debug)]
pub enum Expression {
    Integer(Box<IntegerLit>),
    Float(Box<FloatLit>),
    String(Box<StringLit>),
    Char(Box<CharLit>),
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
impl_from!(Expression::Integer(IntegerLit));
impl_from!(Expression::Float(FloatLit));
impl_from!(Expression::String(StringLit));
impl_from!(Expression::Char(CharLit));
impl_from!(Expression::Atom(AtomLit));
impl_from!(Expression::Match(Match<Pattern, Expression>));
impl_from!(Expression::Var(Variable));
impl_from!(Expression::Tuple(Tuple<Expression>));
impl_from!(Expression::Nil(Nil));
impl_from!(Expression::Cons(Cons<Expression>));
impl_from!(Expression::Binary(Binary<Expression>));
impl_from!(Expression::UnaryOp(UnaryOp<Expression>));
impl_from!(Expression::BinaryOp(BinaryOp<Expression>));
impl_from!(Expression::Record(Record<Expression>));
impl_from!(Expression::RecordIndex(RecordIndex<Expression>));
impl_from!(Expression::Map(Map<Expression>));
impl_from!(Expression::Catch(Catch));
impl_from!(Expression::LocalCall(LocalCall<Expression>));
impl_from!(Expression::RemoteCall(RemoteCall<Expression>));
impl_from!(Expression::Comprehension(Comprehension));
impl_from!(Expression::Block(Block));
impl_from!(Expression::If(If));
impl_from!(Expression::Case(Case));
impl_from!(Expression::Try(Try));
impl_from!(Expression::Receive(Receive));
impl_from!(Expression::InternalFun(InternalFun));
impl_from!(Expression::ExternalFun(ExternalFun));
impl_from!(Expression::AnonymousFun(AnonymousFun));
impl Expression {
    pub fn atom(line: LineNum, name: String) -> Self {
        Expression::Atom(Box::new(AtomLit::new(line, name)))
    }
}

#[derive(Debug)]
pub struct Catch {
    pub line: LineNum,
    pub expr: Expression,
}
impl_node!(Catch);
impl Catch {
    pub fn new(line: LineNum, expr: Expression) -> Self {
        Catch {
            line: line,
            expr: expr,
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
pub struct Comprehension {
    pub line: LineNum,
    pub is_list: bool,
    pub expr: Expression,
    pub qualifiers: Vec<Qualifier>,
}
impl_node!(Comprehension);
impl Comprehension {
    pub fn new(line: LineNum, is_list: bool, expr: Expression, qualifiers: Vec<Qualifier>) -> Self {
        Comprehension {
            line: line,
            is_list: is_list,
            expr: expr,
            qualifiers: qualifiers,
        }
    }
}

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
impl Generator {
    pub fn new(line: LineNum, pattern: Pattern, expr: Expression) -> Self {
        Generator {
            line: line,
            pattern: pattern,
            expr: expr,
        }
    }
}

#[derive(Debug)]
pub struct Block {
    pub line: LineNum,
    pub body: Vec<Expression>,
}
impl_node!(Block);
impl Block {
    pub fn new(line: LineNum, body: Vec<Expression>) -> Self {
        Block {
            line: line,
            body: body,
        }
    }
}

#[derive(Debug)]
pub struct If {
    pub line: LineNum,
    pub clauses: Vec<Clause>,
}
impl_node!(If);
impl If {
    pub fn new(line: LineNum, clauses: Vec<Clause>) -> Self {
        If {
            line: line,
            clauses: clauses,
        }
    }
}

#[derive(Debug)]
pub struct Case {
    pub line: LineNum,
    pub expr: Expression,
    pub clauses: Vec<Clause>,
}
impl_node!(Case);
impl Case {
    pub fn new(line: LineNum, expr: Expression, clauses: Vec<Clause>) -> Self {
        Case {
            line: line,
            expr: expr,
            clauses: clauses,
        }
    }
}

#[derive(Debug)]
pub struct Try {
    pub line: LineNum,
    pub body: Vec<Expression>,
    pub case_clauses: Vec<Clause>,
    pub catch_clauses: Vec<Clause>,
    pub after: Vec<Expression>,
}
impl_node!(Try);
impl Try {
    pub fn new(line: LineNum,
               body: Vec<Expression>,
               case_clauses: Vec<Clause>,
               catch_clauses: Vec<Clause>,
               after: Vec<Expression>)
               -> Self {
        Try {
            line: line,
            body: body,
            case_clauses: case_clauses,
            catch_clauses: catch_clauses,
            after: after,
        }
    }
}

#[derive(Debug)]
pub struct Receive {
    pub line: LineNum,
    pub clauses: Vec<Clause>,
    pub timeout: Option<Expression>,
    pub after: Vec<Expression>,
}
impl_node!(Receive);
impl Receive {
    pub fn new(line: LineNum, clauses: Vec<Clause>) -> Self {
        Receive {
            line: line,
            clauses: clauses,
            timeout: None,
            after: Vec::new(),
        }
    }
    pub fn timeout(mut self, timeout: Expression) -> Self {
        self.timeout = Some(timeout);
        self
    }
    pub fn after(mut self, after: Vec<Expression>) -> Self {
        self.after = after;
        self
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

#[derive(Debug)]
pub struct AnonymousFun {
    pub line: LineNum,
    pub name: Option<String>,
    pub clauses: Vec<Clause>,
}
impl_node!(AnonymousFun);
impl AnonymousFun {
    pub fn new(line: LineNum, clauses: Vec<Clause>) -> Self {
        AnonymousFun {
            line: line,
            name: None,
            clauses: clauses,
        }
    }
    pub fn name(mut self, name: String) -> Self {
        self.name = Some(name);
        self
    }
}

// 6.5 Clauses
#[derive(Debug)]
pub struct Clause {
    pub line: LineNum,
    pub patterns: Vec<Pattern>,
    pub guards: Vec<OrGuard>,
    pub body: Vec<Expression>,
}
impl_node!(Clause);
impl Clause {
    pub fn new(line: LineNum,
               patterns: Vec<Pattern>,
               guards: Vec<OrGuard>,
               body: Vec<Expression>)
               -> Self {
        Clause {
            line: line,
            patterns: patterns,
            guards: guards,
            body: body,
        }
    }
}

// 6.6 Guards
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
    Integer(Box<IntegerLit>),
    Float(Box<FloatLit>),
    String(Box<StringLit>),
    Char(Box<CharLit>),
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
impl_from!(Guard::Integer(IntegerLit));
impl_from!(Guard::Float(FloatLit));
impl_from!(Guard::String(StringLit));
impl_from!(Guard::Char(CharLit));
impl_from!(Guard::Atom(AtomLit));
impl_from!(Guard::Var(Variable));
impl_from!(Guard::Tuple(Tuple<Guard>));
impl_from!(Guard::Nil(Nil));
impl_from!(Guard::Cons(Cons<Guard>));
impl_from!(Guard::Binary(Binary<Guard>));
impl_from!(Guard::UnaryOp(UnaryOp<Guard>));
impl_from!(Guard::BinaryOp(BinaryOp<Guard>));
impl_from!(Guard::Record(Record<Guard>));
impl_from!(Guard::RecordIndex(RecordIndex<Guard>));
impl_from!(Guard::LocalCall(LocalCall<Guard>));
impl_from!(Guard::RemoteCall(RemoteCall<Guard>));

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
