use std::rc::Rc;
use std::ops::Deref;
use std::ops::DerefMut;
use std::ops::Range;

mod module;
mod matcher;
mod parser;

pub use self::module::Module;

#[derive(Debug)]
pub struct Node<T> {
    value: T,
    file: Rc<String>,
    line: u32,
}
impl<T> Node<T> {
    pub fn new(file: Rc<String>, line: u32, value: T) -> Self {
        Node {
            value: value,
            file: file,
            line: line,
        }
    }
    pub fn file(&self) -> &str {
        &self.file
    }
    pub fn line(&self) -> u32 {
        self.line
    }
}
impl<T> Deref for Node<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.value
    }
}
impl<T> DerefMut for Node<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.value
    }
}

#[derive(Debug)]
pub struct Export {
    pub function: String,
    pub arity: u32,
}
impl Export {
    pub fn new(function: String, arity: u32) -> Self {
        Export {
            function: function,
            arity: arity,
        }
    }
}

#[derive(Debug)]
pub struct Import {
    pub module: String,
    pub function: String,
    pub arity: u32,
}
impl Import {
    pub fn new(module: &str, function: &str, arity: u32) -> Self {
        Import {
            module: module.to_string(),
            function: function.to_string(),
            arity: arity,
        }
    }
}

#[derive(Debug)]
pub struct Spec {
    pub module: Option<String>, // `None` means "current module"
    pub name: String,
    pub arity: u32,
    pub clauses: Vec<Node<FunType>>,
}
impl Spec {
    pub fn new(name: String, arity: u32, clauses: Vec<Node<FunType>>) -> Self {
        Spec {
            module: None,
            name: name,
            arity: arity,
            clauses: clauses,
        }
    }
    pub fn module(mut self, module: &str) -> Self {
        self.module = Some(module.to_string());
        self
    }
}

#[derive(Debug)]
pub struct Callback {
    pub name: String,
    pub arity: u32,
    pub clauses: Vec<Node<FunType>>,
}
impl Callback {
    pub fn new(name: String, arity: u32, clauses: Vec<Node<FunType>>) -> Self {
        Callback {
            name: name,
            arity: arity,
            clauses: clauses,
        }
    }
}

#[derive(Debug)]
pub enum Type {
    BuiltIn(Node<BuiltInType>),
    User(Node<UserType>),
    AtomLiteral(Node<String>),
    Tuple(Node<TupleType>),
    Fun(Node<FunType>),
    Union(Node<UnionType>),
    Annotated(Node<AnnotatedType>),
    Var(Node<Variable>),
    Record(Node<RecordType>),
    Range(Node<Range<i64>>),
}

#[derive(Debug)]
pub struct RecordType {
    pub name: String,
    pub fields: Vec<Node<RecordFieldType>>,
}
impl RecordType {
    pub fn new(name: &str, fields: Vec<Node<RecordFieldType>>) -> Self {
        RecordType {
            name: name.to_string(),
            fields: fields,
        }
    }
}

#[derive(Debug)]
pub struct RecordFieldType {
    pub name: String,
    pub typ: Box<Type>,
}
impl RecordFieldType {
    pub fn new(name: &str, typ: Type) -> Self {
        RecordFieldType {
            name: name.to_string(),
            typ: Box::new(typ),
        }
    }
}

#[derive(Debug)]
pub struct FunType {
    pub args: Vec<Type>,
    pub result: Box<Type>,
    pub constraints: Vec<Node<Constraint>>,
}
impl FunType {
    pub fn new(args: Vec<Type>, result: Type) -> Self {
        FunType {
            args: args,
            result: Box::new(result),
            constraints: Vec::new(),
        }
    }
    pub fn set_constraints(&mut self, constraints: Vec<Node<Constraint>>) {
        self.constraints = constraints;
    }
}

#[derive(Debug)]
pub struct Constraint {
    pub variable: Variable,
    pub typ: Type,
}
impl Constraint {
    pub fn new(variable: &str, typ: Type) -> Self {
        Constraint {
            variable: Variable::new(variable),
            typ: typ,
        }
    }
}

#[derive(Debug)]
pub struct UnionType {
    pub types: Vec<Type>,
}
impl UnionType {
    fn new(types: Vec<Type>) -> Self {
        UnionType { types: types }
    }
}

#[derive(Debug)]
pub struct TupleType {
    elements: Vec<Type>,
}

#[derive(Debug)]
pub struct AnnotatedType {
    pub variable: Variable,
    pub ann_type: Box<Type>,
}
impl AnnotatedType {
    pub fn new(variable: &str, ann_type: Type) -> Self {
        AnnotatedType {
            variable: Variable::new(variable),
            ann_type: Box::new(ann_type),
        }
    }
}

#[derive(Debug)]
pub struct Variable {
    // `None` means an anonymous variable
    pub name: Option<String>,
}
impl Variable {
    fn new(name: &str) -> Self {
        match name {
            "_" => Variable { name: None },
            _ => Variable { name: Some(name.to_string()) },
        }
    }
}

#[derive(Debug)]
pub enum BuiltInType {
    Term,
    Atom,
    Binary,
    Integer,
    NegInteger,
    NonNegInteger,
    Float,
    List(Option<Box<Type>>),
    BitString,
    Map,
    Tuple,
    Pid,
    Reference,
}

#[derive(Debug)]
pub struct UserType {
    pub name: String,
    pub args: Vec<Type>,
}

#[derive(Debug)]
pub enum TypeDef {
    Type {
        name: String,
        variables: Vec<Variable>,
        value: Type,
    },
    Opaque {
        name: String,
        variables: Vec<Variable>,
        value: Type,
    },
    Record {
        name: String,
        variables: Vec<Variable>,
        fields: Vec<Node<RecordField>>,
    },
}
impl TypeDef {
    pub fn new_type(name: &str, variables: Vec<Variable>, value: Type) -> Self {
        TypeDef::Type {
            name: name.to_string(),
            variables: variables,
            value: value,
        }
    }
    pub fn new_opaque(name: &str, variables: Vec<Variable>, value: Type) -> Self {
        TypeDef::Opaque {
            name: name.to_string(),
            variables: variables,
            value: value,
        }
    }
    pub fn new_record(name: &str,
                      variables: Vec<Variable>,
                      fields: Vec<Node<RecordField>>)
                      -> Self {
        TypeDef::Record {
            name: name.to_string(),
            variables: variables,
            fields: fields,
        }
    }
}

#[derive(Debug)]
pub struct RecordDecl {
    pub name: String,
    pub fields: Vec<Node<RecordField>>,
}
impl RecordDecl {
    pub fn new(name: &str, fields: Vec<Node<RecordField>>) -> Self {
        RecordDecl {
            name: name.to_string(),
            fields: fields,
        }
    }
}

#[derive(Debug)]
pub struct RecordField {
    pub name: String,
    pub field_type: Option<Type>,
    pub default_value: Option<Expression>,
}
impl RecordField {
    pub fn new(name: &str) -> Self {
        RecordField {
            name: name.to_string(),
            field_type: None,
            default_value: None,
        }
    }
    pub fn default(mut self, value: Expression) -> Self {
        self.default_value = Some(value);
        self
    }
    pub fn field_type(mut self, typ: Type) -> Self {
        self.field_type = Some(typ);
        self
    }
}

#[derive(Debug)]
pub struct Guard {
    pub tests: Vec<Expression>,
}

#[derive(Debug)]
pub enum Expression {
    // MEMO: 解析時にはSome<i64>にする (Noneならinfinity扱い)
    IntegerLiteral(Node<u32>),
    AtomLiteral(Node<String>),
    FloatLiteral(Node<f64>),
    StringLiteral(Node<String>),
    Tuple(Node<TupleExpr>),
    Map(Node<MapExpr>),
    Record(Node<RecordExpr>),
    UnaryOp(Node<UnaryOpExpr>),
    BinaryOp(Node<BinaryOpExpr>),
    Cons(Node<ConsExpr>),
    Nil(Node<()>),
    Binary(Node<BinaryExpr>),
    Call(Node<CallExpr>),
    RemoteCall(Node<RemoteCallExpr>),
    Var(Node<Variable>),
    Lc(Node<ListComprehensionExpr>),
}

#[derive(Debug)]
pub struct ListComprehensionExpr {
    pub expr: Box<Expression>,
    pub qualifiers: Vec<Qualifier>,
}
impl ListComprehensionExpr {
    pub fn new(expr: Expression, qualifiers: Vec<Qualifier>) -> Self {
        ListComprehensionExpr {
            expr: Box::new(expr),
            qualifiers: qualifiers,
        }
    }
}

#[derive(Debug)]
pub struct CallExpr {
    function: Box<Expression>,
    args: Vec<Expression>,
}
impl CallExpr {
    pub fn new(function: Expression, args: Vec<Expression>) -> Self {
        CallExpr {
            function: Box::new(function),
            args: args,
        }
    }
}

#[derive(Debug)]
pub struct RemoteCallExpr {
    module: Box<Expression>,
    function: Box<Expression>,
    args: Vec<Expression>,
}
impl RemoteCallExpr {
    pub fn new(module: Expression, function: Expression, args: Vec<Expression>) -> Self {
        RemoteCallExpr {
            module: Box::new(module),
            function: Box::new(function),
            args: args,
        }
    }
}

#[derive(Debug)]
pub struct TupleExpr {
    pub elements: Vec<Expression>,
}
impl TupleExpr {
    pub fn new(elements: Vec<Expression>) -> Self {
        TupleExpr { elements: elements }
    }
}

#[derive(Debug)]
pub struct MapExpr {
    pub base: Option<Box<Expression>>,
    pub fields: Vec<Node<MapFieldExpr>>,
}
impl MapExpr {
    pub fn new(fields: Vec<Node<MapFieldExpr>>) -> Self {
        MapExpr {
            base: None,
            fields: fields,
        }
    }
    pub fn with_base(base: Expression, fields: Vec<Node<MapFieldExpr>>) -> Self {
        MapExpr {
            base: Some(Box::new(base)),
            fields: fields,
        }
    }
}

#[derive(Debug)]
pub enum MapFieldKind {
    Assoc,
    Exact,
}

#[derive(Debug)]
pub struct MapFieldExpr {
    pub kind: MapFieldKind,
    pub key: Box<Expression>,
    pub value: Box<Expression>,
}
impl MapFieldExpr {
    pub fn new_exact(key: Expression, value: Expression) -> Self {
        MapFieldExpr {
            kind: MapFieldKind::Exact,
            key: Box::new(key),
            value: Box::new(value),
        }
    }
    pub fn new_assoc(key: Expression, value: Expression) -> Self {
        MapFieldExpr {
            kind: MapFieldKind::Assoc,
            key: Box::new(key),
            value: Box::new(value),
        }
    }
}

#[derive(Debug)]
pub struct RecordExpr {
    pub base: Option<Box<Expression>>,
    pub name: String,
    pub fields: Vec<Node<RecordFieldExpr>>,
}
impl RecordExpr {
    pub fn new(name: &str, fields: Vec<Node<RecordFieldExpr>>) -> Self {
        RecordExpr {
            base: None,
            name: name.to_string(),
            fields: fields,
        }
    }
    pub fn with_base(base: Expression, name: &str, fields: Vec<Node<RecordFieldExpr>>) -> Self {
        RecordExpr {
            base: Some(Box::new(base)),
            name: name.to_string(),
            fields: fields,
        }
    }
}

#[derive(Debug)]
pub struct RecordFieldExpr {
    pub key: String,
    pub value: Box<Expression>,
}
impl RecordFieldExpr {
    pub fn new(key: &str, value: Expression) -> Self {
        RecordFieldExpr {
            key: key.to_string(),
            value: Box::new(value),
        }
    }
}

#[derive(Debug)]
pub struct BinaryExpr {
    pub elements: Vec<Node<BinElement>>,
}
impl BinaryExpr {
    pub fn new(elements: Vec<Node<BinElement>>) -> Self {
        BinaryExpr { elements: elements }
    }
}

#[derive(Debug)]
pub struct BinElement {
    pub value: Box<Expression>,
    pub size: Option<Box<Expression>>, // `None` means the default value
    pub tsl: Option<Vec<BinElementTypeSpec>>, // `None` means the default value
}
impl BinElement {
    pub fn new(value: Expression) -> Self {
        BinElement {
            value: Box::new(value),
            size: None,
            tsl: None,
        }
    }
    pub fn set_size(&mut self, size: Expression) {
        self.size = Some(Box::new(size));
    }
    pub fn add_type_spec(&mut self, (name, value): (&str, Option<u32>)) {
        if self.tsl.is_none() {
            self.tsl = Some(Vec::new());
        }
        self.tsl.as_mut().unwrap().push(BinElementTypeSpec {
            name: name.to_string(),
            value: value,
        });
    }
}

#[derive(Debug)]
pub struct BinElementTypeSpec {
    pub name: String,
    pub value: Option<u32>,
}

#[derive(Debug)]
pub struct ConsExpr {
    pub head: Box<Expression>,
    pub tail: Box<Expression>,
}
impl ConsExpr {
    pub fn new(head: Expression, tail: Expression) -> Self {
        ConsExpr {
            head: Box::new(head),
            tail: Box::new(tail),
        }
    }
}

#[derive(Debug)]
pub struct UnaryOpExpr {
    pub operator: String,
    pub operand: Box<Expression>,
}
impl UnaryOpExpr {
    pub fn new(operator: &str, operand: Expression) -> Self {
        UnaryOpExpr {
            operator: operator.to_string(),
            operand: Box::new(operand),
        }
    }
}

#[derive(Debug)]
pub struct BinaryOpExpr {
    pub operator: String,
    pub left_operand: Box<Expression>,
    pub right_operand: Box<Expression>,
}
impl BinaryOpExpr {
    pub fn new(operator: &str, left: Expression, right: Expression) -> Self {
        BinaryOpExpr {
            operator: operator.to_string(),
            left_operand: Box::new(left),
            right_operand: Box::new(right),
        }
    }
}

#[derive(Debug)]
pub struct FunctionDecl {
    pub name: String,
    pub arity: u32,
    pub clauses: Vec<Node<Clause>>,
}
impl FunctionDecl {
    pub fn new(name: &str, arity: u32, clauses: Vec<Node<Clause>>) -> Self {
        FunctionDecl {
            name: name.to_string(),
            arity: arity,
            clauses: clauses,
        }
    }
}

#[derive(Debug)]
pub struct Clause {
    pub patterns: Vec<Pattern>,
    pub guards: Vec<Guard>,
    pub body: Body,
}
impl Clause {
    pub fn new(body: Body) -> Self {
        Clause {
            patterns: Vec::new(),
            guards: Vec::new(),
            body: body,
        }
    }
}

#[derive(Debug)]
pub struct Body {
    pub expressions: Vec<Expression>,
}
impl Body {
    pub fn new(expressions: Vec<Expression>) -> Self {
        Body { expressions: expressions }
    }
}

#[derive(Debug)]
pub enum Pattern {
    Binary(Node<BinaryExpr>),
    Var(Node<Variable>),
    Nil(Node<()>),
    Cons(Node<ConsPat>),
    Map(Node<MapExpr>),
    Tuple(Node<TupleExpr>),
}

#[derive(Debug)]
pub struct ConsPat {
    pub head: Box<Pattern>,
    pub tail: Box<Pattern>,
}
impl ConsPat {
    pub fn new(head: Pattern, tail: Pattern) -> Self {
        ConsPat {
            head: Box::new(head),
            tail: Box::new(tail),
        }
    }
}

#[derive(Debug)]
pub enum Qualifier {
    ListGenerate(Node<ListGenerateExpr>),
    BinaryGenerate(Node<BinaryGenerateExpr>),
    Filter(FilterExpr),
}

#[derive(Debug)]
pub struct ListGenerateExpr {
    pub pattern: Box<Pattern>,
    pub expr: Box<Expression>,
}
impl ListGenerateExpr {
    pub fn new(pattern: Pattern, expr: Expression) -> Self {
        ListGenerateExpr {
            pattern: Box::new(pattern),
            expr: Box::new(expr),
        }
    }
}

#[derive(Debug)]
pub struct BinaryGenerateExpr {
    pub pattern: Box<Pattern>,
    pub expr: Box<Expression>,
}
impl BinaryGenerateExpr {
    pub fn new(pattern: Pattern, expr: Expression) -> Self {
        BinaryGenerateExpr {
            pattern: Box::new(pattern),
            expr: Box::new(expr),
        }
    }
}

#[derive(Debug)]
pub struct FilterExpr {
    pub expr: Box<Expression>,
}
impl FilterExpr {
    pub fn new(expr: Expression) -> Self {
        FilterExpr { expr: Box::new(expr) }
    }
}
