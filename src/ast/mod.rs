use std::rc::Rc;
use std::ops::Deref;

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
}

#[derive(Debug)]
pub struct FunType {
    pub args: Vec<Type>,
    pub result: Box<Type>,
}
impl FunType {
    pub fn new(args: Vec<Type>, result: Type) -> Self {
        FunType {
            args: args,
            result: Box::new(result),
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
    Float,
    List,
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
pub enum Expression {
    IntegerLiteral(Node<u32>),
    AtomLiteral(Node<String>),
    FloatLiteral(Node<f64>),
    StringLiteral(Node<String>),
}
