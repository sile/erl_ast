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
pub struct TypeDef {
    pub is_public: bool,
    pub name: String,
    pub variables: Vec<Variable>,
    pub value: Type,
}
impl TypeDef {
    pub fn new_type(name: &str, variables: Vec<Variable>, value: Type) -> Self {
        TypeDef {
            is_public: true,
            name: name.to_string(),
            variables: variables,
            value: value,
        }
    }
    pub fn new_opaque(name: &str, variables: Vec<Variable>, value: Type) -> Self {
        TypeDef {
            is_public: false,
            name: name.to_string(),
            variables: variables,
            value: value,
        }
    }
}
