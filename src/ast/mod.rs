use std::rc::Rc;
use std::ops::Deref;

mod module;
mod matcher;
mod parser;

pub use self::module::Module;

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
