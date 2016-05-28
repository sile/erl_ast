use std::rc::Rc;
use eetf;
use result::BeamParseResult;
use error::BeamParseError;
use ast;
use ast::{Node, Export};
use ast::matcher::Pattern;
use ast::matcher::Or;
use ast::matcher::{U32, Atom, Str, List, AnyList, Term};

#[derive(Default)]
pub struct Parser {
    current_file: Option<Rc<String>>,
    module: Option<Node<String>>,
    compile_options: Vec<Node<eetf::Term>>,
    behaviours: Vec<Node<String>>,
    exports: Vec<Node<Export>>,
    export_types: Vec<Node<Export>>,
}
impl Parser {
    pub fn new() -> Self {
        Parser::default()
    }
    pub fn parse(mut self, abstract_code: eetf::Term) -> BeamParseResult<ast::Module> {
        let (_, declarations_and_forms) = try!(("raw_abstract_v1", AnyList)
            .do_match(&abstract_code)
            .ok_or(BeamParseError::UnknownAbstractFormat));
        try!(self.parse_declarations_and_forms(&declarations_and_forms));
        unimplemented!();
    }
    pub fn parse_declarations_and_forms(&mut self,
                                        declarations_and_forms: &eetf::List)
                                        -> BeamParseResult<()> {
        for t in &declarations_and_forms.elements {
            try!(self.parse_declaration_or_form(t));
        }
        Ok(())
    }
    pub fn parse_declaration_or_form(&mut self, t: &eetf::Term) -> BeamParseResult<()> {
        if let Some((_, _, _, (file, _))) = ("attribute", U32, "file", (Str, U32)).do_match(t) {
            self.current_file = Some(Rc::new(file));
            return Ok(());
        }
        if let Some((_, line, _, module)) = ("attribute", U32, "module", Atom).do_match(t) {
            self.module = Some(try!(self.node(line, module.name.clone())));
            return Ok(());
        }
        try!(self.module.as_ref().ok_or(BeamParseError::NoModuleAttribute));

        if let Some((_, line, _, options)) = ("attribute", U32, "compile", Term).do_match(t) {
            let options = try!(self.node(line, options.clone()));
            self.compile_options.push(options);
            return Ok(());
        }
        if let Some((_, line, _, behaviour)) = ("attribute",
                                                U32,
                                                Or(&["behaviour", "behavior"]),
                                                Atom)
            .do_match(t) {
            let behaviour = try!(self.node(line, behaviour.name.clone()));
            self.behaviours.push(behaviour);
            return Ok(());
        }
        if let Some((_, line, _, exports)) = ("attribute", U32, "export", List((Atom, U32)))
            .do_match(t) {
            for (function, arity) in exports {
                let e = try!(self.node(line, Export::new(function.name.clone(), arity)));
                self.exports.push(e);
            }
            return Ok(());
        }
        if let Some((_, line, _, exports)) = ("attribute", U32, "export_type", List((Atom, U32)))
            .do_match(t) {
            for (function, arity) in exports {
                let e = try!(self.node(line, Export::new(function.name.clone(), arity)));
                self.export_types.push(e);
            }
            return Ok(());
        }
        println!("  -- {}", t);
        Ok(())
    }
    fn node<T>(&self, line: u32, value: T) -> BeamParseResult<Node<T>> {
        let file = try!(self.current_file.clone().ok_or(BeamParseError::NoFileAttribute));
        Ok(Node::new(file, line, value))
    }
}
