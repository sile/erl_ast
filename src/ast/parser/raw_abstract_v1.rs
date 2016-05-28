use std::rc::Rc;
use eetf;
use result::BeamParseResult;
use error::BeamParseError;
use ast;
use ast::{Node, Export, Spec, Callback, Type, FunType, UnionType};
use ast::{TupleType, Variable, AnnotatedType, BuiltInType, UserType};
use ast::TypeDef;
use ast::matcher::Pattern;
use ast::matcher::Or;
use ast::matcher::{U32, Atom, Str, List, List2, AnyList, Term, Nil};

#[derive(Default)]
pub struct Parser {
    current_file: Option<Rc<String>>,
    module: Option<Node<String>>,
    compile_options: Vec<Node<eetf::Term>>,
    behaviours: Vec<Node<String>>,
    exports: Vec<Node<Export>>,
    export_types: Vec<Node<Export>>,
    specs: Vec<Node<Spec>>,
    callbacks: Vec<Node<Callback>>,
    types: Vec<Node<TypeDef>>,
}
impl Parser {
    pub fn new() -> Self {
        Parser::default()
    }
    pub fn parse(mut self, abstract_code: eetf::Term) -> BeamParseResult<ast::Module> {
        let (_, declarations_and_forms) = try!(("raw_abstract_v1", AnyList)
            .do_match(&abstract_code)
            .ok_or(BeamParseError::UnknownAbstractFormat));
        try!(self.parse_declarations_and_forms(declarations_and_forms));
        unimplemented!();
    }
    pub fn parse_declarations_and_forms(&mut self,
                                        declarations_and_forms: &[eetf::Term])
                                        -> BeamParseResult<()> {
        for t in declarations_and_forms {
            try!(self.parse_declaration_or_form(&t));
        }
        Ok(())
    }
    pub fn parse_declaration_or_form(&mut self, t: &eetf::Term) -> BeamParseResult<()> {
        if let Some((_, _, _, (file, _))) = ("attribute", U32, "file", (Str, U32)).do_match(t) {
            self.current_file = Some(Rc::new(file));
            return Ok(());
        }
        if let Some((_, line, _, name)) = ("attribute", U32, "module", Atom).do_match(t) {
            self.module = Some(try!(self.node(line, name.to_string())));
            return Ok(());
        }
        try!(self.module.as_ref().ok_or(BeamParseError::NoModuleAttribute));

        if let Some((_, line, _, options)) = ("attribute", U32, "compile", Term).do_match(t) {
            let options = try!(self.node(line, options.clone()));
            self.compile_options.push(options);
            return Ok(());
        }
        if let Some((_, line, _, name)) = ("attribute", U32, Or(&["behaviour", "behavior"]), Atom)
            .do_match(t) {
            let behaviour = try!(self.node(line, name.to_string()));
            self.behaviours.push(behaviour);
            return Ok(());
        }
        if let Some((_, line, _, exports)) = ("attribute", U32, "export", List((Atom, U32)))
            .do_match(t) {
            for (name, arity) in exports {
                let e = try!(self.node(line, Export::new(name.to_string(), arity)));
                self.exports.push(e);
            }
            return Ok(());
        }
        if let Some((_, line, _, exports)) = ("attribute", U32, "export_type", List((Atom, U32)))
            .do_match(t) {
            for (name, arity) in exports {
                let e = try!(self.node(line, Export::new(name.to_string(), arity)));
                self.export_types.push(e);
            }
            return Ok(());
        }
        if let Some((_, line, kind, ((name, arity), ftypes))) = {
                ("attribute", U32, Or(&["spec", "callback"]), ((Atom, U32), AnyList))
            }
            .do_match(t) {
            let mut types = Vec::with_capacity(ftypes.len());
            for f in ftypes {
                types.push(try!(self.parse_type(&f).and_then(|t| if let Type::Fun(t) = t {
                    Ok(t)
                } else {
                    Err(BeamParseError::UnexpectedTerm(f.clone()))
                })));
            }
            match kind {
                "spec" => {
                    let spec = try!(self.node(line, Spec::new(name.to_string(), arity, types)));
                    self.specs.push(spec);
                }
                "callback" => {
                    let callback =
                        try!(self.node(line, Callback::new(name.to_string(), arity, types)));
                    self.callbacks.push(callback);
                }
                _ => unreachable!(),
            }
            return Ok(());
        }
        if let Some((_, line, _, ((module, name, arity), ftypes))) = {
                ("attribute", U32, "spec", ((Atom, Atom, U32), AnyList))
            }
            .do_match(t) {
            let mut types = Vec::with_capacity(ftypes.len());
            for f in ftypes {
                types.push(try!(self.parse_type(&f).and_then(|t| if let Type::Fun(t) = t {
                    Ok(t)
                } else {
                    Err(BeamParseError::UnexpectedTerm(f.clone()))
                })));
            }
            let spec = try!(self.node(line,
                                      Spec::new(name.to_string(), arity, types).module(module)));
            self.specs.push(spec);
            return Ok(());
        }
        if let Some((_, line, kind, (name, typ, variables))) = {
                ("attribute", U32, Or(&["type", "opaque"]), (Atom, Term, List(("var", U32, Atom))))
            }
            .do_match(t) {
            let typ = try!(self.parse_type(typ));
            let variables = variables.iter().map(|&(_, _, n)| Variable::new(n)).collect();
            let def = match kind {
                "type" => TypeDef::new_type(name, variables, typ),
                "opaque" => TypeDef::new_opaque(name, variables, typ),
                _ => unreachable!(),
            };
            let def = try!(self.node(line, def));
            self.types.push(def);
            return Ok(());
        }
        println!("  -- {}", t);
        Ok(())
    }
    fn parse_type(&mut self, t: &eetf::Term) -> BeamParseResult<Type> {
        if let Some((_, line, name)) = ("atom", U32, Atom).do_match(t) {
            return Ok(Type::AtomLiteral(try!(self.node(line, name.to_string()))));
        }
        if let Some((_, line, ((_, _, name), ann_type))) = {
                ("ann_type", U32, List2(("var", U32, Atom), Term))
            }
            .do_match(t) {
            let ann_type = try!(self.parse_type(ann_type));
            return Ok(Type::Annotated(try!(self.node(line, AnnotatedType::new(name, ann_type)))));
        }
        if let Some((_, line, _, ((_, _, _, args), result))) = {
                ("type", U32, "fun", List2(("type", U32, "product", AnyList), Term))
            }
            .do_match(t) {
            let mut f = FunType::new(Vec::new(), try!(self.parse_type(result)));
            for a in args {
                f.args.push(try!(self.parse_type(a)));
            }
            return Ok(Type::Fun(try!(self.node(line, f))));
        }
        if let Some((_, line, _, types)) = ("type", U32, "union", AnyList).do_match(t) {
            let mut u = UnionType::new(Vec::new());
            for t in types {
                u.types.push(try!(self.parse_type(t)));
            }
            return Ok(Type::Union(try!(self.node(line, u))));
        }
        if let Some((_, line, _, _)) = ("type", U32, "tuple", "any").do_match(t) {
            return Ok(Type::BuiltIn(try!(self.node(line, BuiltInType::Tuple))));
        }
        if let Some((_, line, _, elements)) = ("type", U32, "tuple", AnyList).do_match(t) {
            let mut list = Vec::with_capacity(elements.len());
            for e in elements {
                list.push(try!(self.parse_type(e)));
            }
            return Ok(Type::Tuple(try!(self.node(line, TupleType { elements: list }))));
        }
        if let Some((_, line, _, _)) = ("type", U32, "map", "any").do_match(t) {
            return Ok(Type::BuiltIn(try!(self.node(line, BuiltInType::Map))));
        }
        if let Some((_, line, name, args)) = ("user_type", U32, Atom, AnyList).do_match(t) {
            let mut user = UserType {
                name: name.to_string(),
                args: Vec::with_capacity(args.len()),
            };
            for a in args {
                user.args.push(try!(self.parse_type(a)));
            }
            return Ok(Type::User(try!(self.node(line, user))));
        }
        if let Some((_, line, name, _)) = ("type", U32, Atom, Nil).do_match(t) {
            let builtin_type = match name {
                "term" => BuiltInType::Term,
                "binary" => BuiltInType::Binary,
                "integer" => BuiltInType::Integer,
                "neg_integer" => BuiltInType::NegInteger,
                "float" => BuiltInType::Float,
                "atom" => BuiltInType::Atom,
                "list" => BuiltInType::List,
                "bitstring" => BuiltInType::BitString,
                "pid" => BuiltInType::Pid,
                "reference" => BuiltInType::Reference,
                _ => panic!("TYPE: {}", t),
            };
            return Ok(Type::BuiltIn(try!(self.node(line, builtin_type))));
        }
        if let Some((_, line, name)) = ("var", U32, Atom).do_match(t) {
            return Ok(Type::Var(try!(self.node(line, Variable::new(name)))));
        }
        println!(" TYPE: {}", t);
        unimplemented!()
    }
    fn node<T>(&self, line: u32, value: T) -> BeamParseResult<Node<T>> {
        let file = try!(self.current_file.clone().ok_or(BeamParseError::NoFileAttribute));
        Ok(Node::new(file, line, value))
    }
}
