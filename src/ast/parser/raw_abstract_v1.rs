use std::rc::Rc;
use std::ops::Range;
use eetf;
use result::BeamParseResult;
use error::BeamParseError;
use ast;
use ast::{Node, Export, Import, Spec, Callback, Type, FunType, UnionType};
use ast::{TupleType, Variable, AnnotatedType, BuiltInType, UserType};
use ast::{TypeDef, RecordDecl, RecordField};
use ast::{Expression, TupleExpr, UnaryOpExpr, BinaryOpExpr, ConsExpr, BinaryExpr, BinElement};
use ast::{MapExpr, MapFieldExpr, CallExpr, RemoteCallExpr, RecordExpr, RecordFieldExpr};
use ast::{FunctionDecl, Clause, Guard, Body, Constraint};
use ast::{Qualifier, ListComprehensionExpr, ListGenerateExpr, BinaryGenerateExpr, FilterExpr};
use ast::{RecordType, RecordFieldType};
use ast::ConsPat;
use ast::matcher::Pattern;
use ast::matcher::Either;
use ast::matcher::{U32, I64, F64, Atom, Str, List, List1, List2, AnyList, Term, Nil, Cons};

#[derive(Default)]
pub struct Parser {
    current_file: Option<Rc<String>>,
    module: Option<Node<String>>,
    compile_options: Vec<Node<eetf::Term>>,
    behaviours: Vec<Node<String>>,
    exports: Vec<Node<Export>>,
    export_types: Vec<Node<Export>>,
    imports: Vec<Node<Import>>,
    specs: Vec<Node<Spec>>,
    callbacks: Vec<Node<Callback>>,
    types: Vec<Node<TypeDef>>,
    record_types: Vec<Node<TypeDef>>,
    records: Vec<Node<RecordDecl>>,
    functions: Vec<Node<FunctionDecl>>,
    user_attributes: Vec<Node<(String, eetf::Term)>>,
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
        Ok(ast::Module {
            name: self.module.unwrap(),
            compile_options: self.compile_options,
            behaviours: self.behaviours,
            exports: self.exports,
            export_types: self.export_types,
            imports: self.imports,
            specs: self.specs,
            callbacks: self.callbacks,
            types: self.types,
            record_types: self.record_types,
            records: self.records,
            functions: self.functions,
            user_attributes: self.user_attributes,
        })
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
        if let Some((_, line, _, name)) = ("attribute",
                                           U32,
                                           Either("behaviour", "behavior"),
                                           Atom)
            .do_match(t) {
            let behaviour = try!(self.node(line, name.to_string()));
            self.behaviours.push(behaviour);
            return Ok(());
        }
        if let Some((_, line, _, (module, functions))) = {
                ("attribute", U32, "import", (Atom, List((Atom, U32))))
            }
            .do_match(t) {
            for (name, arity) in functions {
                let i = try!(self.node(line, Import::new(module, name, arity)));
                self.imports.push(i);
            }
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
        if let Some((_, line, kind, ((name, arity), types))) = {
                ("attribute", U32, Either("spec", "callback"), ((Atom, U32), List(FunTyp(self))))
            }
            .do_match(t) {
            match kind {
                Ok(_) => {
                    let spec = try!(self.node(line, Spec::new(name.to_string(), arity, types)));
                    self.specs.push(spec);
                }
                Err(_) => {
                    let callback =
                        try!(self.node(line, Callback::new(name.to_string(), arity, types)));
                    self.callbacks.push(callback);
                }
            }
            return Ok(());
        }
        if let Some((_, line, _, ((module, name, arity), types))) = {
                ("attribute", U32, "spec", ((Atom, Atom, U32), List(FunTyp(self))))
            }
            .do_match(t) {
            let spec = try!(self.node(line,
                                      Spec::new(name.to_string(), arity, types).module(module)));
            self.specs.push(spec);
            return Ok(());
        }
        if let Some((_, line, kind, (name, typ, variables))) = {
                ("attribute",
                 U32,
                 Either("type", "opaque"),
                 (Atom, Typ(self), List(("var", U32, Atom))))
            }
            .do_match(t) {
            let variables = variables.iter().map(|&(_, _, n)| Variable::new(n)).collect();
            let def = match kind {
                Ok(_) => TypeDef::new_type(name, variables, typ),
                Err(_) => TypeDef::new_opaque(name, variables, typ),
            };
            let def = try!(self.node(line, def));
            self.types.push(def);
            return Ok(());
        }
        if let Some((_, line, _, (name, fields))) = ("attribute", U32, "record", (Atom, AnyList))
            .do_match(t) {
            let mut r = RecordDecl::new(name, Vec::with_capacity(fields.len()));
            for f in fields {
                r.fields.push(try!(self.parse_record_field(f)));
            }
            let r = try!(self.node(line, r));
            self.records.push(r);
            return Ok(());
        }
        if let Some((_, line, _, ((_, name), fields, variables))) = {
                ("attribute", U32, "type", (("record", Atom), AnyList, List(("var", U32, Atom))))
            }
            .do_match(t) {
            let mut fs = Vec::with_capacity(fields.len());
            for f in fields {
                fs.push(try!(self.parse_record_field(f)));
            }
            let variables = variables.iter().map(|&(_, _, n)| Variable::new(n)).collect();
            let r = try!(self.node(line, TypeDef::new_record(name, variables, fs)));
            self.record_types.push(r);
            return Ok(());
        }
        if let Some((_, line, name, arity, clauses)) = ("function", U32, Atom, U32, AnyList)
            .do_match(t) {
            let mut f = FunctionDecl::new(name, arity, Vec::with_capacity(clauses.len()));
            for c in clauses {
                f.clauses.push(try!(self.parse_clause(c)));
            }
            let f = try!(self.node(line, f));
            self.functions.push(f);
            return Ok(());
        }
        if let Some((_, line, name, value)) = ("attribute", U32, Atom, Term).do_match(t) {
            let attr = try!(self.node(line, (name.to_string(), value.clone())));
            self.user_attributes.push(attr);
            return Ok(());
        }
        if let Some(_) = ("eof", U32).do_match(t) {
            return Ok(());
        }
        Err(BeamParseError::UnexpectedTerm(t.clone()))
    }
    fn parse_clause(&self, t: &eetf::Term) -> BeamParseResult<Node<Clause>> {
        if let Some((_, line, patterns, guards, body)) = {
                ("clause", U32, AnyList, AnyList, List(Expr(self)))
            }
            .do_match(t) {
            let body = Body::new(body);
            let mut c = Clause::new(body);
            for p in patterns {
                c.patterns.push(try!(self.parse_pattern(p)));
            }
            for g in guards {
                c.guards.push(try!(self.parse_guard(g)));
            }
            Ok(try!(self.node(line, c)))
        } else {
            Err(BeamParseError::UnexpectedTerm(t.clone()))
        }
    }
    fn parse_pattern(&self, t: &eetf::Term) -> BeamParseResult<ast::Pattern> {
        if let Some((_, line, elements)) = ("bin", U32, List(BinElem(self))).do_match(t) {
            let b = BinaryExpr::new(elements);
            return Ok(ast::Pattern::Binary(try!(self.node(line, b))));
        }
        if let Some((_, line, name)) = ("var", U32, Atom).do_match(t) {
            let v = Variable::new(name);
            return Ok(ast::Pattern::Var(try!(self.node(line, v))));
        }
        if let Some((_, line)) = ("nil", U32).do_match(t) {
            return Ok(ast::Pattern::Nil(try!(self.node(line, ()))));
        }
        if let Some((_, line, head, tail)) = ("cons", U32, Pat(self), Pat(self)).do_match(t) {
            let c = ConsPat::new(head, tail);
            return Ok(ast::Pattern::Cons(try!(self.node(line, c))));
        }
        if let Some((_, line, fields)) = ("map", U32, AnyList).do_match(t) {
            let mut m = MapExpr::new(Vec::with_capacity(fields.len()));
            for f in fields {
                m.fields.push(try!(self.parse_map_field(f)));
            }
            return Ok(ast::Pattern::Map(try!(self.node(line, m))));
        }
        if let Some((_, line, elements)) = ("tuple", U32, List(Expr(self))).do_match(t) {
            let t = TupleExpr::new(elements);
            return Ok(ast::Pattern::Tuple(try!(self.node(line, t))));
        }
        panic!("PATTERN: {}", t);
    }
    fn parse_guard(&self, t: &eetf::Term) -> BeamParseResult<Guard> {
        if let Some(tests) = List(Expr(self)).do_match(t) {
            Ok(Guard { tests: tests })
        } else {
            Err(BeamParseError::UnexpectedTerm(t.clone()))
        }
    }
    fn parse_record_field(&self, t: &eetf::Term) -> BeamParseResult<Node<RecordField>> {
        if let Some((_, line, (_, _, name))) = ("record_field", U32, ("atom", U32, Atom))
            .do_match(t) {
            return Ok(try!(self.node(line, RecordField::new(name))));
        }
        if let Some((_, line, (_, _, name), default)) = {
                ("record_field", U32, ("atom", U32, Atom), Expr(self))
            }
            .do_match(t) {
            return Ok(try!(self.node(line, RecordField::new(name).default(default))));
        }
        if let Some((_, (_, line, (_, _, name)), typ)) = {
                ("typed_record_field", ("record_field", U32, ("atom", U32, Atom)), Typ(self))
            }
            .do_match(t) {
            return Ok(try!(self.node(line, RecordField::new(name).field_type(typ))));
        }
        if let Some((_, (_, line, (_, _, name), default), typ)) = {
                ("typed_record_field",
                 ("record_field", U32, ("atom", U32, Atom), Expr(self)),
                 Typ(self))
            }
            .do_match(t) {
            return Ok(try!(self.node(line,
                                     RecordField::new(name).default(default).field_type(typ))));
        }
        Err(BeamParseError::UnexpectedTerm(t.clone()))
    }
    fn parse_record_field_expr(&self, t: &eetf::Term) -> BeamParseResult<Node<RecordFieldExpr>> {
        if let Some((_, line, (_, _, name), value)) = {
                ("record_field", U32, ("atom", U32, Atom), Expr(self))
            }
            .do_match(t) {
            Ok(try!(self.node(line, RecordFieldExpr::new(name, value))))
        } else {
            Err(BeamParseError::UnexpectedTerm(t.clone()))
        }
    }
    fn parse_expression(&self, t: &eetf::Term) -> BeamParseResult<Expression> {
        if let Some((_, line, i)) = ("integer", U32, U32).do_match(t) {
            return Ok(Expression::IntegerLiteral(try!(self.node(line, i))));
        }
        if let Some((_, line, f)) = ("float", U32, F64).do_match(t) {
            return Ok(Expression::FloatLiteral(try!(self.node(line, f))));
        }
        if let Some((_, line, a)) = ("atom", U32, Atom).do_match(t) {
            return Ok(Expression::AtomLiteral(try!(self.node(line, a.to_string()))));
        }
        if let Some((_, line, s)) = ("string", U32, Str).do_match(t) {
            return Ok(Expression::StringLiteral(try!(self.node(line, s))));
        }
        if let Some((_, line, elements)) = ("tuple", U32, List(Expr(self))).do_match(t) {
            let t = TupleExpr::new(elements);
            return Ok(Expression::Tuple(try!(self.node(line, t))));
        }
        if let Some((_, line, op, operand)) = ("op", U32, Atom, Expr(self)).do_match(t) {
            let op = UnaryOpExpr::new(op, operand);
            return Ok(Expression::UnaryOp(try!(self.node(line, op))));
        }
        if let Some((_, line, op, left, right)) = ("op", U32, Atom, Expr(self), Expr(self))
            .do_match(t) {
            let op = BinaryOpExpr::new(op, left, right);
            return Ok(Expression::BinaryOp(try!(self.node(line, op))));
        }
        if let Some((_, line, head, tail)) = ("cons", U32, Expr(self), Expr(self)).do_match(t) {
            let c = ConsExpr::new(head, tail);
            return Ok(Expression::Cons(try!(self.node(line, c))));
        }
        if let Some((_, line)) = ("nil", U32).do_match(t) {
            return Ok(Expression::Nil(try!(self.node(line, ()))));
        }
        if let Some((_, line, elements)) = ("bin", U32, List(BinElem(self))).do_match(t) {
            let b = BinaryExpr::new(elements);
            return Ok(Expression::Binary(try!(self.node(line, b))));
        }
        if let Some((_, line, (_, _, module, function), args)) = {
                ("call", U32, ("remote", U32, Expr(self), Expr(self)), List(Expr(self)))
            }
            .do_match(t) {
            let c = RemoteCallExpr::new(module, function, args);
            return Ok(Expression::RemoteCall(try!(self.node(line, c))));
        }
        if let Some((_, line, function, args)) = ("call", U32, Expr(self), List(Expr(self)))
            .do_match(t) {
            let c = CallExpr::new(function, args);
            return Ok(Expression::Call(try!(self.node(line, c))));
        }
        if let Some((_, line, name)) = ("var", U32, Atom).do_match(t) {
            return Ok(Expression::Var(try!(self.node(line, Variable::new(name)))));
        }
        if let Some((_, line, expr, qualifiers)) = ("lc", U32, Expr(self), List(Qua(self)))
            .do_match(t) {
            let lc = ListComprehensionExpr::new(expr, qualifiers);
            return Ok(Expression::Lc(try!(self.node(line, lc))));
        }
        if let Some((_, line, fields)) = ("map", U32, AnyList).do_match(t) {
            let mut m = MapExpr::new(Vec::with_capacity(fields.len()));
            for f in fields {
                m.fields.push(try!(self.parse_map_field(f)));
            }
            return Ok(Expression::Map(try!(self.node(line, m))));
        }
        if let Some((_, line, name, fields)) = ("record", U32, Atom, AnyList).do_match(t) {
            let mut r = RecordExpr::new(name, Vec::with_capacity(fields.len()));
            for f in fields {
                r.fields.push(try!(self.parse_record_field_expr(f)));
            }
            return Ok(Expression::Record(try!(self.node(line, r))));
        }
        panic!("EXPR: {}", t);
    }
    fn parse_qualifier(&self, t: &eetf::Term) -> BeamParseResult<Qualifier> {
        if let Some((_, line, pattern, expr)) = ("generate", U32, Pat(self), Expr(self))
            .do_match(t) {
            let g = ListGenerateExpr::new(pattern, expr);
            Ok(Qualifier::ListGenerate(try!(self.node(line, g))))
        } else if let Some((_, line, pattern, expr)) = ("b_generate", U32, Pat(self), Expr(self))
            .do_match(t) {
            let g = BinaryGenerateExpr::new(pattern, expr);
            Ok(Qualifier::BinaryGenerate(try!(self.node(line, g))))
        } else if let Some(expr) = Expr(self).do_match(t) {
            let f = FilterExpr::new(expr);
            Ok(Qualifier::Filter(f))
        } else {
            Err(BeamParseError::UnexpectedTerm(t.clone()))
        }
    }
    fn parse_map_field(&self, t: &eetf::Term) -> BeamParseResult<Node<MapFieldExpr>> {
        if let Some((_, line, key, value)) = ("map_field_assoc", U32, Expr(self), Expr(self))
            .do_match(t) {
            Ok(try!(self.node(line, MapFieldExpr::new_assoc(key, value))))
        } else if let Some((_, line, key, value)) = {
                ("map_field_exact", U32, Expr(self), Expr(self))
            }
            .do_match(t) {
            Ok(try!(self.node(line, MapFieldExpr::new_exact(key, value))))
        } else {
            Err(BeamParseError::UnexpectedTerm(t.clone()))
        }
    }
    fn parse_bin_element(&self, t: &eetf::Term) -> BeamParseResult<Node<BinElement>> {
        if let Some((_, line, value, size, tsl)) = {
                ("bin_element",
                 U32,
                 Expr(self),
                 Either("default", Expr(self)),
                 Either("default", List(Either(Atom, (Atom, U32)))))
            }
            .do_match(t) {
            let mut e = BinElement::new(value);
            if let Err(size) = size {
                e.set_size(size);
            }
            if let Err(list) = tsl {
                for x in list {
                    match x {
                        Ok(name) => e.add_type_spec((name, None)),
                        Err((name, value)) => e.add_type_spec((name, Some(value))),
                    }
                }
            }
            Ok(try!(self.node(line, e)))
        } else {
            Err(BeamParseError::UnexpectedTerm(t.clone()))
        }
    }
    fn parse_constraint(&self, t: &eetf::Term) -> BeamParseResult<Node<Constraint>> {
        if let Some((_, line, _, (_, ((_, _, name), typ)))) = {
                ("type",
                 U32,
                 "constraint",
                 List2(("atom", U32, "is_subtype"),
                       List2(("var", U32, Atom), Typ(self))))
            }
            .do_match(t) {
            let c = Constraint::new(name, typ);
            Ok(try!(self.node(line, c)))
        } else {
            Err(BeamParseError::UnexpectedTerm(t.clone()))
        }
    }
    fn parse_type(&self, t: &eetf::Term) -> BeamParseResult<Type> {
        if let Some((_, line, name)) = ("atom", U32, Atom).do_match(t) {
            return Ok(Type::AtomLiteral(try!(self.node(line, name.to_string()))));
        }
        if let Some((_, line, ((_, _, name), ann_type))) = {
                ("ann_type", U32, List2(("var", U32, Atom), Typ(self)))
            }
            .do_match(t) {
            return Ok(Type::Annotated(try!(self.node(line, AnnotatedType::new(name, ann_type)))));
        }
        if let Some((_, line, _, ((_, _, _, args), result))) = {
                ("type", U32, "fun", List2(("type", U32, "product", List(Typ(self))), Typ(self)))
            }
            .do_match(t) {
            let f = FunType::new(args, result);
            return Ok(Type::Fun(try!(self.node(line, f))));
        }
        if let Some((_, _, _, (mut ftype, constraints))) = {
                ("type", U32, "bounded_fun", List2(FunTyp(self), AnyList))
            }
            .do_match(t) {
            let mut cs = Vec::with_capacity(constraints.len());
            for c in constraints {
                cs.push(try!(self.parse_constraint(c)));
            }
            ftype.set_constraints(cs);
            return Ok(Type::Fun(ftype));
        }
        if let Some((_, line, _, types)) = ("type", U32, "union", List(Typ(self))).do_match(t) {
            let u = UnionType::new(types);
            return Ok(Type::Union(try!(self.node(line, u))));
        }
        if let Some((_, line, _, _)) = ("type", U32, "tuple", "any").do_match(t) {
            return Ok(Type::BuiltIn(try!(self.node(line, BuiltInType::Tuple))));
        }
        if let Some((_, line, _, elements)) = ("type", U32, "tuple", List(Typ(self))).do_match(t) {
            return Ok(Type::Tuple(try!(self.node(line, TupleType { elements: elements }))));
        }
        if let Some((_, line, _, _)) = ("type", U32, "map", "any").do_match(t) {
            return Ok(Type::BuiltIn(try!(self.node(line, BuiltInType::Map))));
        }
        if let Some((_, line, name, args)) = ("user_type", U32, Atom, List(Typ(self))).do_match(t) {
            let user = UserType {
                name: name.to_string(),
                args: args,
            };
            return Ok(Type::User(try!(self.node(line, user))));
        }
        if let Some((_, line, name, _)) = ("type", U32, Atom, Nil).do_match(t) {
            let builtin_type = match name {
                "term" => BuiltInType::Term,
                "binary" => BuiltInType::Binary,
                "integer" => BuiltInType::Integer,
                "neg_integer" => BuiltInType::NegInteger,
                "non_neg_integer" => BuiltInType::NonNegInteger,
                "float" => BuiltInType::Float,
                "atom" => BuiltInType::Atom,
                "list" => BuiltInType::List(None),
                "bitstring" => BuiltInType::BitString,
                "pid" => BuiltInType::Pid,
                "reference" => BuiltInType::Reference,
                _ => panic!("TYPE: {}", t),
            };
            return Ok(Type::BuiltIn(try!(self.node(line, builtin_type))));
        }
        if let Some((_, line, _, arg)) = ("type", U32, "list", List1(Typ(self))).do_match(t) {
            let t = BuiltInType::List(Some(Box::new(arg)));
            return Ok(Type::BuiltIn(try!(self.node(line, t))));
        }
        if let Some((_, line, name)) = ("var", U32, Atom).do_match(t) {
            return Ok(Type::Var(try!(self.node(line, Variable::new(name)))));
        }
        if let Some((_, line, _, ((_, _, name), fields))) = {
                ("type", U32, "record", Cons(("atom", U32, Atom)))
            }
            .do_match(t) {
            let mut r = RecordType::new(name, Vec::with_capacity(fields.len()));
            for f in fields {
                r.fields.push(try!(self.parse_field_type(f)));
            }
            return Ok(Type::Record(try!(self.node(line, r))));
        }
        if let Some((_, line, _, ((_, _, low), (_, _, high)))) = {
                ("type", U32, "range", List2(("integer", U32, I64), ("integer", U32, I64)))
            }
            .do_match(t) {
            let r = Range {
                start: low,
                end: high + 1,
            };
            return Ok(Type::Range(try!(self.node(line, r))));
        }
        panic!(" TYPE: {}", t);
    }
    fn parse_field_type(&self, t: &eetf::Term) -> BeamParseResult<Node<RecordFieldType>> {
        if let Some((_, line, _, ((_, _, name), typ))) = {
                ("type", U32, "field_type", List2(("atom", U32, Atom), Typ(self)))
            }
            .do_match(t) {
            Ok(try!(self.node(line, RecordFieldType::new(name, typ))))
        } else {
            Err(BeamParseError::UnexpectedTerm(t.clone()))
        }
    }
    fn node<T>(&self, line: u32, value: T) -> BeamParseResult<Node<T>> {
        let file = try!(self.current_file.clone().ok_or(BeamParseError::NoFileAttribute));
        Ok(Node::new(file, line, value))
    }
}

struct Expr<'a>(pub &'a Parser);
impl<'a, 'b> Pattern<'a> for Expr<'b> {
    type Value = Expression;
    fn do_match(&self, term: &'a eetf::Term) -> Option<Self::Value> {
        self.0.parse_expression(term).ok()
    }
}

struct Qua<'a>(pub &'a Parser);
impl<'a, 'b> Pattern<'a> for Qua<'b> {
    type Value = Qualifier;
    fn do_match(&self, term: &'a eetf::Term) -> Option<Self::Value> {
        self.0.parse_qualifier(term).ok()
    }
}

struct Typ<'a>(pub &'a Parser);
impl<'a, 'b> Pattern<'a> for Typ<'b> {
    type Value = Type;
    fn do_match(&self, term: &'a eetf::Term) -> Option<Self::Value> {
        self.0.parse_type(term).ok()
    }
}

struct Pat<'a>(pub &'a Parser);
impl<'a, 'b> Pattern<'a> for Pat<'b> {
    type Value = ast::Pattern;
    fn do_match(&self, term: &'a eetf::Term) -> Option<Self::Value> {
        self.0.parse_pattern(term).ok()
    }
}

struct FunTyp<'a>(pub &'a Parser);
impl<'a, 'b> Pattern<'a> for FunTyp<'b> {
    type Value = Node<FunType>;
    fn do_match(&self, term: &'a eetf::Term) -> Option<Self::Value> {
        self.0.parse_type(term).ok().and_then(|t| if let Type::Fun(t) = t {
            Some(t)
        } else {
            None
        })
    }
}

struct BinElem<'a>(pub &'a Parser);
impl<'a, 'b> Pattern<'a> for BinElem<'b> {
    type Value = Node<BinElement>;
    fn do_match(&self, term: &'a eetf::Term) -> Option<Self::Value> {
        self.0.parse_bin_element(term).ok()
    }
}
