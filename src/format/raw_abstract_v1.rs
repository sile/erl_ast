use std::fmt::Debug;
use std::marker::PhantomData;
use std::path::Path;
use std::io;
use beam_file;
use beam_file::chunk::Chunk;
use eetf;
use eetf::pattern::Pattern;
use eetf::pattern::{VarList, FixList, I32, U32, U64, F64, Int, Nil, Or, Union2, Union3, Str, Ascii};
use eetf::pattern::{Unmatch, Cons, Unicode};
use eetf::pattern;
use result::BeamParseResult;
use error::BeamParseError;
use ast;
use ast::literal;
use ast::expr;
use ast::clause;
use ast::guard;
use ast::typ;
use ast::common;

macro_rules! to{
    ($to:ty) => {
        To::<$to>(PhantomData)
    }
}

pub struct AbstractCode {
    pub code: eetf::Term,
}
impl AbstractCode {
    pub fn from_beam_file<P: AsRef<Path>>(path: P) -> BeamParseResult<Self> {
        let beam = try!(beam_file::RawBeamFile::from_file(path));
        let chunk = try!(beam.chunks
            .into_iter()
            .find(|c| c.id() == b"Abst")
            .ok_or(BeamParseError::NoDebugInfo));
        let code = try!(eetf::Term::decode(io::Cursor::new(&chunk.data)));
        Ok(AbstractCode { code: code })
    }
    pub fn to_module_decl(&self) -> BeamParseResult<ast::form::ModuleDecl> {
        let (_, forms) = try!(self.code
            .as_match(("raw_abstract_v1", VarList(to!(ast::form::Form)))));
        Ok(ast::form::ModuleDecl { forms: forms })
    }
}

pub trait FromTerm<'a> {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> where Self: Sized;
}

#[derive(Debug)]
struct To<T>(PhantomData<T>);
impl<T> Clone for To<T> {
    fn clone(&self) -> Self {
        To(PhantomData)
    }
}
impl<'a, F> Pattern<'a> for To<F>
    where F: FromTerm<'a> + Debug + 'static
{
    type Output = F;
    fn try_match(&self, term: &'a eetf::Term) -> pattern::Result<'a, Self::Output> {
        F::try_from(term).map_err(|e| self.unmatched(term).cause(e))
    }
}

fn atom() -> eetf::pattern::Any<eetf::Atom> {
    eetf::pattern::any()
}
fn any() -> eetf::pattern::Any<eetf::Term> {
    eetf::pattern::any()
}
fn expr() -> To<expr::Expression> {
    to!(expr::Expression)
}
fn clause() -> To<clause::Clause> {
    to!(clause::Clause)
}
fn ty() -> To<typ::Type> {
    to!(typ::Type)
}
fn ftype() -> To<typ::Fun> {
    to!(typ::Fun)
}
fn var() -> To<common::Variable> {
    to!(common::Variable)
}
fn atom_lit() -> To<literal::Atom> {
    to!(literal::Atom)
}
fn integer() -> To<literal::Integer> {
    to!(literal::Integer)
}

macro_rules! return_if_ok {
    ($e:expr) => {
        match $e {
            Ok(value) => return Ok(::std::convert::From::from(value)),
            Err(err) => err,
        }
    }
}

impl<'a> FromTerm<'a> for ast::form::Form {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        let e = return_if_ok!(term.as_match(to!(ast::form::ModuleAttr)));
        let e = return_if_ok!(term.as_match(to!(ast::form::ModuleAttr))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::form::FileAttr))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::form::BehaviourAttr))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::form::ExportAttr))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::form::ImportAttr))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::form::ExportTypeAttr))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::form::CompileOptionsAttr))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::form::RecordDecl))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::form::TypeDecl))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::form::FunSpec))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::form::FunDecl))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::form::WildAttr))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::form::Eof))).max_depth(e);
        Err(e)
    }
}
impl<'a> FromTerm<'a> for expr::Expression {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        let e = return_if_ok!(term.as_match(integer()));
        let e = return_if_ok!(term.as_match(to!(literal::Float))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(literal::Str))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(literal::Char))).max_depth(e);
        let e = return_if_ok!(term.as_match(atom_lit())).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(common::Match<_, _>))).max_depth(e);
        let e = return_if_ok!(term.as_match(var())).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(common::Tuple<_>))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(common::Nil))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(common::Cons<_>))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(common::Binary<_>))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(common::UnaryOp<_>))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(common::BinaryOp<_>))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(common::Record<_>))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(common::RecordIndex<_>))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(common::Map<_>))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(expr::Catch))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(common::LocalCall<_>))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(common::RemoteCall<_>))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(expr::Comprehension))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(expr::Block))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(expr::If))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(expr::Case))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(expr::Try))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(expr::Receive))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(common::InternalFun))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(common::ExternalFun))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(expr::AnonymousFun))).max_depth(e);
        Err(e)
    }
}
impl<'a> FromTerm<'a> for expr::Catch {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("catch", I32, expr()))
            .map(|(_, line, expr)| Self::new(line, expr))
    }
}
impl<'a> FromTerm<'a> for expr::Receive {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(Or((("receive", I32, VarList(clause())),
                          ("receive", I32, VarList(clause()), expr(), VarList(expr())))))
            .map(|result| match result {
                Union2::A((_, line, clauses)) => Self::new(line, clauses),
                Union2::B((_, line, clauses, timeout, after)) => {
                    Self::new(line, clauses).timeout(timeout).after(after)
                }
            })
    }
}
impl<'a> FromTerm<'a> for common::InternalFun {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("fun", I32, ("function", atom(), U32)))
            .map(|(_, line, (_, name, arity))| Self::new(line, name.to_string(), arity))
    }
}
impl<'a> FromTerm<'a> for common::ExternalFun {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("fun", I32, ("function", expr(), expr(), expr())))
            .map(|(_, line, (_, module, function, arity))| Self::new(line, module, function, arity))
    }
}
impl<'a> FromTerm<'a> for expr::AnonymousFun {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(Or((("fun", I32, ("clauses", VarList(clause()))),
                          ("named_fun", I32, atom(), VarList(clause())))))
            .map(|result| match result {
                Union2::A((_, line, (_, clauses))) => Self::new(line, clauses),
                Union2::B((_, line, name, clauses)) => {
                    Self::new(line, clauses).name(name.to_string())
                }
            })
    }
}
impl<'a> FromTerm<'a> for expr::Block {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("block", I32, VarList(expr())))
            .map(|(_, line, body)| Self::new(line, body))
    }
}
impl<'a> FromTerm<'a> for expr::If {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("if", I32, VarList(clause())))
            .map(|(_, line, clauses)| Self::new(line, clauses))
    }
}
impl<'a> FromTerm<'a> for expr::Case {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("case", I32, expr(), VarList(clause())))
            .map(|(_, line, expr, clauses)| Self::new(line, expr, clauses))
    }
}
impl<'a> FromTerm<'a> for expr::Try {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("try",
                       I32,
                       VarList(expr()),
                       VarList(clause()),
                       VarList(clause()),
                       VarList(expr())))
            .map(|(_, line, body, case_clauses, catch_clauses, after)| {
                Self::new(line, body, case_clauses, catch_clauses, after)
            })
    }
}
impl<'a> FromTerm<'a> for expr::Comprehension {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match((Or(("lc", "bc")), I32, expr(), VarList(to!(expr::Qualifier))))
            .map(|(is_lc, line, expr, qualifiers)| Self::new(line, is_lc.is_a(), expr, qualifiers))
    }
}
impl<'a> FromTerm<'a> for expr::Qualifier {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        use ast::expr::Qualifier::*;
        term.as_match(Or((("generate", I32, to!(ast::pattern::Pattern), expr()),
                          ("b_generate", I32, to!(ast::pattern::Pattern), expr()),
                          expr())))
            .map(|result| match result {
                Union3::A((_, line, pattern, expr)) => {
                    Generator(ast::expr::Generator::new(line, pattern, expr))
                }
                Union3::B((_, line, pattern, expr)) => {
                    BitStringGenerator(ast::expr::Generator::new(line, pattern, expr))
                }
                Union3::C(expr) => Filter(expr),
            })
    }
}
impl<'a> FromTerm<'a> for clause::Clause {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("clause",
                       I32,
                       VarList(to!(ast::pattern::Pattern)),
                       VarList(to!(guard::OrGuard)),
                       VarList(expr())))
            .map(|(_, line, patterns, guards, body)| Self::new(line, patterns, guards, body))
    }
}
impl<'a> FromTerm<'a> for guard::OrGuard {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(VarList(to!(guard::Guard))).map(|guards| Self::new(guards))
    }
}
impl<'a, T: FromTerm<'a> + Debug + 'static> FromTerm<'a> for common::Tuple<T> {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("tuple", I32, VarList(to!(T))))
            .map(|(_, line, elements)| Self::new(line, elements))
    }
}
impl<'a, T: FromTerm<'a> + Debug + 'static> FromTerm<'a> for common::Cons<T> {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("cons", I32, to!(T), to!(T)))
            .map(|(_, line, head, tail)| Self::new(line, head, tail))
    }
}
impl<'a, T: FromTerm<'a> + Debug + 'static> FromTerm<'a> for common::Binary<T> {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("bin", I32, VarList(to!(common::BinElement<T>))))
            .map(|(_, line, elements)| Self::new(line, elements))
    }
}
impl<'a, T: FromTerm<'a> + Debug + 'static> FromTerm<'a> for common::BinElement<T> {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("bin_element",
                       I32,
                       to!(T),
                       Or((to!(T), "default")),
                       Or((VarList(to!(common::BinElementTypeSpec)), "default"))))
            .map(|(_, line, value, size, tsl)| {
                let mut e = Self::new(line, value);
                if let Union2::A(size) = size {
                    e = e.size(size);
                }
                if let Union2::A(tsl) = tsl {
                    e = e.tsl(tsl);
                }
                e
            })
    }
}
impl<'a> FromTerm<'a> for common::BinElementTypeSpec {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(Or((atom(), (atom(), U64))))
            .map(|ts| {
                match ts {
                    Union2::A(name) => Self::new(name.to_string(), None),
                    Union2::B((name, value)) => Self::new(name.to_string(), Some(value)),
                }
            })
    }
}
impl<'a, T: FromTerm<'a> + Debug + 'static> FromTerm<'a> for common::Record<T> {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(Or((("record", I32, atom(), VarList(to!(common::RecordField<T>))),
                          ("record", I32, expr(), atom(), VarList(to!(common::RecordField<T>))))))
            .map(|result| match result {
                Union2::A((_, line, name, fields)) => Self::new(line, name.to_string(), fields),
                Union2::B((_, line, base, name, fields)) => {
                    Self::new(line, name.to_string(), fields).base(base)
                }
            })
    }
}
impl<'a, T: FromTerm<'a> + Debug + 'static> FromTerm<'a> for common::RecordField<T> {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("record_field", I32, Or((atom_lit(), ("var", I32, "_"))), to!(T)))
            .map(|(_, line, name, value)| {
                let name = name.into_result().ok().map(|n| n.value);
                Self::new(line, name, value)
            })
    }
}
impl<'a, T: FromTerm<'a> + Debug + 'static> FromTerm<'a> for common::Map<T> {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(Or((("map", I32, VarList(to!(common::MapPair<T>))),
                          ("map", I32, expr(), VarList(to!(common::MapPair<T>))))))
            .map(|result| match result {
                Union2::A((_, line, pairs)) => Self::new(line, pairs),
                Union2::B((_, line, base, pairs)) => Self::new(line, pairs).base(base),
            })
    }
}
impl<'a, T: FromTerm<'a> + Debug + 'static> FromTerm<'a> for common::MapPair<T> {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match((Or(("map_field_assoc", "map_field_exact")), I32, to!(T), to!(T)))
            .map(|(is_assoc, line, key, value)| Self::new(line, is_assoc.is_a(), key, value))
    }
}
impl<'a, T: FromTerm<'a> + Debug + 'static> FromTerm<'a> for common::LocalCall<T> {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("call", I32, to!(T), VarList(to!(T))))
            .map(|(_, line, function, args)| Self::new(line, function, args))
    }
}
impl<'a, T: FromTerm<'a> + Debug + 'static> FromTerm<'a> for common::RemoteCall<T> {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("call", I32, ("remote", U32, to!(T), to!(T)), VarList(to!(T))))
            .map(|(_, line, (_, _, module, function), args)| {
                Self::new(line, module, function, args)
            })
    }
}
impl<'a, T: FromTerm<'a> + Debug + 'static> FromTerm<'a> for common::RecordIndex<T> {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(Or((("record_index", I32, atom(), atom_lit()),
                          ("record_field", I32, to!(T), atom(), atom_lit()))))
            .map(|result| match result {
                Union2::A((_, line, name, field)) => Self::new(line, name.to_string(), field.value),
                Union2::B((_, line, base, name, field)) => {
                    Self::new(line, name.to_string(), field.value).base(base)
                }
            })
    }
}
impl<'a, T: FromTerm<'a> + Debug + 'static> FromTerm<'a> for common::BinaryOp<T> {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("op", I32, atom(), to!(T), to!(T)))
            .map(|(_, line, op, left, right)| Self::new(line, op.to_string(), left, right))
    }
}
impl<'a, T: FromTerm<'a> + Debug + 'static> FromTerm<'a> for common::UnaryOp<T> {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("op", I32, atom(), to!(T)))
            .map(|(_, line, op, arg)| Self::new(line, op.to_string(), arg))
    }
}
impl<'a> FromTerm<'a> for common::Nil {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("nil", I32)).map(|(_, line)| Self::new(line))
    }
}
impl<'a, L, R> FromTerm<'a> for common::Match<L, R>
    where L: FromTerm<'a> + Debug + 'static,
          R: FromTerm<'a> + Debug + 'static
{
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("match", I32, to!(L), to!(R)))
            .map(|(_, line, left, right)| Self::new(line, left, right))
    }
}
impl<'a> FromTerm<'a> for typ::Type {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        let e = return_if_ok!(term.as_match(integer()));
        let e = return_if_ok!(term.as_match(atom_lit())).max_depth(e);
        let e = return_if_ok!(term.as_match(var())).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(common::UnaryOp<_>))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(common::BinaryOp<_>))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(common::Nil))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(typ::Annotated))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(typ::BitString))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(typ::AnyFun))).max_depth(e);
        let e = return_if_ok!(term.as_match(ftype())).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(typ::Range))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(typ::Map))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(typ::Record))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(typ::RemoteType))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(typ::AnyTuple))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(typ::Tuple))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(typ::Union))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(typ::BuiltInType))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(typ::UserType))).max_depth(e);
        Err(e)
    }
}
impl<'a> FromTerm<'a> for typ::UserType {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("user_type", I32, atom(), VarList(ty())))
            .map(|(_, line, name, args)| Self::new(line, name.to_string(), args))
    }
}
impl<'a> FromTerm<'a> for typ::Union {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("type", I32, "union", VarList(ty())))
            .map(|(_, line, _, types)| Self::new(line, types))
    }
}
impl<'a> FromTerm<'a> for typ::Tuple {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("type", I32, "tuple", VarList(ty())))
            .map(|(_, line, _, types)| Self::new(line, types))
    }
}
impl<'a> FromTerm<'a> for typ::AnyTuple {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("type", I32, "tuple", "any"))
            .map(|(_, line, _, _)| Self::new(line))
    }
}
impl<'a> FromTerm<'a> for typ::RemoteType {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("remote_type", I32, FixList((atom_lit(), atom_lit(), VarList(ty())))))
            .map(|(_, line, (module, function, args))| {
                Self::new(line, module.value, function.value, args)
            })
    }
}
impl<'a> FromTerm<'a> for typ::Record {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("type", I32, "record", Cons(atom_lit(), to!(typ::RecordField))))
            .map(|(_, line, _, (name, fields))| Self::new(line, name.value, fields))
    }
}
impl<'a> FromTerm<'a> for typ::RecordField {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("type", I32, "field_type", FixList((atom_lit(), ty()))))
            .map(|(_, line, _, (name, type_))| Self::new(line, name.value, type_))
    }
}
impl<'a> FromTerm<'a> for typ::BuiltInType {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("type", I32, atom(), Or(("any", VarList(ty())))))
            .map(|(_, line, name, args)| {
                match args {
                    Union2::A(_) => Self::new(line, name.to_string(), Vec::new()),
                    Union2::B(args) => Self::new(line, name.to_string(), args),
                }
            })
    }
}
impl<'a> FromTerm<'a> for typ::Map {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("type", I32, "map", VarList(to!(typ::MapPair))))
            .map(|(_, line, _, pairs)| Self::new(line, pairs))
    }
}
impl<'a> FromTerm<'a> for typ::MapPair {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("type", I32, "map_field_assoc", FixList((ty(), ty()))))
            .map(|(_, line, _, (key, value))| Self::new(line, key, value))
    }
}
impl<'a> FromTerm<'a> for typ::Range {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("type", I32, "range", FixList((ty(), ty()))))
            .map(|(_, line, _, (low, high))| Self::new(line, low, high))
    }
}
impl<'a> FromTerm<'a> for typ::Fun {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(Or((("type",
                           I32,
                           "bounded_fun",
                           FixList((ftype(), VarList(to!(typ::Constraint))))),
                          ("type",
                           I32,
                           "fun",
                           FixList((("type", I32, "product", VarList(ty())), ty()))))))
            .map(|result| match result {
                Union2::A((_, _, _, (fun, constraints))) => fun.constraints(constraints),
                Union2::B((_, line, _, ((_, _, _, args), return_type))) => {
                    Self::new(line, args, return_type)
                }
            })
    }
}
impl<'a> FromTerm<'a> for typ::Constraint {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("type",
                       I32,
                       "constraint",
                       FixList((("atom", I32, "is_subtype"), FixList((var(), ty()))))))
            .map(|(_, line, _, (_, (var, subtype)))| Self::new(line, var, subtype))
    }
}
impl<'a> FromTerm<'a> for typ::AnyFun {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("type", I32, "fun", Or((Nil, FixList((("type", I32, "any"), ty()))))))
            .map(|(_, line, _, fun)| match fun {
                Union2::A(_) => Self::new(line),
                Union2::B((_, type_)) => Self::new(line).return_type(type_),
            })
    }
}
impl<'a> FromTerm<'a> for typ::Annotated {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("ann_type", I32, FixList((var(), ty()))))
            .map(|(_, line, (var, type_))| Self::new(line, var, type_))
    }
}
impl<'a> FromTerm<'a> for typ::BitString {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("type", I32, "binary", FixList((integer(), integer()))))
            .map(|(_, line, _, (bytes, bits))| {
                Self::new(line, bytes.to_u64().unwrap(), bits.to_u64().unwrap())
            })
    }
}
impl<'a> FromTerm<'a> for ast::pattern::Pattern {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        let e = return_if_ok!(term.as_match(integer()));
        let e = return_if_ok!(term.as_match(to!(literal::Float))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(literal::Str))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(literal::Char))).max_depth(e);
        let e = return_if_ok!(term.as_match(atom_lit())).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(common::Match<_, _>))).max_depth(e);
        let e = return_if_ok!(term.as_match(var())).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(common::Tuple<_>))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(common::Nil))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(common::Cons<_>))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(common::Binary<_>))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(common::UnaryOp<_>))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(common::BinaryOp<_>))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(common::Record<_>))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(common::RecordIndex<_>))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(common::Map<_>))).max_depth(e);
        Err(e)
    }
}
impl<'a> FromTerm<'a> for guard::Guard {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        let e = return_if_ok!(term.as_match(integer()));
        let e = return_if_ok!(term.as_match(to!(literal::Float))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(literal::Str))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(literal::Char))).max_depth(e);
        let e = return_if_ok!(term.as_match(atom_lit())).max_depth(e);
        let e = return_if_ok!(term.as_match(var())).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(common::Tuple<_>))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(common::Nil))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(common::Cons<_>))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(common::Binary<_>))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(common::UnaryOp<_>))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(common::BinaryOp<_>))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(common::Record<_>))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(common::RecordIndex<_>))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(common::LocalCall<_>))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(common::RemoteCall<_>))).max_depth(e);
        Err(e)
    }
}
impl<'a> FromTerm<'a> for ast::form::ModuleAttr {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("attribute", I32, "module", atom()))
            .map(|(_, line, _, name)| Self::new(line, name.to_string()))
    }
}
impl<'a> FromTerm<'a> for ast::form::FileAttr {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("attribute", I32, "file", (Str(Ascii), I32)))
            .map(|(_, line, _, (original_file, original_line))| {
                Self::new(line, original_file, original_line)
            })
    }
}
impl<'a> FromTerm<'a> for ast::form::BehaviourAttr {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("attribute", I32, Or(("behaviour", "behavior")), atom()))
            .map(|(_, line, is_british, name)| {
                Self::new(line, name.to_string()).british(is_british.is_a())
            })
    }
}
impl<'a> FromTerm<'a> for ast::form::RecordDecl {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("attribute",
                       I32,
                       "record",
                       (atom(), VarList(to!(ast::form::RecordFieldDecl)))))
            .map(|(_, line, _, (name, fields))| Self::new(line, name.to_string(), fields))
    }
}
impl<'a> FromTerm<'a> for ast::form::RecordFieldDecl {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(Or((("record_field", I32, atom_lit()),
                          ("record_field", I32, atom_lit(), expr()),
                          ("typed_record_field", to!(ast::form::RecordFieldDecl), ty()))))
            .map(|result| match result {
                Union3::A((_, line, name)) => Self::new(line, name.value),
                Union3::B((_, line, name, value)) => {
                    Self::new(line, name.value).default_value(value)
                }
                Union3::C((_, field, type_)) => field.typ(type_),
            })
    }
}
impl<'a> FromTerm<'a> for literal::Atom {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("atom", I32, atom()))
            .map(|(_, line, name)| Self::new(line, name.to_string()))
    }
}
impl<'a> FromTerm<'a> for literal::Integer {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        // FIXME: Int => Uint
        term.as_match(("integer", I32, Int))
            .map(|(_, line, value)| Self::new(line, value))
    }
}
impl<'a> FromTerm<'a> for literal::Char {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("char", I32, Unicode))
            .map(|(_, line, ch)| Self::new(line, ch))
    }
}
impl<'a> FromTerm<'a> for literal::Float {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("float", I32, F64))
            .map(|(_, line, value)| Self::new(line, value))
    }
}
impl<'a> FromTerm<'a> for literal::Str {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("string", I32, Str(Unicode)))
            .map(|(_, line, value)| Self::new(line, value))
    }
}
impl<'a> FromTerm<'a> for common::Variable {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("var", I32, atom()))
            .map(|(_, line, name)| Self::new(line, name.to_string()))
    }
}
impl<'a> FromTerm<'a> for ast::form::TypeDecl {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("attribute", I32, Or(("opaque", "type")), (atom(), ty(), VarList(var()))))
            .map(|(_, line, kind, (name, type_, vars))| {
                Self::new(line, name.to_string(), vars, type_).opaque(kind.is_a())
            })
    }
}
impl<'a> FromTerm<'a> for ast::form::FunDecl {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("function", I32, atom(), U32, VarList(clause())))
            .map(|(_, line, name, _, clauses)| Self::new(line, name.to_string(), clauses))
    }
}
impl<'a> FromTerm<'a> for ast::form::FunSpec {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(Or((("attribute",
                           I32,
                           Or(("callback", "spec")),
                           ((atom(), U32), VarList(ftype()))),
                          ("attribute", I32, "spec", ((atom(), atom(), U32), VarList(ftype()))))))
            .map(|result| match result {
                Union2::A((_, line, is_callback, ((name, _), types))) => {
                    Self::new(line, name.to_string(), types).callback(is_callback.is_a())
                }
                Union2::B((_, line, _, ((module, name, _), types))) => {
                    Self::new(line, name.to_string(), types).module(module.to_string())
                }
            })
    }
}
impl<'a> FromTerm<'a> for ast::form::ExportAttr {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("attribute", I32, "export", VarList((atom(), U32))))
            .map(|(_, line, _, functions)| {
                Self::new(line,
                          functions.into_iter()
                              .map(|(f, a)| ast::form::Export::new(f.to_string(), a))
                              .collect())
            })
    }
}
impl<'a> FromTerm<'a> for ast::form::ImportAttr {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("attribute", I32, "import", (atom(), VarList((atom(), U32)))))
            .map(|(_, line, _, (module, functions))| {
                Self::new(line,
                          module.to_string(),
                          functions.into_iter()
                              .map(|(f, a)| ast::form::Import::new(f.to_string(), a))
                              .collect())
            })
    }
}
impl<'a> FromTerm<'a> for ast::form::ExportTypeAttr {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("attribute", I32, "export_type", VarList((atom(), U32))))
            .map(|(_, line, _, export_types)| {
                Self::new(line,
                          export_types.into_iter()
                              .map(|(t, a)| ast::form::ExportType::new(t.to_string(), a))
                              .collect())
            })
    }
}
impl<'a> FromTerm<'a> for ast::form::CompileOptionsAttr {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("attribute", I32, "compile", any()))
            .map(|(_, line, _, options)| Self::new(line, options.clone()))
    }
}
impl<'a> FromTerm<'a> for ast::form::WildAttr {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("attribute", I32, atom(), any()))
            .map(|(_, line, name, value)| Self::new(line, name.to_string(), value.clone()))
    }
}
impl<'a> FromTerm<'a> for ast::form::Eof {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("eof", I32)).map(|(_, line)| Self::new(line))
    }
}
