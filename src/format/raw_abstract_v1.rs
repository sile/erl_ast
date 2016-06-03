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

pub fn atom() -> eetf::pattern::Any<eetf::Atom> {
    eetf::pattern::any()
}
pub fn any() -> eetf::pattern::Any<eetf::Term> {
    eetf::pattern::any()
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
impl<'a> FromTerm<'a> for ast::Expression {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        let e = return_if_ok!(term.as_match(to!(ast::IntegerLit)));
        let e = return_if_ok!(term.as_match(to!(ast::FloatLit))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::StringLit))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::CharLit))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::AtomLit))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::Match<_, _>))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::Variable))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::Tuple<_>))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::Nil))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::Cons<_>))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::Binary<_>))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::UnaryOp<_>))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::BinaryOp<_>))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::Record<_>))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::RecordIndex<_>))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::Map<_>))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::Catch))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::LocalCall<_>))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::RemoteCall<_>))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::Comprehension))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::Block))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::If))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::Case))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::Try))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::Receive))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::InternalFun))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::ExternalFun))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::AnonymousFun))).max_depth(e);
        Err(e)
    }
}
impl<'a> FromTerm<'a> for ast::Catch {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("catch", I32, to!(ast::Expression)))
            .map(|(_, line, expr)| Self::new(line, expr))
    }
}
impl<'a> FromTerm<'a> for ast::Receive {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(Or((("receive", I32, VarList(to!(ast::Clause))),
                          ("receive",
                           I32,
                           VarList(to!(ast::Clause)),
                           to!(ast::Expression),
                           VarList(to!(ast::Expression))))))
            .map(|result| {
                match result {
                    Union2::A((_, line, clauses)) => Self::new(line, clauses),
                    Union2::B((_, line, clauses, timeout, after)) => {
                        Self::new(line, clauses).timeout(timeout).after(after)
                    }
                }
            })
    }
}
impl<'a> FromTerm<'a> for ast::InternalFun {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("fun", I32, ("function", atom(), U32)))
            .map(|(_, line, (_, name, arity))| Self::new(line, name.to_string(), arity))
    }
}
impl<'a> FromTerm<'a> for ast::ExternalFun {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("fun",
                       I32,
                       ("function",
                        to!(ast::Expression),
                        to!(ast::Expression),
                        to!(ast::Expression))))
            .map(|(_, line, (_, module, function, arity))| Self::new(line, module, function, arity))
    }
}
impl<'a> FromTerm<'a> for ast::AnonymousFun {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(Or((("fun", I32, ("clauses", VarList(to!(ast::Clause)))),
                          ("named_fun", I32, atom(), VarList(to!(ast::Clause))))))
            .map(|result| {
                match result {
                    Union2::A((_, line, (_, clauses))) => Self::new(line, clauses),
                    Union2::B((_, line, name, clauses)) => {
                        Self::new(line, clauses).name(name.to_string())
                    }
                }
            })
    }
}
impl<'a> FromTerm<'a> for ast::Block {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("block", I32, VarList(to!(ast::Expression))))
            .map(|(_, line, body)| Self::new(line, body))
    }
}
impl<'a> FromTerm<'a> for ast::If {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("if", I32, VarList(to!(ast::Clause))))
            .map(|(_, line, clauses)| Self::new(line, clauses))
    }
}
impl<'a> FromTerm<'a> for ast::Case {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("case", I32, to!(ast::Expression), VarList(to!(ast::Clause))))
            .map(|(_, line, expr, clauses)| Self::new(line, expr, clauses))
    }
}
impl<'a> FromTerm<'a> for ast::Try {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("try",
                       I32,
                       VarList(to!(ast::Expression)),
                       VarList(to!(ast::Clause)),
                       VarList(to!(ast::Clause)),
                       VarList(to!(ast::Expression))))
            .map(|(_, line, body, case_clauses, catch_clauses, after)| {
                Self::new(line, body, case_clauses, catch_clauses, after)
            })
    }
}
impl<'a> FromTerm<'a> for ast::Comprehension {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match((Or(("lc", "bc")), I32, to!(ast::Expression), VarList(to!(ast::Qualifier))))
            .map(|(tag, line, expr, qualifiers)| {
                let is_list = tag.is_a();
                Self::new(line, is_list, expr, qualifiers)
            })
    }
}
impl<'a> FromTerm<'a> for ast::Qualifier {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        use ast::expr::Qualifier::*;
        term.as_match(Or((("generate", I32, to!(ast::Pattern), to!(ast::Expression)),
                          ("b_generate", I32, to!(ast::Pattern), to!(ast::Expression)),
                          to!(ast::Expression))))
            .map(|result| {
                match result {
                    Union3::A((_, line, pattern, expr)) => {
                        Generator(ast::expr::Generator::new(line, pattern, expr))
                    }
                    Union3::B((_, line, pattern, expr)) => {
                        BitStringGenerator(ast::expr::Generator::new(line, pattern, expr))
                    }
                    Union3::C(expr) => Filter(expr),
                }
            })
    }
}
impl<'a> FromTerm<'a> for ast::Clause {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("clause",
                       I32,
                       VarList(to!(ast::Pattern)),
                       VarList(to!(ast::OrGuard)),
                       VarList(to!(ast::Expression))))
            .map(|(_, line, patterns, guards, body)| Self::new(line, patterns, guards, body))
    }
}
impl<'a> FromTerm<'a> for ast::OrGuard {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(VarList(to!(ast::Guard))).map(|guards| Self::new(guards))
    }
}
impl<'a, T: FromTerm<'a> + Debug + 'static> FromTerm<'a> for ast::Tuple<T> {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("tuple", I32, VarList(to!(T))))
            .map(|(_, line, elements)| Self::new(line, elements))
    }
}
impl<'a, T: FromTerm<'a> + Debug + 'static> FromTerm<'a> for ast::Cons<T> {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("cons", I32, to!(T), to!(T)))
            .map(|(_, line, head, tail)| Self::new(line, head, tail))
    }
}
impl<'a, T: FromTerm<'a> + Debug + 'static> FromTerm<'a> for ast::Binary<T> {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("bin", I32, VarList(to!(ast::BinElement<T>))))
            .map(|(_, line, elements)| Self::new(line, elements))
    }
}
impl<'a, T: FromTerm<'a> + Debug + 'static> FromTerm<'a> for ast::BinElement<T> {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("bin_element",
                       I32,
                       to!(T),
                       Or((to!(T), "default")),
                       Or((VarList(to!(ast::BinElementTypeSpec)), "default"))))
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
impl<'a> FromTerm<'a> for ast::BinElementTypeSpec {
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
impl<'a, T: FromTerm<'a> + Debug + 'static> FromTerm<'a> for ast::Record<T> {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(Or((("record", I32, atom(), VarList(to!(ast::RecordField<T>))),
                          ("record",
                           I32,
                           to!(ast::Expression),
                           atom(),
                           VarList(to!(ast::RecordField<T>))))))
            .map(|result| {
                match result {
                    Union2::A((_, line, name, fields)) => Self::new(line, name.to_string(), fields),
                    Union2::B((_, line, base, name, fields)) => {
                        Self::new(line, name.to_string(), fields).base(base)
                    }
                }
            })
    }
}
impl<'a, T: FromTerm<'a> + Debug + 'static> FromTerm<'a> for ast::RecordField<T> {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("record_field", I32, Or((to!(ast::AtomLit), ("var", I32, "_"))), to!(T)))
            .map(|(_, line, name, value)| {
                let name = name.into_result().ok().map(|n| n.value);
                Self::new(line, name, value)
            })
    }
}
impl<'a, T: FromTerm<'a> + Debug + 'static> FromTerm<'a> for ast::Map<T> {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(Or((("map", I32, VarList(to!(ast::MapPair<T>))),
                          ("map", I32, to!(ast::Expression), VarList(to!(ast::MapPair<T>))))))
            .map(|result| {
                match result {
                    Union2::A((_, line, pairs)) => Self::new(line, pairs),
                    Union2::B((_, line, base, pairs)) => Self::new(line, pairs).base(base),
                }
            })
    }
}
impl<'a, T: FromTerm<'a> + Debug + 'static> FromTerm<'a> for ast::MapPair<T> {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match((Or(("map_field_assoc", "map_field_exact")), I32, to!(T), to!(T)))
            .map(|(tag, line, key, value)| {
                let is_assoc = tag.is_a();
                Self::new(line, is_assoc, key, value)
            })
    }
}
impl<'a, T: FromTerm<'a> + Debug + 'static> FromTerm<'a> for ast::LocalCall<T> {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("call", I32, to!(T), VarList(to!(T))))
            .map(|(_, line, function, args)| Self::new(line, function, args))
    }
}
impl<'a, T: FromTerm<'a> + Debug + 'static> FromTerm<'a> for ast::RemoteCall<T> {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("call", I32, ("remote", U32, to!(T), to!(T)), VarList(to!(T))))
            .map(|(_, line, (_, _, module, function), args)| {
                Self::new(line, module, function, args)
            })
    }
}
impl<'a, T: FromTerm<'a> + Debug + 'static> FromTerm<'a> for ast::RecordIndex<T> {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(Or((("record_index", I32, atom(), to!(ast::AtomLit)),
                          ("record_field", I32, to!(T), atom(), to!(ast::AtomLit)))))
            .map(|result| {
                match result {
                    Union2::A((_, line, name, field)) => {
                        Self::new(line, name.to_string(), field.value)
                    }
                    Union2::B((_, line, base, name, field)) => {
                        Self::new(line, name.to_string(), field.value).base(base)
                    }
                }
            })
    }
}
impl<'a, T: FromTerm<'a> + Debug + 'static> FromTerm<'a> for ast::BinaryOp<T> {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("op", I32, atom(), to!(T), to!(T)))
            .map(|(_, line, op, left, right)| Self::new(line, op.to_string(), left, right))
    }
}
impl<'a, T: FromTerm<'a> + Debug + 'static> FromTerm<'a> for ast::UnaryOp<T> {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("op", I32, atom(), to!(T)))
            .map(|(_, line, op, arg)| Self::new(line, op.to_string(), arg))
    }
}
impl<'a> FromTerm<'a> for ast::Nil {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("nil", I32)).map(|(_, line)| Self::new(line))
    }
}
impl<'a, L, R> FromTerm<'a> for ast::Match<L, R>
    where L: FromTerm<'a> + Debug + 'static,
          R: FromTerm<'a> + Debug + 'static
{
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("match", I32, to!(L), to!(R)))
            .map(|(_, line, left, right)| Self::new(line, left, right))
    }
}
impl<'a> FromTerm<'a> for ast::Type {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        let e = return_if_ok!(term.as_match(to!(ast::IntegerLit)));
        let e = return_if_ok!(term.as_match(to!(ast::AtomLit))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::Variable))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::UnaryOp<_>))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::BinaryOp<_>))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::Nil))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::AnnotatedType))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::BitStringType))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::AnyFunType))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::FunctionType))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::RangeType))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::MapType))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::RecordType))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::RemoteType))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::AnyTupleType))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::TupleType))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::UnionType))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::BuiltInType))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::UserType))).max_depth(e);
        Err(e)
    }
}
impl<'a> FromTerm<'a> for ast::UserType {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("user_type", I32, atom(), VarList(to!(ast::Type))))
            .map(|(_, line, name, args)| Self::new(line, name.to_string(), args))
    }
}
impl<'a> FromTerm<'a> for ast::UnionType {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("type", I32, "union", VarList(to!(ast::Type))))
            .map(|(_, line, _, types)| Self::new(line, types))
    }
}
impl<'a> FromTerm<'a> for ast::TupleType {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("type", I32, "tuple", VarList(to!(ast::Type))))
            .map(|(_, line, _, types)| Self::new(line, types))
    }
}
impl<'a> FromTerm<'a> for ast::AnyTupleType {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("type", I32, "tuple", "any"))
            .map(|(_, line, _, _)| Self::new(line))
    }
}
impl<'a> FromTerm<'a> for ast::RemoteType {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("remote_type",
                       I32,
                       FixList((to!(ast::AtomLit), to!(ast::AtomLit), VarList(to!(ast::Type))))))
            .map(|(_, line, (module, function, args))| {
                Self::new(line, module.value, function.value, args)
            })
    }
}
impl<'a> FromTerm<'a> for ast::RecordType {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("type", I32, "record", Cons(to!(ast::AtomLit), to!(ast::RecordFieldType))))
            .map(|(_, line, _, (name, fields))| Self::new(line, name.value, fields))
    }
}
impl<'a> FromTerm<'a> for ast::RecordFieldType {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("type", I32, "field_type", FixList((to!(ast::AtomLit), to!(ast::Type)))))
            .map(|(_, line, _, (name, type_))| Self::new(line, name.value, type_))
    }
}
impl<'a> FromTerm<'a> for ast::BuiltInType {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("type", I32, atom(), Or(("any", VarList(to!(ast::Type))))))
            .map(|(_, line, name, args)| {
                match args {
                    Union2::A(_) => Self::new(line, name.to_string(), Vec::new()),
                    Union2::B(args) => Self::new(line, name.to_string(), args),
                }
            })
    }
}
impl<'a> FromTerm<'a> for ast::MapType {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("type", I32, "map", VarList(to!(ast::MapPairType))))
            .map(|(_, line, _, pairs)| Self::new(line, pairs))
    }
}
impl<'a> FromTerm<'a> for ast::MapPairType {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("type", I32, "map_field_assoc", FixList((to!(ast::Type), to!(ast::Type)))))
            .map(|(_, line, _, (key, value))| Self::new(line, key, value))
    }
}
impl<'a> FromTerm<'a> for ast::RangeType {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("type", I32, "range", FixList((to!(ast::Type), to!(ast::Type)))))
            .map(|(_, line, _, (low, high))| Self::new(line, low, high))
    }
}
impl<'a> FromTerm<'a> for ast::FunctionType {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(Or((
                ("type",
                 I32,
                 "bounded_fun",
                 FixList((to!(ast::FunctionType), VarList(to!(ast::FunctionConstraint))))),
                ("type",
                 I32,
                 "fun",
                 FixList((("type", I32, "product", VarList(to!(ast::Type))),
                         to!(ast::Type)))))))
            .map(|result| {
                match result {
                    Union2::A((_, _, _, (fun, constraints))) => fun.constraints(constraints),
                    Union2::B((_, line, _, ((_, _, _, args), return_type))) => {
                        Self::new(line, args, return_type)}
                }
            })
    }
}
impl<'a> FromTerm<'a> for ast::FunctionConstraint {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("type",
                       I32,
                       "constraint",
                       FixList((("atom", I32, "is_subtype"),
                                FixList((to!(ast::Variable), to!(ast::Type)))))))
            .map(|(_, line, _, (_, (var, subtype)))| Self::new(line, var, subtype))
    }
}
impl<'a> FromTerm<'a> for ast::AnyFunType {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("type",
                       I32,
                       "fun",
                       Or((Nil, FixList((("type", I32, "any"), to!(ast::Type)))))))
            .map(|(_, line, _, fun)| {
                match fun {
                    Union2::A(_) => Self::new(line),
                    Union2::B((_, type_)) => Self::new(line).return_type(type_),
                }
            })
    }
}
impl<'a> FromTerm<'a> for ast::AnnotatedType {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("ann_type", I32, FixList((to!(ast::Variable), to!(ast::Type)))))
            .map(|(_, line, (var, type_))| Self::new(line, var, type_))
    }
}
impl<'a> FromTerm<'a> for ast::BitStringType {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("type",
                       I32,
                       "binary",
                       FixList((to!(ast::IntegerLit), to!(ast::IntegerLit)))))
            .map(|(_, line, _, (bytes, bits))| {
                Self::new(line, bytes.to_u64().unwrap(), bits.to_u64().unwrap())
            })
    }
}
impl<'a> FromTerm<'a> for ast::Pattern {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        let e = return_if_ok!(term.as_match(to!(ast::IntegerLit)));
        let e = return_if_ok!(term.as_match(to!(ast::FloatLit))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::StringLit))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::CharLit))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::AtomLit))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::Match<_, _>))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::Variable))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::Tuple<_>))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::Nil))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::Cons<_>))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::Binary<_>))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::UnaryOp<_>))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::BinaryOp<_>))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::Record<_>))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::RecordIndex<_>))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::Map<_>))).max_depth(e);
        Err(e)
    }
}
impl<'a> FromTerm<'a> for ast::Guard {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        let e = return_if_ok!(term.as_match(to!(ast::IntegerLit)));
        let e = return_if_ok!(term.as_match(to!(ast::FloatLit))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::StringLit))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::CharLit))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::AtomLit))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::Variable))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::Tuple<_>))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::Nil))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::Cons<_>))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::Binary<_>))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::UnaryOp<_>))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::BinaryOp<_>))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::Record<_>))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::RecordIndex<_>))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::LocalCall<_>))).max_depth(e);
        let e = return_if_ok!(term.as_match(to!(ast::RemoteCall<_>))).max_depth(e);
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
            .map(|(_, line, spell, name)| {
                let is_british = spell.is_a();
                Self::new(line, name.to_string()).british(is_british)
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
        term.as_match(Or((("record_field", I32, to!(ast::AtomLit)),
                          ("record_field", I32, to!(ast::AtomLit), to!(ast::Expression)),
                          ("typed_record_field", to!(ast::form::RecordFieldDecl), to!(ast::Type)))))
            .map(|result| {
                match result {
                    Union3::A((_, line, name)) => Self::new(line, name.value),
                    Union3::B((_, line, name, value)) => {
                        Self::new(line, name.value).default_value(value)
                    }
                    Union3::C((_, field, type_)) => field.typ(type_),
                }
            })
    }
}
impl<'a> FromTerm<'a> for ast::AtomLit {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("atom", I32, atom()))
            .map(|(_, line, name)| Self::new(line, name.to_string()))
    }
}
impl<'a> FromTerm<'a> for ast::IntegerLit {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        // FIXME: Int => Uint
        term.as_match(("integer", I32, Int))
            .map(|(_, line, value)| Self::new(line, value))
    }
}
impl<'a> FromTerm<'a> for ast::CharLit {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("char", I32, Unicode))
            .map(|(_, line, ch)| Self::new(line, ch))
    }
}
impl<'a> FromTerm<'a> for ast::FloatLit {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("float", I32, F64))
            .map(|(_, line, value)| Self::new(line, value))
    }
}
impl<'a> FromTerm<'a> for ast::StringLit {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("string", I32, Str(Unicode)))
            .map(|(_, line, value)| Self::new(line, value))
    }
}
impl<'a> FromTerm<'a> for ast::Variable {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("var", I32, atom()))
            .map(|(_, line, name)| Self::new(line, name.to_string()))
    }
}
impl<'a> FromTerm<'a> for ast::form::TypeDecl {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("attribute",
                       I32,
                       Or(("opaque", "type")),
                       (atom(), to!(ast::Type), VarList(to!(ast::Variable)))))
            .map(|(_, line, kind, (name, type_, vars))| {
                Self::new(line, name.to_string(), vars, type_).opaque(kind.is_a())
            })
    }
}
impl<'a> FromTerm<'a> for ast::form::FunDecl {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(("function", I32, atom(), U32, VarList(to!(ast::Clause))))
            .map(|(_, line, name, _, clauses)| Self::new(line, name.to_string(), clauses))
    }
}
impl<'a> FromTerm<'a> for ast::form::FunSpec {
    fn try_from(term: &'a eetf::Term) -> Result<Self, Unmatch<'a>> {
        term.as_match(Or((("attribute",
                           I32,
                           Or(("callback", "spec")),
                           ((atom(), U32), VarList(to!(ast::FunctionType)))),
                          ("attribute",
                           I32,
                           "spec",
                           ((atom(), atom(), U32), VarList(to!(ast::FunctionType)))))))
            .map(|result| match result {
                Union2::A((_, line, tag, ((name, _), types))) => {
                    let is_callback = tag.is_a();
                    Self::new(line, name.to_string(), types).callback(is_callback)
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
