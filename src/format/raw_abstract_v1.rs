use std::path::Path;
use std::io;
use beam_file;
use beam_file::chunk::Chunk;
use eetf;
use result::BeamParseResult;
use error::BeamParseError;
use ast;
use ast::matcher::{Pattern, Either};
use ast::matcher::{U32, I32, U64, F64, Atom, List, List2, List3, Str, Term, Nil, Int};

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
        let (_, forms) = try!(("raw_abstract_v1", List(to::<ast::form::Form>()))
            .try_match(&self.code)
            .map_err(|term| BeamParseError::UnexpectedTerm(term.unwrap().clone())));
        Ok(ast::form::ModuleDecl { forms: forms })
    }
}

pub trait FromTerm {
    fn try_from(term: &eetf::Term) -> Result<Self, Option<&eetf::Term>>
        where Self: Sized
    {
        Self::from(term).ok_or(None)
    }
    fn from(term: &eetf::Term) -> Option<Self>
        where Self: Sized
    {
        Self::try_from(term).ok()
    }
}

use std::marker::PhantomData;
struct To<T>(PhantomData<T>);
fn to<T>() -> To<T> {
    To(PhantomData)
}
impl<'a, F> Pattern<'a> for To<F>
    where F: FromTerm
{
    type Value = F;
    fn try_match(&self, term: &'a eetf::Term) -> Result<Self::Value, Option<&'a eetf::Term>> {
        F::try_from(term)
    }
}
macro_rules! to{
    ($to:ty) => {
        to::<$to>()
    }
}

macro_rules! try_from {
    ($term:expr, $from:ty) => {
            match to!($from).try_match($term).map(::std::convert::From::from) {
                Ok(value) => return Ok(value),
                Err(None) => {},
                Err(Some(unmatched)) => return Err(Some(unmatched))
            }
        }
}

impl FromTerm for ast::form::Form {
    fn try_from(term: &eetf::Term) -> Result<Self, Option<&eetf::Term>> {
        try_from!(term, ast::form::ModuleAttr);
        try_from!(term, ast::form::FileAttr);
        try_from!(term, ast::form::BehaviourAttr);
        try_from!(term, ast::form::ExportAttr);
        try_from!(term, ast::form::ImportAttr);
        try_from!(term, ast::form::ExportTypeAttr);
        try_from!(term, ast::form::CompileOptionsAttr);
        try_from!(term, ast::form::RecordDecl);
        try_from!(term, ast::form::TypeDecl);
        try_from!(term, ast::form::FunSpec);
        try_from!(term, ast::form::FunDecl);
        try_from!(term, ast::form::WildAttr);
        try_from!(term, ast::form::Eof);
        Err(Some(term))
    }
}
impl FromTerm for ast::Expression {
    fn try_from(term: &eetf::Term) -> Result<Self, Option<&eetf::Term>> {
        try_from!(term, ast::IntegerLit);
        try_from!(term, ast::FloatLit);
        try_from!(term, ast::StringLit);
        try_from!(term, ast::CharLit);
        try_from!(term, ast::AtomLit);
        try_from!(term, ast::Match<_, _>);
        try_from!(term, ast::Variable);
        try_from!(term, ast::Tuple<_>);
        try_from!(term, ast::Nil);
        try_from!(term, ast::Cons<_>);
        try_from!(term, ast::Binary<_>);
        try_from!(term, ast::UnaryOp<_>);
        try_from!(term, ast::BinaryOp<_>);
        try_from!(term, ast::Record<_>);
        try_from!(term, ast::RecordIndex<_>);
        try_from!(term, ast::Map<_>);
        try_from!(term, ast::Catch);
        try_from!(term, ast::LocalCall<_>);
        try_from!(term, ast::RemoteCall<_>);
        try_from!(term, ast::Comprehension);
        try_from!(term, ast::Block);
        try_from!(term, ast::If);
        try_from!(term, ast::Case);
        try_from!(term, ast::Try);
        try_from!(term, ast::Receive);
        try_from!(term, ast::InternalFun);
        try_from!(term, ast::ExternalFun);
        try_from!(term, ast::AnonymousFun);
        Err(Some(term))
    }
}
impl FromTerm for ast::Catch {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("catch", I32, to!(ast::Expression))
            .map_match(term, |(_, line, expr)| Self::new(line, expr))
    }
}
impl FromTerm for ast::Receive {
    fn try_from(term: &eetf::Term) -> Result<Self, Option<&eetf::Term>> {
        Either(("receive", I32, List(to!(ast::Clause))),
               ("receive",
                I32,
                List(to!(ast::Clause)),
                to!(ast::Expression),
                List(to!(ast::Expression))))
            .try_map_match(term, |result| {
                match result {
                    Ok((_, line, clauses)) => Self::new(line, clauses),
                    Err((_, line, clauses, timeout, after)) => {
                        Self::new(line, clauses).timeout(timeout).after(after)
                    }
                }
            })
    }
}
impl FromTerm for ast::InternalFun {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("fun", I32, ("function", Atom, U32)).map_match(term, |(_, line, (_, name, arity))| {
            Self::new(line, name.to_string(), arity)
        })
    }
}
impl FromTerm for ast::ExternalFun {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("fun", I32, ("function", to!(ast::Expression), to!(ast::Expression), to!(ast::Expression)))
            .map_match(term, |(_, line, (_, module, function, arity))| {
                Self::new(line, module, function, arity)
            })
    }
}
impl FromTerm for ast::AnonymousFun {
    fn try_from(term: &eetf::Term) -> Result<Self, Option<&eetf::Term>> {
        Either(("fun", I32, ("clauses", List(to!(ast::Clause)))),
               ("named_fun", I32, Atom, List(to!(ast::Clause))))
            .try_map_match(term, |result| {
                match result {
                    Ok((_, line, (_, clauses))) => Self::new(line, clauses),
                    Err((_, line, name, clauses)) => {
                        Self::new(line, clauses).name(name.to_string())
                    }
                }
            })
    }
}
impl FromTerm for ast::Block {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("block", I32, List(to!(ast::Expression)))
            .map_match(term, |(_, line, body)| Self::new(line, body))
    }
}
impl FromTerm for ast::If {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("if", I32, List(to!(ast::Clause)))
            .map_match(term, |(_, line, clauses)| Self::new(line, clauses))
    }
}
impl FromTerm for ast::Case {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("case", I32, to!(ast::Expression), List(to!(ast::Clause))).map_match(term, |(_,
                                                                                line,
                                                                                expr,
                                                                                clauses)| {
            Self::new(line, expr, clauses)
        })
    }
}
impl FromTerm for ast::Try {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("try",
         I32,
         List(to!(ast::Expression)),
         List(to!(ast::Clause)),
         List(to!(ast::Clause)),
         List(to!(ast::Expression)))
            .map_match(term,
                       |(_, line, body, case_clauses, catch_clauses, after)| {
                           Self::new(line, body, case_clauses, catch_clauses, after)
                       })
    }
}
impl FromTerm for ast::Comprehension {
    fn from(term: &eetf::Term) -> Option<Self> {
        (Either("lc", "bc"), I32, to!(ast::Expression), List(to!(ast::Qualifier)))
            .map_match(term, |(is_list, line, expr, qualifiers)| {
                Self::new(line, is_list.is_ok(), expr, qualifiers)
            })
    }
}
impl FromTerm for ast::Qualifier {
    fn from(term: &eetf::Term) -> Option<Self> {
        use ast::expr::Qualifier::*;
        None.or_else(|| {
                ("generate", I32, to!(ast::Pattern), to!(ast::Expression))
                    .map_match(term, |(_, line, pattern, expr)| {
                        Generator(ast::expr::Generator::new(line, pattern, expr))
                    })
            })
            .or_else(|| {
                ("b_generate", I32, to!(ast::Pattern), to!(ast::Expression))
                    .map_match(term, |(_, line, pattern, expr)| {
                        BitStringGenerator(ast::expr::Generator::new(line, pattern, expr))
                    })
            })
            .or_else(|| to!(ast::Expression).map_match(term, |expr| Filter(expr)))
    }
}
impl FromTerm for ast::Clause {
    fn try_from(term: &eetf::Term) -> Result<Self, Option<&eetf::Term>> {
        ("clause",
         I32,
         List(to!(ast::Pattern)),
         List(to!(ast::OrGuard)),
         List(to!(ast::Expression)))
            .try_map_match(term, |(_, line, patterns, guards, body)| {
                Self::new(line, patterns, guards, body)
            })
    }
}
impl FromTerm for ast::OrGuard {
    fn from(term: &eetf::Term) -> Option<Self> {
        List(to!(ast::Guard)).map_match(term, |guards| Self::new(guards))
    }
}
impl<T: FromTerm> FromTerm for ast::Tuple<T> {
    fn try_from(term: &eetf::Term) -> Result<Self, Option<&eetf::Term>> {
        ("tuple", I32, List(to!(T)))
            .try_map_match(term, |(_, line, elements)| Self::new(line, elements))
    }
}
impl<T: FromTerm> FromTerm for ast::Cons<T> {
    fn try_from(term: &eetf::Term) -> Result<Self, Option<&eetf::Term>> {
        ("cons", I32, to!(T), to!(T))
            .try_map_match(term, |(_, line, head, tail)| Self::new(line, head, tail))
    }
}
impl<T: FromTerm> FromTerm for ast::Binary<T> {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("bin", I32, List(to!(ast::BinElement<T>)))
            .map_match(term, |(_, line, elements)| Self::new(line, elements))
    }
}
impl<T: FromTerm> FromTerm for ast::BinElement<T> {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("bin_element",
         I32,
         to!(T),
         Either(to!(T), "default"),
         Either(List(to!(ast::BinElementTypeSpec)), "default"))
            .map_match(term, |(_, line, value, size, tsl)| {
                let mut e = Self::new(line, value);
                if let Ok(size) = size {
                    e = e.size(size);
                }
                if let Ok(tsl) = tsl {
                    e = e.tsl(tsl);
                }
                e
            })
    }
}
impl FromTerm for ast::BinElementTypeSpec {
    fn from(term: &eetf::Term) -> Option<Self> {
        Either(Atom, (Atom, U64)).map_match(term, |ts| {
            match ts {
                Ok(name) => Self::new(name.to_string(), None),
                Err((name, value)) => Self::new(name.to_string(), Some(value)),
            }
        })
    }
}
impl<T: FromTerm> FromTerm for ast::Record<T> {
    fn from(term: &eetf::Term) -> Option<Self> {
        None.or_else(|| {
                ("record", I32, Atom, List(to!(ast::RecordField<T>))).map_match(term, |(_,
                                                                                  line,
                                                                                  name,
                                                                                  fields)| {
                    Self::new(line, name.to_string(), fields)
                })
            })
            .or_else(|| {
                ("record", I32, to!(ast::Expression), Atom, List(to!(ast::RecordField<T>)))
                    .map_match(term, |(_, line, base, name, fields)| {
                        Self::new(line, name.to_string(), fields).base(base)
                    })
            })
    }
}
impl<T: FromTerm> FromTerm for ast::RecordField<T> {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("record_field", I32, Either(to!(ast::AtomLit), ("var", I32, "_")), to!(T))
            .map_match(term,
                       |(_, line, name, value)| Self::new(line, name.ok().map(|x| x.value), value))
    }
}
impl<T: FromTerm> FromTerm for ast::Map<T> {
    fn from(term: &eetf::Term) -> Option<Self> {
        None.or_else(|| {
                ("map", I32, List(to!(ast::MapPair<T>)))
                    .map_match(term, |(_, line, pairs)| Self::new(line, pairs))
            })
            .or_else(|| {
                ("map", I32, to!(ast::Expression), List(to!(ast::MapPair<T>))).map_match(term, |(_,
                                                                                           line,
                                                                                           base,
                                                                                           pairs)| {
                    Self::new(line, pairs).base(base)
                })
            })
    }
}
impl<T: FromTerm> FromTerm for ast::MapPair<T> {
    fn from(term: &eetf::Term) -> Option<Self> {
        (Either("map_field_assoc", "map_field_exact"), I32, to!(T), to!(T))
            .map_match(term, |(is_assoc, line, key, value)| {
                Self::new(line, is_assoc.is_ok(), key, value)
            })
    }
}
impl<T: FromTerm> FromTerm for ast::LocalCall<T> {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("call", I32, to!(T), List(to!(T))).map_match(term, |(_, line, function, args)| {
            Self::new(line, function, args)
        })
    }
}
impl<T: FromTerm> FromTerm for ast::RemoteCall<T> {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("call", I32, ("remote", U32, to!(T), to!(T)), List(to!(T)))
            .map_match(term, |(_, line, (_, _, module, function), args)| {
                Self::new(line, module, function, args)
            })
    }
}
impl<T: FromTerm> FromTerm for ast::RecordIndex<T> {
    fn from(term: &eetf::Term) -> Option<Self> {
        None.or_else(|| {
                ("record_index", I32, Atom, to!(ast::AtomLit))
                    .map_match(term, |(_, line, name, field)| {
                        Self::new(line, name.to_string(), field.value)
                    })
            })
            .or_else(|| {
                ("record_field", I32, to!(T), Atom, to!(ast::AtomLit))
                    .map_match(term, |(_, line, base, name, field)| {
                        Self::new(line, name.to_string(), field.value).base(base)
                    })
            })
    }
}
impl<T: FromTerm> FromTerm for ast::BinaryOp<T> {
    fn try_from(term: &eetf::Term) -> Result<Self, Option<&eetf::Term>> {
        ("op", I32, Atom, to!(T), to!(T)).try_map_match(term, |(_, line, op, left, right)| {
            Self::new(line, op.to_string(), left, right)
        })
    }
}
impl<T: FromTerm> FromTerm for ast::UnaryOp<T> {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("op", I32, Atom, to!(T)).map_match(term, |(_, line, op, arg)| {
            Self::new(line, op.to_string(), arg)
        })
    }
}
impl FromTerm for ast::Nil {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("nil", I32).map_match(term, |(_, line)| Self::new(line))
    }
}
impl<L, R> FromTerm for ast::Match<L, R>
    where L: FromTerm,
          R: FromTerm
{
    fn try_from(term: &eetf::Term) -> Result<Self, Option<&eetf::Term>> {
        ("match", I32, to!(L), to!(R))
            .try_map_match(term, |(_, line, left, right)| Self::new(line, left, right))
    }
}
impl FromTerm for ast::Type {
    fn try_from(term: &eetf::Term) -> Result<Self, Option<&eetf::Term>> {
        try_from!(term, ast::AtomLit);
        try_from!(term, ast::IntegerLit);
        try_from!(term, ast::Variable);
        try_from!(term, ast::AnnotatedType);
        try_from!(term, ast::UnaryOp<_>);
        try_from!(term, ast::BinaryOp<_>);
        try_from!(term, ast::BitStringType);
        try_from!(term, ast::Nil);
        try_from!(term, ast::AnyFunType);
        try_from!(term, ast::FunctionType);
        try_from!(term, ast::RangeType);
        try_from!(term, ast::MapType);
        try_from!(term, ast::RecordType);
        try_from!(term, ast::RemoteType);
        try_from!(term, ast::AnyTupleType);
        try_from!(term, ast::TupleType);
        try_from!(term, ast::UnionType);
        try_from!(term, ast::BuiltInType);
        try_from!(term, ast::UserType);
        Err(Some(term))
    }
}
impl FromTerm for ast::UserType {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("user_type", I32, Atom, List(to!(ast::Type))).map_match(term, |(_, line, name, args)| {
            Self::new(line, name.to_string(), args)
        })
    }
}
impl FromTerm for ast::UnionType {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("type", I32, "union", List(to!(ast::Type)))
            .map_match(term, |(_, line, _, types)| Self::new(line, types))
    }
}
impl FromTerm for ast::TupleType {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("type", I32, "tuple", List(to!(ast::Type)))
            .map_match(term, |(_, line, _, types)| Self::new(line, types))
    }
}
impl FromTerm for ast::AnyTupleType {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("type", I32, "tuple", "any").map_match(term, |(_, line, _, _)| Self::new(line))
    }
}
impl FromTerm for ast::RemoteType {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("remote_type", I32, List3(to!(ast::AtomLit), to!(ast::AtomLit), List(to!(ast::Type))))
            .map_match(term, |(_, line, (module, function, args))| {
                Self::new(line, module.value, function.value, args)
            })
    }
}
impl FromTerm for ast::RecordType {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("type", I32, "record", List(Either(to!(ast::AtomLit), to!(ast::RecordFieldType))))
            .map_match(term, |(_, line, _, mut list)| {
                assert!(!list.is_empty()); // XXX:
                let name = list.remove(0).unwrap();
                let mut fields = Vec::with_capacity(list.len());
                for x in list {
                    if let Err(x) = x {
                        fields.push(x);
                    } else {
                        unreachable!(); // XXX
                    }
                }
                Self::new(line, name.value, fields)
            })
    }
}
impl FromTerm for ast::RecordFieldType {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("type", I32, "field_type", List2(to!(ast::AtomLit), to!(ast::Type)))
            .map_match(term,
                       |(_, line, _, (name, type_))| Self::new(line, name.value, type_))
    }
}
impl FromTerm for ast::BuiltInType {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("type", I32, Atom, Either("any", List(to!(ast::Type))))
            .map_match(term, |(_, line, name, args)| {
                match args {
                    Ok(_) => Self::new(line, name.to_string(), Vec::new()),
                    Err(args) => Self::new(line, name.to_string(), args),
                }
            })
    }
}
impl FromTerm for ast::MapType {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("type", I32, "map", List(to!(ast::MapPairType)))
            .map_match(term, |(_, line, _, pairs)| Self::new(line, pairs))
    }
}
impl FromTerm for ast::MapPairType {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("type", I32, "map_field_assoc", List2(to!(ast::Type), to!(ast::Type)))
            .map_match(term,
                       |(_, line, _, (key, value))| Self::new(line, key, value))
    }
}
impl FromTerm for ast::RangeType {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("type", I32, "range", List2(to!(ast::Type), to!(ast::Type)))
            .map_match(term, |(_, line, _, (low, high))| Self::new(line, low, high))
    }
}
impl FromTerm for ast::FunctionType {
    fn from(term: &eetf::Term) -> Option<Self> {
        None.or_else(|| {
                ("type",
                 I32,
                 "bounded_fun",
                 List2(to!(ast::FunctionType), List(to!(ast::FunctionConstraint))))
                    .map_match(term,
                               |(_, _, _, (fun, constraints))| fun.constraints(constraints))
            })
            .or_else(|| {
                ("type",
                 I32,
                 "fun",
                 List2(("type", I32, "product", List(to!(ast::Type))),
                       to!(ast::Type)))
                    .map_match(term, |(_, line, _, ((_, _, _, args), return_type))| {
                        Self::new(line, args, return_type)
                    })
            })
    }
}
impl FromTerm for ast::FunctionConstraint {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("type",
         I32,
         "constraint",
         List2(("atom", I32, "is_subtype"),
               List2(to!(ast::Variable), to!(ast::Type))))
            .map_match(term,
                       |(_, line, _, (_, (var, subtype)))| Self::new(line, var, subtype))
    }
}
impl FromTerm for ast::AnyFunType {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("type", I32, "fun", Either(Nil, List2(("type", I32, "any"), to!(ast::Type))))
            .map_match(term, |(_, line, _, fun)| {
                match fun {
                    Ok(_) => Self::new(line),
                    Err((_, type_)) => Self::new(line).return_type(type_),
                }
            })
    }
}
impl FromTerm for ast::AnnotatedType {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("ann_type", I32, List2(to!(ast::Variable), to!(ast::Type)))
            .map_match(term, |(_, line, (var, type_))| Self::new(line, var, type_))
    }
}
impl FromTerm for ast::BitStringType {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("type", I32, "binary", List2(to!(ast::IntegerLit), to!(ast::IntegerLit)))
            .map_match(term, |(_, line, _, (bytes, bits))| {
                Self::new(line, bytes.to_u64().unwrap(), bits.to_u64().unwrap())
            })
    }
}
impl FromTerm for ast::Pattern {
    fn try_from(term: &eetf::Term) -> Result<Self, Option<&eetf::Term>> {
        try_from!(term, ast::IntegerLit);
        try_from!(term, ast::FloatLit);
        try_from!(term, ast::StringLit);
        try_from!(term, ast::CharLit);
        try_from!(term, ast::AtomLit);
        try_from!(term, ast::Variable);
        try_from!(term, ast::Match<_,_>);
        try_from!(term, ast::Tuple<_>);
        try_from!(term, ast::Nil);
        try_from!(term, ast::Cons<_>);
        try_from!(term, ast::Binary<_>);
        try_from!(term, ast::UnaryOp<_>);
        try_from!(term, ast::BinaryOp<_>);
        try_from!(term, ast::Record<_>);
        try_from!(term, ast::RecordIndex<_>);
        try_from!(term, ast::Map<_>);
        Err(Some(term))
    }
}
impl FromTerm for ast::Guard {
    fn try_from(term: &eetf::Term) -> Result<Self, Option<&eetf::Term>> {
        try_from!(term, ast::IntegerLit);
        try_from!(term, ast::FloatLit);
        try_from!(term, ast::StringLit);
        try_from!(term, ast::CharLit);
        try_from!(term, ast::AtomLit);
        try_from!(term, ast::Variable);
        try_from!(term, ast::Tuple<_>);
        try_from!(term, ast::Nil);
        try_from!(term, ast::Cons<_>);
        try_from!(term, ast::Binary<_>);
        try_from!(term, ast::UnaryOp<_>);
        try_from!(term, ast::BinaryOp<_>);
        try_from!(term, ast::Record<_>);
        try_from!(term, ast::RecordIndex<_>);
        try_from!(term, ast::LocalCall<_>);
        try_from!(term, ast::RemoteCall<_>);
        Err(Some(term))
    }
}
impl FromTerm for ast::form::ModuleAttr {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("attribute", I32, "module", Atom)
            .map_match(term, |(_, line, _, name)| Self::new(line, name.to_string()))
    }
}
impl FromTerm for ast::form::FileAttr {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("attribute", I32, "file", (Str, I32))
            .map_match(term, |(_, line, _, (original_file, original_line))| {
                Self::new(line, original_file, original_line)
            })
    }
}
impl FromTerm for ast::form::BehaviourAttr {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("attribute", I32, Either("behaviour", "behavior"), Atom)
            .map_match(term, |(_, line, british, name)| {
                Self::new(line, name.to_string()).british(british.is_ok())
            })
    }
}
impl FromTerm for ast::form::RecordDecl {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("attribute", I32, "record", (Atom, List(to::<ast::form::RecordFieldDecl>())))
            .map_match(term,
                       |(_, line, _, (name, fields))| Self::new(line, name.to_string(), fields))
    }
}
impl FromTerm for ast::form::RecordFieldDecl {
    fn from(term: &eetf::Term) -> Option<Self> {
        None.or_else(|| {
                ("record_field", I32, to!(ast::AtomLit))
                    .map_match(term, |(_, line, name)| Self::new(line, name.value))
            })
            .or_else(|| {
                ("record_field", I32, to!(ast::AtomLit), to!(ast::Expression))
                    .map_match(term, |(_, line, name, value)| {
                        Self::new(line, name.value).default_value(value)
                    })
            })
            .or_else(|| {
                ("typed_record_field", to!(ast::form::RecordFieldDecl), to!(ast::Type))
                    .map_match(term, |(_, field, type_)| field.typ(type_))
            })
    }
}
impl FromTerm for ast::AtomLit {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("atom", I32, Atom).map_match(term, |(_, line, name)| Self::new(line, name.to_string()))
    }
}
impl FromTerm for ast::IntegerLit {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("integer", I32, Int).map_match(term, |(_, line, value)| Self::new(line, value))
    }
}
impl FromTerm for ast::CharLit {
    fn from(term: &eetf::Term) -> Option<Self> {
        use std::char;
        ("char", I32, U32).map_match(term, |(_, line, value)| {
            Self::new(line, char::from_u32(value).unwrap())
        }) // XXX
    }
}
impl FromTerm for ast::FloatLit {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("float", I32, F64).map_match(term, |(_, line, value)| Self::new(line, value))
    }
}
impl FromTerm for ast::StringLit {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("string", I32, Str).map_match(term, |(_, line, value)| Self::new(line, value))
    }
}
impl FromTerm for ast::Variable {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("var", I32, Atom).map_match(term, |(_, line, name)| Self::new(line, name.to_string()))
    }
}
impl FromTerm for ast::form::TypeDecl {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("attribute",
         I32,
         Either("opaque", "type"),
         (Atom, to!(ast::Type), List(to!(ast::Variable))))
            .map_match(term, |(_, line, is_opaque, (name, type_, vars))| {
                Self::new(line, name.to_string(), vars, type_).opaque(is_opaque.is_ok())
            })
    }
}
impl FromTerm for ast::form::FunDecl {
    fn try_from(term: &eetf::Term) -> Result<Self, Option<&eetf::Term>> {
        ("function", I32, Atom, U32, List(to!(ast::Clause))).try_map_match(term, |(_,
                                                                             line,
                                                                             name,
                                                                             _,
                                                                             clauses)| {
            Self::new(line, name.to_string(), clauses)
        })
    }
}
impl FromTerm for ast::form::FunSpec {
    fn from(term: &eetf::Term) -> Option<Self> {
        None.or_else(|| {
                ("attribute",
                 I32,
                 Either("callback", "spec"),
                 ((Atom, U32), List(to!(ast::FunctionType))))
                    .map_match(term, |(_, line, is_callback, ((name, _), types))| {
                        Self::new(line, name.to_string(), types).callback(is_callback.is_ok())
                    })
            })
            .or_else(|| {
                ("attribute", I32, "spec", ((Atom, Atom, U32), List(to!(ast::FunctionType))))
                    .map_match(term, |(_, line, _, ((module, name, _), types))| {
                        Self::new(line, name.to_string(), types).module(module.to_string())
                    })
            })
    }
}
impl FromTerm for ast::form::ExportAttr {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("attribute", I32, "export", List((Atom, U32))).map_match(term, |(_, line, _, functions)| {
            Self::new(line,
                      functions.into_iter()
                          .map(|(f, a)| ast::form::Export::new(f.to_string(), a))
                          .collect())
        })
    }
}
impl FromTerm for ast::form::ImportAttr {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("attribute", I32, "import", (Atom, List((Atom, U32))))
            .map_match(term, |(_, line, _, (module, functions))| {
                Self::new(line,
                          module.to_string(),
                          functions.into_iter()
                              .map(|(f, a)| ast::form::Import::new(f.to_string(), a))
                              .collect())
            })
    }
}
impl FromTerm for ast::form::ExportTypeAttr {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("attribute", I32, "export_type", List((Atom, U32)))
            .map_match(term, |(_, line, _, export_types)| {
                Self::new(line,
                          export_types.into_iter()
                              .map(|(t, a)| ast::form::ExportType::new(t.to_string(), a))
                              .collect())
            })
    }
}
impl FromTerm for ast::form::CompileOptionsAttr {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("attribute", I32, "compile", Term).map_match(term, |(_, line, _, options)| {
            Self::new(line, options.clone())
        })
    }
}
impl FromTerm for ast::form::WildAttr {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("attribute", I32, Atom, Term).map_match(term, |(_, line, name, value)| {
            Self::new(line, name.to_string(), value.clone())
        })
    }
}
impl FromTerm for ast::form::Eof {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("eof", I32).map_match(term, |(_, line)| Self::new(line))
    }
}
type Line = I32;
