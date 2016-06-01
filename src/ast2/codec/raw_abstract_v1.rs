use std::path::Path;
use std::io;
use beam_file;
use beam_file::chunk::Chunk;
use eetf;
use result::BeamParseResult;
use error::BeamParseError;
use ast2;
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
    pub fn to_module_decl(&self) -> BeamParseResult<ast2::ModuleDecl> {
        let (_, forms) = try!(("raw_abstract_v1", List(to::<ast2::Form>()))
            .try_match(&self.code)
            .map_err(|term| BeamParseError::UnexpectedTerm(term.unwrap().clone())));
        Ok(ast2::ModuleDecl { forms: forms })
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

impl FromTerm for ast2::Form {
    fn try_from(term: &eetf::Term) -> Result<Self, Option<&eetf::Term>> {
        try_from!(term, ast2::ModuleAttr);
        try_from!(term, ast2::FileAttr);
        try_from!(term, ast2::BehaviourAttr);
        try_from!(term, ast2::ExportAttr);
        try_from!(term, ast2::ImportAttr);
        try_from!(term, ast2::ExportTypeAttr);
        try_from!(term, ast2::CompileOptionsAttr);
        try_from!(term, ast2::RecordDecl);
        try_from!(term, ast2::TypeDecl);
        try_from!(term, ast2::FunctionSpec);
        try_from!(term, ast2::FunctionDecl);
        try_from!(term, ast2::WildAttr);
        try_from!(term, ast2::Eof);
        Err(Some(term))
    }
}
impl FromTerm for ast2::Expression {
    fn try_from(term: &eetf::Term) -> Result<Self, Option<&eetf::Term>> {
        try_from!(term, ast2::IntegerLit);
        try_from!(term, ast2::FloatLit);
        try_from!(term, ast2::StringLit);
        try_from!(term, ast2::CharLit);
        try_from!(term, ast2::AtomLit);
        try_from!(term, ast2::Match<_, _>);
        try_from!(term, ast2::Variable);
        try_from!(term, ast2::Tuple<_>);
        try_from!(term, ast2::Nil);
        try_from!(term, ast2::Cons<_>);
        try_from!(term, ast2::Binary<_>);
        try_from!(term, ast2::UnaryOp<_>);
        try_from!(term, ast2::BinaryOp<_>);
        try_from!(term, ast2::Record<_>);
        try_from!(term, ast2::RecordIndex<_>);
        try_from!(term, ast2::Map<_>);
        try_from!(term, ast2::Catch);
        try_from!(term, ast2::LocalCall<_>);
        try_from!(term, ast2::RemoteCall<_>);
        try_from!(term, ast2::Comprehension);
        try_from!(term, ast2::Block);
        try_from!(term, ast2::If);
        try_from!(term, ast2::Case);
        try_from!(term, ast2::Try);
        try_from!(term, ast2::Receive);
        try_from!(term, ast2::InternalFun);
        try_from!(term, ast2::ExternalFun);
        try_from!(term, ast2::AnonymousFun);
        Err(Some(term))
    }
}
impl FromTerm for ast2::Catch {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("catch", I32, to!(ast2::Expression))
            .map_match(term, |(_, line, expr)| Self::new(line, expr))
    }
}
impl FromTerm for ast2::Receive {
    fn try_from(term: &eetf::Term) -> Result<Self, Option<&eetf::Term>> {
        Either(("receive", I32, List(to!(ast2::Clause))),
               ("receive",
                I32,
                List(to!(ast2::Clause)),
                to!(ast2::Expression),
                List(to!(ast2::Expression))))
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
impl FromTerm for ast2::InternalFun {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("fun", I32, ("function", Atom, U32)).map_match(term, |(_, line, (_, name, arity))| {
            Self::new(line, name.to_string(), arity)
        })
    }
}
impl FromTerm for ast2::ExternalFun {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("fun",
         I32,
         ("function", to!(ast2::Expression), to!(ast2::Expression), to!(ast2::Expression)))
            .map_match(term, |(_, line, (_, module, function, arity))| {
                Self::new(line, module, function, arity)
            })
    }
}
impl FromTerm for ast2::AnonymousFun {
    fn try_from(term: &eetf::Term) -> Result<Self, Option<&eetf::Term>> {
        Either(("fun", I32, ("clauses", List(to!(ast2::Clause)))),
               ("named_fun", I32, Atom, List(to!(ast2::Clause))))
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
impl FromTerm for ast2::Block {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("block", I32, List(to!(ast2::Expression)))
            .map_match(term, |(_, line, body)| Self::new(line, body))
    }
}
impl FromTerm for ast2::If {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("if", I32, List(to!(ast2::Clause)))
            .map_match(term, |(_, line, clauses)| Self::new(line, clauses))
    }
}
impl FromTerm for ast2::Case {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("case", I32, to!(ast2::Expression), List(to!(ast2::Clause))).map_match(term, |(_,
                                                                                  line,
                                                                                  expr,
                                                                                  clauses)| {
            Self::new(line, expr, clauses)
        })
    }
}
impl FromTerm for ast2::Try {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("try",
         I32,
         List(to!(ast2::Expression)),
         List(to!(ast2::Clause)),
         List(to!(ast2::Clause)),
         List(to!(ast2::Expression)))
            .map_match(term,
                       |(_, line, body, case_clauses, catch_clauses, after)| {
                           Self::new(line, body, case_clauses, catch_clauses, after)
                       })
    }
}
impl FromTerm for ast2::Comprehension {
    fn from(term: &eetf::Term) -> Option<Self> {
        (Either("lc", "bc"), I32, to!(ast2::Expression), List(to!(ast2::Qualifier)))
            .map_match(term, |(is_list, line, expr, qualifiers)| {
                Self::new(line, is_list.is_ok(), expr, qualifiers)
            })
    }
}
impl FromTerm for ast2::Qualifier {
    fn from(term: &eetf::Term) -> Option<Self> {
        use ast2::Qualifier::*;
        None.or_else(|| {
                ("generate", I32, to!(ast2::Pattern), to!(ast2::Expression))
                    .map_match(term, |(_, line, pattern, expr)| {
                        Generator(ast2::Generator::new(line, pattern, expr))
                    })
            })
            .or_else(|| {
                ("b_generate", I32, to!(ast2::Pattern), to!(ast2::Expression))
                    .map_match(term, |(_, line, pattern, expr)| {
                        BitStringGenerator(ast2::Generator::new(line, pattern, expr))
                    })
            })
            .or_else(|| to!(ast2::Expression).map_match(term, |expr| Filter(expr)))
    }
}
impl FromTerm for ast2::Clause {
    fn try_from(term: &eetf::Term) -> Result<Self, Option<&eetf::Term>> {
        ("clause",
         I32,
         List(to!(ast2::Pattern)),
         List(to!(ast2::OrGuard)),
         List(to!(ast2::Expression)))
            .try_map_match(term, |(_, line, patterns, guards, body)| {
                Self::new(line, patterns, guards, body)
            })
    }
}
impl FromTerm for ast2::OrGuard {
    fn from(term: &eetf::Term) -> Option<Self> {
        List(to!(ast2::Guard)).map_match(term, |guards| Self::new(guards))
    }
}
impl<T: FromTerm> FromTerm for ast2::Tuple<T> {
    fn try_from(term: &eetf::Term) -> Result<Self, Option<&eetf::Term>> {
        ("tuple", I32, List(to!(T)))
            .try_map_match(term, |(_, line, elements)| Self::new(line, elements))
    }
}
impl<T: FromTerm> FromTerm for ast2::Cons<T> {
    fn try_from(term: &eetf::Term) -> Result<Self, Option<&eetf::Term>> {
        ("cons", I32, to!(T), to!(T))
            .try_map_match(term, |(_, line, head, tail)| Self::new(line, head, tail))
    }
}
impl<T: FromTerm> FromTerm for ast2::Binary<T> {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("bin", I32, List(to!(ast2::BinElement<T>)))
            .map_match(term, |(_, line, elements)| Self::new(line, elements))
    }
}
impl<T: FromTerm> FromTerm for ast2::BinElement<T> {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("bin_element",
         I32,
         to!(T),
         Either(to!(T), "default"),
         Either(List(to!(ast2::BinElementTypeSpec)), "default"))
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
impl FromTerm for ast2::BinElementTypeSpec {
    fn from(term: &eetf::Term) -> Option<Self> {
        Either(Atom, (Atom, U64)).map_match(term, |ts| {
            match ts {
                Ok(name) => Self::new(name.to_string(), None),
                Err((name, value)) => Self::new(name.to_string(), Some(value)),
            }
        })
    }
}
impl<T: FromTerm> FromTerm for ast2::Record<T> {
    fn from(term: &eetf::Term) -> Option<Self> {
        None.or_else(|| {
                ("record", I32, Atom, List(to!(ast2::RecordField<T>))).map_match(term, |(_,
                                                                                   line,
                                                                                   name,
                                                                                   fields)| {
                    Self::new(line, name.to_string(), fields)
                })
            })
            .or_else(|| {
                ("record", I32, to!(ast2::Expression), Atom, List(to!(ast2::RecordField<T>)))
                    .map_match(term, |(_, line, base, name, fields)| {
                        Self::new(line, name.to_string(), fields).base(base)
                    })
            })
    }
}
impl<T: FromTerm> FromTerm for ast2::RecordField<T> {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("record_field", I32, Either(to!(ast2::AtomLit), ("var", I32, "_")), to!(T))
            .map_match(term,
                       |(_, line, name, value)| Self::new(line, name.ok().map(|x| x.value), value))
    }
}
impl<T: FromTerm> FromTerm for ast2::Map<T> {
    fn from(term: &eetf::Term) -> Option<Self> {
        None.or_else(|| {
                ("map", I32, List(to!(ast2::MapPair<T>)))
                    .map_match(term, |(_, line, pairs)| Self::new(line, pairs))
            })
            .or_else(|| {
                ("map", I32, to!(ast2::Expression), List(to!(ast2::MapPair<T>)))
                    .map_match(term,
                               |(_, line, base, pairs)| Self::new(line, pairs).base(base))
            })
    }
}
impl<T: FromTerm> FromTerm for ast2::MapPair<T> {
    fn from(term: &eetf::Term) -> Option<Self> {
        (Either("map_field_assoc", "map_field_exact"), I32, to!(T), to!(T))
            .map_match(term, |(is_assoc, line, key, value)| {
                Self::new(line, is_assoc.is_ok(), key, value)
            })
    }
}
impl<T: FromTerm> FromTerm for ast2::LocalCall<T> {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("call", I32, to!(T), List(to!(T))).map_match(term, |(_, line, function, args)| {
            Self::new(line, function, args)
        })
    }
}
impl<T: FromTerm> FromTerm for ast2::RemoteCall<T> {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("call", I32, ("remote", U32, to!(T), to!(T)), List(to!(T)))
            .map_match(term, |(_, line, (_, _, module, function), args)| {
                Self::new(line, module, function, args)
            })
    }
}
impl<T: FromTerm> FromTerm for ast2::RecordIndex<T> {
    fn from(term: &eetf::Term) -> Option<Self> {
        None.or_else(|| {
                ("record_index", I32, Atom, to!(ast2::AtomLit))
                    .map_match(term, |(_, line, name, field)| {
                        Self::new(line, name.to_string(), field.value)
                    })
            })
            .or_else(|| {
                ("record_field", I32, to!(T), Atom, to!(ast2::AtomLit))
                    .map_match(term, |(_, line, base, name, field)| {
                        Self::new(line, name.to_string(), field.value).base(base)
                    })
            })
    }
}
impl<T: FromTerm> FromTerm for ast2::BinaryOp<T> {
    fn try_from(term: &eetf::Term) -> Result<Self, Option<&eetf::Term>> {
        ("op", I32, Atom, to!(T), to!(T)).try_map_match(term, |(_, line, op, left, right)| {
            Self::new(line, op.to_string(), left, right)
        })
    }
}
impl<T: FromTerm> FromTerm for ast2::UnaryOp<T> {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("op", I32, Atom, to!(T)).map_match(term, |(_, line, op, arg)| {
            Self::new(line, op.to_string(), arg)
        })
    }
}
impl FromTerm for ast2::Nil {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("nil", I32).map_match(term, |(_, line)| Self::new(line))
    }
}
impl<L, R> FromTerm for ast2::Match<L, R>
    where L: FromTerm,
          R: FromTerm
{
    fn try_from(term: &eetf::Term) -> Result<Self, Option<&eetf::Term>> {
        ("match", I32, to!(L), to!(R))
            .try_map_match(term, |(_, line, left, right)| Self::new(line, left, right))
    }
}
impl FromTerm for ast2::Type {
    fn try_from(term: &eetf::Term) -> Result<Self, Option<&eetf::Term>> {
        try_from!(term, ast2::AtomLit);
        try_from!(term, ast2::IntegerLit);
        try_from!(term, ast2::Variable);
        try_from!(term, ast2::AnnotatedType);
        try_from!(term, ast2::UnaryOp<_>);
        try_from!(term, ast2::BinaryOp<_>);
        try_from!(term, ast2::BitStringType);
        try_from!(term, ast2::Nil);
        try_from!(term, ast2::AnyFunType);
        try_from!(term, ast2::FunctionType);
        try_from!(term, ast2::RangeType);
        try_from!(term, ast2::MapType);
        try_from!(term, ast2::RecordType);
        try_from!(term, ast2::RemoteType);
        try_from!(term, ast2::AnyTupleType);
        try_from!(term, ast2::TupleType);
        try_from!(term, ast2::UnionType);
        try_from!(term, ast2::BuiltInType);
        try_from!(term, ast2::UserType);
        Err(Some(term))
    }
}
impl FromTerm for ast2::UserType {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("user_type", I32, Atom, List(to!(ast2::Type))).map_match(term, |(_, line, name, args)| {
            Self::new(line, name.to_string(), args)
        })
    }
}
impl FromTerm for ast2::UnionType {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("type", I32, "union", List(to!(ast2::Type)))
            .map_match(term, |(_, line, _, types)| Self::new(line, types))
    }
}
impl FromTerm for ast2::TupleType {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("type", I32, "tuple", List(to!(ast2::Type)))
            .map_match(term, |(_, line, _, types)| Self::new(line, types))
    }
}
impl FromTerm for ast2::AnyTupleType {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("type", I32, "tuple", "any").map_match(term, |(_, line, _, _)| Self::new(line))
    }
}
impl FromTerm for ast2::RemoteType {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("remote_type",
         I32,
         List3(to!(ast2::AtomLit),
               to!(ast2::AtomLit),
               List(to!(ast2::Type))))
            .map_match(term, |(_, line, (module, function, args))| {
                Self::new(line, module.value, function.value, args)
            })
    }
}
impl FromTerm for ast2::RecordType {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("type", I32, "record", List(Either(to!(ast2::AtomLit), to!(ast2::RecordFieldType))))
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
impl FromTerm for ast2::RecordFieldType {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("type", I32, "field_type", List2(to!(ast2::AtomLit), to!(ast2::Type)))
            .map_match(term,
                       |(_, line, _, (name, type_))| Self::new(line, name.value, type_))
    }
}
impl FromTerm for ast2::BuiltInType {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("type", I32, Atom, Either("any", List(to!(ast2::Type))))
            .map_match(term, |(_, line, name, args)| {
                match args {
                    Ok(_) => Self::new(line, name.to_string(), Vec::new()),
                    Err(args) => Self::new(line, name.to_string(), args),
                }
            })
    }
}
impl FromTerm for ast2::MapType {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("type", I32, "map", List(to!(ast2::MapPairType)))
            .map_match(term, |(_, line, _, pairs)| Self::new(line, pairs))
    }
}
impl FromTerm for ast2::MapPairType {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("type", I32, "map_field_assoc", List2(to!(ast2::Type), to!(ast2::Type)))
            .map_match(term,
                       |(_, line, _, (key, value))| Self::new(line, key, value))
    }
}
impl FromTerm for ast2::RangeType {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("type", I32, "range", List2(to!(ast2::Type), to!(ast2::Type)))
            .map_match(term, |(_, line, _, (low, high))| Self::new(line, low, high))
    }
}
impl FromTerm for ast2::FunctionType {
    fn from(term: &eetf::Term) -> Option<Self> {
        None.or_else(|| {
                ("type",
                 I32,
                 "bounded_fun",
                 List2(to!(ast2::FunctionType), List(to!(ast2::FunctionConstraint))))
                    .map_match(term,
                               |(_, _, _, (fun, constraints))| fun.constraints(constraints))
            })
            .or_else(|| {
                ("type",
                 I32,
                 "fun",
                 List2(("type", I32, "product", List(to!(ast2::Type))),
                       to!(ast2::Type)))
                    .map_match(term, |(_, line, _, ((_, _, _, args), return_type))| {
                        Self::new(line, args, return_type)
                    })
            })
    }
}
impl FromTerm for ast2::FunctionConstraint {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("type",
         I32,
         "constraint",
         List2(("atom", I32, "is_subtype"),
               List2(to!(ast2::Variable), to!(ast2::Type))))
            .map_match(term,
                       |(_, line, _, (_, (var, subtype)))| Self::new(line, var, subtype))
    }
}
impl FromTerm for ast2::AnyFunType {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("type", I32, "fun", Either(Nil, List2(("type", I32, "any"), to!(ast2::Type))))
            .map_match(term, |(_, line, _, fun)| {
                match fun {
                    Ok(_) => Self::new(line),
                    Err((_, type_)) => Self::new(line).return_type(type_),
                }
            })
    }
}
impl FromTerm for ast2::AnnotatedType {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("ann_type", I32, List2(to!(ast2::Variable), to!(ast2::Type)))
            .map_match(term, |(_, line, (var, type_))| Self::new(line, var, type_))
    }
}
impl FromTerm for ast2::BitStringType {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("type", I32, "binary", List2(to!(ast2::IntegerLit), to!(ast2::IntegerLit)))
            .map_match(term, |(_, line, _, (bytes, bits))| {
                Self::new(line, bytes.to_u64().unwrap(), bits.to_u64().unwrap())
            })
    }
}
impl FromTerm for ast2::Pattern {
    fn try_from(term: &eetf::Term) -> Result<Self, Option<&eetf::Term>> {
        try_from!(term, ast2::IntegerLit);
        try_from!(term, ast2::FloatLit);
        try_from!(term, ast2::StringLit);
        try_from!(term, ast2::CharLit);
        try_from!(term, ast2::AtomLit);
        try_from!(term, ast2::Variable);
        try_from!(term, ast2::Match<_,_>);
        try_from!(term, ast2::Tuple<_>);
        try_from!(term, ast2::Nil);
        try_from!(term, ast2::Cons<_>);
        try_from!(term, ast2::Binary<_>);
        try_from!(term, ast2::UnaryOp<_>);
        try_from!(term, ast2::BinaryOp<_>);
        try_from!(term, ast2::Record<_>);
        try_from!(term, ast2::RecordIndex<_>);
        try_from!(term, ast2::Map<_>);
        Err(Some(term))
    }
}
impl FromTerm for ast2::Guard {
    fn try_from(term: &eetf::Term) -> Result<Self, Option<&eetf::Term>> {
        try_from!(term, ast2::IntegerLit);
        try_from!(term, ast2::FloatLit);
        try_from!(term, ast2::StringLit);
        try_from!(term, ast2::CharLit);
        try_from!(term, ast2::AtomLit);
        try_from!(term, ast2::Variable);
        try_from!(term, ast2::Tuple<_>);
        try_from!(term, ast2::Nil);
        try_from!(term, ast2::Cons<_>);
        try_from!(term, ast2::Binary<_>);
        try_from!(term, ast2::UnaryOp<_>);
        try_from!(term, ast2::BinaryOp<_>);
        try_from!(term, ast2::Record<_>);
        try_from!(term, ast2::RecordIndex<_>);
        try_from!(term, ast2::LocalCall<_>);
        try_from!(term, ast2::RemoteCall<_>);
        Err(Some(term))
    }
}
impl FromTerm for ast2::ModuleAttr {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("attribute", I32, "module", Atom)
            .map_match(term, |(_, line, _, name)| Self::new(line, name.to_string()))
    }
}
impl FromTerm for ast2::FileAttr {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("attribute", I32, "file", (Str, I32))
            .map_match(term, |(_, line, _, (original_file, original_line))| {
                Self::new(line, original_file, original_line)
            })
    }
}
impl FromTerm for ast2::BehaviourAttr {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("attribute", I32, Either("behaviour", "behavior"), Atom)
            .map_match(term, |(_, line, british, name)| {
                Self::new(line, name.to_string()).british(british.is_ok())
            })
    }
}
impl FromTerm for ast2::RecordDecl {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("attribute", I32, "record", (Atom, List(to::<ast2::RecordFieldDecl>())))
            .map_match(term,
                       |(_, line, _, (name, fields))| Self::new(line, name.to_string(), fields))
    }
}
impl FromTerm for ast2::RecordFieldDecl {
    fn from(term: &eetf::Term) -> Option<Self> {
        None.or_else(|| {
                ("record_field", I32, to!(ast2::AtomLit))
                    .map_match(term, |(_, line, name)| Self::new(line, name.value))
            })
            .or_else(|| {
                ("record_field", I32, to!(ast2::AtomLit), to!(ast2::Expression))
                    .map_match(term, |(_, line, name, value)| {
                        Self::new(line, name.value).default_value(value)
                    })
            })
            .or_else(|| {
                ("typed_record_field", to!(ast2::RecordFieldDecl), to!(ast2::Type))
                    .map_match(term, |(_, field, type_)| field.type_(type_))
            })
    }
}
impl FromTerm for ast2::AtomLit {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("atom", I32, Atom).map_match(term, |(_, line, name)| Self::new(line, name.to_string()))
    }
}
impl FromTerm for ast2::IntegerLit {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("integer", I32, Int).map_match(term, |(_, line, value)| Self::new(line, value))
    }
}
impl FromTerm for ast2::CharLit {
    fn from(term: &eetf::Term) -> Option<Self> {
        use std::char;
        ("char", I32, U32).map_match(term, |(_, line, value)| {
            Self::new(line, char::from_u32(value).unwrap())
        }) // XXX
    }
}
impl FromTerm for ast2::FloatLit {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("float", I32, F64).map_match(term, |(_, line, value)| Self::new(line, value))
    }
}
impl FromTerm for ast2::StringLit {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("string", I32, Str).map_match(term, |(_, line, value)| Self::new(line, value))
    }
}
impl FromTerm for ast2::Variable {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("var", I32, Atom).map_match(term, |(_, line, name)| Self::new(line, name.to_string()))
    }
}
impl FromTerm for ast2::TypeDecl {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("attribute",
         I32,
         Either("opaque", "type"),
         (Atom, to!(ast2::Type), List(to!(ast2::Variable))))
            .map_match(term, |(_, line, is_opaque, (name, type_, vars))| {
                Self::new(line, name.to_string(), vars, type_).opaque(is_opaque.is_ok())
            })
    }
}
impl FromTerm for ast2::FunctionDecl {
    fn try_from(term: &eetf::Term) -> Result<Self, Option<&eetf::Term>> {
        ("function", I32, Atom, U32, List(to!(ast2::Clause))).try_map_match(term, |(_,
                                                                              line,
                                                                              name,
                                                                              _,
                                                                              clauses)| {
            Self::new(line, name.to_string(), clauses)
        })
    }
}
impl FromTerm for ast2::FunctionSpec {
    fn from(term: &eetf::Term) -> Option<Self> {
        None.or_else(|| {
                ("attribute",
                 I32,
                 Either("callback", "spec"),
                 ((Atom, U32), List(to!(ast2::FunctionType))))
                    .map_match(term, |(_, line, is_callback, ((name, _), types))| {
                        Self::new(line, name.to_string(), types).callback(is_callback.is_ok())
                    })
            })
            .or_else(|| {
                ("attribute", I32, "spec", ((Atom, Atom, U32), List(to!(ast2::FunctionType))))
                    .map_match(term, |(_, line, _, ((module, name, _), types))| {
                        Self::new(line, name.to_string(), types).module(module.to_string())
                    })
            })
    }
}
impl FromTerm for ast2::ExportAttr {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("attribute", I32, "export", List((Atom, U32))).map_match(term, |(_, line, _, functions)| {
            Self::new(line,
                      functions.into_iter()
                          .map(|(f, a)| ast2::Export::new(f.to_string(), a))
                          .collect())
        })
    }
}
impl FromTerm for ast2::ImportAttr {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("attribute", I32, "import", (Atom, List((Atom, U32))))
            .map_match(term, |(_, line, _, (module, functions))| {
                Self::new(line,
                          module.to_string(),
                          functions.into_iter()
                              .map(|(f, a)| ast2::Import::new(f.to_string(), a))
                              .collect())
            })
    }
}
impl FromTerm for ast2::ExportTypeAttr {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("attribute", I32, "export_type", List((Atom, U32)))
            .map_match(term, |(_, line, _, export_types)| {
                Self::new(line,
                          export_types.into_iter()
                              .map(|(t, a)| ast2::ExportType::new(t.to_string(), a))
                              .collect())
            })
    }
}
impl FromTerm for ast2::CompileOptionsAttr {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("attribute", I32, "compile", Term).map_match(term, |(_, line, _, options)| {
            Self::new(line, options.clone())
        })
    }
}
impl FromTerm for ast2::WildAttr {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("attribute", I32, Atom, Term).map_match(term, |(_, line, name, value)| {
            Self::new(line, name.to_string(), value.clone())
        })
    }
}
impl FromTerm for ast2::Eof {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("eof", I32).map_match(term, |(_, line)| Self::new(line))
    }
}
type Line = I32;
