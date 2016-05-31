use std::path::Path;
use std::io;
use beam_file;
use beam_file::chunk::Chunk;
use eetf;
use result::BeamParseResult;
use error::BeamParseError;
use ast2;
use ast::matcher::{Pattern, Either};
use ast::matcher::{U32, Atom, List, Str, Term};

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
            .do_match(&self.code)
            .ok_or(BeamParseError::UnknownAbstractFormat));
        Ok(ast2::ModuleDecl { forms: forms })
    }
}

pub trait FromTerm {
    fn from(term: &eetf::Term) -> Option<Self> where Self: Sized;
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
    fn do_match(&self, term: &'a eetf::Term) -> Option<Self::Value> {
        F::from(term)
    }
}

impl FromTerm for ast2::Form {
    fn from(term: &eetf::Term) -> Option<Self> {
        use std::convert::From;
        None.or_else(|| to::<ast2::ModuleAttr>().do_match(term).map(From::from))
            .or_else(|| to::<ast2::FileAttr>().do_match(term).map(From::from))
            .or_else(|| to::<ast2::BehaviourAttr>().do_match(term).map(From::from))
            .or_else(|| to::<ast2::ExportAttr>().do_match(term).map(From::from))
            .or_else(|| to::<ast2::ImportAttr>().do_match(term).map(From::from))
            .or_else(|| to::<ast2::ExportTypeAttr>().do_match(term).map(From::from))
            .or_else(|| to::<ast2::CompileOptionsAttr>().do_match(term).map(From::from))
            .or_else(|| to::<ast2::RecordDecl>().do_match(term).map(From::from))
            .or_else(|| to::<ast2::TypeDecl>().do_match(term).map(From::from))
            .or_else(|| to::<ast2::FunctionSpec>().do_match(term).map(From::from))
            .or_else(|| to::<ast2::FunctionDecl>().do_match(term).map(From::from))
            .or_else(|| to::<ast2::WildAttr>().do_match(term).map(From::from))
    }
}
impl FromTerm for ast2::ModuleAttr {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("attribute", U32, "module", Atom)
            .map_match(term, |(_, line, _, name)| Self::new(line, name.to_string()))
    }
}
impl FromTerm for ast2::FileAttr {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("attribute", U32, "file", (Str, U32))
            .map_match(term, |(_, line, _, (original_file, original_line))| {
                Self::new(line, original_file, original_line)
            })
    }
}
impl FromTerm for ast2::BehaviourAttr {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("attribute", U32, Either("behaviour", "behavior"), Atom)
            .map_match(term, |(_, line, british, name)| {
                Self::new(line, name.to_string()).british(british.is_ok())
            })
    }
}
impl FromTerm for ast2::RecordDecl {
    fn from(term: &eetf::Term) -> Option<Self> {
        None
    }
}
impl FromTerm for ast2::TypeDecl {
    fn from(term: &eetf::Term) -> Option<Self> {
        None
    }
}
impl FromTerm for ast2::FunctionDecl {
    fn from(term: &eetf::Term) -> Option<Self> {
        None
    }
}
impl FromTerm for ast2::FunctionSpec {
    fn from(term: &eetf::Term) -> Option<Self> {
        None
    }
}
impl FromTerm for ast2::ExportAttr {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("attribute", U32, "export", List((Atom, U32))).map_match(term, |(_, line, _, functions)| {
            Self::new(line,
                      functions.into_iter()
                          .map(|(f, a)| ast2::Export::new(f.to_string(), a))
                          .collect())
        })
    }
}
impl FromTerm for ast2::ImportAttr {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("attribute", U32, "import", (Atom, List((Atom, U32))))
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
        ("attribute", U32, "export_type", List((Atom, U32)))
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
        ("attribute", U32, "compile", Term).map_match(term, |(_, line, _, options)| {
            Self::new(line, options.clone())
        })
    }
}
impl FromTerm for ast2::WildAttr {
    fn from(term: &eetf::Term) -> Option<Self> {
        ("attribute", U32, Atom, Term).map_match(term, |(_, line, name, value)| {
            Self::new(line, name.to_string(), value.clone())
        })
    }
}
