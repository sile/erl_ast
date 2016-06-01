//! Module Declarations and Forms
//!
//! See: [6.1 Module Declarations and Forms](http://erlang.org/doc/apps/erts/absform.html#id86691)
use std::path::Path;
use eetf;
use result::BeamParseResult;
use ast;
use ast::LineNum;
use ast::Arity;
use ast::typ;
use ast::expr;

#[derive(Debug)]
pub struct ModuleDecl {
    pub forms: Vec<Form>,
}
impl ModuleDecl {
    pub fn from_beam_file<P: AsRef<Path>>(path: P) -> BeamParseResult<Self> {
        let code = try!(ast::codec::raw_abstract_v1::AbstractCode::from_beam_file(path));
        code.to_module_decl()
    }
}

#[derive(Debug)]
pub enum Form {
    Module(ModuleAttr),
    Behaviour(BehaviourAttr),
    Export(ExportAttr),
    Import(ImportAttr),
    ExportType(ExportTypeAttr),
    Compile(CompileOptionsAttr),
    File(FileAttr),
    Record(RecordDecl),
    Type(TypeDecl),
    Spec(FunSpec),
    Attr(WildAttr),
    Fun(FunDecl),
    Eof(Eof),
}
impl_from!(Form::Module(ModuleAttr));
impl_from!(Form::Behaviour(BehaviourAttr));
impl_from!(Form::Export(ExportAttr));
impl_from!(Form::Import(ImportAttr));
impl_from!(Form::ExportType(ExportTypeAttr));
impl_from!(Form::Compile(CompileOptionsAttr));
impl_from!(Form::File(FileAttr));
impl_from!(Form::Record(RecordDecl));
impl_from!(Form::Type(TypeDecl));
impl_from!(Form::Spec(FunSpec));
impl_from!(Form::Attr(WildAttr));
impl_from!(Form::Fun(FunDecl));
impl_from!(Form::Eof(Eof));

#[derive(Debug)]
pub struct Eof {
    pub line: LineNum,
}
impl_node!(Eof);
impl Eof {
    pub fn new(line: LineNum) -> Self {
        Eof { line: line }
    }
}

#[derive(Debug)]
pub struct ModuleAttr {
    pub line: LineNum,
    pub name: String,
}
impl_node!(ModuleAttr);
impl ModuleAttr {
    pub fn new(line: LineNum, name: String) -> Self {
        ModuleAttr {
            line: line,
            name: name,
        }
    }
}

#[derive(Debug)]
pub struct BehaviourAttr {
    pub line: LineNum,
    pub is_british: bool,
    pub name: String,
}
impl_node!(BehaviourAttr);
impl BehaviourAttr {
    pub fn new(line: LineNum, name: String) -> Self {
        BehaviourAttr {
            line: line,
            name: name,
            is_british: true,
        }
    }
    pub fn british(mut self, is_british: bool) -> Self {
        self.is_british = is_british;
        self
    }
}

#[derive(Debug)]
pub struct ExportAttr {
    pub line: LineNum,
    pub funs: Vec<Export>,
}
impl_node!(ExportAttr);
impl ExportAttr {
    pub fn new(line: LineNum, funs: Vec<Export>) -> Self {
        ExportAttr {
            line: line,
            funs: funs,
        }
    }
}

#[derive(Debug)]
pub struct ImportAttr {
    pub line: LineNum,
    pub module: String,
    pub funs: Vec<Import>,
}
impl_node!(ImportAttr);
impl ImportAttr {
    pub fn new(line: LineNum, module: String, funs: Vec<Import>) -> Self {
        ImportAttr {
            line: line,
            module: module,
            funs: funs,
        }
    }
}

#[derive(Debug)]
pub struct ExportTypeAttr {
    pub line: LineNum,
    pub types: Vec<ExportType>,
}
impl_node!(ExportTypeAttr);
impl ExportTypeAttr {
    pub fn new(line: LineNum, types: Vec<ExportType>) -> Self {
        ExportTypeAttr {
            line: line,
            types: types,
        }
    }
}

#[derive(Debug)]
pub struct CompileOptionsAttr {
    pub line: LineNum,
    pub options: eetf::Term,
}
impl_node!(CompileOptionsAttr);
impl CompileOptionsAttr {
    pub fn new(line: LineNum, options: eetf::Term) -> Self {
        CompileOptionsAttr {
            line: line,
            options: options,
        }
    }
}

#[derive(Debug)]
pub struct FileAttr {
    pub line: LineNum,
    pub original_file: String,
    pub original_line: LineNum,
}
impl_node!(FileAttr);
impl FileAttr {
    pub fn new(line: LineNum, original_file: String, original_line: LineNum) -> Self {
        FileAttr {
            line: line,
            original_file: original_file,
            original_line: original_line,
        }
    }
}

#[derive(Debug)]
pub struct RecordDecl {
    pub line: LineNum,
    pub name: String,
    pub fields: Vec<RecordFieldDecl>,
}
impl_node!(RecordDecl);
impl RecordDecl {
    pub fn new(line: LineNum, name: String, fields: Vec<RecordFieldDecl>) -> Self {
        RecordDecl {
            line: line,
            name: name,
            fields: fields,
        }
    }
}

#[derive(Debug)]
pub struct TypeDecl {
    pub line: LineNum,
    pub is_opaque: bool,
    pub name: String,
    pub vars: Vec<typ::Var>,
    pub typ: typ::Type,
}
impl_node!(TypeDecl);
impl TypeDecl {
    pub fn new(line: LineNum, name: String, vars: Vec<typ::Var>, typ: typ::Type) -> Self {
        TypeDecl {
            line: line,
            name: name,
            vars: vars,
            typ: typ,
            is_opaque: false,
        }
    }
    pub fn opaque(mut self, is_opaque: bool) -> Self {
        self.is_opaque = is_opaque;
        self
    }
}

#[derive(Debug)]
pub struct FunSpec {
    pub line: LineNum,
    pub module: Option<String>,
    pub name: String,
    pub types: Vec<typ::Fun>,
    pub is_callback: bool,
}
impl_node!(FunSpec);
impl FunSpec {
    pub fn new(line: LineNum, name: String, types: Vec<typ::Fun>) -> Self {
        FunSpec {
            line: line,
            module: None,
            name: name,
            types: types,
            is_callback: false,
        }
    }
    pub fn module(mut self, module: String) -> Self {
        self.module = Some(module);
        self
    }
    pub fn callback(mut self, is_callback: bool) -> Self {
        self.is_callback = is_callback;
        self
    }
}

#[derive(Debug)]
pub struct WildAttr {
    pub line: LineNum,
    pub name: String,
    pub value: eetf::Term,
}
impl_node!(WildAttr);
impl WildAttr {
    pub fn new(line: LineNum, name: String, value: eetf::Term) -> Self {
        WildAttr {
            line: line,
            name: name,
            value: value,
        }
    }
}

#[derive(Debug)]
pub struct FunDecl {
    pub line: LineNum,
    pub name: String,
    pub clauses: Vec<expr::Clause>,
}
impl_node!(FunDecl);
impl FunDecl {
    pub fn new(line: LineNum, name: String, clauses: Vec<expr::Clause>) -> Self {
        FunDecl {
            line: line,
            name: name,
            clauses: clauses,
        }
    }
}

#[derive(Debug)]
pub struct RecordFieldDecl {
    pub line: LineNum,
    pub name: String,
    pub typ: typ::Type,
    pub default_value: expr::Expression,
}
impl_node!(RecordFieldDecl);
impl RecordFieldDecl {
    pub fn new(line: LineNum, name: String) -> Self {
        RecordFieldDecl {
            line: line,
            name: name,
            typ: typ::Type::any(line),
            default_value: expr::Expression::atom(line, "undefined".to_string()),
        }
    }
    pub fn typ(mut self, typ: typ::Type) -> Self {
        self.typ = typ;
        self
    }
    pub fn default_value(mut self, value: expr::Expression) -> Self {
        self.default_value = value;
        self
    }
}

#[derive(Debug)]
pub struct Export {
    pub fun: String,
    pub arity: Arity,
}
impl Export {
    pub fn new(fun: String, arity: Arity) -> Self {
        Export {
            fun: fun,
            arity: arity,
        }
    }
}

#[derive(Debug)]
pub struct Import {
    pub fun: String,
    pub arity: Arity,
}
impl Import {
    pub fn new(fun: String, arity: Arity) -> Self {
        Import {
            fun: fun,
            arity: arity,
        }
    }
}

#[derive(Debug)]
pub struct ExportType {
    pub typ: String,
    pub arity: Arity,
}
impl ExportType {
    pub fn new(typ: String, arity: Arity) -> Self {
        ExportType {
            typ: typ,
            arity: arity,
        }
    }
}
