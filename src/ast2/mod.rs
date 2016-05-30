use eetf;

macro_rules! impl_node {
    ($x:ty) => {
        impl Node for $x {
            fn line(&self) -> LineNum {
                self.line
            }
        }
    }
}

pub type LineNum = u32;
pub type Arity = u32;

pub trait Node {
    fn line(&self) -> LineNum;
}

#[derive(Debug)]
pub struct ModuleDecl {
    pub module: ModuleAttr,
    pub behaviours: Vec<BehaviourAttr>,
    pub exports: Vec<ExportAttr>,
    pub imports: Vec<ImportAttr>,
    pub export_types: Vec<ExportTypeAttr>,
    pub compiles: Vec<CompileOptionsAttr>,
    pub files: Vec<FileAttr>,
    pub records: Vec<RecordDecl>,
    pub types: Vec<TypeDecl>,
    pub specs: Vec<FunctionSpec>,
    pub attrs: Vec<WildAttr>,
    pub functions: Vec<FunctionDecl>,
}

#[derive(Debug)]
pub struct ModuleAttr {
    pub line: LineNum,
    pub name: String,
}
impl_node!(ModuleAttr);

#[derive(Debug)]
pub struct BehaviourAttr {
    pub line: LineNum,
    pub is_british: bool,
    pub name: String,
}
impl_node!(BehaviourAttr);

#[derive(Debug)]
pub struct ExportAttr {
    pub line: LineNum,
    pub functions: Vec<FA>,
}
impl_node!(ExportAttr);

#[derive(Debug)]
pub struct ImportAttr {
    pub line: LineNum,
    pub module: String,
    pub functions: Vec<FA>,
}
impl_node!(ImportAttr);

#[derive(Debug)]
pub struct ExportTypeAttr {
    pub line: LineNum,
    pub types: Vec<TA>,
}
impl_node!(ExportTypeAttr);

#[derive(Debug)]
pub struct CompileOptionsAttr {
    pub line: LineNum,
    pub options: eetf::Term,
}
impl_node!(CompileOptionsAttr);

#[derive(Debug)]
pub struct FileAttr {
    pub line: LineNum,
    pub original_file: String,
    pub original_line: LineNum,
}
impl_node!(FileAttr);

#[derive(Debug)]
pub struct RecordDecl {
    pub line: LineNum,
    pub name: String,
    pub fields: Vec<RecordFieldDecl>,
}
impl_node!(RecordDecl);

#[derive(Debug)]
pub struct TypeDecl {
    pub line: LineNum,
    pub is_opaque: bool,
    pub name: String,
    pub variables: Vec<Variable>,
    pub type_: Type,
}
impl_node!(TypeDecl);

#[derive(Debug)]
pub struct FunctionSpec {
    pub line: LineNum,
    pub module: Option<String>,
    pub name: String,
    pub types: Vec<FunctionType>,
}
impl_node!(FunctionSpec);

#[derive(Debug)]
pub struct WildAttr {
    pub line: LineNum,
    pub name: String,
    pub value: eetf::Term,
}
impl_node!(WildAttr);

#[derive(Debug)]
pub struct FunctionDecl {
    pub line: LineNum,
    pub name: String,
    pub arity: Arity,
    pub clauses: Vec<Clause>,
}
impl_node!(FunctionDecl);

#[derive(Debug)]
pub struct RecordFieldDecl {
    pub line: LineNum,
    pub name: String,
    pub type_: Type,
    pub default_value: Expr,
}
impl_node!(RecordFieldDecl);

#[derive(Debug)]
pub struct Type;

#[derive(Debug)]
pub struct FunctionType;

#[derive(Debug)]
pub struct Expr;

#[derive(Debug)]
pub struct Clause;

#[derive(Debug)]
pub struct Variable {
    line: LineNum,
    name: String,
}
impl_node!(Variable);
impl Variable {
    pub fn is_anonymous(&self) -> bool {
        self.name == "_"
    }
}

#[derive(Debug)]
pub struct FA {
    pub function: String,
    pub arity: Arity,
}

#[derive(Debug)]
pub struct TA {
    pub type_: String,
    pub arity: Arity,
}
