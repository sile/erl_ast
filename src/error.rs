use beam_file;
use std::convert;
use std::fmt;
use std::io;

#[derive(Debug, thiserror::Error)]
pub enum FromBeamError {
    #[error("I/O error")]
    Io(#[from] io::Error),

    #[error("beam file parse error")]
    BeamFile(#[from] beam_file::Error),

    #[error("erlang term decoding error")]
    TermDecode(#[from] eetf::DecodeError),

    #[error("no debug information")]
    NoDebugInfo,

    #[error("no module attribute")]
    NoModuleAttribute,

    #[error("unexpected terms: {:?}", .0.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(","))]
    UnexpectedTerm(Vec<Unmatched>),
}
impl<'a> convert::From<eetf::pattern::Unmatch<'a>> for FromBeamError {
    fn from(x: eetf::pattern::Unmatch<'a>) -> Self {
        use std::ops::Deref;
        let mut trace = Vec::new();
        let mut curr = Some(&x);
        while let Some(x) = curr {
            trace.push(Unmatched {
                value: x.input.clone(),
                pattern: format!("{:?}", x.pattern),
            });
            curr = x.cause.as_ref().map(|x| x.deref());
        }
        trace.reverse();
        FromBeamError::UnexpectedTerm(trace)
    }
}

#[derive(Debug)]
pub struct Unmatched {
    pub value: eetf::Term,
    pub pattern: String,
}
impl fmt::Display for Unmatched {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{{pattern:{}, value:{}}}", self.pattern, self.value)
    }
}
