use std::io;
use std::fmt;
use std::error;
use std::convert;
use beam_file;
use eetf;

#[derive(Debug)]
pub enum FromBeamError {
    Io(io::Error),
    BeamFile(beam_file::Error),
    TermDecode(eetf::DecodeError),
    NoDebugInfo,
    NoModuleAttribute,
    UnexpectedTerm(Vec<Unmatched>),
}
impl error::Error for FromBeamError {
    fn description(&self) -> &str {
        match *self {
            FromBeamError::Io(ref x) => x.description(),
            FromBeamError::BeamFile(ref x) => x.description(),
            FromBeamError::TermDecode(ref x) => x.description(),
            FromBeamError::NoDebugInfo => "No debug information",
            FromBeamError::NoModuleAttribute => "No module attribute",
            FromBeamError::UnexpectedTerm(_) => "Unexpected term",
        }
    }
    fn cause(&self) -> Option<&error::Error> {
        match *self {
            FromBeamError::Io(ref x) => x.cause(),
            FromBeamError::BeamFile(ref x) => x.cause(),
            FromBeamError::TermDecode(ref x) => x.cause(),
            _ => None,
        }
    }
}
impl fmt::Display for FromBeamError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            FromBeamError::Io(ref x) => x.fmt(f),
            FromBeamError::BeamFile(ref x) => x.fmt(f),
            FromBeamError::TermDecode(ref x) => x.fmt(f),
            FromBeamError::NoDebugInfo => write!(f, "The beam has no debug information"),
            FromBeamError::NoModuleAttribute => write!(f, "No module attribute"),
            FromBeamError::UnexpectedTerm(ref trace) => {
                try!(write!(f, "Unexpected term: ["));
                let limit = 3;
                for (i, e) in trace.iter().take(limit).enumerate() {
                    if i != 0 {
                        try!(write!(f, ","));
                    }
                    try!(write!(f, "{}", e));
                }
                if trace.len() > limit {
                    try!(write!(f, " ..{}..", trace.len() - limit));
                }
                try!(write!(f, "]"));
                Ok(())
            }
        }
    }
}
impl convert::From<io::Error> for FromBeamError {
    fn from(x: io::Error) -> Self {
        FromBeamError::Io(x)
    }
}
impl convert::From<beam_file::Error> for FromBeamError {
    fn from(x: beam_file::Error) -> Self {
        FromBeamError::BeamFile(x)
    }
}
impl convert::From<eetf::DecodeError> for FromBeamError {
    fn from(x: eetf::DecodeError) -> Self {
        FromBeamError::TermDecode(x)
    }
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
