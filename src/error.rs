use std::io;
use std::fmt;
use std::error;
use std::convert;
use beam_file;
use eetf;

#[derive(Debug)]
pub enum BeamParseError {
    Io(io::Error),
    BeamFile(beam_file::Error),
    TermDecode(eetf::DecodeError),
    NoDebugInfo,
}
impl error::Error for BeamParseError {
    fn description(&self) -> &str {
        match *self {
            BeamParseError::Io(ref x) => x.description(),
            BeamParseError::BeamFile(ref x) => x.description(),
            BeamParseError::TermDecode(ref x) => x.description(),
            BeamParseError::NoDebugInfo => "No debug information",
        }
    }
    fn cause(&self) -> Option<&error::Error> {
        match *self {
            BeamParseError::Io(ref x) => x.cause(),
            BeamParseError::BeamFile(ref x) => x.cause(),
            BeamParseError::TermDecode(ref x) => x.cause(),
            _ => None,
        }
    }
}
impl fmt::Display for BeamParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            BeamParseError::Io(ref x) => x.fmt(f),
            BeamParseError::BeamFile(ref x) => x.fmt(f),
            BeamParseError::TermDecode(ref x) => x.fmt(f),
            BeamParseError::NoDebugInfo => write!(f, "The beam has no debug information"),
        }
    }
}
impl convert::From<io::Error> for BeamParseError {
    fn from(x: io::Error) -> Self {
        BeamParseError::Io(x)
    }
}
impl convert::From<beam_file::Error> for BeamParseError {
    fn from(x: beam_file::Error) -> Self {
        BeamParseError::BeamFile(x)
    }
}
impl convert::From<eetf::DecodeError> for BeamParseError {
    fn from(x: eetf::DecodeError) -> Self {
        BeamParseError::TermDecode(x)
    }
}
