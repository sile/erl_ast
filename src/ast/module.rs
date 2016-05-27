use std::io;
use std::path::Path;
use beam_file;
use beam_file::chunk::Chunk;
use eetf;

use result::BeamParseResult;
use error::BeamParseError;

#[derive(Debug)]
pub struct Module {
    pub code: eetf::Term,
}

impl Module {
    pub fn from_beam_file<P: AsRef<Path>>(path: P) -> BeamParseResult<Self> {
        let beam = try!(beam_file::RawBeamFile::from_file(path));
        let chunk = try!(beam.chunks
            .into_iter()
            .find(|c| c.id() == b"Abst")
            .ok_or(BeamParseError::NoDebugInfo));
        let abstract_code = try!(eetf::Term::decode(io::Cursor::new(&chunk.data)));
        // let _forms = try!(Matcher::pattern(Tuple2(Atom("raw_abstract_v1"), Any))
        //     .apply(abstract_code.clone())
        //     .ok_or(BeamParseError::UnknownAbstractCode));
        Ok(Module { code: abstract_code })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let module = Module::from_beam_file("src/testdata/test.beam").unwrap();
        println!("{}", module.code);
        assert!(false)
    }
}
