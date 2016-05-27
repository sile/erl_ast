use error;

pub type BeamParseResult<T> = Result<T, error::BeamParseError>;
