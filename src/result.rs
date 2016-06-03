use error;

pub type FromBeamResult<T> = Result<T, error::FromBeamError>;
