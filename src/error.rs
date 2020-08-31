use failure::Fail;

use pchomp::ParseError;

#[derive(Debug, Fail)]
#[fail(
    display = "Error in file {:?} at {}:{}: `{}`",
    filename, line, col, kind
)]
pub struct JavaParseError {
    pub line: usize,
    pub col: usize,
    pub byte: usize,
    pub filename: Option<String>,
    pub kind: JavaParseErrorKind,
}

#[derive(Debug, Fail)]
#[fail(display = "Internal error type")]
pub(crate) struct InnerParseError {
    pub byte: usize,
    pub kind: JavaParseErrorKind,
}

#[derive(Debug, Fail)]
pub enum JavaParseErrorKind {
    #[fail(display = "Parse error")]
    ParseError(#[cause] ParseError),

    #[fail(display = "Unknown error: parser line {}", _0)]
    UnknownError(u32),
}

impl From<ParseError> for InnerParseError {
    fn from(e: ParseError) -> InnerParseError {
        InnerParseError {
            byte: e.byte,
            kind: JavaParseErrorKind::ParseError(e),
        }
    }
}

