pub(crate) mod code_reader;
#[allow(clippy::module_inception)]
pub(crate) mod parser;
pub(crate) mod tokenizer;

pub use code_reader::{FileReader, StringReader};
pub use parser::Parser;
pub(crate) use tokenizer::Tokenizer;

use std::path::PathBuf;

impl From<String> for Parser<StringReader> {
    fn from(value: String) -> Parser<StringReader> {
        let reader = StringReader::from(value);
        let tokenizer = Tokenizer::new(reader);
        Parser(tokenizer)
    }
}

impl TryFrom<&PathBuf> for Parser<FileReader> {
    type Error = crate::Error;

    fn try_from(value: &PathBuf) -> std::result::Result<Self, Self::Error> {
        let reader = FileReader::try_from(value)?;
        let tokenizer = Tokenizer::new(reader);
        Ok(Parser(tokenizer))
    }
}
