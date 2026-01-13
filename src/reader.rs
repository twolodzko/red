use std::{
    fs::File,
    io::{self, BufRead, BufReader, Stdin},
};

pub struct Reader<'a> {
    reader: Box<dyn BufRead + 'a>,
}

impl<'a> Reader<'a> {
    pub fn lines(self) -> impl Iterator<Item = io::Result<String>> {
        self.reader.lines()
    }
}

impl<'a> From<Stdin> for Reader<'a> {
    fn from(value: Stdin) -> Self {
        let reader = Box::new(BufReader::new(value));
        Self { reader }
    }
}

impl<'a> From<File> for Reader<'a> {
    fn from(value: File) -> Self {
        let reader = Box::new(BufReader::new(value));
        Self { reader }
    }
}

impl<'a> From<&'a [u8]> for Reader<'a> {
    fn from(value: &'a [u8]) -> Self {
        let reader = Box::new(BufReader::new(value));
        Self { reader }
    }
}
