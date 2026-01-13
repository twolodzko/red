use std::str::Chars;

pub(crate) struct Reader<'a>(Chars<'a>);

impl<'a> From<&'a String> for Reader<'a> {
    fn from(value: &'a String) -> Self {
        Reader(value.chars())
    }
}

impl<'a> Iterator for Reader<'a> {
    type Item = String;

    fn next(&mut self) -> Option<Self::Item> {
        // https://www.rfc-editor.org/rfc/rfc4180
        let mut acc = String::new();
        let c = self.0.next()?;
        if c == '"' {
            // quoted field
            loop {
                let Some(c) = self.0.next() else {
                    break;
                };
                if c == '"' {
                    match self.0.next() {
                        Some(',') => break,
                        Some('"') => acc.push(c),
                        _ => {
                            // edge case: waiting for delimiter
                            // anything else than delimiter is not in the standard
                            // treating as a honest mistake and discarding
                            loop {
                                if let Some(',') | None = self.0.next() {
                                    break;
                                }
                            }
                            break;
                        }
                    }
                } else {
                    acc.push(c);
                }
            }
        } else {
            // unquoted field
            acc.push(c);
            loop {
                let Some(c) = self.0.next() else {
                    break;
                };
                if c == ',' {
                    break;
                }
                acc.push(c);
            }
        }
        Some(acc)
    }
}

#[cfg(test)]
mod tests {

    #[test]
    fn parse() {
        let line = String::from("foo,bar,\"hello, world!\",42,baz");
        let reader = super::Reader::from(&line);
        let result: Vec<String> = reader.collect();
        assert_eq!(result, vec!["foo", "bar", "hello, world!", "42", "baz"]);
    }
}
