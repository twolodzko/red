use std::str::Chars;

pub(crate) struct Reader<'a>(Chars<'a>);

impl<'a> From<&'a String> for Reader<'a> {
    fn from(value: &'a String) -> Self {
        Reader(value.chars())
    }
}

impl<'a> Iterator for Reader<'a> {
    type Item = (String, String);

    fn next(&mut self) -> Option<Self::Item> {
        // https://betterstack.com/community/guides/logging/logfmt/
        let mut key = String::new();
        let mut val = String::new();

        // key
        loop {
            let Some(c) = self.0.next() else {
                break;
            };
            if c.is_whitespace() {
                // there was no value after key, start again
                key.clear();
                continue;
            }
            if c != '=' {
                // key
                key.push(c);
            } else {
                // value
                if key.is_empty() {
                    continue;
                }
                let Some(c) = self.0.next() else {
                    break;
                };
                if c.is_whitespace() {
                    // empty value
                    break;
                }
                // quoted value
                if c == '"' {
                    loop {
                        let Some(c) = self.0.next() else {
                            return Some((key, val));
                        };
                        // delimiter
                        if c == '"' {
                            return Some((key, val));
                        }
                        if c == '\\' {
                            let Some(c) = self.0.next() else {
                                return Some((key, val));
                            };
                            // handle escaped quotes
                            if c != '"' {
                                val.push('\\');
                            }
                            val.push(c);
                        } else {
                            val.push(c);
                        }
                    }
                // unquoted value
                } else {
                    val.push(c);
                    loop {
                        let Some(c) = self.0.next() else {
                            return Some((key, val));
                        };
                        // delimiter
                        if c.is_whitespace() {
                            return Some((key, val));
                        }
                        val.push(c);
                    }
                }
            }
        }

        if key.is_empty() && val.is_empty() {
            None
        } else {
            Some((key, val))
        }
    }
}

#[cfg(test)]
mod tests {

    #[test]
    fn parse() {
        let line = String::from(
            r#"garbage foo=bar ignore empty= msg="hello, world!" num=42 float=3.14 quote="[[\"]]" =trash final="#,
        );
        let reader = super::Reader::from(&line);
        let result: Vec<(String, String)> = reader.collect();
        let expected: Vec<(_, _)> = vec![
            ("foo", "bar"),
            ("empty", ""),
            ("msg", "hello, world!"),
            ("num", "42"),
            ("float", "3.14"),
            ("quote", "[[\"]]"),
            ("final", ""),
        ]
        .iter()
        .map(|(k, v)| (k.to_string(), v.to_string()))
        .collect();
        assert_eq!(result, expected);
    }
}
