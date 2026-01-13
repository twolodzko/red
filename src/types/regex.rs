use crate::Error;
use serde::Serialize;

#[derive(Debug, Clone)]
pub struct Regex(regex::Regex);

impl Regex {
    pub(crate) fn is_match(&self, s: &str) -> bool {
        self.0.is_match(s)
    }

    pub(crate) fn capture_names(&self) -> impl Iterator<Item = &str> {
        self.0.capture_names().flatten()
    }

    pub(crate) fn captures<'a>(&self, haystack: &'a str) -> Option<regex::Captures<'a>> {
        self.0.captures(haystack)
    }

    pub(crate) fn replacen<'a>(&self, haystack: &'a str, rep: &'a str, limit: usize) -> String {
        self.0.replacen(haystack, limit, rep).to_string()
    }

    pub(crate) fn is_capturing(&self) -> bool {
        self.0.captures_len() > 1
    }
}

impl std::str::FromStr for Regex {
    type Err = Error;

    fn from_str(s: &str) -> std::result::Result<Regex, Self::Err> {
        let regex = regex::Regex::new(s)?;
        Ok(Regex(regex))
    }
}

impl PartialEq for Regex {
    fn eq(&self, other: &Self) -> bool {
        self.0.as_str() == other.0.as_str()
    }
}

impl PartialOrd for Regex {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.0.as_str().partial_cmp(other.0.as_str())
    }
}

impl std::fmt::Display for Regex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "/{}/", self.0.as_str())
    }
}

impl Serialize for Regex {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.0.as_str().serialize(serializer)
    }
}

impl std::hash::Hash for Regex {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.as_str().hash(state);
    }
}
