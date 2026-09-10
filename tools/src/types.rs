use serde::{Deserialize, Serialize};

/// A classroom layout: rows of named students, front row first.
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct Roster {
    pub rows: Vec<Vec<String>>,
}

impl Roster {
    pub fn names(&self) -> Vec<String> {
        self.rows.iter().flatten().cloned().collect()
    }

    pub fn row_count(&self) -> usize {
        self.rows.len()
    }

    pub fn cols_per_row(&self) -> usize {
        self.rows.first().map(|r| r.len()).unwrap_or(0)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub enum Relation {
    NextTo,
    LeftOf,
    SameBench,
    Behind,
    InRow,
}

/// A clue about the seating. `b` is a student name for every relation
/// except `InRow`, where it is a 1-indexed row number.
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct Clue {
    pub rel: Relation,
    pub a: String,
    pub b: serde_json::Value,
    pub negated: bool,
}

impl Clue {
    pub fn new(rel: Relation, a: &str, b: impl Into<ClueTarget>, negated: bool) -> Self {
        let b = match b.into() {
            ClueTarget::Name(n) => serde_json::Value::String(n),
            ClueTarget::Row(r) => serde_json::Value::from(r),
        };
        Clue {
            rel,
            a: a.to_string(),
            b,
            negated,
        }
    }

    pub fn b_name(&self) -> &str {
        self.b.as_str().expect("clue.b is not a name")
    }

    pub fn b_row(&self) -> u32 {
        self.b.as_u64().expect("clue.b is not a row number") as u32
    }
}

pub enum ClueTarget {
    Name(String),
    Row(u32),
}

impl From<&str> for ClueTarget {
    fn from(s: &str) -> Self {
        ClueTarget::Name(s.to_string())
    }
}

impl From<String> for ClueTarget {
    fn from(s: String) -> Self {
        ClueTarget::Name(s)
    }
}

impl From<u32> for ClueTarget {
    fn from(r: u32) -> Self {
        ClueTarget::Row(r)
    }
}
