use crate::types::{Clue, Relation, Roster};

fn strip_accents(c: char) -> char {
    match c {
        'á' | 'à' | 'ä' | 'â' => 'a',
        'Á' | 'À' | 'Ä' | 'Â' => 'A',
        'é' | 'è' | 'ë' | 'ê' => 'e',
        'É' | 'È' | 'Ë' | 'Ê' => 'E',
        'í' | 'ì' | 'ï' | 'î' => 'i',
        'Í' | 'Ì' | 'Ï' | 'Î' => 'I',
        'ó' | 'ò' | 'ö' | 'ô' => 'o',
        'Ó' | 'Ò' | 'Ö' | 'Ô' => 'O',
        'ú' | 'ù' | 'ü' | 'û' => 'u',
        'Ú' | 'Ù' | 'Ü' | 'Û' => 'U',
        'ñ' => 'n',
        'Ñ' => 'N',
        other => other,
    }
}

/// A valid, lowercase Elm identifier for a student's name, e.g. "Nicolás" ->
/// "nicolas". Used both for the `StudentId` constant and as the argument in
/// generated axioms.
pub fn elm_ident(name: &str) -> String {
    let mut out: String = name.chars().map(strip_accents).collect();
    if let Some(first) = out.get_mut(0..1) {
        first.make_ascii_lowercase();
    }
    out
}

fn hsl_to_hex(h: f64, s: f64, l: f64) -> String {
    let c = (1.0 - (2.0 * l - 1.0).abs()) * s;
    let x = c * (1.0 - ((h / 60.0) % 2.0 - 1.0).abs());
    let m = l - c / 2.0;
    let (r1, g1, b1) = match h as u32 {
        0..=59 => (c, x, 0.0),
        60..=119 => (x, c, 0.0),
        120..=179 => (0.0, c, x),
        180..=239 => (0.0, x, c),
        240..=299 => (x, 0.0, c),
        _ => (c, 0.0, x),
    };
    let to_byte = |v: f64| ((v + m) * 255.0).round().clamp(0.0, 255.0) as u8;
    format!("#{:02x}{:02x}{:02x}", to_byte(r1), to_byte(g1), to_byte(b1))
}

/// Evenly spaced, readable colors for however many students the roster has.
fn palette(n: usize) -> Vec<String> {
    (0..n)
        .map(|i| {
            let hue = (i as f64 * 360.0 / n as f64) % 360.0;
            hsl_to_hex(hue, 0.55, 0.55)
        })
        .collect()
}

/// Renders `src/Puzzle.elm`'s module body (students + axioms) for the given
/// roster and clue set.
pub fn emit_puzzle_elm(roster: &Roster, clues: &[Clue]) -> String {
    let names = roster.names();
    let colors = palette(names.len());

    let mut out = String::new();
    out.push_str("module Puzzle exposing (axioms, students)\n\n");
    out.push_str("import Types exposing (Axiom, Relation(..), Student)\n\n\n");

    out.push_str("students : List Student\n");
    out.push_str("students =\n");
    for (i, name) in names.iter().enumerate() {
        let bracket = if i == 0 { "[ " } else { ", " };
        out.push_str(&format!(
            "    {}{{ id = {}, name = \"{}\", color = \"{}\" }}\n",
            bracket, i, name, colors[i]
        ));
    }
    out.push_str("    ]\n\n\n");

    for (i, name) in names.iter().enumerate() {
        out.push_str(&format!("{} : Int\n{} =\n    {}\n\n\n", elm_ident(name), elm_ident(name), i));
    }

    out.push_str("axioms : List Axiom\n");
    out.push_str("axioms =\n");
    for (i, clue) in clues.iter().enumerate() {
        let bracket = if i == 0 { "[ " } else { ", " };
        let rel = match clue.rel {
            Relation::NextTo => "NextTo",
            Relation::LeftOf => "LeftOf",
            Relation::SameBench => "SameBench",
            Relation::Behind => "Behind",
            Relation::InRow => "InRow",
        };
        let a = elm_ident(&clue.a);
        let b = if clue.rel == Relation::InRow {
            clue.b_row().to_string()
        } else {
            elm_ident(clue.b_name())
        };
        let negated = if clue.negated { "True" } else { "False" };
        out.push_str(&format!(
            "    {}Axiom ({} {} {}) {}\n",
            bracket, rel, a, b, negated
        ));
    }
    out.push_str("    ]\n");

    out
}
