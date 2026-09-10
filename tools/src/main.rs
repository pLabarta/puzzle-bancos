mod author;
mod board;
mod elmgen;
mod solver;
mod types;

use board::Board;
use clap::{Parser, Subcommand};
use solver::Solver;
use std::fs;
use std::path::PathBuf;
use types::{Clue, Roster};

#[derive(Parser)]
#[command(
    name = "puzzle-tools",
    about = "Authoring and verification tools for the seating-puzzle Elm app"
)]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    /// Count how many seatings satisfy a clue set.
    Solve {
        #[arg(long)]
        roster: PathBuf,
        #[arg(long)]
        clues: PathBuf,
        #[arg(long, default_value_t = 10)]
        cap: usize,
        #[arg(long, default_value_t = 200_000)]
        budget: u64,
    },
    /// Verify a clue set has exactly one solution; exits non-zero otherwise.
    Verify {
        #[arg(long)]
        roster: PathBuf,
        #[arg(long)]
        clues: PathBuf,
        #[arg(long, default_value_t = 10)]
        cap: usize,
        #[arg(long, default_value_t = 200_000)]
        budget: u64,
    },
    /// Find a minimal negative-only clue set that pins one row's
    /// left-to-right order, for every row in the roster.
    AuthorRow {
        #[arg(long)]
        roster: PathBuf,
        #[arg(long, default_value_t = 1)]
        target_remaining: usize,
        #[arg(long)]
        out: PathBuf,
    },
    /// Greedily author a full-grid clue set (across all rows at once) that
    /// pins the roster's arrangement as the unique solution.
    AuthorGrid {
        #[arg(long)]
        roster: PathBuf,
        #[arg(long, default_value_t = 40)]
        max_axioms: usize,
        #[arg(long, default_value_t = 600)]
        cap: usize,
        #[arg(long, default_value_t = 200_000)]
        budget: u64,
        #[arg(long)]
        out: PathBuf,
    },
    /// Render a roster + clue set as `src/Puzzle.elm`-ready Elm source.
    EmitElm {
        #[arg(long)]
        roster: PathBuf,
        #[arg(long)]
        clues: PathBuf,
        #[arg(long)]
        out: Option<PathBuf>,
    },
}

fn load_roster(path: &PathBuf) -> Roster {
    let text = fs::read_to_string(path).unwrap_or_else(|e| panic!("reading {path:?}: {e}"));
    serde_json::from_str(&text).unwrap_or_else(|e| panic!("parsing roster {path:?}: {e}"))
}

fn load_clues(path: &PathBuf) -> Vec<Clue> {
    let text = fs::read_to_string(path).unwrap_or_else(|e| panic!("reading {path:?}: {e}"));
    serde_json::from_str(&text).unwrap_or_else(|e| panic!("parsing clues {path:?}: {e}"))
}

fn write_json<T: serde::Serialize>(path: &PathBuf, value: &T) {
    let text = serde_json::to_string_pretty(value).unwrap();
    fs::write(path, text).unwrap_or_else(|e| panic!("writing {path:?}: {e}"));
}

fn main() {
    let cli = Cli::parse();

    match cli.command {
        Command::Solve {
            roster,
            clues,
            cap,
            budget,
        } => {
            let roster = load_roster(&roster);
            let clues = load_clues(&clues);
            let board = Board::new(roster.row_count(), roster.cols_per_row());
            let solver = Solver::new(roster.names(), board);
            let result = solver.count_solutions(&clues, cap, budget);
            println!("clues: {}", clues.len());
            println!(
                "solutions (cap {}): {}{}",
                cap,
                result.count,
                if result.hit_cap { "+ (hit cap)" } else { "" }
            );
            println!(
                "nodes explored: {}{}",
                result.nodes,
                if result.nodes_exhausted {
                    " (node budget exhausted)"
                } else {
                    ""
                }
            );
            if let Some(sol) = result.first_solution {
                println!("first solution: {sol:?}");
            }
        }

        Command::Verify {
            roster,
            clues,
            cap,
            budget,
        } => {
            let roster = load_roster(&roster);
            let clues = load_clues(&clues);
            let board = Board::new(roster.row_count(), roster.cols_per_row());
            let solver = Solver::new(roster.names(), board);
            let result = solver.count_solutions(&clues, cap.max(2), budget);
            println!("clues: {}", clues.len());
            if result.count == 1 {
                println!("UNIQUE solution.");
                println!("solution: {:?}", result.first_solution.unwrap());
            } else {
                println!(
                    "NOT unique: {} solutions found{} (nodes: {}{})",
                    result.count,
                    if result.hit_cap { "+ (hit cap)" } else { "" },
                    result.nodes,
                    if result.nodes_exhausted {
                        ", node budget exhausted"
                    } else {
                        ""
                    }
                );
                std::process::exit(1);
            }
        }

        Command::AuthorRow {
            roster,
            target_remaining,
            out,
        } => {
            let roster = load_roster(&roster);
            let mut rows_out: Vec<Vec<Clue>> = Vec::new();
            for (i, row) in roster.rows.iter().enumerate() {
                let result = author::find_minimal_row_clue_set(row, target_remaining);
                println!(
                    "row {} ({}): clues = {}, remaining permutations = {}",
                    i + 1,
                    row.join(", "),
                    result.chosen.len(),
                    result.remaining_count
                );
                for c in &result.chosen {
                    println!(
                        "  {}{:?}({}, {})",
                        if c.negated { "NOT " } else { "" },
                        c.rel,
                        c.a,
                        c.b
                    );
                }
                rows_out.push(result.chosen);
            }
            write_json(&out, &serde_json::json!({ "rows": rows_out }));
            println!("\nSaved {out:?}");
        }

        Command::AuthorGrid {
            roster,
            max_axioms,
            cap,
            budget,
            out,
        } => {
            let roster = load_roster(&roster);
            let result = author::author_grid_clues(&roster, max_axioms, cap, budget);
            for (i, c) in result.chosen.iter().enumerate() {
                println!(
                    "[{}] {}{:?}({}, {})",
                    i + 1,
                    if c.negated { "NOT " } else { "" },
                    c.rel,
                    c.a,
                    c.b
                );
            }
            println!(
                "\nfinal axiom count: {}\nfinal solution count: {}{}",
                result.chosen.len(),
                result.final_count,
                if result.hit_cap { " (hit cap, NOT unique)" } else { "" }
            );
            if result.final_count == 1 {
                write_json(&out, &result.chosen);
                println!("Saved {out:?}");
            } else {
                eprintln!(
                    "FAILED to reach a unique solution within {max_axioms} axioms."
                );
                std::process::exit(1);
            }
        }

        Command::EmitElm { roster, clues, out } => {
            let roster = load_roster(&roster);
            let clues = load_clues(&clues);
            let code = elmgen::emit_puzzle_elm(&roster, &clues);
            match out {
                Some(path) => {
                    fs::write(&path, code).unwrap_or_else(|e| panic!("writing {path:?}: {e}"));
                    println!("Saved {path:?}");
                }
                None => print!("{code}"),
            }
        }
    }
}
