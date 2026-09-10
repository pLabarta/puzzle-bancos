use crate::board::Board;
use crate::solver::Solver;
use crate::types::{Clue, ClueTarget, Relation, Roster};

fn permutations(items: &[String]) -> Vec<Vec<String>> {
    if items.len() <= 1 {
        return vec![items.to_vec()];
    }
    let mut result = Vec::new();
    for i in 0..items.len() {
        let mut rest = items.to_vec();
        let picked = rest.remove(i);
        for mut p in permutations(&rest) {
            p.insert(0, picked.clone());
            result.push(p);
        }
    }
    result
}

fn col_of(perm: &[String], name: &str) -> usize {
    perm.iter().position(|n| n == name).unwrap() + 1
}

fn bench_of(col: usize) -> usize {
    (col - 1) / 2
}

fn next_to(perm: &[String], a: &str, b: &str) -> bool {
    col_of(perm, a).abs_diff(col_of(perm, b)) == 1
}

fn same_bench(perm: &[String], a: &str, b: &str) -> bool {
    bench_of(col_of(perm, a)) == bench_of(col_of(perm, b))
}

/// `a` immediately left of `b`, on the same bench (see `Board::left_of`).
fn left_of(perm: &[String], a: &str, b: &str) -> bool {
    same_bench(perm, a, b) && col_of(perm, b) as i64 - col_of(perm, a) as i64 == 1
}

fn rel_value(perm: &[String], rel: Relation, a: &str, b: &str) -> bool {
    match rel {
        Relation::NextTo => next_to(perm, a, b),
        Relation::LeftOf => left_of(perm, a, b),
        Relation::SameBench => same_bench(perm, a, b),
        Relation::Behind | Relation::InRow => unreachable!(),
    }
}

pub struct RowClueResult {
    pub chosen: Vec<Clue>,
    pub remaining_count: usize,
}

/// Finds a small set of negative-only clues (NOT nextTo / NOT sameBench /
/// NOT leftOf) that narrows (or, for small rows, uniquely pins) one row's
/// left-to-right order, by steepest-descent greedy search over the row's
/// n! permutations (exact, no sampling - fine up to 8-ish per row).
pub fn find_minimal_row_clue_set(order: &[String], target_remaining: usize) -> RowClueResult {
    let stop_at = target_remaining.max(1);
    let all_perms = permutations(order);

    let mut candidates: Vec<Clue> = Vec::new();
    for i in 0..order.len() {
        for j in (i + 1)..order.len() {
            let a = &order[i];
            let b = &order[j];
            if !next_to(order, a, b) {
                candidates.push(Clue::new(Relation::NextTo, a, b.clone(), true));
            }
            if !same_bench(order, a, b) {
                candidates.push(Clue::new(Relation::SameBench, a, b.clone(), true));
            }
            if !left_of(order, a, b) {
                candidates.push(Clue::new(Relation::LeftOf, a, b.clone(), true));
            }
            if !left_of(order, b, a) {
                candidates.push(Clue::new(Relation::LeftOf, b, a.clone(), true));
            }
        }
    }

    let axiom_holds = |perm: &[String], c: &Clue| -> bool {
        rel_value(perm, c.rel, &c.a, c.b_name()) != c.negated
    };

    let mut chosen: Vec<Clue> = Vec::new();
    let mut remaining: Vec<&Vec<String>> = all_perms.iter().collect();
    let mut used = vec![false; candidates.len()];

    while remaining.len() > stop_at {
        let mut best_idx: Option<usize> = None;
        let mut best_remaining: Option<Vec<&Vec<String>>> = None;

        for (idx, cand) in candidates.iter().enumerate() {
            if used[idx] {
                continue;
            }
            let next: Vec<&Vec<String>> = remaining
                .iter()
                .copied()
                .filter(|p| axiom_holds(p, cand))
                .collect();
            let better = match &best_remaining {
                None => true,
                Some(b) => next.len() < b.len(),
            };
            if better {
                best_remaining = Some(next);
                best_idx = Some(idx);
            }
        }

        match (best_idx, best_remaining) {
            (Some(idx), Some(next)) if next.len() < remaining.len() => {
                used[idx] = true;
                chosen.push(candidates[idx].clone());
                remaining = next;
            }
            _ => {
                break;
            }
        }
    }

    RowClueResult {
        chosen,
        remaining_count: remaining.len(),
    }
}

pub struct GridClueResult {
    pub chosen: Vec<Clue>,
    pub final_count: usize,
    pub hit_cap: bool,
}

/// Greedy set-cover style clue authoring for a full rows x cols grid. The
/// full board is far too large to enumerate permutations directly, so this
/// leans on the solver's pruned backtracking with a solution-count cap as
/// the objective to minimize. At each step, score every remaining candidate
/// clue by how many solutions remain when it's added to the chosen set
/// (capped for speed), and greedily take whichever clue shrinks the
/// solution count the most. Stop at either a unique solution or a hard
/// ceiling of `max_axioms` clues.
pub fn author_grid_clues(
    roster: &Roster,
    max_axioms: usize,
    cap: usize,
    node_budget: u64,
) -> GridClueResult {
    let rows = roster.row_count();
    let cols = roster.cols_per_row();
    let board = Board::new(rows, cols);
    let names = roster.names();
    let solver = Solver::new(names.clone(), board);

    fn pos_of(roster: &Roster, name: &str) -> (usize, usize) {
        for (r, row) in roster.rows.iter().enumerate() {
            if let Some(c) = row.iter().position(|n| n == name) {
                return (r + 1, c + 1);
            }
        }
        panic!("unknown name {name}");
    }
    let bench_of = |col: usize| (col - 1) / 2;

    let truth_next_to = |a: &str, b: &str| {
        let (ra, ca) = pos_of(roster, a);
        let (rb, cb) = pos_of(roster, b);
        ra == rb && ca.abs_diff(cb) == 1
    };
    let truth_same_bench = |a: &str, b: &str| {
        let (ra, ca) = pos_of(roster, a);
        let (rb, cb) = pos_of(roster, b);
        ra == rb && bench_of(ca) == bench_of(cb)
    };
    // `a` immediately left of `b`, on the same bench (see Board::left_of).
    let truth_left_of = |a: &str, b: &str| {
        let (_, ca) = pos_of(roster, a);
        let (_, cb) = pos_of(roster, b);
        truth_same_bench(a, b) && cb as i64 - ca as i64 == 1
    };
    let truth_behind = |a: &str, b: &str| {
        let (ra, ca) = pos_of(roster, a);
        let (rb, cb) = pos_of(roster, b);
        ra == rb + 1 && ca == cb
    };
    let truth_in_row = |a: &str, row: u32| pos_of(roster, a).0 as u32 == row;

    // Build the candidate pool: for every relevant pair/row, add the clue
    // whose truth value matches the target arrangement (positive if true,
    // negated if false). This guarantees every candidate is consistent with
    // the target.
    let mut candidates: Vec<Clue> = Vec::new();
    for row in &roster.rows {
        for i in 0..row.len() {
            for j in 0..row.len() {
                if i == j {
                    continue;
                }
                let a = &row[i];
                let b = &row[j];
                if i < j {
                    candidates.push(Clue::new(
                        Relation::NextTo,
                        a,
                        b.clone(),
                        !truth_next_to(a, b),
                    ));
                    candidates.push(Clue::new(
                        Relation::SameBench,
                        a,
                        b.clone(),
                        !truth_same_bench(a, b),
                    ));
                }
                candidates.push(Clue::new(Relation::LeftOf, a, b.clone(), !truth_left_of(a, b)));
            }
        }
    }
    for r in 1..roster.rows.len() {
        let back = &roster.rows[r];
        let front = &roster.rows[r - 1];
        for a in back {
            for b in front {
                candidates.push(Clue::new(Relation::Behind, a, b.clone(), !truth_behind(a, b)));
            }
        }
    }
    for a in &names {
        for row in 1..=(rows as u32) {
            candidates.push(Clue::new(
                Relation::InRow,
                a,
                ClueTarget::Row(row),
                !truth_in_row(a, row),
            ));
        }
    }

    // Dedupe defensively.
    let mut seen = std::collections::HashSet::new();
    candidates.retain(|c| {
        let key = format!("{:?}|{}|{}|{}", c.rel, c.a, c.b, c.negated);
        seen.insert(key)
    });

    let mut chosen: Vec<Clue> = Vec::new();
    let mut remaining_pool = candidates;
    let mut last_result = solver.count_solutions(&[], cap, node_budget);
    let mut hit_cap = last_result.hit_cap;

    while chosen.len() < max_axioms && last_result.count != 1 {
        let mut best_idx: Option<usize> = None;
        let mut best_score: Option<usize> = None;
        let mut best_negated = true;

        for (idx, cand) in remaining_pool.iter().enumerate() {
            let mut trial = chosen.clone();
            trial.push(cand.clone());
            let result = solver.count_solutions(&trial, cap, node_budget);
            // A candidate whose node budget got exhausted before resolving
            // is not reliably scored (its count is only a lower bound on a
            // search that stalled) - treat it as worse than any candidate
            // that resolved cleanly.
            let score = if result.nodes_exhausted {
                usize::MAX
            } else {
                result.count
            };
            let is_better = match best_score {
                None => true,
                Some(b) => {
                    score < b || (score == b && !cand.negated && best_negated)
                }
            };
            if is_better {
                best_score = Some(score);
                best_idx = Some(idx);
                best_negated = cand.negated;
            }
            if let Some(s) = best_score {
                if s <= 1 {
                    break; // can't do better than unique
                }
            }
        }

        let Some(idx) = best_idx else {
            break;
        };
        let cand = remaining_pool.remove(idx);
        let mut trial = chosen.clone();
        trial.push(cand.clone());
        last_result = solver.count_solutions(&trial, cap, node_budget);
        hit_cap = last_result.hit_cap;
        chosen.push(cand);
    }

    GridClueResult {
        chosen,
        final_count: last_result.count,
        hit_cap,
    }
}
