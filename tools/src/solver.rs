use crate::board::Board;
use crate::types::{Clue, Relation};
use std::collections::HashMap;

/// Backtracking CSP solver for the seating puzzle. Assigns students to seats
/// one seat at a time (in a fixed order), pruning as soon as any axiom whose
/// referenced students are both already placed is violated. This is
/// feasible even for many students because real constraints prune the tree
/// heavily, unlike full n! enumeration.
pub struct Solver {
    pub board: Board,
    names: Vec<String>,
    index_of: HashMap<String, usize>,
}

struct AxiomIdx {
    rel: Relation,
    a: usize,
    b_student: Option<usize>,
    b_row: Option<u32>,
    negated: bool,
}

#[derive(Debug)]
pub struct SolveResult {
    pub count: usize,
    pub nodes: u64,
    pub hit_cap: bool,
    pub nodes_exhausted: bool,
    pub first_solution: Option<Vec<String>>,
}

impl Solver {
    pub fn new(names: Vec<String>, board: Board) -> Self {
        assert_eq!(
            board.seat_count(),
            names.len(),
            "board size ({} seats) does not match roster size ({} students)",
            board.seat_count(),
            names.len()
        );
        let index_of = names
            .iter()
            .enumerate()
            .map(|(i, n)| (n.clone(), i))
            .collect();
        Solver {
            board,
            names,
            index_of,
        }
    }

    fn id_of(&self, name: &str) -> usize {
        *self
            .index_of
            .get(name)
            .unwrap_or_else(|| panic!("unknown name {name}"))
    }

    fn to_axioms_idx(&self, clues: &[Clue]) -> Vec<AxiomIdx> {
        clues
            .iter()
            .map(|c| {
                let a = self.id_of(&c.a);
                let (b_student, b_row) = if c.rel == Relation::InRow {
                    (None, Some(c.b_row()))
                } else {
                    (Some(self.id_of(c.b_name())), None)
                };
                AxiomIdx {
                    rel: c.rel,
                    a,
                    b_student,
                    b_row,
                    negated: c.negated,
                }
            })
            .collect()
    }

    /// nodeBudget bounds total backtracking work: a partial assignment can
    /// look locally fine but be globally infeasible, and plain
    /// chronological backtracking only discovers that once the conflicting
    /// student is finally placed - which can be almost the entire remaining
    /// subtree away. When the budget is hit, `count` is a lower bound and
    /// `nodes_exhausted` is true, rather than exploring for a very long time.
    pub fn count_solutions(&self, clues: &[Clue], cap: usize, node_budget: u64) -> SolveResult {
        let n = self.names.len();
        let axioms = self.to_axioms_idx(clues);
        let board = self.board;

        let rel_holds = |rel: Relation, a: usize, b: usize| match rel {
            Relation::NextTo => board.next_to(a, b),
            Relation::LeftOf => board.left_of(a, b),
            Relation::SameBench => board.same_bench(a, b),
            Relation::Behind => board.behind(a, b),
            Relation::InRow => unreachable!(),
        };

        // Place the most-constrained students first, so axioms get checked
        // (and prune dead branches) as close to the root as possible.
        // Without this, a student who happens to sit late in a fixed
        // placement order won't have their axioms checked until
        // near-complete assignments, making the cap-based solution count
        // meaningless as a difficulty/reduction signal.
        let mut ref_count = vec![0u32; n];
        for ax in &axioms {
            ref_count[ax.a] += 1;
            if let Some(b) = ax.b_student {
                ref_count[b] += 1;
            }
        }
        let mut visit_order: Vec<usize> = (0..n).collect();
        visit_order.sort_by(|&x, &y| ref_count[y].cmp(&ref_count[x]).then(x.cmp(&y)));

        let mut seat_of_student: Vec<Option<usize>> = vec![None; n];
        let mut student_at_seat: Vec<Option<usize>> = vec![None; n];
        let mut used = vec![false; n];

        let axiom_holds = |ax: &AxiomIdx, seat_of_student: &[Option<usize>]| -> bool {
            let sa = seat_of_student[ax.a].unwrap();
            let value = match ax.rel {
                Relation::InRow => board.in_row(sa, ax.b_row.unwrap()),
                _ => rel_holds(ax.rel, sa, seat_of_student[ax.b_student.unwrap()].unwrap()),
            };
            value != ax.negated
        };

        // Forward check for an axiom with exactly one side seated: is there
        // any still-unused seat the other side could occupy that would
        // satisfy it? Without this, a doomed assignment isn't detected
        // until the conflicting student is finally placed.
        let can_still_satisfy =
            |ax: &AxiomIdx, known_seat: usize, unknown_is_a: bool, student_at_seat: &[Option<usize>]| -> bool {
                for seat_idx in 0..n {
                    if student_at_seat[seat_idx].is_some() {
                        continue;
                    }
                    let sa = if unknown_is_a { seat_idx } else { known_seat };
                    let value = match ax.rel {
                        Relation::InRow => board.in_row(sa, ax.b_row.unwrap()),
                        _ => {
                            let sb = if unknown_is_a { known_seat } else { seat_idx };
                            rel_holds(ax.rel, sa, sb)
                        }
                    };
                    if value != ax.negated {
                        return true;
                    }
                }
                false
            };

        let check_axiom = |ax: &AxiomIdx,
                            seat_of_student: &[Option<usize>],
                            student_at_seat: &[Option<usize>]|
         -> bool {
            let a_seat = seat_of_student[ax.a];
            let a_ready = a_seat.is_some();
            let b_ready = ax.rel == Relation::InRow || seat_of_student[ax.b_student.unwrap()].is_some();
            if a_ready && b_ready {
                return axiom_holds(ax, seat_of_student);
            }
            if a_ready {
                return can_still_satisfy(ax, a_seat.unwrap(), false, student_at_seat);
            }
            if b_ready {
                // `known_seat` is ignored inside `can_still_satisfy` for
                // `InRow` (there's no "b student", just a row number), so
                // it's fine to pass a placeholder there.
                let known_seat = match ax.rel {
                    Relation::InRow => 0,
                    _ => seat_of_student[ax.b_student.unwrap()].unwrap(),
                };
                return can_still_satisfy(ax, known_seat, true, student_at_seat);
            }
            true // neither side placed yet, nothing to check
        };

        let mut count = 0usize;
        let mut nodes = 0u64;
        let mut first_solution: Option<Vec<String>> = None;
        let mut nodes_exhausted = false;

        fn backtrack(
            seat_index: usize,
            n: usize,
            cap: usize,
            budget: u64,
            visit_order: &[usize],
            axioms: &[AxiomIdx],
            used: &mut [bool],
            seat_of_student: &mut [Option<usize>],
            student_at_seat: &mut [Option<usize>],
            count: &mut usize,
            nodes: &mut u64,
            nodes_exhausted: &mut bool,
            first_solution: &mut Option<Vec<String>>,
            names: &[String],
            check_axiom: &dyn Fn(&AxiomIdx, &[Option<usize>], &[Option<usize>]) -> bool,
        ) {
            if *count >= cap || *nodes_exhausted {
                return;
            }
            if seat_index == n {
                *count += 1;
                if first_solution.is_none() {
                    *first_solution = Some(
                        student_at_seat
                            .iter()
                            .map(|s| names[s.unwrap()].clone())
                            .collect(),
                    );
                }
                return;
            }
            for &s in visit_order {
                if used[s] {
                    continue;
                }
                *nodes += 1;
                if *nodes >= budget {
                    *nodes_exhausted = true;
                    return;
                }
                used[s] = true;
                seat_of_student[s] = Some(seat_index);
                student_at_seat[seat_index] = Some(s);

                let ok = axioms.iter().all(|ax| check_axiom(ax, seat_of_student, student_at_seat));
                if ok {
                    backtrack(
                        seat_index + 1,
                        n,
                        cap,
                        budget,
                        visit_order,
                        axioms,
                        used,
                        seat_of_student,
                        student_at_seat,
                        count,
                        nodes,
                        nodes_exhausted,
                        first_solution,
                        names,
                        check_axiom,
                    );
                }
                used[s] = false;
                seat_of_student[s] = None;
                student_at_seat[seat_index] = None;
                if *count >= cap || *nodes_exhausted {
                    return;
                }
            }
        }

        backtrack(
            0,
            n,
            cap,
            node_budget,
            &visit_order,
            &axioms,
            &mut used,
            &mut seat_of_student,
            &mut student_at_seat,
            &mut count,
            &mut nodes,
            &mut nodes_exhausted,
            &mut first_solution,
            &self.names,
            &check_axiom,
        );

        SolveResult {
            count,
            nodes,
            hit_cap: count >= cap,
            nodes_exhausted,
            first_solution,
        }
    }
}
