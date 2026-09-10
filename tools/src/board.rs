/// Board geometry: `rows` rows of `cols_per_row` seats each, addressed by a
/// flat row-major seat index 0..rows*cols_per_row-1. Row 1 is the front row.
/// Benches hold 2 seats each.
#[derive(Debug, Clone, Copy)]
pub struct Board {
    pub rows: usize,
    pub cols_per_row: usize,
}

impl Board {
    pub fn new(rows: usize, cols_per_row: usize) -> Self {
        Board { rows, cols_per_row }
    }

    pub fn seat_count(&self) -> usize {
        self.rows * self.cols_per_row
    }

    pub fn row_of(&self, seat: usize) -> u32 {
        (seat / self.cols_per_row) as u32 + 1
    }

    pub fn col_of(&self, seat: usize) -> u32 {
        (seat % self.cols_per_row) as u32 + 1
    }

    pub fn bench_of(&self, seat: usize) -> usize {
        (seat % self.cols_per_row) / 2
    }

    pub fn next_to(&self, a: usize, b: usize) -> bool {
        self.row_of(a) == self.row_of(b) && self.col_of(a).abs_diff(self.col_of(b)) == 1
    }

    pub fn same_bench(&self, a: usize, b: usize) -> bool {
        self.row_of(a) == self.row_of(b) && self.bench_of(a) == self.bench_of(b)
    }

    pub fn behind(&self, a: usize, b: usize) -> bool {
        self.row_of(a) == self.row_of(b) + 1 && self.col_of(a) == self.col_of(b)
    }

    /// `a` sits immediately to the left of `b`, on the same two-seat bench.
    /// Unlike `next_to`, this does NOT hold across the aisle between
    /// benches - "immediately to the left" means desk-mates, not just
    /// adjacent columns. Matches `Axioms.leftOf` in the Elm app.
    pub fn left_of(&self, a: usize, b: usize) -> bool {
        self.same_bench(a, b) && self.col_of(b) as i64 - self.col_of(a) as i64 == 1
    }

    pub fn in_row(&self, a: usize, row: u32) -> bool {
        self.row_of(a) == row
    }
}
