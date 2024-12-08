use std::ops::Range;

pub fn vert_slice(grid: &Vec<Vec<char>>, rows: Range<usize>, col: usize) -> Vec<char> {
    grid[rows].iter().map(|row| row[col]).collect()
}

pub fn lr_diag_slice(grid: &Vec<Vec<char>>, row_start: usize, col_start: usize, len: usize) -> Vec<char> {
    let mut res = Vec::new();

    for i in 0..len {
        res.push(grid[row_start + i][col_start + i]);
    }

    res
}


pub fn rl_diag_slice(grid: &Vec<Vec<char>>, row_start: usize, col_start: usize, len: usize) -> Vec<char> {
    let mut res = Vec::new();

    for i in 0..len {
        res.push(grid[row_start + i][col_start - i]);
    }

    res
}

#[cfg(test)]
mod tests {
    use crate::get_input;

    use super::*;

    #[test]
    fn d4p1() {
        let input = get_input(4);

        let grid: Vec<Vec<_>> = input.lines().map(|line| line.chars().collect()).collect();

        let mut n_xmas = 0;
        const XMAS: [char; 4] = ['X', 'M', 'A', 'S'];
        const SAMX: [char; 4] = ['S', 'A', 'M', 'X'];
        
        // Horizontal
        for row in 0..grid.len() {
            for col in 0..grid[0].len() - XMAS.len() + 1 {
                let horiz = &grid[row][col..col + XMAS.len()];
                if horiz == XMAS || horiz == SAMX {
                    println!("FOUND HORIZ {row} {col}");
                    n_xmas += 1;
                }
            }
        }

        // Vertical 
        for col in 0..grid[0].len() {
            for row in 0..grid.len() - XMAS.len() + 1 {
                let vert = vert_slice(&grid, row..row + XMAS.len(), col);
                if vert == XMAS || vert == SAMX {
                    println!("FOUND VERT {row} {col}");
                    n_xmas += 1;
                }
            }
        }

        // Left to right
        for row_start in 0..grid.len() - XMAS.len() + 1 {
            for col_start in 0..grid[0].len() - XMAS.len() + 1 {
                let lr_diag = lr_diag_slice(&grid, dbg!(row_start), dbg!(col_start), XMAS.len());
                println!("{lr_diag:?}");
                if lr_diag == XMAS || lr_diag == SAMX {
                    println!("FOUND LR {row_start} {col_start}");
                    n_xmas += 1;
                }
            }
        }

        // Right to left
        for row_start in 0..grid.len() - XMAS.len() + 1 {
            for col_start in XMAS.len() - 1..grid[0].len()  {
                let rl_diag = rl_diag_slice(&grid, row_start, col_start, XMAS.len());
                if rl_diag == XMAS || rl_diag == SAMX {
                    println!("FOUND RL {row_start} {col_start}");
                    n_xmas += 1;
                }
            }
        }

        println!("{n_xmas}")
    }

    #[test]
    fn d4p2() {
        let input = get_input(4);

        const MAS: [char; 3] = ['M', 'A', 'S'];
        const SAM: [char; 3] = ['S', 'A', 'M'];

        let grid: Vec<Vec<_>> = input.lines().map(|line| line.chars().collect()).collect();

        let mut n_x_mas = 0;
        for row in 0..grid.len() - MAS.len() + 1 {
            for col in 0..grid[0].len() - MAS.len() + 1 {
                let lr = lr_diag_slice(&grid, row, col, MAS.len());
                let rl = rl_diag_slice(&grid, row, col + MAS.len() - 1, MAS.len());
                if (rl == MAS || rl == SAM) && (lr == MAS || lr == SAM) {
                    n_x_mas += 1;
                }
            }
        }

        println!("{n_x_mas}")
    }
}
