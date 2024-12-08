use crate::Dir;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct GaurdState {
    row: usize,
    col: usize,
    dir: Dir,
}

pub fn get_next_move(grid: &Vec<Vec<char>>, gaurd: &GaurdState) -> Option<GaurdState> {
    let (dr, dc) = gaurd.dir.as_vec();
    let new_r = gaurd.row.checked_add_signed(dr)?;
    let new_c = gaurd.col.checked_add_signed(dc)?;

    if new_r < grid.len() && new_c < grid[0].len() {
        if grid[new_r][new_c] == '#' {
            Some(GaurdState {
                row: gaurd.row,
                col: gaurd.col,
                dir: gaurd.dir.rotate_90(),
            })
        } else {
            Some(GaurdState {
                row: new_r,
                col: new_c,
                dir: gaurd.dir,
            })
        }
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use crate::get_input;

    use super::*;

    #[test]
    fn d6p1() {
        let input = get_input(6);

        let grid: Vec<Vec<_>> = input.lines().map(|line| line.chars().collect()).collect();

        let mut gaurd = GaurdState {
            row: 0,
            col: 0,
            dir: Dir::Up,
        };
        for row in 0..grid.len() {
            for col in 0..grid[0].len() {
                if grid[row][col] == '^' {
                    gaurd.row = row;
                    gaurd.col = col;
                }
            }
        }

        let mut visited = HashSet::new();

        while let Some(gaurd_new) = get_next_move(&grid, &gaurd) {
            gaurd = gaurd_new;
            visited.insert((gaurd.row, gaurd.col));
        }

        println!("{}", visited.len())
    }


    #[test]
    fn d6p2() {
        let input = get_input(6);

        let mut grid: Vec<Vec<_>> = input.lines().map(|line| line.chars().collect()).collect();

        let mut gaurd_start = GaurdState {
            row: 0,
            col: 0,
            dir: Dir::Up,
        };

        for row in 0..grid.len() {
            for col in 0..grid[0].len() {
                if grid[row][col] == '^' {
                    gaurd_start.row = row;
                    gaurd_start.col = col;
                }
            }
        }

        let mut n_cycles = 0;

        for obs_row in 0..grid.len() {
            for obs_col in 0..grid[0].len() {
                if grid[obs_row][obs_col] == '#' || grid[obs_row][obs_col] == '^' {
                    continue;
                }

                grid[obs_row][obs_col] = '#';

                let mut visited = HashSet::new();
                
                let mut gaurd = gaurd_start.clone();
                while let Some(gaurd_new) = get_next_move(&grid, &gaurd) {
                    if visited.contains(&gaurd_new) {
                        n_cycles += 1;
                        break;
                    }
                    gaurd = gaurd_new;
                    visited.insert(gaurd.clone());
                }

                grid[obs_row][obs_col] = '.';
            }
        }
        println!("{}", n_cycles)
    }
}
