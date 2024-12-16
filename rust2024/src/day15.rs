use std::{collections::VecDeque, thread::sleep, time::Duration};

use crate::{vec_to_string, Dir};

pub fn move_entity(
    warehouse: &mut Vec<Vec<char>>,
    (r, c): (usize, usize),
    dir: Dir,
) -> Option<(usize, usize)> {
    let Some((nr, nc)) = dir.translate((r, c)) else {
        return None;
    };

    if warehouse[nr][nc] == '#' {
        return None;
    }

    if warehouse[nr][nc] == 'o' && move_entity(warehouse, (nr, nc), dir).is_none() {
        return None;
    }

    if warehouse[r][c] == 'o' {
        warehouse[r][c] = '.';
        warehouse[nr][nc] = 'o';
    }

    return Some((nr, nc));
}

pub fn can_push(warehouse: &mut Vec<Vec<char>>, (r, c): (usize, usize), dir: Dir, depth: usize) -> bool {
    let Some((nr, nc)) = dir.translate((r, c)) else {
        return false;
    };

    if warehouse[nr][nc] == '.' {
        return true;
    }

    if warehouse[nr][nc] == '#' {
        return false;
    }

    let (or, oc) = if warehouse[nr][nc] == '[' {
        Dir::Right.translate((nr, nc)).unwrap()
    } else {
        Dir::Left.translate((nr, nc)).unwrap()
    };

    if can_push(warehouse, (nr, nc), dir, depth + 1) {
        if (or, oc) != (r, c) && (or, oc) != dir.translate((nr, nc)).unwrap() {
            return can_push(warehouse, (or, oc), dir, depth + 1);
        }

        return true;
    }

    return false;

}
pub fn move_big_entity(
    warehouse: &mut Vec<Vec<char>>,
    (r, c): (usize, usize),
    dir: Dir,
) -> (usize, usize) {
    let (nr, nc) = dir.translate((r, c)).unwrap();
    
    if warehouse[nr][nc] == '[' || warehouse[nr][nc] == ']' {
        let (or, oc) = if warehouse[nr][nc] == '[' {
            Dir::Right.translate((nr, nc)).unwrap()
        } else {
            Dir::Left.translate((nr, nc)).unwrap()
        };

        move_big_entity(warehouse, (nr, nc), dir);

        if (or, oc) != dir.translate((nr, nc)).unwrap() {
            move_big_entity(warehouse, (or, oc), dir);
        }
    }

    let curr = warehouse[r][c];
    if curr == '[' || curr == ']' {
        warehouse[nr][nc] = warehouse[r][c];
        warehouse[r][c] = '.';
    }

    (nr, nc)
}

#[cfg(test)]
mod tests {
    use core::panic;

    use crate::{get_input, grid_to_vec, vec_to_string, Dir};

    use super::*;

    #[test]
    fn d15p1() {
        let input = get_input(15);

        let (warehouse, moves) = input.split_once("\n\n").unwrap();

        let mut warehouse = grid_to_vec(warehouse);
        let mut r = warehouse.iter().position(|row| row.contains(&'@')).unwrap();
        let mut c = warehouse[r].iter().position(|x| *x == '@').unwrap();

        let moves: Vec<_> = moves.chars().filter(|c| *c != '\n').collect();

        for m in moves {
            let dir = match m {
                '^' => Dir::Up,
                'v' => Dir::Down,
                '<' => Dir::Left,
                '>' => Dir::Right,
                _ => panic!(),
            };

            if let Some((nr, nc)) = move_entity(&mut warehouse, (r, c), dir) {
                warehouse[r][c] = '.';
                warehouse[nr][nc] = '@';
                r = nr;
                c = nc;
            }
        }

        let sum_gps: usize = warehouse
            .into_iter()
            .enumerate()
            .flat_map(|(r, row)| {
                row.into_iter()
                    .enumerate()
                    .filter(|(_, x)| *x == 'O')
                    .map(move |(c, _)| 100 * r + c)
            })
            .sum();

        println!("{sum_gps}");
    }

    #[test]
    fn d15p2() {
        let input = get_input(15);

        let (warehouse, moves) = input.split_once("\n\n").unwrap();

        let mut warehouse = grid_to_vec(&warehouse.replace("#", "##").replace("O", "[]").replace(".", "..").replace("@", "@."));
        let mut r = warehouse.iter().position(|row| row.contains(&'@')).unwrap();
        let mut c = warehouse[r].iter().position(|x| *x == '@').unwrap();

        let moves: Vec<_> = moves.chars().filter(|c| *c != '\n').collect();

        for m in moves {
            let dir = match m {
                '^' => Dir::Up,
                'v' => Dir::Down,
                '<' => Dir::Left,
                '>' => Dir::Right,
                _ => panic!(),
            };

            if can_push(&mut warehouse, (r, c), dir, 0) {
                let (nr, nc) = move_big_entity(&mut warehouse, (r, c), dir);
                warehouse[r][c] = '.';
                warehouse[nr][nc] = '@';
                r = nr;
                c = nc;
            }
        }

        let sum_gps: usize = warehouse
            .into_iter()
            .enumerate()
            .flat_map(|(r, row)| {
                row.into_iter()
                    .enumerate()
                    .filter(|(_, x)| *x == '[')
                    .map(move |(c, _)| 100 * r + c)
            })
            .sum();

        println!("{sum_gps}");
    }
}
