use std::{
    collections::{HashMap, HashSet},
    isize, usize,
};

use crate::get_input;

pub fn get_antennas() -> (HashMap<char, Vec<(usize, usize)>>, usize, usize) {
    let input = &get_input(8);

    //        let input = r"............
    // ........0...
    // .....0......
    // .......0....
    // ....0.......
    // ......A.....
    // ............
    // ............
    // ........A...
    // .........A..
    // ............
    // ............
    // ";
    //
    let mut antennas = HashMap::new();

    for (row, line) in input.lines().enumerate() {
        for (col, x) in line.chars().enumerate() {
            if x != '.' {
                antennas.entry(x).or_insert(Vec::new()).push((row, col));
            }
        }
    }

    let rows = input.lines().count();
    let cols = input.lines().nth(0).unwrap().len();

    (antennas, rows, cols)
}

#[cfg(test)]
mod tests {
    use num::integer::gcd;

    use super::*;

    #[test]
    fn d8p1() {
        let (antennas, rows, cols) = get_antennas();
        let mut antinodes = HashSet::new();
        for (_, freq) in antennas {
            for (i, (r1, c1)) in freq.iter().enumerate() {
                for (r2, c2) in &freq[i + 1..] {
                    let rise = *r2 as isize - *r1 as isize;
                    let run = *c2 as isize - *c1 as isize;

                    let nr = *r1 as isize - rise;
                    let nc = *c1 as isize - run;

                    if nr >= 0 && (nr as usize) < rows && nc >= 0 && (nc as usize) < cols {
                        antinodes.insert((nr, nc));
                    }

                    let nr = *r2 as isize + rise;
                    let nc = *c2 as isize + run;

                    if nr >= 0 && (nr as usize) < rows && nc >= 0 && (nc as usize) < cols {
                        antinodes.insert((nr, nc));
                    }
                }
            }
        }

        println!("{}", antinodes.len());
    }

    #[test]
    fn d8p2() {
        let (antennas, rows, cols) = get_antennas();
        let mut antinodes = HashSet::new();
        for (_, freq) in antennas {
            for (i, (r1, c1)) in freq.iter().enumerate() {
                for (r2, c2) in &freq[i + 1..] {
                    let rise = *r2 as isize - *r1 as isize;
                    let run = *c2 as isize - *c1 as isize;

                    let drow = rise / gcd(rise, run);
                    let dcol = run / gcd(rise, run);

                    for i in 0.. {
                        let nr = *r1 as isize - drow * i;
                        let nc = *c1 as isize - dcol * i;

                        if nr >= 0 && (nr as usize) < rows && nc >= 0 && (nc as usize) < cols {
                            antinodes.insert((nr, nc));
                        } else {
                            break;
                        }
                    }

                    for i in 0.. {
                        let nr = *r2 as isize + drow * i;
                        let nc = *c2 as isize + dcol * i;

                        if nr >= 0 && (nr as usize) < rows && nc >= 0 && (nc as usize) < cols {
                            antinodes.insert((nr, nc));
                        } else {
                            break;
                        }
                    }
                }
            }
        }

        println!("{}", antinodes.len());
    }
}
