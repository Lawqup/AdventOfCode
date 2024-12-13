use std::collections::{HashMap, HashSet};

use crate::Dir;

pub fn th_score(
    map: &Vec<Vec<u32>>,
    visited: &mut HashSet<(usize, usize)>,
    (ri, ci): (usize, usize),
) -> u32 {
    if visited.contains(&(ri, ci)) {
        return 0;
    }

    visited.insert((ri, ci));

    let curr = map[ri][ci];

    if curr == 9 {
        return 1;
    }

    return Dir::into_iter()
        .filter_map(|dir| {
            let (dr, dc) = dir.to_vec();
            let nr = ri.checked_add_signed(dr)?;
            let nc = ci.checked_add_signed(dc)?;
            map.get(nr)
                .and_then(|row| row.get(nc))
                .and_then(|&e| (e == curr + 1).then(|| th_score(map, visited, (nr, nc))))
        })
        .sum();
}

pub fn th_rating(
    map: &Vec<Vec<u32>>,
    visited: &mut HashSet<(usize, usize)>,
    (ri, ci): (usize, usize),
) -> u32 {
    if visited.contains(&(ri, ci)) {
        return 0;
    }

    visited.insert((ri, ci));

    let curr = map[ri][ci];

    if curr == 9 {
        visited.remove(&(ri, ci));
        return 1;
    }

    let rating =  Dir::into_iter()
        .filter_map(|dir| {
            let (dr, dc) = dir.to_vec();
            let nr = ri.checked_add_signed(dr)?;
            let nc = ci.checked_add_signed(dc)?;
            map.get(nr)
                .and_then(|row| row.get(nc))
                .and_then(|&e| (e == curr + 1).then(|| th_rating(map, visited, (nr, nc))))
        })
        .sum();

    visited.remove(&(ri, ci));

    return rating;
}

pub fn get_map(input: &str) -> Vec<Vec<u32>> {
    input
        .lines()
        .map(|line| line.chars().map(|c| c.to_digit(10).unwrap_or(99)).collect())
        .collect()
}

pub fn get_trailheads(map: &Vec<Vec<u32>>) -> Vec<(usize, usize)> {
    map.iter()
        .enumerate()
        .flat_map(|(ri, row)| {
            row.iter()
                .enumerate()
                .filter_map(move |(ci, e)| (e == &0).then_some((ri, ci)))
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use crate::get_input;

    use super::*;

    #[test]
    fn d10p1() {
        let input = &get_input(10);

        let map = get_map(input);

        let trailheads = get_trailheads(&map);

        let score_sum: u32 = trailheads
            .into_iter()
            .map(|th| th_score(&map, &mut HashSet::new(), th))
            .sum();

        println!("{score_sum}");
    }

    #[test]
    fn d10p2() {
        let input = &get_input(10);

        let map = get_map(input);

        let trailheads = get_trailheads(&map);
        println!("{map:?}");

        let score_sum: u32 = trailheads
            .into_iter()
            .map(|th| th_rating(&map, &mut HashSet::new(), th))
            .sum();

        println!("{score_sum}");
    }
}
