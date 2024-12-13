use std::collections::HashSet;

use crate::Dir;

pub fn get_area_and_perimeter(
    garden: &Vec<Vec<char>>,
    visited: &mut HashSet<(usize, usize)>,
    pos: (usize, usize),
) -> (u32, u32) {
    if visited.contains(&pos) {
        return (0, 0);
    }

    visited.insert(pos);

    let region = garden[pos.0][pos.1];

    let mut area = 1;
    let mut perimiter = 0;

    for dir in Dir::into_iter() {
        let Some((ri, ci)) = dir.translate(pos).filter(|(ri, ci)| {
            *ri < garden.len() && *ci < garden[0].len() && garden[*ri][*ci] == region
        }) else {
            perimiter += 1;
            continue;
        };

        let (a, p) = get_area_and_perimeter(garden, visited, (ri, ci));

        area += a;
        perimiter += p;
    }

    (area, perimiter)
}

pub fn get_area_and_sides(
    garden: &Vec<Vec<char>>,
    visited: &mut HashSet<(usize, usize)>,
    pos: (usize, usize),
) -> (u32, u32) {
    if visited.contains(&pos) {
        return (0, 0);
    }

    let region = garden[pos.0][pos.1];

    visited.insert(pos);

    let mut area = 1;
    let mut sides = corners(garden, pos) ;

    for dir in Dir::into_iter() {
        let Some((ri, ci)) = dir.translate(pos).filter(|(ri, ci)| {
            *ri < garden.len() && *ci < garden[0].len() && garden[*ri][*ci] == region
        }) else {
            continue;
        };

        let (a, s) = get_area_and_sides(garden, visited, (ri, ci));

        area += a;
        sides += s;
    }

    (area, sides)
}

fn corners(garden: &Vec<Vec<char>>, pos: (usize, usize)) -> u32 {
    let region = garden[pos.0][pos.1];

    let in_region = |(ri, ci): &(usize, usize)| {
        *ri < garden.len() && *ci < garden[0].len() && garden[*ri][*ci] == region
    };

    let mut corners = 0;

    for dir in Dir::into_iter() {
        let a = dir.translate(pos).filter(in_region);
        let b = dir.rotate_90().translate(pos).filter(in_region);
        if a.is_none() && b.is_none()
        {
            corners += 1;
            continue;
        }

        if a.is_some() && b.is_some() {
            let (dr1, dc1) = dir.to_vec();
            let (dr2, dc2) = dir.rotate_90().to_vec();

            let r = pos.0.checked_add_signed(dr1 + dr2);
            let c = pos.1.checked_add_signed(dc1 + dc2);

            if !r.is_some_and(|r| c.is_some_and(|c| in_region(&(r, c)))) {
                corners += 1;
            }
        }
    }

    corners
}

#[cfg(test)]
mod tests {
    use crate::get_input;

    use super::*;

    #[test]
    fn d12p1() {
        let input = get_input(12);

        let garden: Vec<Vec<_>> = input.lines().map(|line| line.chars().collect()).collect();

        let mut visited = HashSet::new();

        let mut price = 0;
        for ri in 0..garden.len() {
            for ci in 0..garden[0].len() {
                let (a, p) = get_area_and_perimeter(&garden, &mut visited, (ri, ci));
                price += a * p;
            }
        }

        println!("{price}");
    }

    #[test]
    fn d12p2() {
        let input = get_input(12);

        let garden: Vec<Vec<_>> = input.lines().map(|line| line.chars().collect()).collect();

        let mut visited = HashSet::new();

        let mut price = 0;
        for ri in 0..garden.len() {
            for ci in 0..garden[0].len() {
                let (a, s) = get_area_and_sides(&garden, &mut visited, (ri, ci));
                price += a * s;
            }
        }

        println!("{price}");
    }
}
