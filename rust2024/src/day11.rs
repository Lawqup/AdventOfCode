use std::collections::HashMap;

use crate::{get_input, p_char, p_u64};

fn update_stone_once(s: u64) -> Vec<u64> {
    if s == 0 {
        return vec![1];
    }

    let digits = (s as f64).log10() as u32 + 1;

    if digits % 2 == 0 {
        let mask = 10_u64.pow(digits / 2);
        return vec![s / mask, s % mask];
    }

    vec![s * 2024]
}

fn update_stone(cache: &mut HashMap<(u64, u32), u64>, s: u64, blinks: u32) -> u64 {
    if blinks == 0 {
        return 1;
    }

    let key = (s, blinks);
    if cache.contains_key(&key) {
        return cache[&key];
    }
    
    let res = update_stone_once(s).into_iter().map(|st| update_stone(cache, st, blinks - 1)).sum();

    cache.insert(key, res);

    res
}

pub fn update_stones(stones: Vec<u64>, blinks: u32) -> u64 {
    let mut cache = HashMap::new();

    stones.into_iter().map(|s| update_stone(&mut cache, s, blinks)).sum()
}

pub fn parse_input() -> Vec<u64> {
    let input = &get_input(11);

    let p_stones = p_u64().sep_by(p_char(' '));
    p_stones.parse(input).unwrap()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn d11p1() {
        println!("{}", update_stones(parse_input(), 25));
    }

    #[test]
    fn d11p2() {
        println!("{}", update_stones(parse_input(), 75));
    }
}
