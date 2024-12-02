use std::collections::HashMap;

fn get_lists() -> (Vec<u32>, Vec<u32>) {
    let input = std::fs::read_to_string("/Users/lawqup/AdventOfCode/aoc2024/src/input/d1.txt").unwrap();

    let mut llist = Vec::new();
    let mut rlist = Vec::new();

    for line in input.lines() {
        let (l, r) = line.split_once("   ").unwrap();
        llist.push(l.parse::<u32>().unwrap());
        rlist.push(r.parse::<u32>().unwrap());
    }

    (llist, rlist)
}

fn get_counts() -> (HashMap<u32, u32>, HashMap<u32, u32>) {
    let input = std::fs::read_to_string("/Users/lawqup/AdventOfCode/aoc2024/src/input/d1.txt").unwrap();

    let mut lmap = HashMap::new();
    let mut rmap = HashMap::new();

    for line in input.lines() {
        let (l, r) = line.split_once("   ").unwrap();
        let l = l.parse::<u32>().unwrap();
        let r = r.parse::<u32>().unwrap();

        *lmap.entry(l).or_insert(0) += 1;
        *rmap.entry(r).or_insert(0) += 1;
    }

    (lmap, rmap)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn d1p1() {
        let (mut llist, mut rlist) = get_lists();

        llist.sort();
        rlist.sort();

        let mut total_dist = 0;
        for (l, r) in llist.into_iter().zip(rlist.into_iter()) {
            total_dist += l.abs_diff(r);
        }

        println!("{total_dist}");
    }
    
    #[test]
    fn d1p2() {
        let (lmap, rmap) = get_counts();

        let mut sim_score = 0;
        for (val, lc) in lmap {
            let rc = rmap.get(&val).unwrap_or(&0);
            sim_score += val * lc * rc;
        }

        println!("{sim_score}");
    }
}
