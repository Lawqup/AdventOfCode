pub fn reports() -> Vec<Vec<i32>> {
    let input =
        std::fs::read_to_string("/Users/lawqup/projects/AdventOfCode/rust2024/src/input/d2.txt")
            .unwrap();

    input
        .lines()
        .map(|line| line.split(" ").map(|c| c.parse::<i32>().unwrap()).collect())
        .collect()
}

pub fn is_safe(x: i32, y: i32, inc: bool) -> bool {
    let diff = x - y;
    ((inc && diff > 0) || (!inc && diff < 0)) && diff.abs() >= 1 && diff.abs() <= 3
}

pub fn get_bad_index(report: &Vec<i32>, skip: Option<usize>) -> Option<usize> {
    let first;
    let second;

    if skip.is_some_and(|j| j == 0) {
        first = 1;
        second = 2;
    } else if skip.is_some_and(|j| j == 1) {
        first = 0;
        second = 2;
    } else {
        first = 0;
        second = 1;
    }

    let inc = report[first] < report[second];
    for i in second..report.len() {
        if skip.is_some_and(|j| j == i) {
            continue;
        }

        let prev = if skip.is_some_and(|j| j == i - 1) { i - 2 } else { i - 1 };

        if !is_safe(report[i], report[prev], inc) {
            return Some(i);
        }
    }

    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn d2p1() {
        let mut safe = 0;

        for report in reports() {
            if get_bad_index(&report, None).is_none() {
                safe += 1;
            };
        }

        println!("{safe}");
    }

    #[test]
    fn d2p2() {
        let mut safe = 0;

        for report in reports() {
            let Some(bad) = get_bad_index(&report, None) else {
                safe += 1;
                continue;
            };

            if get_bad_index(&report, Some(bad)).is_none() {
                safe += 1;
                continue;
            };

            if get_bad_index(&report, Some(bad - 1)).is_none() {
                safe += 1;
                continue;
            };

            if get_bad_index(&report, Some(0)).is_none() {
                safe += 1;
                continue;
            };
        }

        println!("{safe}");
    }
}
