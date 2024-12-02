fn reports() -> Vec<Vec<i32>> {
    let input =
        std::fs::read_to_string("/Users/lawqup/projects/AdventOfCode/rust2024/src/input/d2.txt")
            .unwrap();

    input
        .lines()
        .map(|line| line.split(" ").map(|c| c.parse::<i32>().unwrap()).collect())
        .collect()
}

fn is_safe(x: i32, y: i32, inc: bool) -> bool {
    let diff = x - y;
    ((inc && diff > 0) || (!inc && diff < 0)) && diff.abs() >= 1 && diff.abs() <= 3
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn d2p1() {
        let mut safe = 0;

        'outer: for report in reports() {
            let inc = report[0] < report[1];
            for i in 1..report.len() {
                if !is_safe(report[i], report[i-1], inc) {
                    continue 'outer;
                }
            }
            safe += 1;
        }

        println!("{safe}");
    }

    #[test]
    fn d2p2() {
        let mut safe = 0;

        'outer: for report in reports().into_iter() {
            'remove: for remove in 0..report.len() {
                let mut removed = report.clone();
                removed.remove(remove);

                let inc = removed[0] < removed[1];
                for i in 1..removed.len() {
                    if !is_safe(removed[i], removed[i-1], inc) {
                        continue 'remove;
                    }
                }

                safe += 1;
                continue 'outer;
            }
        }

        println!("{safe}");
    }
}
