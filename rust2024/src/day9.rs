#[derive(Clone, Copy, Debug)]
pub struct File {
    pub id: Option<u64>,
    pub len: u64,
}

#[cfg(test)]
mod tests {
    use crate::get_input;

    use super::*;

    #[test]
    fn d9p1() {
        let input = get_input(9);

        let mut disk: Vec<_> = input
            .chars()
            .enumerate()
            .flat_map(|(i, len)| {
                let len = len.to_digit(10).unwrap() as u64;
                if i % 2 == 0 {
                    (0..len).map(|_| Some(i / 2)).collect::<Vec<_>>()
                } else {
                    (0..len).map(|_| None).collect::<Vec<_>>()
                }
            })
            .collect();

        println!(
            "{}",
            disk.iter()
                .map(|x| x.map(|v| v.to_string()).unwrap_or(".".to_string()))
                .collect::<String>()
        );
        let mut l = 0;
        let mut r = disk.len() - 1;

        while l < r {
            if disk[r].is_none() {
                r -= 1;
                continue;
            }

            if disk[l].is_some() {
                l += 1;
                continue;
            }

            disk[l] = disk[r];
            disk[r] = None;
        }

        let checksum: u64 = disk
            .into_iter()
            .take(r + 1)
            .enumerate()
            .map(|(i, v)| i as u64 * v.unwrap() as u64)
            .sum();

        println!("{checksum}");
    }

    #[test]
    fn d9p2() {
        let input = get_input(9);

        let mut disk: Vec<_> = input
            .chars()
            .enumerate()
            .map(|(i, len)| {
                let len = len.to_digit(10).unwrap() as u64;
                if i % 2 == 0 {
                    File {
                        id: Some((i / 2) as u64),
                        len,
                    }
                } else {
                    File { id: None, len }
                }
            })
            .collect();

        let mut r = disk.len() - 1;
        loop {
            if disk[r].id.is_none() {
                if r == 0 {
                    break;
                }
                r -= 1;
                continue;
            }

            let mut l = 0;

            while l < r {
                if disk[l].id.is_none() && disk[l].len >= disk[r].len {
                    disk[l].len -= disk[r].len;
                    if disk[l].len == 0 {
                        disk.remove(l);
                        r -= 1;
                    }

                    disk.insert(l, disk[r]);
                    r += 1;

                    disk[r].id = None;

                    break;
                }
                l += 1;
            }

            if r == 0 {
                break;
            }
            r -= 1;
        }

        let mut checksum = 0;

        let mut i = 0;
        for File { id, len } in disk {
            if let Some(id) = id {
                for j in i..i + len as usize {
                    checksum += id * j as u64;
                }
            }

            i += len as usize;
        }

        println!("{checksum}");
    }
}
