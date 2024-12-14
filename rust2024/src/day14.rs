use crate::{get_input, p_char, p_i32, p_string};

const WIDTH: i32 = 101;
const HEIGHT: i32 = 103;

pub fn advance_robot(((x0, y0), (vx, vy)): ((i32, i32), (i32, i32)), secs: i32) -> (i32, i32) {
    let xf = (x0 + (vx * secs)).rem_euclid(WIDTH);
    let yf = (y0 + (vy * secs)).rem_euclid(HEIGHT);

    (xf, yf)
}

pub fn get_robots() -> Vec<((i32, i32), (i32, i32))> {
    let input = &get_input(14);

    let p_tuple = |label| {
        p_string(&format!("{label}="))
            .and_then_right(p_i32())
            .and_then_left(p_char(','))
            .chain(p_i32())
    };
    let p_robots = p_tuple("p")
        .and_then_left(p_char(' '))
        .chain(p_tuple("v"))
        .sep_by(p_char('\n'));

    p_robots.parse(input).unwrap()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn d14p1() {
        let (q1, q2, q3, q4) = get_robots()
            .into_iter()
            .map(|robot| advance_robot(robot, 100))
            .fold((0, 0, 0, 0), |mut acc, (x, y)| {
                if x > WIDTH / 2 {
                    if y > HEIGHT / 2 {
                        acc.0 += 1
                    } else if y < HEIGHT / 2 {
                        acc.1 += 1;
                    }
                } else if x < WIDTH / 2 {
                    if y > HEIGHT / 2 {
                        acc.2 += 1
                    } else if y < HEIGHT / 2 {
                        acc.3 += 1;
                    }
                }

                acc
            });

        let score = q1 * q2 * q3 * q4;

        println!("{}", score);
    }

    #[test]
    fn d14p2() {
        let robots = get_robots();
        
        let bx = (0..WIDTH).min_by_key(|i| {
                    let adv = robots.iter().map(|robot| advance_robot(*robot, *i).0);
                    let mean: f32 = adv.clone().sum::<i32>() as f32 / robots.len() as f32;

                    let variance: f32 = adv.map(|x| (x as f32 - mean).powi(2)).sum::<f32>() / robots.len() as f32;

                    variance as i32
                }).unwrap();


        let by = (0..HEIGHT).min_by_key(|i| {
                    let adv = robots.iter().map(|robot| advance_robot(*robot, *i).1);
                    let mean: f32 = adv.clone().sum::<i32>() as f32 / robots.len() as f32;

                    let variance: f32 = adv.map(|y| (y as f32 - mean).powi(2)).sum::<f32>() / robots.len() as f32;

                    variance as i32
                }).unwrap();

        let mut x = 0;
        let mut y = 0;
        loop {
            let t1 = bx + WIDTH * x;
            let t2 = by + HEIGHT * y;

            if t1 == t2 {
                println!("{t1}");
                break;
            } else if t1 < t2 {
                x += 1;
            } else {
                y += 1;
            }

        }
    }
}
