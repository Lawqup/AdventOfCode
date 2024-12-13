use num::Integer;

use crate::{get_input, p_char, p_string, p_u64};

pub fn get_eqs() -> Vec<(u64, u64, u64, u64, u64, u64)> {
    let input = &get_input(13);

    let p_button = |c| {
        p_string(&format!("Button {c}: X+"))
            .and_then_right(p_u64())
            .and_then_left(p_string(", Y+"))
            .chain(p_u64())
    };
    let p_prize = p_string("Prize: X=")
        .and_then_right(p_u64())
        .and_then_left(p_string(", Y="))
        .chain(p_u64());

    let p_eqs = p_button("A")
        .map(|(ax, ay)| move |(bx, by)| move |(px, py)| (ax, ay, bx, by, px, py))
        .and_then_left(p_char('\n'))
        .and_then(p_button("B"))
        .and_then_left(p_char('\n'))
        .and_then(p_prize)
        .sep_by(p_string("\n\n"));

    p_eqs.parse(input).unwrap()
}

pub fn solve((ax, ay, bx, by, px, py): (u64, u64, u64, u64, u64, u64), shift: u64) -> u64 {
    // Matrix of the form
    // ax bx | px
    // ay by | py
    let det = (ax * by) as i64 - (bx * ay) as i64;
    if det == 0 {
        return 0;
    }

    let det_a = ((px + shift) * by) as i64 - (bx * (py + shift)) as i64;
    let det_b = (ax * (py + shift)) as i64 - ((px + shift) * ay) as i64;

    let (a_times, r) = det_a.div_rem(&det);
    if r != 0 {
        return 0;
    }


    let (b_times, r) = det_b.div_rem(&det);
    if r != 0 {
        return 0;
    }

    (a_times * 3 + b_times) as u64

}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn d13p1() {
        let eqs = get_eqs();

        let tokens: u64 = eqs.into_iter().map(|eq| solve(eq, 0)).sum();

        println!("{tokens}")
    }

    #[test]
    fn d13p2() {
        let eqs = get_eqs();

        let tokens: u64 = eqs.into_iter().map(|eq| solve(eq, 10000000000000)).sum();

        println!("{tokens}")
    }
}
