use crate::Parser;

use crate::{p_char, p_i32, p_string};

use super::get_input;

pub fn p_mul() -> Parser<i32> {
    p_string("mul(")
        .and_then_right(p_i32())
        .and_then_left(p_char(','))
        .map(|l| move |r| l * r)
        .and_then(p_i32())
        .and_then_left(p_char(')'))
}

#[derive(Debug, Clone)]
pub enum Instr {
    Do,
    Dont,
    Mul(i32),
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn d3p1() {
        let input = get_input(3);

        let parser = p_mul().not().and_then_right(p_mul().sep_by(p_mul().not()));

        let result: i32 = parser.parse(&input).unwrap().into_iter().sum();

        println!("{result}")
    }

    #[test]
    fn d3p2() {
        let input = get_input(3);

        let p_do = p_string("do()").map(|_| Instr::Do);
        let p_dont = p_string("don't()").map(|_| Instr::Dont);
        let p_mul = p_mul().map(|x| Instr::Mul(x));

        let p_instr = p_do.or::<Instr>(p_dont).or::<Instr>(p_mul);

        let parser = p_instr
            .clone()
            .not()
            .and_then_right(p_instr.clone().sep_by(p_instr.not()));

        let mut enabled = true;
        let mut sum = 0;
        for instr in parser.parse(&input).unwrap() {
            match instr {
                Instr::Do => { enabled = true },
                Instr::Dont => { enabled = false },
                Instr::Mul(val) if enabled => { sum += val }
                _ => {}
            }
        }

        println!("{sum}")
    }
}
