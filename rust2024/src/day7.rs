use std::convert::identity;

use crate::{get_input, p_char, p_string, p_u64};


pub fn concat(a: u64, b: u64) -> u64 {
    (a.to_string() + &b.to_string()).parse().unwrap()
}

pub fn correct_test_val(test_val: u64, nums: Vec<u64>, can_concat: bool) -> Option<u64> {
    pub fn rec(test_val: u64, nums: &[u64], can_concat: bool, curr: u64, i: usize) -> bool {
        if i == nums.len() {
            return test_val == curr;
        }

        if rec(test_val, nums, can_concat, curr + nums[i], i + 1) {
            return true;
        }

        if rec(test_val, nums, can_concat, curr * nums[i], i + 1) {
            return true;
        }

        if can_concat && rec(test_val, nums, can_concat, concat(curr, nums[i]), i + 1) {
            return true;
        }

        false
    }

    if rec(test_val, &nums[..], can_concat, nums[0], 1) {
        Some(test_val)
    } else {
        None
    }
}

pub fn solve(can_concat: bool) {
        let input = &get_input(7);

        let p_eqs = p_u64()
            .and_then_left(p_string(": "))
            .map(move |t| move |ns| correct_test_val(t, ns, can_concat))
            .and_then(p_u64().sep_by(p_char(' ')))
            .sep_by(p_char('\n'));

        let res: u64 = p_eqs.parse(input).unwrap().into_iter().filter_map(identity).sum();

        println!("{res}")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn d7p1() {
        solve(false);
    }

    #[test]
    fn d7p2() {
        solve(true);
    }
}
