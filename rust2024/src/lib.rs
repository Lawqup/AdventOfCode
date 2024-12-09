use reqwest::{blocking as req, cookie::Jar, Url};
use std::{fs, path::Path, rc::Rc, str::FromStr, sync::Arc};

pub mod day1;
pub mod day2;
pub mod day3;
pub mod day4;
pub mod day5;
pub mod day6;
pub mod day7;
pub mod day8;
pub mod day9;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Dir {
    Up,
    Down,
    Left,
    Right,
}

impl Dir {
    /// Returns (delta_row, delta_col)
    pub fn as_vec(&self) -> (isize, isize) {
        match self {
            Self::Up => (-1, 0),
            Self::Down => (1, 0),
            Self::Left => (0, -1),
            Self::Right => (0, 1),
        }
    }

    pub fn rotate_90(&self) -> Self {
        match self {
            Self::Up => Self::Right,
            Self::Down => Self::Left,
            Self::Left => Dir::Up,
            Self::Right => Self::Down,
        }
    }
}

pub fn get_input(day: u32) -> String {
    let local_path = format!("src/input/d{day}.txt");
    let local_path = Path::new(&local_path);

    if local_path.exists() {
        return fs::read_to_string(local_path).unwrap();
    }

    let url = format!("https://adventofcode.com/2024/day/{day}/input")
        .parse::<Url>()
        .unwrap();

    let cookie = fs::read_to_string(".aoc_token").unwrap();

    let jar = Jar::default();
    jar.add_cookie_str(&cookie, &url);

    let client = req::Client::builder()
        .cookie_provider(Arc::new(jar))
        .build()
        .unwrap();

    let text = client
        .get(url)
        .send()
        .unwrap()
        .text()
        .unwrap()
        .trim()
        .to_owned();

    fs::write(local_path, &text).unwrap();

    text
}

#[derive(Clone)]
pub struct Parser<T> {
    run_parser: Rc<dyn Fn(String) -> Option<(String, T)>>,
}

impl<T: 'static> Parser<T> {
    fn from_raw_func<F>(func: F) -> Self
    where
        F: Fn(String) -> Option<(String, T)> + 'static,
    {
        Parser {
            run_parser: Rc::new(func),
        }
    }

    pub fn parse(&self, input: &str) -> Option<T> {
        self.exec_parser(input.to_string())
            .map(|(_, parsed)| parsed)
    }

    fn exec_parser(&self, input: String) -> Option<(String, T)> {
        (self.run_parser)(input)
    }

    pub fn map<F, U: 'static>(self, func: F) -> Parser<U>
    where
        F: Fn(T) -> U + 'static,
    {
        self.filter_map(move |parsed| Some(func(parsed)))
    }

    pub fn filter_map<F, U: 'static>(self, func: F) -> Parser<U>
    where
        F: Fn(T) -> Option<U> + 'static,
    {
        let run_parser = move |input: String| {
            self.exec_parser(input)
                .and_then(|(rest, parsed)| func(parsed).map(|parsed| (rest, parsed)))
        };

        Parser::from_raw_func(run_parser)
    }

    pub fn and_then<U: 'static, V: 'static>(self, right: Parser<U>) -> Parser<V>
    where
        T: FnOnce(U) -> V,
    {
        let run_parser = move |input: String| {
            let (rest, f) = self.exec_parser(input)?;
            let (rest, a) = right.exec_parser(rest)?;

            Some((rest, f(a)))
        };

        Parser::from_raw_func(run_parser)
    }

    pub fn and_then_left<U: 'static>(self, right: Parser<U>) -> Self {
        self.map(|left| |_right| left).and_then(right)
    }

    pub fn and_then_right<U: 'static>(self, right: Parser<U>) -> Parser<U> {
        self.map(|_left| |right| right).and_then(right)
    }

    // Returns both left and right as a tuple
    pub fn chain<U: 'static>(self, right: Parser<U>) -> Parser<(T, U)> {
        self.map(|l| |r| (l, r)).and_then(right)
    }

    /// Returns a parser that doesn't advance to the next tokens after parsing, allowing the
    /// repeat parsing of the same tokens
    pub fn no_advance(self) -> Self {
        let run_parser = move |input: String| {
            self.exec_parser(input.clone())
                .map(|(_, parsed)| (input, parsed))
        };

        Parser::from_raw_func(run_parser)
    }

    /// Tries to parse with self, then tries alternative if self fails
    pub fn or<U: 'static>(self, alternative: Self) -> Self {
        let run_parser = move |input: String| {
            self.exec_parser(input.clone())
                .or(alternative.exec_parser(input))
        };

        Parser::from_raw_func(run_parser)
    }

    /// Parses everything up until what self would parse, returning that everything
    pub fn not(self) -> Parser<String>
    where
        T: Clone,
    {
        let run_parser = move |input: String| {
            let p_stop = self.clone().no_advance();

            let mut i = 0;
            while i < input.len() && p_stop.parse(&input[i..]).is_none() {
                i += 1;
            }

            Some((input[i..].to_string(), input[..i].to_string()))
        };

        Parser::from_raw_func(run_parser)
    }

    pub fn repeat(self) -> Parser<Vec<T>> {
        let run_parser = move |mut input: String| {
            let mut parsed = Vec::new();
            while let Some((rest, p)) = self.exec_parser(input.clone()) {
                input = rest;
                parsed.push(p)
            }

            Some((input, parsed))
        };

        Parser::from_raw_func(run_parser)
    }

    /// Parses out a Vec of Ts seperated by sep
    pub fn sep_by<U: 'static>(self, sep: Parser<U>) -> Parser<Vec<T>>
    where
        T: Clone,
    {
        self.clone()
            .map(|first| {
                |mut elems: Vec<T>| {
                    let mut parsed = vec![first];
                    parsed.append(&mut elems);
                    parsed
                }
            })
            .and_then(sep.and_then_right(self).repeat())
            .or::<Vec<T>>(p_empty(Vec::new())) // In the case that the last element fails
    }

    // Returns left and right strings concatted
    pub fn concat<U>(self, right: Parser<U>) -> Parser<String>
    where
        T: ToString,
        U: ToString + 'static,
    {
        self.map(|l| move |r: U| l.to_string() + &r.to_string()).and_then(right)
    }
}

/// Tries to parse a single character at the start of th se input given a fn
pub fn p_once<F>(pred: F) -> Parser<char>
where
    F: Fn(char) -> bool + 'static,
{
    let run_parser = move |input: String| {
        let x = input.chars().next();

        if x.is_some_and(|c| pred(c)) {
            Some((input[1..].to_string(), x.unwrap()))
        } else {
            None
        }
    };

    Parser::from_raw_func(run_parser)
}

/// Doesn't parse anything and just returns something
pub fn p_empty<T: Clone + 'static>(to_return: T) -> Parser<T> {
    Parser::from_raw_func(move |input| Some((input, to_return.clone())))
}

/// Tries to parse a single character at the start of the input given a fn
pub fn p_char(c: char) -> Parser<char> {
    p_once(move |x| x == c)
}

pub fn p_while<'a, F>(pred: F) -> Parser<String>
where
    F: Fn(char) -> bool + 'static,
{
    let run_parser = move |input: String| {
        let parsed: String = input.chars().take_while(|x| pred(*x)).collect();

        Some((input[parsed.len()..].to_string(), parsed))
    };

    Parser::from_raw_func(run_parser)
}

pub fn p_string(s: &str) -> Parser<String> {
    let s = s.to_string();
    let run_parser = move |input: String| {
        if input.get(0..s.len()).is_some_and(|xs| xs == s) {
            Some((input[s.len()..].to_string(), s.clone()))
        } else {
            None
        }
    };

    Parser::from_raw_func(run_parser)
}

pub fn p_int<T: num::Integer + FromStr + 'static>() -> Parser<T> {
    p_while(|c| c.is_ascii_digit()).filter_map(|xs| xs.parse::<T>().ok())
}

pub fn p_float<T: num::Float + FromStr + 'static>() -> Parser<T> {
    let p_dig = p_while(|c| c.is_ascii_digit());
    p_dig
        .clone()
        .concat(p_char('.'))
        .concat(p_dig)
        .filter_map(|xs| xs.parse::<T>().ok())
}

pub fn p_i32() -> Parser<i32> {
    p_int()
}

pub fn p_u64() -> Parser<u64> {
    p_int()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_once() {
        let input = "Hello, world!";
        let parser = p_once(char::is_uppercase);

        assert_eq!(parser.parse(input), Some('H'));

        let parser = p_once(char::is_whitespace);

        assert_eq!(parser.parse(input), None);
    }

    #[test]
    fn parse_char() {
        let input = "Hello, world!";
        let parser = p_char('H');

        assert_eq!(parser.parse(input), Some('H'));

        let parser = p_char('e');

        assert_eq!(parser.parse(input), None);
    }

    #[test]
    fn parse_while() {
        let input = "HELLO, world!";
        let parser = p_while(char::is_uppercase);

        assert_eq!(parser.parse(input), Some("HELLO".to_string()));

        let parser = p_while(char::is_lowercase);

        assert_eq!(parser.parse(input), Some(String::new()));
    }

    #[test]
    fn parse_string() {
        let input = "Hello, world!";
        let parser = p_string("Hello");

        assert_eq!(parser.parse(input), Some("Hello".to_string()));

        let parser = p_string("ello");

        assert_eq!(parser.parse(input), None);
    }

    #[test]
    fn parse_i32() {
        let input = "100 200 300";
        let parser = p_i32();

        assert_eq!(parser.parse(input), Some(100));

        let input = "a100b200";

        assert_eq!(parser.parse(input), None);
    }

    #[test]
    fn parse_and_then() {
        let parser = p_i32()
            .map(|a: i32| move |b: String| (b, a))
            .and_then(p_string("hello"));

        assert_eq!(parser.parse("100hello"), Some(("hello".to_string(), 100)));
        assert_eq!(parser.parse("hello100"), None);
        assert_eq!(parser.parse("100"), None);

        let parser = p_string("a")
            .and_then_right(p_string("b"))
            .and_then_left(p_string("c"));

        assert_eq!(parser.parse("abc"), Some("b".to_string()));
        assert_eq!(parser.parse("cba"), None);
        assert_eq!(parser.parse("b"), None);
    }

    #[test]
    fn parse_no_advance() {
        let input = "Hello, world!";
        let hello_parser = p_string("Hello");
        let parser = hello_parser
            .clone()
            .no_advance()
            .and_then_right(hello_parser.clone())
            .no_advance()
            .and_then_right(hello_parser);

        assert_eq!(parser.parse(input), Some("Hello".to_string()));
    }

    #[test]
    fn parse_not() {
        let input = "Hello, worl, world!";
        let parser = p_string("world").not();
        assert_eq!(parser.parse(input), Some("Hello, worl, ".to_string()));
    }

    #[test]
    fn parse_sep_by() {
        let sep_parser = p_char(',').and_then_right(p_while(char::is_whitespace));

        let parser = p_i32().sep_by(sep_parser);

        assert_eq!(parser.parse("1, 2,  3,    4"), Some(vec![1, 2, 3, 4]));
        assert_eq!(parser.parse("1 2,  3,    4"), Some(vec![1]));
        assert_eq!(parser.parse("abcd"), Some(vec![]));
    }
}
