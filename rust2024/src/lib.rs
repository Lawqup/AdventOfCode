use std::{fs, path::Path, sync::Arc};
use reqwest::{blocking as req, cookie::Jar, Url};

pub mod day1;
pub mod day2;
pub mod day3;

pub fn get_input(day: u32) -> String {
    let local_path = format!("src/input/d{day}.txt");
    let local_path = Path::new(&local_path);

    if local_path.exists() {
        return fs::read_to_string(local_path).unwrap()
    }

    let url = format!("https://adventofcode.com/2024/day/{day}/input").parse::<Url>().unwrap();

    let cookie = fs::read_to_string(".aoc_token").unwrap();

    let jar = Jar::default();
    jar.add_cookie_str(&cookie, &url);

    let client = req::Client::builder().cookie_provider(Arc::new(jar)).build().unwrap();

    let text = client.get(url).send().unwrap().text().unwrap();

    fs::write(local_path, &text).unwrap();

    text
}


pub struct Parser<T> {
    run_parser: Box<dyn Fn(String) -> Option<(String, T)>>,
}

impl<T: 'static> Parser<T> {
    pub fn parse(&self, input: &str) -> Option<T> {

        self.exec_parser(input.to_string()).map(|(_, parsed)| parsed)
    }

    fn exec_parser(&self, input: String) -> Option<(String, T)> {
        (self.run_parser)(input)
    }

    pub fn map<F, U>(self, func: F) -> Parser<U>
    where F: Fn(T) -> U + 'static
    {
        self.map_fallible(move |parsed| Some(func(parsed)))
    }

    pub fn map_fallible<F, U>(self, func: F) -> Parser<U>
    where F: Fn(T) -> Option<U> + 'static
    {
        let run_parser = Box::new(move |input: String| {
            self.exec_parser(input).and_then(|(rest, parsed)| func(parsed).map(|parsed| (rest, parsed)))
        });

        Parser { run_parser }
    }

    pub fn and_then<U: 'static, V: 'static>(self, next: Parser<U>) -> Parser<V>
    where T: Fn(U) -> V
    {
        let run_parser = Box::new(move |input: String| {
            let (rest, f) = self.exec_parser(input)?;
            let (rest, a) = next.exec_parser(rest)?;

            Some((rest, f(a)))
        });

        Parser { run_parser }
    }
}

/// Tries to parse a single character at the start of th se input given a fn
pub fn p_once<F>(pred: F) -> Parser<char>
where F: Fn(char) -> bool + 'static
{
    let run_parser = Box::new(move |input: String| {
        let x = input.chars().next();

        if x.is_some_and(|c| pred(c)) {
            Some((input[1..].to_string(), x.unwrap()))
        } else {
            None
        }
    });

    Parser { run_parser }
}


/// Tries to parse a single character at the start of the input given a fn
pub fn p_char(c: char) -> Parser<char> {
    p_once(move |x| x == c)
}

pub fn p_while<'a, F>(pred: F) -> Parser<String>
where F: Fn(char) -> bool + 'static
{
    let run_parser = Box::new(move |input: String| {
        let parsed: String = input.chars().take_while(|x| pred(*x)).collect();
        
        Some((input[parsed.len()..].to_string(), parsed))
    });

    Parser { run_parser }
}

pub fn p_string(s: &str) -> Parser<String> {
    let s = s.to_string();
    let run_parser = Box::new(move |input: String| {
        if input.get(0..s.len()).is_some_and(|xs| xs == s) {
            Some((input[s.len()..].to_string(), s.clone()))
        } else {
            None
        }
    });

    Parser { run_parser }
}

pub fn p_i32() -> Parser<i32> {
    p_while(|c| c.is_ascii_digit()).map_fallible(|xs| xs.parse::<i32>().ok())
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
        let parser = p_i32().map(|a: i32| move |b: String| (b, a)).and_then(p_string("hello"));

        assert_eq!(parser.parse("100hello"), Some(("hello".to_string(), 100)));
        assert_eq!(parser.parse("hello100"), None);
        assert_eq!(parser.parse("100"), None);
    }
}
