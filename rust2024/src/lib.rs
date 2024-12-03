use std::{fs, path::Path, sync::Arc};
use reqwest::{blocking as req, cookie::Jar, Url};

pub mod day1;
pub mod day2;

pub fn input(day: u32) -> String {
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

