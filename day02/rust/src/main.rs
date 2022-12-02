use std::fs;

fn main() {
    let input = fs::read_to_string("../input").expect("input file missing");

    let first_answer = input.lines()
        .map(score)
        .sum::<u32>();
    println!("First answer: {first_answer}");

    let second_answer = input.lines()
        .map(|game| score(translate(game)))
        .sum::<u32>();
    println!("Second answer: {second_answer}");
}

fn score(game: &str) -> u32 {
    match game {
        "A X" => 1 + 3,
        "A Y" => 2 + 6,
        "A Z" => 3 + 0,
        "B X" => 1 + 0,
        "B Y" => 2 + 3,
        "B Z" => 3 + 6,
        "C X" => 1 + 6,
        "C Y" => 2 + 0,
        "C Z" => 3 + 3,
        _ => panic!("unknown game: {game}"),
    }
}

fn translate(game: &str) -> &str {
    match game {
        "A X" => "A Z",
        "A Y" => "A X",
        "A Z" => "A Y",
        "B X" => "B X",
        "B Y" => "B Y",
        "B Z" => "B Z",
        "C X" => "C Y",
        "C Y" => "C Z",
        "C Z" => "C X",
        _ => panic!("unknown game: {game}"),
    }
}
