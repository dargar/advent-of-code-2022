use std::collections::HashSet;
use std::fs;

fn main() {
    let input = fs::read_to_string("../input").expect("input file missing");
    let mut common_items = Vec::new();
    for rucksack in input.lines() {
        let first_compartment = rucksack
            .chars()
            .take(rucksack.len() / 2)
            .collect::<HashSet<char>>();
        let second_compartment = rucksack
            .chars()
            .skip(rucksack.len() / 2)
            .collect::<HashSet<char>>();
        common_items.extend(&first_compartment & &second_compartment);
    }
    let first_answer = common_items.into_iter().map(priority).sum::<u32>();
    println!("First answer: {first_answer}");

    let rucksacks = input.lines().collect::<Vec<_>>();
    let mut badges = Vec::new();
    for group in rucksacks.chunks(3) {
        let badge = group
            .iter()
            .map(|rucksack| rucksack.chars().collect::<HashSet<char>>())
            .reduce(|a, b| &a & &b)
            .expect("missing rucksacks");
        badges.extend(badge);
    }
    let second_answer = badges.into_iter().map(priority).sum::<u32>();
    println!("Second answer: {second_answer}");
}

fn priority(item: char) -> u32 {
    if item.is_lowercase() {
        1 + item as u32 - 'a' as u32
    } else {
        27 + item as u32 - 'A' as u32
    }
}
