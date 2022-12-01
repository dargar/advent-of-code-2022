use std::collections::BinaryHeap;
use std::fs;

fn main() {
    let input = fs::read_to_string("../input").expect("missing input file");
    let mut inventories = BinaryHeap::new();
    for inventory in input.split("\n\n") {
        let calories = inventory
            .lines()
            .flat_map(str::parse::<u32>)
            .sum::<u32>();
        inventories.push(calories);
    }
    let first_answer = inventories.peek().unwrap();
    println!("First answer: {first_answer}");
    let second_answer = inventories.into_iter().take(3).sum::<u32>();
    println!("Second answer: {second_answer}");
}
