use std::collections::VecDeque;
use std::fs;

fn main() {
    let input = fs::read_to_string("../input").expect("input file missing");
    let first_answer = first_answer(&input);
    println!("First answer: {first_answer}");
    let second_answer = second_answer(&input);
    println!("Second answer: {second_answer}");
}

fn first_answer(input: &str) -> usize {
    let mut items = Vec::new();
    let mut inspections = Vec::new();
    let mut throw_tests = Vec::new();
    for input_monkey in input.split("\n\n") {
        let mut input_monkey_lines = input_monkey.lines();
        let _monkey = input_monkey_lines.next().unwrap();
        let starting_items = input_monkey_lines.next().unwrap()[18..]
            .split(", ")
            .flat_map(str::parse::<usize>)
            .collect::<VecDeque<_>>();
        let operation = &input_monkey_lines.next().unwrap()[13..]
            .split(' ')
            .collect::<Vec<_>>();
        let operation: Box<dyn Fn(usize) -> usize> = match operation[..] {
            ["new", "=", "old", "*", "old"] => Box::new(|old: usize| old * old),
            ["new", "=", "old", "+", "old"] => Box::new(|old: usize| old + old),
            ["new", "=", "old", "*", n] => {
                let n: usize = n.parse().unwrap();
                Box::new(move |old: usize| old * n)
            }
            ["new", "=", "old", "+", n] => {
                let n: usize = n.parse().unwrap();
                Box::new(move |old: usize| old + n)
            }
            _ => unreachable!(),
        };
        let divisible_by = input_monkey_lines
            .next()
            .unwrap()
            .split(' ')
            .last()
            .map(str::parse::<usize>)
            .unwrap()
            .unwrap();
        let if_true = input_monkey_lines
            .next()
            .unwrap()
            .split(' ')
            .last()
            .map(str::parse::<usize>)
            .unwrap()
            .unwrap();
        let if_false = input_monkey_lines
            .next()
            .unwrap()
            .split(' ')
            .last()
            .map(str::parse::<usize>)
            .unwrap()
            .unwrap();
        let test: Box<dyn Fn(usize) -> usize> = Box::new(move |worry_level| {
            if worry_level % divisible_by == 0 {
                if_true
            } else {
                if_false
            }
        });
        items.push(starting_items);
        inspections.push(operation);
        throw_tests.push(test);
    }
    let mut inspection_counts = vec![0; items.len()];
    for _round in 0..20 {
        for monkey in 0..items.len() {
            let is = items[monkey].clone();
            for worry_level in is {
                let worry_level_during_inspection = inspections[monkey](worry_level);
                let worry_level_after_inspection = worry_level_during_inspection / 3;
                let next_monkey = throw_tests[monkey](worry_level_after_inspection);
                items[next_monkey].push_back(worry_level_after_inspection);
                inspection_counts[monkey] += 1;
            }
            items[monkey].clear();
        }
    }
    inspection_counts.sort();
    inspection_counts.into_iter().rev().take(2).product()
}

fn second_answer(input: &str) -> u64 {
    let mut items = Vec::new();
    let mut inspections = Vec::new();
    let mut divisibles = Vec::new();
    let mut if_trues = Vec::new();
    let mut if_falses = Vec::new();
    for input_monkey in input.split("\n\n") {
        let mut input_monkey_lines = input_monkey.lines();
        let _monkey = input_monkey_lines.next().unwrap();
        let starting_items = input_monkey_lines.next().unwrap()[18..]
            .split(", ")
            .flat_map(str::parse::<u64>)
            .collect::<VecDeque<_>>();
        let operation = &input_monkey_lines.next().unwrap()[13..]
            .split(' ')
            .collect::<Vec<_>>();
        let operation: Box<dyn Fn(u64) -> u64> = match operation[..] {
            ["new", "=", "old", "*", "old"] => Box::new(|old| old * old),
            ["new", "=", "old", "+", "old"] => Box::new(|old| old + old),
            ["new", "=", "old", "*", n] => {
                let n: u64 = n.parse().unwrap();
                Box::new(move |old| old * n)
            }
            ["new", "=", "old", "+", n] => {
                let n: u64 = n.parse().unwrap();
                Box::new(move |old| old + n)
            }
            _ => unreachable!(),
        };
        let divisible_by = input_monkey_lines
            .next()
            .unwrap()
            .split(' ')
            .last()
            .map(str::parse::<u64>)
            .unwrap()
            .unwrap();
        let if_true = input_monkey_lines
            .next()
            .unwrap()
            .split(' ')
            .last()
            .map(str::parse::<usize>)
            .unwrap()
            .unwrap();
        let if_false = input_monkey_lines
            .next()
            .unwrap()
            .split(' ')
            .last()
            .map(str::parse::<usize>)
            .unwrap()
            .unwrap();
        divisibles.push(divisible_by);
        if_trues.push(if_true);
        if_falses.push(if_false);
        items.push(starting_items);
        inspections.push(operation);
    }

    let worry_management: u64 = divisibles.iter().product();
    let mut inspection_counts = vec![0; items.len()];
    for _round in 0..10_000 {
        for monkey in 0..items.len() {
            let is = items[monkey].clone();
            for worry_level in is {
                let worry_level_during_inspection = inspections[monkey](worry_level);
                let worry_level_after_inspection = worry_level_during_inspection % worry_management;
                let divisible_by = divisibles[monkey];
                let next_monkey = if worry_level_after_inspection % divisible_by == 0 {
                    if_trues[monkey]
                } else {
                    if_falses[monkey]
                };
                items[next_monkey].push_back(worry_level_after_inspection);
                inspection_counts[monkey] += 1;
            }
            items[monkey].clear();
        }
    }
    inspection_counts.sort();
    inspection_counts.into_iter().rev().take(2).product()
}
