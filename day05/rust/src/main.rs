use std::collections::HashMap;
use std::fs;

type Crates = HashMap<usize, Vec<char>>;
type Instruction = (usize, usize, usize);

fn main() {
    let input = fs::read_to_string("../input").expect("input file missing");
    let first_answer = answer(&input, &move_crates_one_by_one);
    println!("First answer: {first_answer}");
    let second_answer = answer(&input, &move_crates_as_stack);
    println!("Second answer: {second_answer}");
}

fn answer(input: &str, crate_moving_function: &dyn Fn(&mut Crates, Instruction)) -> String {
    let mut input_sections = input.split("\n\n");
    let crates_description = input_sections.next().unwrap();
    let instruction_description = input_sections.next().unwrap();

    let mut first_crates = parse_crates(crates_description);
    for line in instruction_description.lines() {
        let instruction = parse_instruction(line);
        crate_moving_function(&mut first_crates, instruction);
    }

    top_crates(&first_crates)
}

fn parse_crates(crates_description: &str) -> Crates {
    let mut crates: Crates = HashMap::new();
    for line in crates_description.lines().rev().skip(1) {
        for (i, c) in line.chars().skip(1).step_by(4).enumerate() {
            if !c.is_whitespace() {
                crates
                    .entry(i + 1)
                    .and_modify(|stack| stack.push(c))
                    .or_insert_with(|| vec![c]);
            }
        }
    }
    crates
}

fn parse_instruction(line: &str) -> Instruction {
    let mut parts = line.split(' ');
    let _move = parts.next().unwrap();
    let n = parts.next().map(str::parse::<usize>).unwrap().unwrap();
    let _from = parts.next().unwrap();
    let a = parts.next().map(str::parse::<usize>).unwrap().unwrap();
    let _to = parts.next().unwrap();
    let b = parts.next().map(str::parse::<usize>).unwrap().unwrap();
    (n, a, b)
}

fn move_crates_one_by_one(crates: &mut Crates, (n, from, to): Instruction) {
    for _ in 0..n {
        if let Some(c) = crates.get_mut(&from).and_then(std::vec::Vec::pop) {
            crates
                .entry(to)
                .and_modify(|stack| stack.push(c))
                .or_insert_with(|| vec![c]);
        }
    }
}

fn move_crates_as_stack(crates: &mut Crates, (n, from, to): Instruction) {
    let mut cs = Vec::new();
    for _ in 0..n {
        if let Some(c) = crates.get_mut(&from).and_then(std::vec::Vec::pop) {
            cs.push(c);
        }
    }
    for c in cs.into_iter().rev() {
        crates
            .entry(to)
            .and_modify(|stack| stack.push(c))
            .or_insert_with(|| vec![c]);
    }
}

fn top_crates(crates: &Crates) -> String {
    (1..=crates.len())
        .flat_map(|n| crates.get(&(n)))
        .flat_map(|cs| cs.last())
        .collect()
}
