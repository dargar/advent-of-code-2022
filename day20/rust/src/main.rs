use std::collections::VecDeque;
use std::fs;

fn main() {
    let input = fs::read_to_string("../input").expect("input file missing");
    let numbers = input.lines()
        .map(str::parse::<i64>)
        .flat_map(Result::ok)
        .collect::<VecDeque<i64>>();
    println!("First answer: {}", answer(mix(numbers.clone(), 1, 1)));
    println!("Second answer: {}", answer(mix(numbers.clone(), 10, 811589153)));
}

fn mix(numbers: VecDeque<i64>, rounds: usize, decryption_key: i64) -> VecDeque<i64> {
    let mut numbers = numbers.into_iter()
        .map(|n| n * decryption_key)
        .enumerate()
        .collect::<VecDeque<_>>();
    for _ in 0..rounds {
        for i in 0..numbers.len() {
            loop {
                let entry @ (j, n) = numbers.pop_front().unwrap();
                if i != j {
                    numbers.push_front(entry);
                    numbers.rotate_left(1);
                } else {
                    let len = numbers.len();
                    if n < 0 {
                        for _ in 0..(n.abs() % len as i64) {
                            numbers.rotate_right(1);
                        }
                        numbers.push_front(entry);
                    } else {
                        for _ in 0..(n % len as i64) {
                            numbers.rotate_left(1);
                        }
                        numbers.push_front(entry);
                    }
                    break;
                }
            }
        }
    }
    loop {
        let entry @ (_, n) = numbers.pop_front().unwrap();
        if n == 0 {
            numbers.push_front(entry);
            break;
        }
        numbers.push_front(entry);
        numbers.rotate_left(1);
    }
    numbers.into_iter()
        .map(|(_, n)| n)
        .collect()
}

fn answer(mixed_numbers: VecDeque<i64>) -> i64 {
    let a = mixed_numbers[1000 % mixed_numbers.len()];
    let b = mixed_numbers[2000 % mixed_numbers.len()];
    let c = mixed_numbers[3000 % mixed_numbers.len()];
    a + b + c
}
