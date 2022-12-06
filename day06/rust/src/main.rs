use std::fs;

fn main() {
    let input = fs::read_to_string("../input").expect("input file missing");
    let chars: Vec<char> = input.chars().collect();
    let first_answer = find_first_marker(&chars, 4);
    println!("First answer: {first_answer}");
    let second_answer = find_first_marker(&chars, 14);
    println!("Second answer: {second_answer}");
}

fn find_first_marker(chars: &[char], sequence_length: usize) -> usize {
    for (n, sequence) in chars.windows(sequence_length).enumerate() {
        let mut deduped_sequence = Vec::from(sequence);
        deduped_sequence.sort();
        deduped_sequence.dedup();
        if sequence.len() == deduped_sequence.len() {
            return sequence_length + n;
        }
    }
    unreachable!();
}
