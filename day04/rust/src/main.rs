use std::fs;

fn main() {
    let input = fs::read_to_string("../input").expect("input file missing");

    let mut fully_contained_count = 0;
    for pair in input.lines() {
        let mut elves = pair.split(",");
        let elf1 = elves.next().unwrap().split("-").flat_map(str::parse::<u32>).collect::<Vec<_>>();
        let elf2 = elves.next().unwrap().split("-").flat_map(str::parse::<u32>).collect::<Vec<_>>();
        if fully_contains(&elf1, &elf2) || fully_contains(&elf2, &elf1) {
            fully_contained_count += 1;
        }
    }
    println!("First answer: {fully_contained_count}");

    let mut partly_contained_count = 0;
    for pair in input.lines() {
        let mut elves = pair.split(",");
        let elf1 = elves.next().unwrap().split("-").flat_map(str::parse::<u32>).collect::<Vec<_>>();
        let elf2 = elves.next().unwrap().split("-").flat_map(str::parse::<u32>).collect::<Vec<_>>();
        if partly_contains(&elf1, &elf2) || partly_contains(&elf2, &elf1) {
            partly_contained_count += 1;
        }
    }
    println!("Second answer: {partly_contained_count}");
}

fn fully_contains(a: &[u32], b: &[u32]) -> bool {
    a[0] <= b[0] && b[1] <= a[1]
}

fn partly_contains(a: &[u32], b: &[u32]) -> bool {
    a[0] <= b[0] && b[0] <= a[1] || a[0] <= b[1] && b[1] <= a[1]
}

