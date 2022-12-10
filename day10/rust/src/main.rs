use std::fs;

fn main() {
    let input = fs::read_to_string("../input").expect("input file missing");
    let mut x = 1;
    let mut states = Vec::new();
    for line in input.lines() {
        let parts: Vec<&str> = line.split(' ').collect();
        match parts[0] {
            "addx" => {
                let n = parts[1].parse::<i32>().unwrap();
                states.push(x);
                states.push(x);
                x += n;
            }
            "noop" => {
                states.push(x);
            }
            _ => unreachable!(),
        }
    }
    let first_answer = states.iter()
        .enumerate()
        .skip(19)
        .step_by(40)
        .take(6)
        .map(|(cycle, x)| (1 + cycle as i32) * *x)
        .sum::<i32>();
    println!("First answer: {first_answer}");

    let width = 40;
    let height = 6;
    let mut screen = vec![vec!['.'; width]; height];
    for (cycle, x) in states.iter().enumerate() {
        let screen_x = cycle % width;
        let screen_y = cycle / width;
        if x - 1 <= screen_x as i32 && screen_x as i32 <= x + 1 {
            screen[screen_y][screen_x] = '#';
        }
    }
    println!("Second answer:");
    for line in screen {
        for pixel in line {
            print!("{pixel}");
        }
        println!();
    }
}
