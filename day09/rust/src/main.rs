use std::collections::HashSet;
use std::fs;

fn main() {
    let input = fs::read_to_string("../input").expect("input file missing");
    let first_answer = rope_simulation(&input, 2);
    println!("First answer: {first_answer}");
    let second_answer = rope_simulation(&input, 10);
    println!("Second answer: {second_answer}");
}

fn rope_simulation(input: &str, k: usize) -> usize {
    let mut knots = vec![(0i32, 0i32); k];
    let mut tail_visits = HashSet::new();
    tail_visits.insert((0, 0));
    for motion in input.lines() {
        let mut parts = motion.split(' ');
        let direction = parts.next().unwrap();
        let steps = parts.next().unwrap().parse::<u32>().unwrap();

        for _ in 0..steps {
            let (head_x, head_y) = &mut knots[0];
            match direction {
                "U" => {
                    *head_y -= 1;
                }
                "D" => {
                    *head_y += 1;
                }
                "L" => {
                    *head_x -= 1;
                }
                "R" => {
                    *head_x += 1;
                }
                _ => unreachable!(),
            }

            for i in 0..knots.len() - 1 {
                let (head_x, head_y) = knots[i];
                let (mut tail_x, mut tail_y) = knots[i + 1];
                let diff = (tail_x - head_x, tail_y - head_y);
                match diff {
                    (2, 0) => tail_x -= 1,
                    (-2, 0) => tail_x += 1,
                    (0, 2) => tail_y -= 1,
                    (0, -2) => tail_y += 1,
                    (-1, -2) => {
                        tail_x += 1;
                        tail_y += 1;
                    }
                    (-2, 1) => {
                        tail_x += 1;
                        tail_y -= 1;
                    }
                    (-1, 2) => {
                        tail_x += 1;
                        tail_y -= 1;
                    }
                    (-2, -1) => {
                        tail_x += 1;
                        tail_y += 1;
                    }
                    (1, -2) => {
                        tail_x -= 1;
                        tail_y += 1;
                    }
                    (2, -1) => {
                        tail_x -= 1;
                        tail_y += 1;
                    }
                    (2, 1) => {
                        tail_x -= 1;
                        tail_y -= 1;
                    }
                    (1, 2) => {
                        tail_x -= 1;
                        tail_y -= 1;
                    }
                    (2, -2) => {
                        tail_x -= 1;
                        tail_y += 1;
                    }
                    (2, 2) => {
                        tail_x -= 1;
                        tail_y -= 1;
                    }
                    (-2, 2) => {
                        tail_x += 1;
                        tail_y -= 1;
                    }
                    (-2, -2) => {
                        tail_x += 1;
                        tail_y += 1;
                    }
                    _ => (),
                }
                knots[i + 1] = (tail_x, tail_y);
            }
            tail_visits.insert(knots[k - 1]);
        }
    }
    tail_visits.len()
}
