use std::collections::HashSet;
use std::fs;

fn main() {
    let input = fs::read_to_string("../input").expect("input file missing");
    let mut walls = HashSet::new();
    let mut grounds = HashSet::new();
    let mut blizzards = Vec::new();
    for (y, line) in input.lines().enumerate() {
        for (x, cell) in line.chars().enumerate() {
            let position = (x, y);
            match cell {
                '#' => {
                    walls.insert(position);
                }
                '.' => {
                    grounds.insert(position);
                }
                blizzard => {
                    blizzards.push((position, blizzard));
                    grounds.insert(position);
                }
            }
        }
    }

    let start = *grounds.iter().min_by_key(|(x, y)| (y, x)).unwrap();
    let destination = *grounds.iter().max_by_key(|(x, y)| (y, x)).unwrap();
    let a = traverse_valley(&walls, &mut blizzards, start, destination);
    let b = traverse_valley(&walls, &mut blizzards, destination, start);
    let c = traverse_valley(&walls, &mut blizzards, start, destination);
    println!("First answer: {}", a);
    println!("Second answer: {}", a + b + c);
}

fn traverse_valley(
    walls: &HashSet<(usize, usize)>,
    blizzards: &mut [((usize, usize), char)],
    start: (usize, usize),
    destination: (usize, usize),
) -> usize {
    let left_wall_x = *walls.iter().map(|(x, _)| x).min().unwrap();
    let right_wall_x = *walls.iter().map(|(x, _)| x).max().unwrap();
    let top_wall_y = *walls.iter().map(|(_, y)| y).min().unwrap();
    let bottom_wall_y = *walls.iter().map(|(_, y)| y).max().unwrap();

    let mut expeditions = HashSet::from([start]);
    for minute in 1.. {
        // update expeditions
        let updated_expeditions = expeditions
            .into_iter()
            .flat_map(|(x, y)| {
                let mut moves = vec![(x, y)];
                if let Some(nx) = x.checked_sub(1) {
                    let position = (nx, y);
                    if !walls.contains(&position) {
                        moves.push(position);
                    }
                }
                if let Some(nx) = x.checked_add(1) {
                    let position = (nx, y);
                    if !walls.contains(&position) {
                        moves.push(position);
                    }
                }
                if let Some(ny) = y.checked_sub(1) {
                    let position = (x, ny);
                    if !walls.contains(&position) {
                        moves.push(position);
                    }
                }
                if let Some(ny) = y.checked_add(1) {
                    let position = (x, ny);
                    if !walls.contains(&position) {
                        moves.push(position);
                    }
                }
                moves
            })
            .collect::<HashSet<_>>();
        // update blizzards
        for ((x, y), direction) in blizzards.iter_mut() {
            match direction {
                '>' => {
                    *x += 1;
                    if *x == right_wall_x {
                        *x = left_wall_x + 1;
                    }
                }
                '<' => {
                    *x -= 1;
                    if *x == left_wall_x {
                        *x = right_wall_x - 1;
                    }
                }
                'v' => {
                    *y += 1;
                    if *y == bottom_wall_y {
                        *y = top_wall_y + 1;
                    }
                }
                '^' => {
                    *y -= 1;
                    if *y == top_wall_y {
                        *y = bottom_wall_y - 1;
                    }
                }
                _ => unreachable!(),
            }
        }
        let blizzard_positions = blizzards
            .iter()
            .map(|(position, _)| position)
            .cloned()
            .collect::<HashSet<_>>();
        // prune invalid expeditions
        expeditions = &updated_expeditions - &blizzard_positions;
        if expeditions.contains(&destination) {
            return minute;
        }
    }
    unreachable!();
}
