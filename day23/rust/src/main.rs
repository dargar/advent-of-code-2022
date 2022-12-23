use std::collections::HashMap;
use std::fs;

fn main() {
    let input = fs::read_to_string("../input").expect("input file missing");
    let mut elves = Vec::new();
    for (y, row) in input.lines().enumerate() {
        for (x, cell) in row.chars().enumerate() {
            if cell == '#' {
                elves.push((x as i32, y as i32));
            }
        }
    }
    elves.sort();
    for round in 0.. {
        let mut next_elves: Vec<(i32, i32)> = Vec::new();
        let mut proposals: HashMap<(i32, i32), Vec<usize>> = HashMap::new();
        for (id, elf @ &(x, y)) in elves.iter().enumerate() {
            let adjacent_positions = vec![
                (x - 1, y - 1), (x, y - 1), (x + 1, y - 1),
                (x - 1, y    ),             (x + 1, y    ),
                (x - 1, y + 1), (x, y + 1), (x + 1, y + 1),
            ];
            if adjacent_positions.iter().all(|ap| !elves.contains(ap)) {
                next_elves.push(*elf);
            } else {
                let tests: Vec<fn(&Vec<(i32, i32)>, &Vec<(i32, i32)>, (i32, i32)) -> Option<(i32, i32)>> = vec![n_ne_nw, s_se_sw, w_nw_sw, e_ne_se];
                let mut proposal = None;
                for f in tests.into_iter().cycle().skip(round % 4).take(4) {
                    if let Some(prop) = f(&adjacent_positions, &elves, *elf) {
                        proposal = Some(prop);
                        break;
                    }
                }
                if let Some(proposed_position) = proposal {
                    proposals.entry(proposed_position)
                        .and_modify(|es| es.push(id))
                        .or_insert(vec![id]);
                } else {
                    next_elves.push(*elf);
                }
            }
        }
        for (proposed_position, candidates) in proposals {
            if candidates.len() == 1 {
                next_elves.push(proposed_position);
            } else {
                for candidate in candidates {
                    let original_position = elves[candidate];
                    next_elves.push(original_position);
                }
            }
        }
        next_elves.sort();
        if elves == next_elves {
            println!("Second answer: {}", round + 1);
            break;
        }
        elves = next_elves;
        if round == 9 {
            let min_x = elves.iter().map(|(x, _)| *x).min().unwrap();
            let max_x = elves.iter().map(|(x, _)| *x).max().unwrap();
            let min_y = elves.iter().map(|(_, y)| *y).min().unwrap();
            let max_y = elves.iter().map(|(_, y)| *y).max().unwrap();
            let area = (1 + max_x - min_x) * (1 + max_y - min_y);
            let empty_tiles = area - elves.len() as i32;
            println!("First answer: {empty_tiles}");
        }
    }
}

fn n_ne_nw(adjacent_positions: &Vec<(i32, i32)>, elves: &Vec<(i32, i32)>, (x, y): (i32, i32)) -> Option<(i32, i32)> {
    let ps = vec![
        adjacent_positions[0],
        adjacent_positions[1],
        adjacent_positions[2],
    ];
    if ps.iter().all(|ap| !elves.contains(ap)) {
        Some((x, y - 1))
    } else {
        None
    }
}

fn s_se_sw(adjacent_positions: &Vec<(i32, i32)>, elves: &Vec<(i32, i32)>, (x, y): (i32, i32)) -> Option<(i32, i32)> {
    let ps = vec![
        adjacent_positions[5],
        adjacent_positions[6],
        adjacent_positions[7],
    ];
    if ps.iter().all(|ap| !elves.contains(ap)) {
        Some((x, y + 1))
    } else {
        None
    }
}

fn w_nw_sw(adjacent_positions: &Vec<(i32, i32)>, elves: &Vec<(i32, i32)>, (x, y): (i32, i32)) -> Option<(i32, i32)> {
    let ps = vec![
        adjacent_positions[0],
        adjacent_positions[3],
        adjacent_positions[5],
    ];
    if ps.iter().all(|ap| !elves.contains(ap)) {
        Some((x - 1, y))
    } else {
        None
    }
}

fn e_ne_se(adjacent_positions: &Vec<(i32, i32)>, elves: &Vec<(i32, i32)>, (x, y): (i32, i32)) -> Option<(i32, i32)> {
    let ps = vec![
        adjacent_positions[2],
        adjacent_positions[4],
        adjacent_positions[7],
    ];
    if ps.iter().all(|ap| !elves.contains(ap)) {
        Some((x + 1, y))
    } else {
        None
    }
}
