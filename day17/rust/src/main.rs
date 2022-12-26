use std::collections::HashSet;
use std::fs;

fn main() {
    let input = fs::read_to_string("../input").expect("input file missing");
    let jets = input.trim().chars().collect::<Vec<char>>();
    let rocks = vec![
        vec![(0, 0), (1, 0), (2, 0), (3, 0)],
        vec![(1, 0), (0, 1), (1, 1), (2, 1), (1, 2)],
        vec![(2, 0), (2, 1), (0, 2), (1, 2), (2, 2)],
        vec![(0, 0), (0, 1), (0, 2), (0, 3)],
        vec![(0, 0), (1, 0), (0, 1), (1, 1)],
    ];
    first_answer(rocks.clone(), jets.clone());
    second_answer(rocks, jets);
}

fn first_answer(rocks: Vec<Vec<(i32, i32)>>, jets: Vec<char>) {
    let mut rocks = rocks.into_iter().cycle();
    let mut jets = jets.into_iter().cycle();

    let mut rock_count = 0;
    let mut rocks_at_rest: HashSet<(i32, i32)> = HashSet::new();
    let mut rock: Vec<(i32, i32)> = rocks.next().unwrap();
    let top_of_rock_pile = rocks_at_rest
        .iter()
        .map(|(_, y)| *y)
        .min()
        .unwrap_or(std::i32::MAX);
    let rock_bottom = rock.iter().map(|(_, y)| *y).max().unwrap();
    let rock_translation = top_of_rock_pile - 4 - rock_bottom;
    rock.iter_mut().for_each(|(x, y)| {
        *x += 2;
        *y += rock_translation;
    });
    loop {
        let jet = jets.next().unwrap();
        let blown_rock: Vec<(i32, i32)> = match jet {
            '<' => rock.iter().map(|(x, y)| (x - 1, *y)).collect(),
            '>' => rock.iter().map(|(x, y)| (x + 1, *y)).collect(),
            _ => unreachable!(),
        };
        let blown_rock = if blown_rock
            .iter()
            .all(|p| 0 <= p.0 && p.0 < 7 && !rocks_at_rest.contains(p))
        {
            blown_rock
        } else {
            rock.clone()
        };

        let moved_rock: Vec<(i32, i32)> = blown_rock.iter().map(|(x, y)| (*x, y + 1)).collect();
        if moved_rock
            .iter()
            .all(|p| p.1 < std::i32::MAX && !rocks_at_rest.contains(p))
        {
            rock = moved_rock;
        } else {
            rocks_at_rest.extend(blown_rock.iter());
            rock = rocks.next().unwrap();
            rock_count += 1;
            let top_of_rock_pile = rocks_at_rest.iter().map(|(_, y)| *y).min().unwrap();
            let rock_bottom = rock.iter().map(|(_, y)| *y).max().unwrap();
            let rock_translation = top_of_rock_pile - 4 - rock_bottom;
            rock.iter_mut().for_each(|(x, y)| {
                *x += 2;
                *y += rock_translation;
            });
        }

        if rock_count == 2022 {
            break;
        }
    }
    let height = std::i32::MAX - rocks_at_rest.iter().map(|(_, y)| *y).min().unwrap();
    println!("First answer: {height}");
}

fn second_answer(rocks: Vec<Vec<(i32, i32)>>, jets: Vec<char>) {
    let full_cycle = rocks.len() * jets.len();

    let mut rocks = rocks.into_iter().cycle();
    let mut jets = jets.into_iter().cycle();

    let mut tops: Vec<HashSet<(i32, i32)>> = Vec::new();
    let mut heights: Vec<i32> = Vec::new();

    let mut rock_count = 0;
    let mut rock = Vec::new();
    let mut rocks_at_rest: HashSet<(i32, i32)> = HashSet::new();
    let mut top_of_rock_pile = std::i32::MAX;
    loop {
        if rock.is_empty() {
            rock.extend(rocks.next().unwrap());
            let rock_bottom = rock.iter().map(|(_, y)| *y).max().unwrap();
            let rock_translation = top_of_rock_pile - 4 - rock_bottom;
            rock.iter_mut().for_each(|(x, y)| {
                *x += 2;
                *y += rock_translation;
            });
        }
        let blown_rock: Vec<(i32, i32)> = match jets.next().unwrap() {
            '<' => rock.iter().map(|(x, y)| (x - 1, *y)).collect(),
            '>' => rock.iter().map(|(x, y)| (x + 1, *y)).collect(),
            _ => unreachable!(),
        };
        let blown_rock = if blown_rock
            .iter()
            .all(|p| 0 <= p.0 && p.0 < 7 && !rocks_at_rest.contains(p))
        {
            blown_rock
        } else {
            rock.clone()
        };

        let moved_rock: Vec<(i32, i32)> = blown_rock.iter().map(|(x, y)| (*x, y + 1)).collect();
        if moved_rock
            .iter()
            .all(|p| p.1 < std::i32::MAX && !rocks_at_rest.contains(p))
        {
            rock = moved_rock;
        } else {
            top_of_rock_pile = std::cmp::min(
                blown_rock.iter().map(|(_, y)| *y).min().unwrap(),
                top_of_rock_pile,
            );
            rocks_at_rest.extend(blown_rock.iter());
            rock.clear();
            rock_count += 1;
            heights.push(top_of_rock_pile);

            if rock_count % full_cycle == 0 {
                let min_y = top_of_rock_pile;
                let mut rs = HashSet::new();
                let mut xs = vec![false; 7];
                for y in min_y.. {
                    for x in 0..7 {
                        if !xs[x as usize] && rocks_at_rest.contains(&(x, y)) {
                            rs.insert((x, y - min_y));
                            xs[x as usize] = true;
                        }
                    }
                    if xs.iter().all(|x| *x) {
                        break;
                    }
                }
                if !tops.is_empty() && tops[0] == rs {
                    let matched_height = std::i32::MAX - heights[full_cycle - 1];
                    let current_height = std::i32::MAX - min_y;
                    let diff = current_height - matched_height;
                    let cycle_len = tops.len();
                    let remaining_iterations = 1_000_000_000_000i64 - rock_count as i64;
                    let extra_height_from_full_cycles =
                        (remaining_iterations / (full_cycle * cycle_len) as i64) * diff as i64;
                    let rem = remaining_iterations % (full_cycle * cycle_len) as i64;
                    let matched_height_plus_remainder =
                        std::i32::MAX - heights[full_cycle - 1 + rem as usize];
                    let result = current_height as i64 + extra_height_from_full_cycles;
                    let second_answer =
                        result + (matched_height_plus_remainder - matched_height) as i64;
                    println!("Second answer: {second_answer}");
                    break;
                }
                tops.push(rs);
            }
        }
    }
}
