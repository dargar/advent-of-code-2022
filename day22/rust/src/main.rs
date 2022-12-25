use std::collections::HashMap;
use std::fs;

fn main() {
    let input = fs::read_to_string("../input").expect("input file missing");
    first_answer(&input);
    second_answer(&input, 50);
}

fn first_answer(input: &str) {
    let mut parts = input.split("\n\n");

    let mut map: HashMap<(i32, i32), char> = HashMap::new();
    for (y, line) in parts.next().unwrap().lines().enumerate() {
        for (x, c) in line.chars().enumerate() {
            if c != ' ' {
                map.insert((x as i32, y as i32), c);
            }
        }
    }

    let min_x: i32 = *map
        .keys()
        .filter(|(_, y)| *y == 0)
        .map(|(x, _)| x)
        .min()
        .unwrap();
    let min_y: i32 = 0;

    let raw_path = parts.next().unwrap();
    let steps = raw_path
        .split(|c: char| !c.is_numeric())
        .filter(|s| !s.is_empty())
        .map(str::parse)
        .map(Result::unwrap);
    let mut turns = raw_path
        .split(char::is_numeric)
        .flat_map(str::chars)
        .filter(|c| c.is_alphabetic());

    let mut position = (min_x, min_y);
    let mut facing = '>';
    for step in steps {
        for _ in 0..step {
            let (x, y) = position;
            let mut next_position = match facing {
                '>' => (x + 1, y),
                '<' => (x - 1, y),
                'v' => (x, y + 1),
                '^' => (x, y - 1),
                _ => unreachable!(),
            };
            if let Some(next_tile) = map.get(&next_position) {
                if *next_tile == '#' {
                    break;
                }
            } else {
                let (np, tile) = match facing {
                    '>' => map
                        .iter()
                        .filter(|((_, ty), _)| *ty == y)
                        .min_by_key(|((x, _), _)| x)
                        .unwrap(),
                    '<' => map
                        .iter()
                        .filter(|((_, ty), _)| *ty == y)
                        .max_by_key(|((x, _), _)| x)
                        .unwrap(),
                    'v' => map
                        .iter()
                        .filter(|((tx, _), _)| *tx == x)
                        .min_by_key(|((_, y), _)| y)
                        .unwrap(),
                    '^' => map
                        .iter()
                        .filter(|((tx, _), _)| *tx == x)
                        .max_by_key(|((_, y), _)| y)
                        .unwrap(),
                    _ => unreachable!(),
                };
                if *tile == '#' {
                    break;
                } else {
                    next_position = *np;
                }
            }
            position = next_position;
        }
        if let Some(turn) = turns.next() {
            facing = match (facing, turn) {
                ('>', 'L') => '^',
                ('<', 'L') => 'v',
                ('v', 'L') => '>',
                ('^', 'L') => '<',
                ('>', 'R') => 'v',
                ('<', 'R') => '^',
                ('v', 'R') => '<',
                ('^', 'R') => '>',
                _ => unreachable!(),
            }
        }
    }
    let (x, y) = position;
    let f = match facing {
        '>' => 0,
        '<' => 2,
        'v' => 1,
        '^' => 3,
        _ => unreachable!(),
    };
    let final_password = 1000 * (y + 1) + 4 * (x + 1) + f;
    println!("First answer: {}", final_password);
}

fn second_answer(input: &str, size: i32) {
    let mut parts = input.split("\n\n");

    let max_x = input
        .split("\n\n")
        .next()
        .unwrap()
        .lines()
        .map(|s| s.len() as i32)
        .max()
        .unwrap();
    let sides_width = max_x / size;

    let mut maps: HashMap<i32, HashMap<(i32, i32), char>> = HashMap::new();
    let mut original_position = HashMap::new();
    for (y, line) in parts.next().unwrap().lines().enumerate() {
        for (x, c) in line.chars().enumerate() {
            let (x, y) = (x as i32, y as i32);
            let side_x = x / size;
            let side_y = y / size;
            let side = side_y * sides_width + side_x;
            if c != ' ' {
                let position = (x % size, y % size);
                maps.entry(side)
                    .and_modify(|map| {
                        map.insert(position, c);
                    })
                    .or_insert_with(|| HashMap::from([(position, c)]));
                original_position.insert((side, position), (x + 1, y + 1));
            }
        }
    }

    let raw_path = parts.next().unwrap();
    let steps = raw_path
        .split(|c: char| !c.is_numeric())
        .filter(|s| !s.is_empty())
        .map(str::parse)
        .map(Result::unwrap);
    let mut turns = raw_path
        .split(char::is_numeric)
        .flat_map(str::chars)
        .filter(|c| c.is_alphabetic());

    let mut side = *maps.keys().min().unwrap();
    let mut position = (0, 0);
    let mut facing = '>';
    for step in steps {
        for _ in 0..step {
            let (x, y) = position;

            let mut next_side = side;
            let mut next_position = match facing {
                '>' => (x + 1, y),
                '<' => (x - 1, y),
                'v' => (x, y + 1),
                '^' => (x, y - 1),
                _ => unreachable!(),
            };
            let mut next_facing = facing;
            if let Some(next_tile) = maps.get(&side).unwrap().get(&next_position) {
                if *next_tile == '#' {
                    break;
                }
            } else {
                let (ns, np, nf) = next_state(size, side, position, facing);
                if *maps.get(&ns).unwrap().get(&np).unwrap() == '#' {
                    break;
                } else {
                    next_side = ns;
                    next_position = np;
                    next_facing = nf;
                }
            }

            side = next_side;
            position = next_position;
            facing = next_facing;
        }

        if let Some(turn) = turns.next() {
            facing = match (facing, turn) {
                ('>', 'L') => '^',
                ('<', 'L') => 'v',
                ('v', 'L') => '>',
                ('^', 'L') => '<',
                ('>', 'R') => 'v',
                ('<', 'R') => '^',
                ('v', 'R') => '<',
                ('^', 'R') => '>',
                _ => unreachable!(),
            }
        }
    }

    let (x, y) = original_position.get(&(side, position)).unwrap();
    let f = match facing {
        '>' => 0,
        '<' => 2,
        'v' => 1,
        '^' => 3,
        _ => unreachable!(),
    };
    let final_password = 1000 * y + 4 * x + f;
    println!("Second answer: {}", final_password);
}

// Manually defined with the help of a paper cube based on the input map...
fn next_state(
    size: i32,
    current_side: i32,
    (x, y): (i32, i32),
    current_facing: char,
) -> (i32, (i32, i32), char) {
    match (current_side, current_facing) {
        (1, '>') => (2, (0, y), '>'),
        (1, 'v') => (4, (x, 0), 'v'),
        (1, '<') => (6, (0, size - y - 1), '>'),
        (1, '^') => (9, (0, x), '>'),

        (2, '>') => (7, (size - 1, size - y - 1), '<'),
        (2, 'v') => (4, (size - 1, x), '<'),
        (2, '<') => (1, (size - 1, y), '<'),
        (2, '^') => (9, (x, size - 1), '^'),

        (4, '>') => (2, (y, size - 1), '^'),
        (4, 'v') => (7, (x, 0), 'v'),
        (4, '<') => (6, (y, 0), 'v'),
        (4, '^') => (1, (x, size - 1), '^'),

        (6, '>') => (7, (0, y), '>'),
        (6, 'v') => (9, (x, 0), 'v'),
        (6, '<') => (1, (0, size - y - 1), '>'),
        (6, '^') => (4, (0, x), '>'),

        (7, '>') => (2, (size - 1, size - y - 1), '<'),
        (7, 'v') => (9, (size - 1, x), '<'),
        (7, '<') => (6, (size - 1, y), '<'),
        (7, '^') => (4, (x, size - 1), '^'),

        (9, '>') => (7, (y, size - 1), '^'),
        (9, 'v') => (2, (x, 0), 'v'),
        (9, '<') => (1, (y, 0), 'v'),
        (9, '^') => (6, (x, size - 1), '^'),

        _ => panic!(
            "transition from {current_side} with current facing {current_facing} not defined"
        ),
    }
}
