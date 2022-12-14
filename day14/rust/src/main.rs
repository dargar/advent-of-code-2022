use std::collections::HashSet;
use std::fs;
use std::iter::repeat;

fn main() {
    let input = fs::read_to_string("../input").expect("input file missing");
    let mut rock = HashSet::new();
    for line in input.lines() {
        let mut points = Vec::new();
        for coord in line.split(" -> ") {
            let point: Vec<i32> = coord
                .split(',')
                .map(str::parse)
                .flat_map(Result::ok)
                .collect();
            points.push((point[0], point[1]));
        }
        let rs = points.windows(2).flat_map(|ps| {
            let (x0, y0) = ps[0];
            let (x1, y1) = ps[1];
            if x0 == x1 {
                if y0 < y1 {
                    repeat(x0).zip(y0..=y1).collect::<Vec<_>>()
                } else {
                    repeat(x0).zip(y1..=y0).collect::<Vec<_>>()
                }
            } else if y0 == y1 {
                if x0 < x1 {
                    (x0..=x1).zip(repeat(y0)).collect::<Vec<_>>()
                } else {
                    (x1..=x0).zip(repeat(y0)).collect::<Vec<_>>()
                }
            } else {
                unreachable!()
            }
        });
        rock.extend(rs);
    }
    let first_answer = first_answer(&rock);
    println!("First answer: {first_answer}");
    let second_answer = second_answer(&rock);
    println!("Second answer: {second_answer}");
}

fn first_answer(rock: &HashSet<(i32, i32)>) -> usize {
    let lowest_rock = *rock.iter().map(|(_, y)| y).max().unwrap();
    let mut sand_at_rest = HashSet::new();
    let mut sand = (500, 0);
    loop {
        let (x, y) = sand;
        if y >= lowest_rock {
            return sand_at_rest.len();
        }
        let next_down = (x, y + 1);
        let next_left = (x - 1, y + 1);
        let next_right = (x + 1, y + 1);
        if !rock.contains(&next_down) && !sand_at_rest.contains(&next_down) {
            sand = next_down;
        } else if !rock.contains(&next_left) && !sand_at_rest.contains(&next_left) {
            sand = next_left;
        } else if !rock.contains(&next_right) && !sand_at_rest.contains(&next_right) {
            sand = next_right;
        } else {
            sand_at_rest.insert((x, y));
            sand = (500, 0);
        }
    }
}

fn second_answer(rock: &HashSet<(i32, i32)>) -> usize {
    let floor = 2 + rock.iter().map(|(_, y)| y).max().unwrap();
    let mut sand_at_rest = HashSet::new();
    let mut sand = (500, 0);
    loop {
        if sand_at_rest.contains(&sand) {
            return sand_at_rest.len();
        }
        let (x, y) = sand;
        let next_down = (x, y + 1);
        let next_left = (x - 1, y + 1);
        let next_right = (x + 1, y + 1);
        if next_down.1 < floor && !rock.contains(&next_down) && !sand_at_rest.contains(&next_down) {
            sand = next_down;
        } else if next_left.1 < floor
            && !rock.contains(&next_left)
            && !sand_at_rest.contains(&next_left)
        {
            sand = next_left;
        } else if next_right.1 < floor
            && !rock.contains(&next_right)
            && !sand_at_rest.contains(&next_right)
        {
            sand = next_right;
        } else {
            sand_at_rest.insert((x, y));
            sand = (500, 0);
        }
    }
}
