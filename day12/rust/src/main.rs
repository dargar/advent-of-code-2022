use std::collections::{HashSet, VecDeque};
use std::fs;

fn main() {
    let input = fs::read_to_string("../input").expect("input file missing");
    let grid: Vec<Vec<char>> = input.lines().map(|line| line.chars().collect()).collect();
    let start = grid
        .iter()
        .enumerate()
        .flat_map(|(y, row)| {
            row.iter()
                .enumerate()
                .map(move |(x, elevation)| ((x, y), elevation))
        })
        .filter(|(_, elevation)| **elevation == 'S')
        .map(|(position, _)| position)
        .next()
        .unwrap();
    let starts = grid
        .iter()
        .enumerate()
        .flat_map(|(y, row)| {
            row.iter()
                .enumerate()
                .map(move |(x, elevation)| ((x, y), elevation))
        })
        .filter(|(_, elevation)| **elevation == 'S' || **elevation == 'a')
        .map(|(position, _)| position);
    let goal = grid
        .iter()
        .enumerate()
        .flat_map(|(y, row)| {
            row.iter()
                .enumerate()
                .map(move |(x, elevation)| ((x, y), elevation))
        })
        .filter(|(_, elevation)| **elevation == 'E')
        .map(|(position, _)| position)
        .next()
        .unwrap();

    let first_answer = steps_from(&grid, start, goal).unwrap();
    println!("First answer: {first_answer}");
    let second_answer = starts
        .into_iter()
        .flat_map(|start| steps_from(&grid, start, goal))
        .min()
        .unwrap();
    println!("Second answer: {second_answer}");
}

fn steps_from(grid: &Vec<Vec<char>>, start: (usize, usize), goal: (usize, usize)) -> Option<usize> {
    let height = grid.len();
    let width = grid[0].len();
    let mut visited = HashSet::new();
    let mut frontier = VecDeque::new();
    frontier.push_back((start, 0));
    while let Some((position, steps)) = frontier.pop_front() {
        if position == goal {
            return Some(steps);
        }

        visited.insert(position);
        let current_elevation = grid[position.1][position.0];
        let neighbours: Vec<(usize, usize)> = neighbours(width, height, position)
            .into_iter()
            .filter(|(x, y)| reachable(current_elevation, grid[*y][*x]))
            .filter(|p| !visited.contains(p))
            .collect();
        visited.extend(neighbours.clone());
        frontier.extend(neighbours.into_iter().map(|p| (p, steps + 1)));
    }
    None
}

fn neighbours(width: usize, height: usize, (x, y): (usize, usize)) -> Vec<(usize, usize)> {
    let mut neighbours = Vec::new();
    if x > 0 {
        neighbours.push((x - 1, y));
    }
    if x < width - 1 {
        neighbours.push((x + 1, y));
    }
    if y > 0 {
        neighbours.push((x, y - 1));
    }
    if y < height - 1 {
        neighbours.push((x, y + 1));
    }
    neighbours
}

fn reachable(start: char, destination: char) -> bool {
    let normalize = |c| match c {
        'S' => 'a',
        'E' => 'z',
        _ => c,
    };
    normalize(destination) as i32 - normalize(start) as i32 <= 1
}
