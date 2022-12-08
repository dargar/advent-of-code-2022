use std::cmp::max;
use std::collections::HashSet;
use std::fs;

fn main() {
    let input = fs::read_to_string("../input").expect("input file missing");
    let grid: Vec<Vec<i32>> = input.lines()
        .map(|line| line.chars().map(|c| c.to_digit(10).unwrap() as i32).collect())
        .collect();
    let height = grid.len();
    let width = grid[0].len();

    let mut visible = HashSet::new();
    // Scanning from top side
    for x in 0..width {
        let mut max_height = -1;
        for y in 0..height {
            let tree_height = grid[y][x];
            if tree_height > max_height {
                visible.insert((x, y));
                max_height = tree_height;
            }
        }
    }
    // Scanning from bottom side
    for x in 0..width {
        let mut max_height = -1;
        for y in (0..height).rev() {
            if grid[y][x] > max_height {
                visible.insert((x, y));
                max_height = grid[y][x];
            }
        }
    }
    // Scanning from left side
    for y in 0..height {
        let mut max_height = -1;
        for x in 0..width {
            if grid[y][x] > max_height {
                visible.insert((x, y));
                max_height = grid[y][x];
            }
        }
    }
    // Scanning from right side
    for y in 0..height {
        let mut max_height = -1;
        for x in (0..width).rev() {
            if grid[y][x] > max_height {
                visible.insert((x, y));
                max_height = grid[y][x];
            }
        }
    }
    let first_answer = visible.len();
    println!("First answer: {first_answer}");

    let mut highest_scenic_score = 0;
    for y in 1..height-1 {
        for x in 1..width-1 {
            let tree_height = grid[y][x];
            let mut trees_visible = vec![0, 0, 0, 0];
            // looking right
            for xr in x + 1..width {
                trees_visible[0] += 1;
                if tree_height <= grid[y][xr] {
                    break;
                }
            }
            // looking left
            for xl in (0..=x - 1).rev() {
                trees_visible[1] += 1;
                if tree_height <= grid[y][xl] {
                    break;
                }
            }
            // looking down
            for yd in y + 1..height {
                trees_visible[2] += 1;
                if tree_height <= grid[yd][x] {
                    break;
                }
            }
            // looking up
            for yu in (0..=y - 1).rev() {
                trees_visible[3] += 1;
                if tree_height <= grid[yu][x] {
                    break;
                }
            }
            let scenic_score = trees_visible.into_iter().product();
            highest_scenic_score = max(scenic_score, highest_scenic_score);
        }
    }
    println!("Second answer: {highest_scenic_score}");
}
