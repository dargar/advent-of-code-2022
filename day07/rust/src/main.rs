use std::collections::{HashMap, HashSet};
use std::fs;

fn main() {
    let input = fs::read_to_string("../input").expect("input file missing");
    let mut file_tree: HashMap<Vec<String>, HashSet<Vec<String>>> = HashMap::new();
    let mut file_size: HashMap<Vec<String>, u32> = HashMap::new();
    let mut current_path: Vec<String> = Vec::new();
    let mut lines = input.lines().peekable();
    while let Some(line) = lines.next() {
        let parts = line
            .split(' ')
            .map(|s| s.to_string())
            .collect::<Vec<String>>();
        match parts[1].as_str() {
            "cd" => match parts[2].as_str() {
                "/" => {
                    current_path.clear();
                }
                ".." => {
                    current_path.pop();
                }
                x => {
                    current_path.push(x.to_string());
                }
            },
            "ls" => {
                while let Some(&l) = lines.peek() {
                    if l.starts_with('$') {
                        break;
                    }
                    let l = lines.next().unwrap();
                    let ps = l.split(' ').map(|s| s.to_string()).collect::<Vec<String>>();
                    let mut dir_path = current_path.clone();
                    dir_path.push(ps[1].clone());
                    file_tree
                        .entry(current_path.clone())
                        .and_modify(|dirs| {
                            dirs.insert(dir_path.clone());
                        })
                        .or_insert_with(|| {
                            let mut dirs = HashSet::new();
                            dirs.insert(dir_path.clone());
                            dirs
                        });
                    if ps[0] != "dir" {
                        let size = ps[0].parse::<u32>().unwrap();
                        file_size.insert(dir_path.clone(), size);
                    }
                }
            }
            _ => unreachable!(),
        }
    }
    let mut result = HashMap::new();
    du(&file_tree, &file_size, Vec::new(), &mut result);
    let first_answer = result
        .iter()
        .filter(|&(path, _size)| !file_size.contains_key(path))
        .filter(|&(_path, size)| *size <= 100_000)
        .map(|(_path, size)| size)
        .sum::<u32>();
    println!("First answer: {first_answer}");

    let total_space_available = 70_000_000;
    let total_space_required = 30_000_000;
    let total_space_used = result.get(&Vec::new()).unwrap();
    let space_remaining = total_space_available - total_space_used;
    let to_remove = total_space_required - space_remaining;
    let second_answer = result
        .iter()
        .filter(|&(path, _size)| !file_size.contains_key(path))
        .filter(|&(_path, size)| *size >= to_remove)
        .map(|(_path, size)| size)
        .min()
        .unwrap();
    println!("Second answer: {second_answer}");
}

fn du(
    file_tree: &HashMap<Vec<String>, HashSet<Vec<String>>>,
    file_size: &HashMap<Vec<String>, u32>,
    path: Vec<String>,
    result: &mut HashMap<Vec<String>, u32>,
) -> u32 {
    let mut size = *file_size.get(&path).unwrap_or(&0);
    if let Some(children) = file_tree.get(&path) {
        for child in children {
            size += du(file_tree, file_size, child.clone(), result);
        }
    }
    result.insert(path, size);
    size
}

