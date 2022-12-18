use std::collections::{HashSet, VecDeque};
use std::fs;

type Droplet = (i32, i32, i32);

fn main() {
    let input = fs::read_to_string("../input").expect("input file missing");
    let mut droplets = HashSet::new();
    for line in input.lines() {
        let numbers = line.split(',')
            .map(str::parse::<i32>)
            .flat_map(Result::ok)
            .collect::<Vec<_>>();
        droplets.insert((numbers[0], numbers[1], numbers[2]));
    }

    let mut exposed_surfaces = 0;
    for droplet in &droplets {
        for neighbour in neighbours(droplet) {
            if !droplets.contains(&neighbour) {
                exposed_surfaces += 1;
            }
        }
    }
    println!("First answer: {exposed_surfaces}");

    let mut exposed_exterior_surfaces = 0;
    for droplet in &droplets {
        for neighbour in neighbours(droplet) {
            if !droplets.contains(&neighbour) && is_external(&droplets, &neighbour) {
                exposed_exterior_surfaces += 1;
            }
        }
    }
    println!("Second answer: {exposed_exterior_surfaces}");
}

fn is_external(droplets: &HashSet<Droplet>, droplet: &Droplet) -> bool {
    let min_x = droplets.iter().map(|(x, _y, _z)| *x).min().unwrap();
    let max_x = droplets.iter().map(|(x, _y, _z)| *x).max().unwrap();
    let min_y = droplets.iter().map(|(_x, y, _z)| *y).min().unwrap();
    let max_y = droplets.iter().map(|(_x, y, _z)| *y).max().unwrap();
    let min_z = droplets.iter().map(|(_x, _y, z)| *z).min().unwrap();
    let max_z = droplets.iter().map(|(_x, _y, z)| *z).max().unwrap();
    let mut visited = HashSet::new();
    let mut queue = VecDeque::new();
    queue.push_back(*droplet);
    while let Some(droplet @ (dx, dy, dz)) = queue.pop_front() {
        if dx < min_x || dx > max_x || dy < min_y || dy > max_y || dz < min_z || dz > max_z {
            return true;
        }

        for neighbour in neighbours(&droplet) {
            if droplets.contains(&neighbour) {
                continue;
            }

            if visited.contains(&neighbour) {
                continue;
            }

            visited.insert(neighbour);
            queue.push_back(neighbour);
        }
    }
    false
}

fn neighbours((x, y, z): &Droplet) -> Vec<Droplet> {
    vec![
        (*x + 1, *y, *z),
        (*x - 1, *y, *z),
        (*x, *y + 1, *z),
        (*x, *y - 1, *z),
        (*x, *y, *z + 1),
        (*x, *y, *z - 1),
    ]
}
