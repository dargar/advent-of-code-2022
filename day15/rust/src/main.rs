use std::collections::{HashSet, HashMap};
use std::fs;
use std::cmp::Ordering;

fn main() {
    let input = fs::read_to_string("../input").unwrap();
    let mut sensors = HashMap::new();
    let mut beacons = HashSet::new();
    for line in input.lines() {
        let mut nums = Vec::new();
        for word in line.split(' ') {
            if word.starts_with("x=") || word.starts_with("y=") {
                let num_as_string = word.chars()
                    .filter(|c| c.is_digit(10) || *c == '-')
                    .collect::<String>();
                let num = num_as_string.parse::<i64>().unwrap();
                nums.push(num);
            }
        }
        sensors.insert((nums[0], nums[1]), (nums[2], nums[3]));
        beacons.insert((nums[2], nums[3]));
    }

    let target_y = 2_000_000;
    let beacon_bound = 4_000_000;
    let mut covered = HashSet::new();
    let mut perimiter = HashMap::new();
    for ((sx, sy), (bx, by)) in &sensors {
        let mhd = (sx - bx).abs() + (sy - by).abs();
        match sy.cmp(&target_y) {
            Ordering::Less => {
                let mhd_remaining = mhd - (target_y - sy);
                if mhd_remaining >= 0 {
                    let x_min = sx - mhd_remaining;
                    let x_max = sx + mhd_remaining;
                    for x in x_min..=x_max {
                        covered.insert((x, target_y));
                    }
                }
            }
            Ordering::Greater => {
                let mhd_remaining = mhd - (sy - target_y);
                if mhd_remaining >= 0 {
                    let x_min = sx - mhd_remaining;
                    let x_max = sx + mhd_remaining;
                    for x in x_min..=x_max {
                        covered.insert((x, target_y));
                    }
                }
            }
            Ordering::Equal => {
                let x_min = sx - mhd;
                let x_max = sx + mhd;
                for x in x_min..=x_max {
                    covered.insert((x, target_y));
                }
            }
        }

        let mut y = mhd + 1;
        let mut x = 0;
        while y >= -(mhd + 1) {
            let rhs = (sx + x, sy - y);
            let lhs = (sx - x, sy - y);
            if (0 <= rhs.0 && rhs.0 <= beacon_bound) && (0 <= rhs.1 && rhs.1 <= beacon_bound) {
                perimiter.entry(rhs).and_modify(|counter| *counter += 1).or_insert(1);
            }
            if (0 <= lhs.0 && lhs.0 <= beacon_bound) && (0 <= lhs.1 && lhs.1 <= beacon_bound) {
                perimiter.entry(lhs).and_modify(|counter| *counter += 1).or_insert(1);
            }
            y -= 1;
            x += 1;
        }
    }
    let tmp = &covered - &beacons;
    let covered_count = tmp.len();
    println!("First answer: {covered_count}");

    for (x, y) in perimiter.keys() {
        let outside_all = sensors.iter().all(|((sx, sy), (bx, by))| {
            let inner_mhd = (sx - bx).abs() + (sy - by).abs();
            let outer_mhd = (sx - x).abs() + (sy - y).abs();
            inner_mhd < outer_mhd
        });
        if outside_all {
            println!("Second answer: {}", x * 4_000_000 + y);
        }
    }
}
