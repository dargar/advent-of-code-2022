use std::collections::{HashMap, HashSet, VecDeque};
use std::fs;

fn main() {
    let input = fs::read_to_string("../input").expect("input file missing");
    let mut valves = HashMap::new();
    let mut flow_rates = HashMap::new();
    for line in input.lines() {
        let words = line.split(' ').collect::<Vec<_>>();
        // 0 Valve
        let label = words[1];
        // 2 has
        // 3 flow
        let flow_rate: u32 = words[4]
            .split('=')
            .nth(1)
            .map(|s| s.chars().take_while(|c| *c != ';').collect::<String>())
            .map(|s| s.parse().unwrap())
            .unwrap();
        // 4 tunnels
        // 5 lead
        // 6 to
        // 7 valves
        let vs: Vec<String> = words[9..]
            .iter()
            .map(|s| s.chars().take_while(|c| *c != ',').collect::<String>())
            .collect();
        valves.insert(label.to_string(), vs);
        flow_rates.insert(label.to_string(), flow_rate);
    }
    first_answer(&valves, &flow_rates);
    second_answer(&valves, &flow_rates);
}

fn first_answer(valves: &HashMap<String, Vec<String>>, flow_rates: &HashMap<String, u32>) {
    let mut path_cache = HashMap::new();
    let num_choices = valves
        .keys()
        .filter(|v| *flow_rates.get(*v).unwrap() > 0)
        .count();
    let mut choices = vec![vec![]];
    for _ in 0..num_choices {
        let mut next_candidates = Vec::new();
        for choice in choices {
            let (last_valve, last_minute) = choice.last().cloned().unwrap_or(("AA".to_string(), 0));
            let previous_valves = choice
                .iter()
                .map(|(v, _)| v.clone())
                .collect::<Vec<String>>();
            let candidates = valves
                .keys()
                .filter(|v| !previous_valves.contains(v))
                .map(|v| (v, *flow_rates.get(v).unwrap()))
                .filter(|(_, flow_rate)| *flow_rate > 0)
                .map(|(v, flow_rate)| {
                    (
                        v,
                        flow_rate,
                        1 + pathfind(valves, &last_valve, v, &mut path_cache),
                    )
                })
                .map(|(v, _, time_to_turn_on_valve)| {
                    (v.clone(), last_minute + time_to_turn_on_valve)
                })
                .filter(|(_, t)| *t <= 30)
                .collect::<Vec<_>>();
            if !candidates.is_empty() {
                for candidate in candidates {
                    let mut next_choice = choice.clone();
                    next_choice.push(candidate);
                    next_candidates.push(next_choice);
                }
            } else {
                next_candidates.push(choice);
            }
        }
        next_candidates.sort_by_key(|choice| {
            -choice
                .iter()
                .map(|(v, t)| {
                    let flow_rate = *flow_rates.get(v).unwrap();
                    (30 - t) * flow_rate as i32
                })
                .sum::<i32>()
        });
        if next_candidates.len() > 1 {
            let (top_half, _) = next_candidates.split_at(2 * (next_candidates.len() / 3));
            choices = top_half.to_vec();
        } else {
            choices = next_candidates;
        }
    }
    let max_pressured_released = choices
        .first()
        .unwrap()
        .iter()
        .map(|(v, t)| {
            let flow_rate = *flow_rates.get(v).unwrap();
            (30 - t) * flow_rate as i32
        })
        .sum::<i32>();
    println!("First answer: {max_pressured_released}");
}

fn second_answer(valves: &HashMap<String, Vec<String>>, flow_rates: &HashMap<String, u32>) {
    let mut path_cache = HashMap::new();
    let num_choices = valves
        .keys()
        .filter(|v| *flow_rates.get(*v).unwrap() > 0)
        .count();
    let mut choices = vec![(vec![], vec![])];
    for _ in 0..num_choices {
        let mut next_candidates = Vec::new();
        for (a, b) in choices {
            let previous_valves = a
                .iter()
                .chain(b.iter())
                .map(|(v, _): &(String, i32)| v.clone())
                .collect::<Vec<String>>();
            let next_as = next_choices(
                valves,
                flow_rates,
                a.clone(),
                &previous_valves,
                &mut path_cache,
            );
            let next_bs = next_choices(
                valves,
                flow_rates,
                b.clone(),
                &previous_valves,
                &mut path_cache,
            );
            if next_as.is_empty() && next_bs.is_empty() {
                next_candidates.push((a.clone(), b.clone()));
            } else if next_as.is_empty() {
                for choice in next_bs {
                    next_candidates.push((a.clone(), choice));
                }
            } else if next_bs.is_empty() {
                for choice in next_as {
                    next_candidates.push((choice, b.clone()));
                }
            } else {
                for choice_a in next_as.iter() {
                    for choice_b in next_bs.iter() {
                        let (va, _) = choice_a.last().unwrap();
                        let (vb, _) = choice_b.last().unwrap();
                        if va != vb {
                            next_candidates.push((choice_a.clone(), choice_b.clone()));
                        }
                    }
                }
            }
        }
        next_candidates.sort_by_key(|(next_as, next_bs)| {
            -next_as
                .iter()
                .chain(next_bs.iter())
                .map(|(v, t)| {
                    let flow_rate = *flow_rates.get(v).unwrap();
                    (30 - t) * flow_rate as i32
                })
                .sum::<i32>()
        });
        if next_candidates.len() > 1 {
            let (top_half, _) = next_candidates.split_at(next_candidates.len() / 2);
            choices = top_half.to_vec();
        } else {
            choices = next_candidates;
        }
    }
    let (me, elephant) = choices.first().unwrap();
    let max_pressured_released = me
        .iter()
        .chain(elephant.iter())
        .map(|(v, t)| {
            let flow_rate = *flow_rates.get(v).unwrap();
            (30 - t) * flow_rate as i32
        })
        .sum::<i32>();
    println!("Second answer: {max_pressured_released}");
}

fn next_choices(
    valves: &HashMap<String, Vec<String>>,
    flow_rates: &HashMap<String, u32>,
    choice: Vec<(String, i32)>,
    previous_valves: &[String],
    path_cache: &mut HashMap<(String, String), i32>,
) -> Vec<Vec<(String, i32)>> {
    let mut next_candidates = Vec::new();
    let (last_valve, last_minute) = choice.last().cloned().unwrap_or(("AA".to_string(), 4));
    let candidates = valves
        .keys()
        .filter(|v| !previous_valves.contains(v))
        .map(|v| (v, *flow_rates.get(v).unwrap()))
        .filter(|(_, flow_rate)| *flow_rate > 0)
        .map(|(v, flow_rate)| {
            (
                v,
                flow_rate,
                1 + pathfind(valves, &last_valve, v, path_cache),
            )
        })
        .map(|(v, _, time_to_turn_on_valve)| (v.clone(), last_minute + time_to_turn_on_valve))
        .filter(|(_, t)| *t <= 30)
        .collect::<Vec<_>>();
    if !candidates.is_empty() {
        for candidate in candidates {
            let mut next_choice = choice.clone();
            next_choice.push(candidate);
            next_candidates.push(next_choice);
        }
    } else {
        next_candidates.push(choice);
    }
    next_candidates
}

fn pathfind(
    valves: &HashMap<String, Vec<String>>,
    from: &String,
    to: &String,
    cache: &mut HashMap<(String, String), i32>,
) -> i32 {
    if let Some(steps) = cache.get(&(from.to_string(), to.to_string())) {
        return *steps;
    }
    let mut visited = HashSet::new();
    visited.insert(from);
    let mut queue = VecDeque::new();
    queue.push_back((from, 0));
    while let Some((valve, steps)) = queue.pop_front() {
        if valve == to || steps > 30 {
            cache.insert((from.clone(), to.clone()), steps);
            return steps;
        }

        for v in valves.get(valve).unwrap() {
            if !visited.contains(v) {
                visited.insert(v);
                queue.push_back((v, steps + 1));
            }
        }
    }
    unreachable!();
}
