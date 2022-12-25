use std::collections::HashSet;
use std::fs;
use std::io::Write;

#[derive(Debug, Copy, Clone)]
struct Blueprint {
    id: u16,
    ore_robot: u16,
    clay_robot: u16,
    obsidian_robot: (u16, u16),
    geode_robot: (u16, u16),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
struct Resources {
    ore: u16,
    clay: u16,
    obsidian: u16,
    geode: u16,
}

impl Resources {
    fn new() -> Self {
        Self {
            ore: 0,
            clay: 0,
            obsidian: 0,
            geode: 0,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
struct Collectors {
    ore: u8,
    clay: u8,
    obsidian: u8,
    geode: u8,
}

impl Collectors {
    fn new() -> Self {
        Self {
            ore: 1,
            clay: 0,
            obsidian: 0,
            geode: 0,
        }
    }
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq)]
enum Choice {
    Nothing,
    Geode,
    Obsidian,
    Clay,
    Ore,
}

fn main() {
    let input = fs::read_to_string("../input").expect("input file missing");
    let mut blueprints = Vec::new();
    for (i, blueprint) in input.lines().enumerate() {
        let numbers = blueprint
            .split(' ')
            .map(str::parse::<u16>)
            .flat_map(Result::ok)
            .collect::<Vec<_>>();
        blueprints.push(Blueprint {
            id: 1 + i as u16,
            ore_robot: numbers[0],
            clay_robot: numbers[1],
            obsidian_robot: (numbers[2], numbers[3]),
            geode_robot: (numbers[4], numbers[5]),
        });
    }

    first_answer(&blueprints);
    second_answer(&blueprints);
}

fn first_answer(blueprints: &Vec<Blueprint>) {
    let mut geode_counts = Vec::new();
    for blueprint in blueprints {
        let mut timelines = HashSet::new();
        timelines.insert((Collectors::new(), Resources::new()));
        for _ in 0..24 {
            let mut updated_timelines = HashSet::new();
            for (collectors, resources) in timelines {
                let choices = collect_construction_choices(blueprint, &collectors, &resources);
                for choice in choices {
                    let mut updated_collectors = collectors;
                    let mut updated_resources = resources;

                    updated_resources.ore += updated_collectors.ore as u16;
                    updated_resources.clay += updated_collectors.clay as u16;
                    updated_resources.obsidian += updated_collectors.obsidian as u16;
                    updated_resources.geode += updated_collectors.geode as u16;

                    match choice {
                        Choice::Nothing => (),
                        Choice::Ore => {
                            updated_collectors.ore += 1;
                            updated_resources.ore -= blueprint.ore_robot;
                        }
                        Choice::Clay => {
                            updated_collectors.clay += 1;
                            updated_resources.ore -= blueprint.clay_robot;
                        }
                        Choice::Obsidian => {
                            updated_collectors.obsidian += 1;
                            updated_resources.ore -= blueprint.obsidian_robot.0;
                            updated_resources.clay -= blueprint.obsidian_robot.1;
                        }
                        Choice::Geode => {
                            updated_collectors.geode += 1;
                            updated_resources.ore -= blueprint.geode_robot.0;
                            updated_resources.obsidian -= blueprint.geode_robot.1;
                        }
                    }

                    updated_timelines.insert((updated_collectors, updated_resources));
                }
            }
            timelines = updated_timelines;
        }
        let max_geode_count = timelines
            .iter()
            .map(|(_, resources)| resources)
            .map(|resources| resources.geode)
            .max()
            .unwrap();
        geode_counts.push(blueprint.id * max_geode_count);
    }
    let sum_of_quality_levels = geode_counts.into_iter().sum::<u16>();
    println!("First answer: {sum_of_quality_levels}");
}

fn second_answer(blueprints: &[Blueprint]) {
    let mut geode_counts = Vec::new();
    for blueprint in blueprints.iter().take(3) {
        let mut timelines = HashSet::new();
        timelines.insert((Collectors::new(), Resources::new()));
        for minute in 0..32 {
            std::io::stdout().flush().unwrap();
            let mut updated_timelines = HashSet::new();
            for (collectors, resources) in timelines {
                let choices = collect_construction_choices(blueprint, &collectors, &resources);
                for choice in choices {
                    let mut updated_collectors = collectors;
                    let mut updated_resources = resources;

                    updated_resources.ore += updated_collectors.ore as u16;
                    updated_resources.clay += updated_collectors.clay as u16;
                    updated_resources.obsidian += updated_collectors.obsidian as u16;
                    updated_resources.geode += updated_collectors.geode as u16;

                    match choice {
                        Choice::Nothing => (),
                        Choice::Ore => {
                            updated_collectors.ore += 1;
                            updated_resources.ore -= blueprint.ore_robot;
                        }
                        Choice::Clay => {
                            updated_collectors.clay += 1;
                            updated_resources.ore -= blueprint.clay_robot;
                        }
                        Choice::Obsidian => {
                            updated_collectors.obsidian += 1;
                            updated_resources.ore -= blueprint.obsidian_robot.0;
                            updated_resources.clay -= blueprint.obsidian_robot.1;
                        }
                        Choice::Geode => {
                            updated_collectors.geode += 1;
                            updated_resources.ore -= blueprint.geode_robot.0;
                            updated_resources.obsidian -= blueprint.geode_robot.1;
                        }
                    }

                    if minute > 32 / 4 && updated_collectors.clay == 0 {
                        continue;
                    }
                    if minute > 32 / 2 && updated_collectors.obsidian == 0 {
                        continue;
                    }
                    if minute > 3 * (32 / 4) && updated_collectors.geode == 0 {
                        continue;
                    }
                    updated_timelines.insert((updated_collectors, updated_resources));
                }
            }
            timelines = updated_timelines;
        }
        let max_geode_count = timelines
            .into_iter()
            .map(|(_, resources)| resources)
            .map(|resources| resources.geode)
            .max()
            .unwrap();
        geode_counts.push(max_geode_count);
    }
    let sum_of_quality_levels = geode_counts.into_iter().product::<u16>();
    println!("Second answer: {sum_of_quality_levels}");
}

fn collect_construction_choices(
    blueprint: &Blueprint,
    collectors: &Collectors,
    resources: &Resources,
) -> Vec<Choice> {
    if collectors.ore as u16 >= blueprint.geode_robot.0
        && collectors.obsidian as u16 >= blueprint.geode_robot.1
    {
        return vec![Choice::Geode];
    }

    let mut max_ore_cost = 0;
    max_ore_cost = std::cmp::max(max_ore_cost, blueprint.ore_robot);
    max_ore_cost = std::cmp::max(max_ore_cost, blueprint.clay_robot);
    max_ore_cost = std::cmp::max(max_ore_cost, blueprint.obsidian_robot.0);
    max_ore_cost = std::cmp::max(max_ore_cost, blueprint.geode_robot.0);

    let mut choices = Vec::new();
    if resources.ore >= blueprint.ore_robot && (collectors.ore as u16) < max_ore_cost {
        choices.push(Choice::Ore);
    }
    if resources.ore >= blueprint.clay_robot
        && (collectors.clay as u16) < blueprint.obsidian_robot.1
    {
        choices.push(Choice::Clay);
    }
    if resources.ore >= blueprint.obsidian_robot.0
        && resources.clay >= blueprint.obsidian_robot.1
        && (collectors.obsidian as u16) < blueprint.geode_robot.1
    {
        choices.push(Choice::Obsidian);
    }
    if resources.ore >= blueprint.geode_robot.0 && resources.obsidian >= blueprint.geode_robot.1 {
        choices.push(Choice::Geode);
    }
    choices.push(Choice::Nothing);
    choices.sort();
    choices.into_iter().take(3).collect()
}
