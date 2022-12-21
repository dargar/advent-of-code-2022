use std::collections::HashMap;
use std::fs;

fn main() {
    let input = fs::read_to_string("../input").expect("input file missing");
    let mut numbers: HashMap<String, i64> = HashMap::new();
    let mut operations: HashMap<String, (String, String, String)> = HashMap::new();
    for line in input.lines() {
        let words = line.split(' ').collect::<Vec<&str>>();
        let mut name = words[0].to_string();
        name.pop();
        if words.len() == 2 {
            let number = words[1].parse().unwrap();
            numbers.insert(name, number);
        } else if words.len() == 4 {
            let a = words[1].to_string();
            let o = words[2].to_string();
            let b = words[3].to_string();
            operations.insert(name, (a, o, b));
        } else {
            unreachable!();
        }
    }
    println!("First answer: {}", first_answer(&numbers, &operations));
    println!("Second answer: {}", second_answer(&numbers, &operations));
}

fn first_answer(
    numbers: &HashMap<String, i64>,
    operations: &HashMap<String, (String, String, String)>,
) -> i64 {
    answer(numbers, operations, "root".to_string())
}

fn second_answer(
    numbers: &HashMap<String, i64>,
    operations: &HashMap<String, (String, String, String)>,
) -> i64 {
    let (a, _, b) = operations.get("root").unwrap();
    let mut operations = operations.clone();
    operations.insert(
        "root".to_string(),
        (a.to_string(), "=".to_string(), b.to_string()),
    );

    let mut numbers = numbers.clone();

    let mut lo = 0;
    let mut hi = std::i64::MAX;
    for n in 0.. {
        numbers.insert("humn".to_string(), 10i64.pow(n));

        let answer = answer(&numbers, &operations, "root".to_string());
        if answer < 0 {
            lo = 10i64.pow(n - 1);
            hi = 10i64.pow(n);
            break;
        }
    }
    loop {
        let mid = lo + ((hi - lo) / 2);
        numbers.insert("humn".to_string(), mid);
        let ans = answer(&numbers, &operations, "root".to_string());

        if ans == 0 {
            return mid - 1;
        }

        if ans > 0 {
            lo = mid;
        } else {
            hi = mid;
        }
    }
}

fn answer(
    numbers: &HashMap<String, i64>,
    operations: &HashMap<String, (String, String, String)>,
    name: String,
) -> i64 {
    if let Some(number) = numbers.get(&name) {
        return *number;
    }

    let (a, o, b) = operations.get(&name).unwrap().clone();
    let a_num = answer(numbers, operations, a);
    let b_num = answer(numbers, operations, b);

    match o.as_str() {
        "=" => a_num - b_num,
        "+" => a_num + b_num,
        "-" => a_num - b_num,
        "*" => a_num * b_num,
        "/" => a_num / b_num,
        _ => unreachable!(),
    }
}
