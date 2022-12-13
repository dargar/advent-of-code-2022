use std::cmp::Ordering;
use std::fs;
use std::str::FromStr;

fn main() {
    let input = fs::read_to_string("../input").expect("input file missing");
    let packets: Vec<Packet> = input.lines().map(str::parse).flat_map(Result::ok).collect();
    let right_order_sum: usize = packets
        .chunks(2)
        .enumerate()
        .filter(|(_, pair)| pair[0] < pair[1])
        .map(|(index, _)| index + 1)
        .sum();
    println!("First answer: {right_order_sum}");

    let mut packets = packets;
    let ctrl_a = Packet::List(vec![Packet::List(vec![Packet::Int(2)])]);
    let ctrl_b = Packet::List(vec![Packet::List(vec![Packet::Int(6)])]);
    packets.push(ctrl_a.clone());
    packets.push(ctrl_b.clone());
    packets.sort();
    let decoder_key = packets
        .iter()
        .enumerate()
        .filter(|(_, p)| **p == ctrl_a || **p == ctrl_b)
        .map(|(i, _)| i + 1)
        .product::<usize>();
    println!("Second answer: {decoder_key}");
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Packet {
    Int(u32),
    List(Vec<Packet>),
}

impl FromStr for Packet {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut packets = vec![Packet::List(Vec::new())];
        let mut contents = String::new();
        for c in s.chars() {
            if c == '[' {
                let ints = contents
                    .split(',')
                    .flat_map(str::parse::<u32>)
                    .map(Packet::Int)
                    .collect::<Vec<Packet>>();
                contents.clear();
                if let Some(Packet::List(ps)) = packets.last_mut() {
                    ps.extend(ints);
                }
                packets.push(Packet::List(Vec::new()));
            } else if c == ']' {
                let ints = contents
                    .split(',')
                    .flat_map(str::parse::<u32>)
                    .map(Packet::Int)
                    .collect::<Vec<Packet>>();
                contents.clear();
                if let Some(Packet::List(ps)) = packets.last_mut() {
                    ps.extend(ints);
                }
                let p = packets.pop().unwrap();
                if let Some(Packet::List(ps)) = packets.last_mut() {
                    ps.push(p);
                }
            } else {
                contents.push(c);
            }
        }
        match packets.pop().unwrap() {
            Packet::List(xs) if !xs.is_empty() => Ok(xs[0].clone()),
            _ => Err("Invalid input string".to_string()),
        }
    }
}

impl Ord for Packet {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Packet::Int(l), Packet::Int(r)) => l.cmp(r),
            (Packet::List(ls), Packet::List(rs)) => {
                for (l, r) in ls.iter().zip(rs.iter()) {
                    match l.cmp(r) {
                        Ordering::Equal => (),
                        ord => return ord,
                    }
                }
                ls.len().cmp(&rs.len())
            }
            (Packet::Int(l), rs) => {
                let wrapped_l = Packet::List(vec![Packet::Int(*l)]);
                wrapped_l.cmp(rs)
            }
            (ls, Packet::Int(r)) => {
                let wrapped_r = Packet::List(vec![Packet::Int(*r)]);
                ls.cmp(&wrapped_r)
            }
        }
    }
}

impl PartialOrd for Packet {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
