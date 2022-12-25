fn main() {
    let input = std::fs::read_to_string("../input").expect("input file missing");
    let fuel_requirements_in_decimal = input
        .lines()
        .map(snafu_to_decimal)
        .sum::<i64>();
    let fuel_requirement_sum_in_snafu = decimal_to_snafu(fuel_requirements_in_decimal);
    println!("Answer: {fuel_requirement_sum_in_snafu}");
}

fn snafu_to_decimal(snafu: &str) -> i64 {
    let mut decimal_number = 0;
    for (index, snafu_digit) in snafu.chars().rev().enumerate() {
        let snafu_magnitude = 5_i64.pow(index as u32);
        let decimal_value = match snafu_digit {
            '2' =>  2 * snafu_magnitude,
            '1' =>  1 * snafu_magnitude,
            '0' =>  0 * snafu_magnitude,
            '-' => -1 * snafu_magnitude,
            '=' => -2 * snafu_magnitude,
            _ => unreachable!(),
        };
        decimal_number += decimal_value;
    }
    decimal_number
}

fn decimal_to_snafu(decimal: i64) -> String {
    if decimal == 0 {
        return "0".to_string();
    }

    let mut max_exponent = 0;
    for exponent in 0.. {
        if decimal / 5_i64.pow(exponent) == 0 {
            max_exponent = exponent - 1;
            break;
        }
    }
    let mut quintal_digits = Vec::new();
    let mut remaining_decimal = decimal;
    for exponent in (0..=max_exponent).rev() {
        let quintal_base = 5_i64.pow(exponent);
        let quintal_digit = remaining_decimal / quintal_base;
        quintal_digits.push(quintal_digit);
        remaining_decimal -= quintal_digit * quintal_base;
    }
    let mut snafu_digits_reversed = Vec::new();
    let mut carry = false;
    for quintal_digit in quintal_digits.into_iter().rev() {
        let carried_quintal_digit = if carry {
            carry = false;
            quintal_digit + 1
        } else {
            quintal_digit
        };
        if let Some(snafu_digit) = char::from_digit(carried_quintal_digit as u32, 3) {
            snafu_digits_reversed.push(snafu_digit);
        } else {
            carry = true;
            let digit_to_leave_behind = -(5 - carried_quintal_digit);
            let snafu_digit = match digit_to_leave_behind {
                -0 => '0',
                -1 => '-',
                -2 => '=',
                _ => unreachable!(),
            };
            snafu_digits_reversed.push(snafu_digit);
        }
    }
    if carry {
        snafu_digits_reversed.push('1');
    }
    snafu_digits_reversed.into_iter().rev().collect()
}
