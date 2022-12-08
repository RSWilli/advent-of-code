pub fn gauss_sum(n: i64) -> i64 {
    (n * (n + 1)) / 2
}

pub fn gcd(a: i64, b: i64) -> i64 {
    if a < 0 {
        gcd(-a, b)
    } else if b < 0 {
        gcd(a, -b)
    } else if b == 0 {
        a
    } else {
        gcd(b, a % b)
    }
}
