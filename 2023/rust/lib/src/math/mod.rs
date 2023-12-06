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

// solve quadratic equations in the form of
// x² + px + q
pub fn pq(p: f64, q: f64) -> Option<(f64, f64)> {
    // x1,2 = -p/2 +- sqrt(-(p/2)² - q)
    let denom = (p * p / 4.0) - q;

    let pre = -p / 2.0;

    if denom < 0.0 {
        None
    } else {
        let sq = denom.sqrt();
        Some((pre + sq, pre - sq))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_gauss_sum() {
        assert_eq!(gauss_sum(0), 0);
        assert_eq!(gauss_sum(1), 1);
        assert_eq!(gauss_sum(2), 3);
        assert_eq!(gauss_sum(3), 6);
        assert_eq!(gauss_sum(4), 10);
        assert_eq!(gauss_sum(5), 15);
        assert_eq!(gauss_sum(6), 21);
        assert_eq!(gauss_sum(7), 28);
        assert_eq!(gauss_sum(8), 36);
        assert_eq!(gauss_sum(9), 45);
        assert_eq!(gauss_sum(10), 55);
    }

    #[test]
    fn test_pq() {
        assert_eq!(pq(-7.0, 9.0), Some((5.302775637731995, 1.6972243622680054)));
    }
}
