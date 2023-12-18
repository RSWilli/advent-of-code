use super::Position;

// calculate the area of a polygon given its vertices using the shoelace formula
pub fn polygon_area(points: &[Position]) -> f64 {
    let mut area = 0.0;

    for i in 0..points.len() {
        let j = (i + 1) % points.len();

        area += points[i].x as f64 * points[j].y as f64;
        area -= points[j].x as f64 * points[i].y as f64;
    }

    area.abs() / 2.0
}
