use std::{collections::HashMap, hash::Hash};

pub fn count_elements<I: Hash + Eq, T: IntoIterator<Item = I>>(coll: T) -> HashMap<I, usize> {
    let mut hashmap = HashMap::new();

    for el in coll.into_iter() {
        hashmap
            .entry(el)
            .and_modify(|counter| *counter += 1)
            .or_insert(1);
    }

    hashmap
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_count_elements() {
        let v = vec![
            1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5,
        ];
        let res = count_elements(v);

        assert_eq!(res.get(&1), Some(&5));
        assert_eq!(res.get(&2), Some(&5));
        assert_eq!(res.get(&3), Some(&5));
        assert_eq!(res.get(&4), Some(&5));
        assert_eq!(res.get(&5), Some(&5));
    }
}
