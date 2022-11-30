use std::{collections::HashMap, hash::Hash};

pub fn count_elements<I: Hash + Eq, T: IntoIterator<Item = I>>(coll: T) -> HashMap<I, usize> {
    let mut hashmap = HashMap::new();

    for el in coll.into_iter() {
        if let Some(c) = hashmap.get_mut(&el) {
            *c += 1;
        } else {
            hashmap.insert(el, 1);
        }
    }

    hashmap
}

pub fn chunks<T>(l: &Vec<T>, size: usize) -> Vec<&[T]> {
    let count = l.len() / size;
    let mut res = Vec::with_capacity(count);

    for i in 0..count {
        let offset = i * size;
        res.push(&l[offset..offset + size]);
    }

    res
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

    #[test]
    fn test_chunks() {
        let v = vec![1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
        let res = chunks(&v, 3);

        assert_eq!(res.len(), 3);
        assert_eq!(res[0], &[1, 2, 3]);
        assert_eq!(res[1], &[4, 5, 6]);
        assert_eq!(res[2], &[7, 8, 9]);
    }
}
