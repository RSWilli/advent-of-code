use std::{collections::HashMap, hash::Hash};

pub fn count_elements<I: Hash + Eq, T: IntoIterator<Item = I>>(coll: T) -> HashMap<I, usize> {
    let mut hashmap: HashMap<I, usize> = HashMap::new();

    for el in coll.into_iter() {
        if let Some(c) = hashmap.get_mut(&el) {
            *c += 1;
        } else {
            hashmap.insert(el, 1);
        }
    }

    hashmap
}
