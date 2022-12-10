use std::ops::Index;

pub trait Get<P> {
    type Output;

    fn get(&self, index: P) -> Option<&Self::Output>;
}
