use std::collections::HashSet;

#[derive(Clone)]
pub(super) struct State<'a, ID>
where
    ID: Clone + Copy + Eq + std::hash::Hash,
{
    time_left: usize,
    position: ID,
    open_valves: HashSet<ID>,
    connections: &'a dyn Fn(ID) -> Vec<(ID, usize, usize)>,
}

// impl<ID> PartialEq for State<'_, ID>
// where
//     ID: Clone + Copy + Eq + std::hash::Hash,
// {
//     fn eq(&self, other: &Self) -> bool {
//         self.position == other.position && self.open_valves == other.open_valves
//     }
// }

// impl<ID> Eq for State<'_, ID> where ID: Clone + Copy + Eq + std::hash::Hash {}

// impl<ID> std::hash::Hash for State<'_, ID>
// where
//     ID: Clone + Copy + Eq + std::hash::Hash,
// {
//     fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
//         self.position.hash(state);
//         self.open_valves.iter().collect::<Vec<_>>().hash(state);
//     }
// }

impl<'a, ID> State<'a, ID>
where
    ID: Clone + Copy + Eq + std::hash::Hash,
{
    pub(super) fn new(
        position: ID,
        time: usize,
        connections: &'a dyn Fn(ID) -> Vec<(ID, usize, usize)>,
    ) -> Self {
        Self {
            position,
            time_left: time,
            open_valves: HashSet::new(),
            connections,
        }
    }

    // return next state and possible flow release for each valve
    pub(super) fn successors(&self) -> Vec<(Self, usize)> {
        let mut successors = Vec::new();
        for (next_pos, time_cost, flow) in (self.connections)(self.position) {
            if self.open_valves.contains(&next_pos) {
                continue;
            }

            let mut open_valves = self.open_valves.clone();
            open_valves.insert(next_pos);

            if self.time_left >= time_cost {
                successors.push((
                    Self {
                        time_left: self.time_left - time_cost - 1,
                        position: next_pos,
                        open_valves,
                        connections: self.connections,
                    },
                    flow * self.time_left - time_cost - 1,
                ));
            }
        }

        if successors.is_empty() && self.time_left > 0 {
            // try standing still
            successors.push((
                Self {
                    time_left: self.time_left - 1,
                    position: self.position,
                    open_valves: self.open_valves.clone(),
                    connections: self.connections,
                },
                0,
            ))
        }

        successors
    }

    pub(super) fn is_time_up(&self) -> bool {
        self.time_left == 0
    }
}
