use lib::{parse::*, AOCError, AdventOfCode};
use nom::combinator::map;
use std::{cmp::Ordering, collections::HashMap};

#[derive(Debug, Clone)]
struct PartRange {
    extremly_cool_looking: (u64, u64),
    musical: (u64, u64),
    aerodynamic: (u64, u64),
    shiny: (u64, u64),
}

impl PartRange {
    fn count(&self) -> u64 {
        (self.extremly_cool_looking.1 - self.extremly_cool_looking.0 + 1)
            * (self.musical.1 - self.musical.0 + 1)
            * (self.aerodynamic.1 - self.aerodynamic.0 + 1)
            * (self.shiny.1 - self.shiny.0 + 1)
    }

    fn narrow(&self, prop: Category, ordering: Ordering, threshold: u64) -> Option<Self> {
        let (min, max) = match prop {
            Category::ExtremelyCoolLooking => self.extremly_cool_looking,
            Category::Musical => self.musical,
            Category::Aerodynamic => self.aerodynamic,
            Category::Shiny => self.shiny,
        };

        let (min, max) = match ordering {
            Ordering::Less => (min, threshold - 1),
            Ordering::Greater => (threshold + 1, max),
            Ordering::Equal => unreachable!(),
        };

        if min > max {
            return None;
        }

        let mut new = self.clone();

        match prop {
            Category::ExtremelyCoolLooking => new.extremly_cool_looking = (min, max),
            Category::Musical => new.musical = (min, max),
            Category::Aerodynamic => new.aerodynamic = (min, max),
            Category::Shiny => new.shiny = (min, max),
        }

        Some(new)
    }

    fn narrow_inverse(&self, prop: Category, ordering: Ordering, threshold: u64) -> Option<Self> {
        let (min, max) = match prop {
            Category::ExtremelyCoolLooking => self.extremly_cool_looking,
            Category::Musical => self.musical,
            Category::Aerodynamic => self.aerodynamic,
            Category::Shiny => self.shiny,
        };

        let (min, max) = match ordering {
            Ordering::Less => (threshold, max),
            Ordering::Greater => (min, threshold),
            Ordering::Equal => unreachable!(),
        };

        if min > max {
            return None;
        }

        let mut new = self.clone();

        match prop {
            Category::ExtremelyCoolLooking => new.extremly_cool_looking = (min, max),
            Category::Musical => new.musical = (min, max),
            Category::Aerodynamic => new.aerodynamic = (min, max),
            Category::Shiny => new.shiny = (min, max),
        }

        Some(new)
    }
}

#[derive(Debug)]
struct Part {
    extremly_cool_looking: u64,
    musical: u64,
    aerodynamic: u64,
    shiny: u64,
}

impl Part {
    fn satisfies(&self, prop: Category, ordering: Ordering, threshold: u64) -> bool {
        match prop {
            Category::ExtremelyCoolLooking => self.extremly_cool_looking,
            Category::Musical => self.musical,
            Category::Aerodynamic => self.aerodynamic,
            Category::Shiny => self.shiny,
        }
        .cmp(&threshold)
            == ordering
    }

    fn rating(&self) -> u64 {
        self.extremly_cool_looking + self.musical + self.aerodynamic + self.shiny
    }
}

// {x=787,m=2655,a=1222,s=2876}
fn parse_part(inp: &str) -> ParseResult<Part> {
    let (inp, (_, x, _, m, _, a, _, s)) = delimited(
        tag("{"),
        tuple((
            tag("x="),
            u64,
            tag(",m="),
            u64,
            tag(",a="),
            u64,
            tag(",s="),
            u64,
        )),
        tag("}"),
    )(inp)?;

    Ok((
        inp,
        Part {
            extremly_cool_looking: x,
            musical: m,
            aerodynamic: a,
            shiny: s,
        },
    ))
}

#[derive(Debug, Clone, Copy)]
enum Category {
    ExtremelyCoolLooking,
    Musical,
    Aerodynamic,
    Shiny,
}

fn parse_category(s: &str) -> ParseResult<Category> {
    let (s, c) = one_of("xmas")(s)?;

    Ok((
        s,
        match c {
            'x' => Category::ExtremelyCoolLooking,
            'm' => Category::Musical,
            'a' => Category::Aerodynamic,
            's' => Category::Shiny,
            _ => unreachable!(),
        },
    ))
}

#[derive(Debug, Clone)]
enum WorkflowAction {
    Goto(String),
    Accept,
    Reject,
}

fn parse_workflow_action(s: &str) -> ParseResult<WorkflowAction> {
    let (s, action) = alt((
        map(tag("A"), |_| WorkflowAction::Accept),
        map(tag("R"), |_| WorkflowAction::Reject),
        map(alpha0, |s: &str| WorkflowAction::Goto(s.to_owned())),
    ))(s)?;

    Ok((s, action))
}

#[derive(Debug)]
enum WorkflowStep {
    Conditional {
        category: Category,
        threshold: u64,
        condition: Ordering,
        action: WorkflowAction,
    },
    Unconditional(WorkflowAction),
}

impl WorkflowStep {
    fn run(&self, part: &Part) -> Option<WorkflowAction> {
        match self {
            WorkflowStep::Conditional {
                category,
                threshold,
                condition,
                action,
            } => {
                if part.satisfies(*category, *condition, *threshold) {
                    Some(action.clone())
                } else {
                    None
                }
            }
            WorkflowStep::Unconditional(action) => Some(action.clone()),
        }
    }

    fn apply_range(
        &self,
        range: &PartRange,
    ) -> (Option<(WorkflowAction, PartRange)>, Option<PartRange>) {
        match self {
            WorkflowStep::Conditional {
                category,
                threshold,
                condition,
                action,
            } => (
                range
                    .narrow(*category, *condition, *threshold)
                    .map(|r| (action.clone(), r)),
                range.narrow_inverse(*category, *condition, *threshold),
            ),
            WorkflowStep::Unconditional(action) => (Some((action.clone(), range.clone())), None),
        }
    }
}

fn parse_workflow_unconditional(s: &str) -> ParseResult<WorkflowStep> {
    let (s, action) = parse_workflow_action(s)?;

    Ok((s, WorkflowStep::Unconditional(action)))
}

fn parse_ordering(s: &str) -> ParseResult<Ordering> {
    let (s, o) = alt((
        map(tag("<"), |_| Ordering::Less),
        // map(tag("="), |_| Ordering::Equal),
        map(tag(">"), |_| Ordering::Greater),
    ))(s)?;

    Ok((s, o))
}

// s>2770:qs m<1801:hdj
fn parse_workflow_step_conditional(s: &str) -> ParseResult<WorkflowStep> {
    let (s, (category, condition, threshold, _, action)) = tuple((
        parse_category,
        parse_ordering,
        u64,
        tag(":"),
        parse_workflow_action,
    ))(s)?;

    Ok((
        s,
        WorkflowStep::Conditional {
            category,
            threshold,
            condition,
            action,
        },
    ))
}

fn parse_workflow_step(s: &str) -> ParseResult<WorkflowStep> {
    alt((
        parse_workflow_step_conditional,
        parse_workflow_unconditional,
    ))(s)
}

#[derive(Debug)]
struct Workflow {
    steps: Vec<WorkflowStep>,
}

impl Workflow {
    fn run(&self, part: &Part) -> WorkflowAction {
        for step in &self.steps {
            if let Some(action) = step.run(part) {
                return action;
            }
        }

        unreachable!()
    }

    fn apply_range(&self, range: &PartRange) -> Vec<(WorkflowAction, PartRange)> {
        let mut actions = Vec::new();
        let mut range = range.clone();

        for step in &self.steps {
            let (action, fallthrough) = step.apply_range(&range);

            if let Some((action, success_range)) = action {
                actions.push((action, success_range));
            }

            if let Some(fallthrough_range) = fallthrough {
                range = fallthrough_range;
            }
        }

        actions
    }
}

fn parse_workflow(s: &str) -> ParseResult<(String, Workflow)> {
    let (s, (name, steps)) = tuple((
        alpha0,
        delimited(
            tag("{"),
            separated_list1(tag(","), parse_workflow_step),
            tag("}"),
        ),
    ))(s)?;

    Ok((s, (name.to_owned(), Workflow { steps })))
}

#[derive(Debug)]
struct Workflows {
    workflows: HashMap<String, Workflow>,
}

impl Workflows {
    fn accepts(&self, part: &Part) -> bool {
        let mut current_state = "in".to_owned();

        loop {
            let workflow = self.workflows.get(&current_state).unwrap();

            match workflow.run(part) {
                WorkflowAction::Goto(state) => current_state = state,
                WorkflowAction::Accept => return true,
                WorkflowAction::Reject => return false,
            }
        }
    }

    fn count_accepted_parts(&self) -> u64 {
        let mut todo = vec![(
            String::from("in"),
            PartRange {
                extremly_cool_looking: (1, 4000),
                musical: (1, 4000),
                aerodynamic: (1, 4000),
                shiny: (1, 4000),
            },
        )];

        let mut accepted = vec![];

        while let Some((name, range)) = todo.pop() {
            let workflow = self.workflows.get(&name).unwrap();

            for (action, new_range) in workflow.apply_range(&range) {
                match action {
                    WorkflowAction::Goto(state) => todo.push((state, new_range)),
                    WorkflowAction::Accept => accepted.push(new_range),
                    WorkflowAction::Reject => {}
                }
            }
        }

        accepted.into_iter().map(|r| r.count()).sum()
    }
}

fn parse_workflows(s: &str) -> ParseResult<Workflows> {
    map(separated_list1(tag("\n"), parse_workflow), |workflows| {
        Workflows {
            workflows: workflows.into_iter().collect(),
        }
    })(s)
}

struct Day {}

impl AdventOfCode for Day {
    const DAY: usize = 19;

    type In = (Vec<Part>, Workflows);

    type Out = u64;

    fn parse(s: &str) -> ParseResult<Self::In> {
        let (s, (workflows, _, parts)) = parse_all(tuple((
            parse_workflows,
            tag("\n\n"),
            separated_list1(tag("\n"), parse_part),
        )))(s)?;

        Ok((s, (parts, workflows)))
    }

    fn part1(input: &Self::In) -> Result<Self::Out, AOCError> {
        Ok(input
            .0
            .iter()
            .filter(|p| input.1.accepts(p))
            .map(|p| p.rating())
            .sum())
    }

    fn part2(input: &Self::In) -> Result<Self::Out, AOCError> {
        Ok(input.1.count_accepted_parts())
    }
}

fn main() -> Result<(), AOCError> {
    lib::run(Day {})
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test1() -> Result<(), AOCError> {
        lib::test(Day {}, lib::Part::Part1, 1, 19114)
    }

    #[test]
    fn test2() -> Result<(), AOCError> {
        lib::test(Day {}, lib::Part::Part2, 1, 167409079868000)
    }
}
