use std::collections::{HashMap, HashSet};

use crate::{get_input, p_char, p_i32, p_string};

fn build_graph(edges: Vec<(i32, i32)>) -> HashMap<i32, HashSet<i32>> {
    let mut graph = HashMap::new();

    for (a, b) in edges {
        graph.entry(b).or_insert(HashSet::new()).insert(a);
    }

    graph
}

pub fn get_parsed_inputs() -> (HashMap<i32, HashSet<i32>>, Vec<Vec<i32>>) {
    let p_edges = p_i32()
        .and_then_left(p_char('|'))
        .chain(p_i32())
        .sep_by(p_char('\n'))
        .map(build_graph);

    let p_updates = p_i32().sep_by(p_char(',')).sep_by(p_char('\n'));

    let input = &get_input(5);
    p_edges
        .and_then_left(p_string("\n\n"))
        .chain(p_updates)
        .parse(input)
        .unwrap()
}

pub fn get_correct_updates(graph: &HashMap<i32, HashSet<i32>>, updates: &Vec<Vec<i32>>) -> HashSet<usize> {

    let mut correct = HashSet::new();
    
    'updates: for (i, update) in updates.iter().enumerate() {
        let update_set = HashSet::from_iter(update.clone());
        for (j, x) in update.iter().enumerate() {
            if graph.get(x).is_some_and(|deps| {
                deps.intersection(&update_set)
                    .filter(|y| !update[..j].contains(y))
                    .count()
                    != 0
            }) {
                continue 'updates;
            }
        }
        
        correct.insert(i);
    }

    correct
}

pub fn sort_update(graph: &HashMap<i32, HashSet<i32>>, update: Vec<i32>) -> Vec<i32> {
    let mut sub_graph: HashMap<i32, HashSet<i32>>= HashMap::new();

    let mut update_set = HashSet::from_iter(update.clone());

    for x in update {
        sub_graph.insert(x, graph[&x].intersection(&update_set).copied().collect());
    }

    let mut sorted = Vec::new();

    while !update_set.is_empty() {
        let mut to_remove = Vec::new();
        for &x in &update_set {
            if sub_graph[&x].iter().filter(|y| update_set.contains(y)).count() == 0 {
                to_remove.push(x)
            }
        }

        for x in to_remove {
            update_set.remove(&x);
            sorted.push(x);
        }
    }

    sorted
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn d5p1() {
        let (graph, updates) = get_parsed_inputs();
        let correct = get_correct_updates(&graph, &updates);

        let mut sum = 0;
        for (_, update) in updates.into_iter().enumerate().filter(|(i, _)| correct.contains(i)) {
            sum += update[update.len() / 2]
        }

        println!("{sum}");
    }

    #[test]
    fn d5p2() {

        let (graph, updates) = get_parsed_inputs();

        let correct = get_correct_updates(&graph, &updates);

        let mut sum = 0;
        for (_, update) in updates.into_iter().enumerate().filter(|(i, _)| !correct.contains(i)) {
            let sorted = sort_update(&graph, update);

            sum += sorted[sorted.len() / 2]
        }

        println!("{sum}");
    }
}
