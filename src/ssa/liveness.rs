use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
};

use crate::{
    ssa::cfg::CFG,
    types::{hir_types::Label, ssa_types::BasicBlock},
};

pub struct Liveness {
    pub live_in: HashMap<Label, HashSet<String>>,
    pub live_out: HashMap<Label, HashSet<String>>,
}

impl Liveness {
    pub fn new() -> Liveness {
        return Liveness {
            live_in: HashMap::new(),
            live_out: HashMap::new(),
        };
    }
    pub fn compute_liveness(&mut self, cfg: &CFG, blocks: &HashMap<Label, BasicBlock>)
    // -> HashMap<Label, HashSet<String>>
    {
        // initialize empty sets
        for h_index in &cfg.pre_order {
            let label = cfg.get_label(*h_index);
            self.live_in.insert(label.clone(), HashSet::new());
            self.live_out.insert(label.clone(), HashSet::new());
        }

        let mut changed = true;
        while changed {
            changed = false;
            for h_index in cfg.pre_order.iter().rev() {
                let label = cfg.get_label(*h_index);

                let block = blocks.get(label).unwrap();
                let in_set = self.live_in.get(label).unwrap().clone();
                let mut out_set = HashSet::new();

                // union of successors' in sets
                for succ in &block.terminator.successors() {
                    if let Some(succ_in) = self.live_in.get(succ) {
                        out_set.extend(succ_in.iter().cloned());
                    }
                }

                // current defs
                let mut defs: HashSet<String> = HashSet::new();
                let mut uses: HashSet<String> = HashSet::new();
                for instr in &block.instrs {
                    if let Some(name) = instr.defines() {
                        defs.insert(name);
                    }
                    for name in instr.uses() {
                        uses.insert(name);
                    }
                }

                let diff: HashSet<_> = out_set.difference(&defs).cloned().collect();
                let new_in: HashSet<_> = uses.union(&diff).cloned().collect();
                if new_in != in_set {
                    self.live_in.insert(label.clone(), new_in);
                    changed = true;
                }
                self.live_out.insert(label.clone(), out_set);
            }

            
        }
        // for (label, live) in &live_in {
        //     println!("Live-in @ {}: {:?}", label.0, live);
        // }

        // self.live_in
    }
}
