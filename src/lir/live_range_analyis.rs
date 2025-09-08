use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
    iter::Map,
};

use itertools::min;
use proptest::bits::usize;

use crate::{
    lir::{self, lir::LIRProgram},
    ssa::ssa::SSAContext,
    types::{
        hir_types::Label,
        lir_types::{LIRBasicBlock, VirtualReg},
        op,
    },
};

#[derive(Debug, Clone)]
pub struct LiveRange {
    pub virtual_reg: VirtualReg,
    pub start: usize,
    pub end: usize,
}

pub fn live_range_multiple_analysis(programs: &Vec<LIRProgram>) -> Vec<Vec<LiveRange>> {
    let mut res: Vec<Vec<LiveRange>> = Vec::new();
    for i in programs {
        res.push(live_range_analysis(i));
    }
    return res;
}

pub fn live_range_analysis(program: &LIRProgram) -> Vec<LiveRange> {
    let mut analysis = LiveRangeAnalysis::new();
    analysis.build_use_defs(&program);
    analysis.build_liveness(&program);

    // analysis.idx_block_pass(&program);

    // let mut base_index: HashMap<Label, usize> = HashMap::new();
    analysis.pre_backward_pass(program);

    let mut curr_idx = 0;
    for i in &program.block_order {
        analysis.backward_scan(program.blocks.get(&i).unwrap(), i, curr_idx);
        curr_idx += program.blocks.get(&i).unwrap().instrs.len();
    }

    for i in &program.block_order {
        analysis.post_backward_scan_pass(program.blocks.get(&i).unwrap(), i, curr_idx)
    }

    analysis.finish();

    return analysis.live_ranges;
}

struct Interval {
    segments: Vec<(usize, usize)>,
}

pub struct LiveRangeAnalysis {
    pub uses: HashMap<Label, HashSet<VirtualReg>>,
    pub defs: HashMap<Label, HashSet<VirtualReg>>,
    pub live_in: HashMap<Label, HashSet<VirtualReg>>,
    pub live_out: HashMap<Label, HashSet<VirtualReg>>,
    pub live_ranges: Vec<LiveRange>,
    live_before: HashMap<Label, Vec<HashSet<VirtualReg>>>,
    intervals: HashMap<VirtualReg, Interval>,
    base_index: HashMap<Label, usize>,
    idx: usize,
}

impl LiveRangeAnalysis {
    pub fn new() -> LiveRangeAnalysis {
        // let mut uses = HashMap::new();
        // let mut defs = HashMap::new();

        return LiveRangeAnalysis {
            uses: HashMap::new(),
            defs: HashMap::new(),
            live_in: HashMap::new(),
            live_out: HashMap::new(),
            base_index: HashMap::new(),
            live_ranges: Vec::new(),
            live_before: HashMap::new(),
            intervals: HashMap::new(),
            idx: 0,
        };
    }

    pub fn finish(&mut self) {
        for (reg, interval) in &self.intervals {
            for (start, end) in &interval.segments {
                self.live_ranges.push(LiveRange {
                    virtual_reg: reg.clone(),
                    start: start.clone(),
                    end: end.clone(),
                });
            }
        }
    }
    fn pre_backward_pass(&mut self, program: &LIRProgram) {
        let mut curr = 0;
        for label in &program.block_order {
            self.base_index.insert(label.clone(), curr);
            let block = program.blocks.get(label).unwrap();
            curr += block.instrs.len() +1 ; // number of instructions in this block
        }
    }

    fn post_backward_scan_pass(&mut self, block: &LIRBasicBlock, label: &Label, curr_idx: usize) {
        let mut prev_live: HashSet<VirtualReg> = HashSet::new();
        let mut open_start: HashMap<VirtualReg, usize> = HashMap::new();


        for (idx, instr) in block.instrs.iter().enumerate() {
            let p = if (idx < block.instrs.len()) {
                pos_before(label, idx, &self.base_index)
            } else {
                pos_before_term(label, &self.base_index, block.instrs.len())
            };

            let s = self.live_before.get(label).unwrap().get(idx).unwrap();
            for v in s.difference(&prev_live) {
                open_start.insert(v.clone(), p);
            }
            for v in prev_live.difference(&s) {
                self.intervals
                    .entry(v.clone())
                    .or_insert_with(|| Interval{segments: vec![]})
                    .segments
                    .push((open_start[v], p));
                open_start.remove(v);
            }
            prev_live = s.clone();
        }
    }
    pub fn backward_scan(&mut self, block: &LIRBasicBlock, label: &Label, curr_idx: usize) {
        self.live_before
            .insert(label.clone(), vec![HashSet::new(); block.instrs.len() + 1]);
        let live_before_b = self.live_before.get_mut(label).unwrap();
        // let mut live_before_b: Vec<HashSet<VirtualReg>> =
        //     vec![HashSet::new(); block.instrs.len() + 1];

        let mut live = self.live_out.get(label).unwrap().clone();
        // let block =
        // live_block_out.retain(|reg| !block.terminator.uses().iter().collect::<Has());
        live.extend(
            block
                .terminator
                .uses()
                .iter()
                .cloned()
                .collect::<HashSet<_>>(),
        );

        // let before_term = live;
        // for r in &before_term {
        //     self.extend_cover(
        //         r.clone(),
        //         LiveRangeAnalysis::pos_before(curr_idx + block.instrs.len()),
        //     );
        // }

        live_before_b[block.instrs.len()] = live.clone();
        // live = before_term;

        for (idx, instr) in block.instrs.iter().enumerate().rev() {
            let writes: HashSet<_> = instr.defs().iter().cloned().collect();
            let reads: HashSet<_> = instr.uses().iter().cloned().collect();

            let mut before = live.clone();
            before.retain(|x| !writes.contains(x));
            before.extend(reads.iter().cloned());

            live_before_b[idx] = before.clone();

            // for r in &before {
            //     self.extend_cover(r.clone(), LiveRangeAnalysis::pos_before(curr_idx + idx));
            // }

            // for d in writes {
            //     if !before.contains(&d) {
            //         self.start_interval(d, LiveRangeAnalysis::pos_after(curr_idx + idx));
            //     } else {
            //         self.extend_cover(d, LiveRangeAnalysis::pos_after(curr_idx + idx));
            //     }
            // }

            live = before;
        }
    }

    pub fn build_use_defs(&mut self, lir_program: &LIRProgram) {
        for label in &lir_program.block_order {
            self.uses.insert(label.clone(), HashSet::new());
            self.defs.insert(label.clone(), HashSet::new());

            let block = lir_program.blocks.get(label).unwrap();
            for instr in &block.instrs {
                let writes = instr.defs();
                let reads = instr.uses();

                let w: HashSet<VirtualReg> = writes.into_iter().collect();
                let r: HashSet<VirtualReg> = reads.into_iter().collect();
                self.uses.get_mut(label).unwrap().extend(
                    r.difference(self.defs.get(label).unwrap())
                        .cloned()
                        .collect::<HashSet<_>>(),
                );

                self.defs.get_mut(label).unwrap().extend(w);
            }

            let terminator_reads: HashSet<VirtualReg> =
                block.terminator.uses().into_iter().collect();
            self.uses.get_mut(label).unwrap().extend(
                terminator_reads
                    .difference(self.defs.get(label).unwrap())
                    .cloned()
                    .collect::<HashSet<_>>(),
            );
        }
        // return (uses, defines);
    }

    pub fn build_liveness(&mut self, lir_program: &LIRProgram) {
        // self.build_use_defs(&lir_program)
        let current_defs: HashMap<VirtualReg, LiveRange> = HashMap::new();

        for label in &lir_program.block_order {
            self.live_in.insert(label.clone(), HashSet::new());
            self.live_out.insert(label.clone(), HashSet::new());
        }

        let mut changed = true;
        while changed {
            changed = false;
            for label in &lir_program.block_order {
                let block = lir_program.blocks.get(&label).unwrap();
                // let mut in_set = HashSet::new();
                let mut out_set = HashSet::new();

                // union of successors' in sets
                for succ in &block.terminator.successors() {
                    if let Some(succ_in) = self.live_in.get(succ) {
                        out_set.extend(succ_in.iter().cloned());
                    }
                }

                let diff: HashSet<_> = out_set
                    .difference(&self.defs.get(&label).unwrap())
                    .cloned()
                    .collect();
                let inset: HashSet<_> = self
                    .uses
                    .get_mut(&label)
                    .unwrap()
                    .union(&diff)
                    .cloned()
                    .collect();
                // let diff: HashSet<_> = out_set.difference(&defs).cloned().collect();
                // let new_in: HashSet<_> = uses.union(&diff).cloned().collect();

                if inset != *self.live_in.get(&label).unwrap() {
                    self.live_in.insert(label.clone(), inset);
                    changed = true;
                }
                if out_set != *self.live_out.get(&label).unwrap() {
                    self.live_out.insert(label.clone(), out_set);
                    changed = true;
                }
            }
        }
    }

    fn pos_after(i: usize) -> usize {
        return 2 * i + 1;
    }
    fn pos_before(i: usize) -> usize {
        return 2 * i;
    }
}

pub fn pretty_print_live_ranges(live_ranges: &Vec<LiveRange>) {
    println!("Live Ranges:");
    for (idx, lr) in live_ranges.iter().enumerate() {
        println!("{}\t→ [start={}, end={}]", lr.virtual_reg, lr.start, lr.end);
    }
}

fn pos_before(label: &Label, instr_idx: usize, base_index: &HashMap<Label, usize>) -> usize {
    let global_i = base_index[label] + instr_idx;
    2 * global_i
}

fn pos_after(label: &Label, instr_idx: usize, base_index: &HashMap<Label, usize>) -> usize {
    let global_i = base_index[label] + instr_idx;
    2 * global_i + 1
}

fn pos_before_term(label: &Label, base_index: &HashMap<Label, usize>, block_size: usize) -> usize {
    // let block = program.blocks.get(label).unwrap();
    let global_i = base_index[label] + block_size;
    2 * global_i // BEFORE the terminator
}
