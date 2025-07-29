use std::{collections::HashMap, hash::Hash};

use crate::{
    ssa::cfg::{CFG, CFGNode, DFSNumber, HashIndex},
    types::hir_types::Label,
};

pub struct Dominator {
    pub s_dominators: Vec<DFSNumber>,
    pub i_dominators: Vec<Option<DFSNumber>>,
    ancestor: Vec<Option<DFSNumber>>, // union-find tree
    label: Vec<DFSNumber>,
    bucket: Vec<Vec<DFSNumber>>,

    pub dom_tree: Vec<Vec<DFSNumber>>,
    pub dominance_frontier: Vec<Vec<DFSNumber>>,
}

impl Dominator {
    pub fn new(n: usize) -> Dominator {
        // let n = cfg.pre_order.len();
        let dominator = Dominator {
            bucket: vec![vec![]; n],
            s_dominators: (0..n).map(DFSNumber).collect(),
            i_dominators: vec![None; n],
            ancestor: vec![None; n],
            label: (0..n).map(DFSNumber).collect(),
            // cfg,
            dom_tree: vec![vec![]; n],
            dominance_frontier: vec![vec![]; n],
        };

        // println!("{:?}", dominator.s_dominators);

        return dominator;
    }

    pub fn compute_semi_dominators(&mut self, cfg: &CFG) {
        let pre_order_rev: Vec<HashIndex> = cfg.pre_order.iter().skip(1).rev().cloned().collect();
        for v_hash in pre_order_rev {
            // v is just an index to blocks not the&cfg dfs #
            let v_label = cfg.get_label(v_hash);
            let v_dfs = cfg.dfs_num(v_label);
            // limit the immutable borrow of self.blocks[v] to this block
            let preds = {
                let val = cfg.get_block(v_label);

                let parent_label = cfg.get_label(cfg.get_dfs_parents(v_hash).unwrap());

                // println!(
                //     "{} raw_index:{} dfs_number:{} parent label:{} parent raw_index: {} parent dfs number {}",
                //     v_label.0,
                //     v_hash.0,
                //     v_dfs.0,
                //     parent_label.0,
                //     cfg.get_index(parent_label).unwrap().0,
                //     cfg.dfs_num(parent_label).0,
                // );
                val.preds.clone()
            };
            for pred_label in &preds {
                // let u = if let Some(&pred_idx) = cfg.get_index(pred_label) {
                let pred_dfs = cfg.dfs_num(pred_label);
                let u_dfs = if pred_dfs.less_then(v_dfs) {
                    pred_dfs
                } else {
                    self.eval(pred_dfs.clone())
                };

                if self.get_semi_dominator(u_dfs) < self.get_semi_dominator(v_dfs) {
                    // self.assign_semi_dominator(v_dfs, u_dfs);
                    self.s_dominators[v_dfs.0] = self.s_dominators[u_dfs.0];
                }
            }
            self.bucket[self.s_dominators[v_dfs.0].0].push(v_dfs);

            if let Some(parent) = cfg.get_dfs_parents(v_hash) {
                let parent_label = cfg.get_label(parent);
                let parent_dfs = cfg.dfs_num(parent_label);
                self.link(parent_dfs, v_dfs);
            } else {
                panic!("insert error here into dominators");
            }
        }
    }
    pub fn pretty_print_buckets(&mut self, cfg: &CFG) {
        println!("DFS#\tbucket");
        for v_hash in cfg.pre_order.iter() {
            let v_label = cfg.get_label(*v_hash);
            let v_dfs = cfg.dfs_num(v_label);
            println!("{}\t{:?}", v_dfs.0, self.bucket[v_dfs.0]);
        }
        // todo!();
    }
    pub fn compute_immediate_dominators(&mut self, cfg: &CFG) {
        let pre_order_rev: Vec<HashIndex> = cfg.pre_order.iter().skip(1).rev().cloned().collect();
        for v_hash in pre_order_rev {
            let v_label = cfg.get_label(v_hash);
            let v_dfs = cfg.dfs_num(v_label);
            if let Some(parent_hash) = cfg.get_dfs_parents(v_hash) {
                let parent_label = cfg.get_label(parent_hash);
                let parent_dfs = cfg.dfs_num(parent_label);
                let bucket_clone = self.bucket[parent_dfs.0].clone();
                for w_dfs in &bucket_clone {
                    let u_dfs = self.eval(*w_dfs);

                    if u_dfs == *w_dfs {
                        self.i_dominators[w_dfs.0] = Some(parent_dfs);
                    } else if self.s_dominators[u_dfs.0] == self.s_dominators[w_dfs.0] {
                        self.i_dominators[w_dfs.0] = Some(u_dfs);
                    } else {
                        self.i_dominators[w_dfs.0] = Some(parent_dfs);
                    }
                }
                self.link(parent_dfs, v_dfs);
            }
        }
    }

    pub fn finalize_immediate_dominators(&mut self, cfg: &CFG) {
        self.i_dominators[0] = None;

        for v_hash in cfg.pre_order.iter().skip(1) {
            let v_label = cfg.get_label(*v_hash);
            let v_dfs = cfg.dfs_num(v_label);

            let imm_dom = self.i_dominators[v_dfs.0];
            let semi_dom = self.s_dominators[v_dfs.0];
            if let Some(imm_dom_unwrap) = imm_dom {
                if imm_dom_unwrap != semi_dom {
                    self.i_dominators[v_dfs.0] = self.i_dominators[imm_dom_unwrap.0];
                }
            }
        }
    }

    pub fn pretty_print_semi_dominators(&self, cfg: &CFG) {
        println!("Block\tSemi-Dominator\tancestor");

        for dfs_idx in 0..cfg.pre_order.len() {
            let block = cfg.pre_order[dfs_idx]; // HashIndex
            let label = cfg.get_label(block);
            let sdom_dfs = self.s_dominators[dfs_idx]; // DFSNumber
            let sdom_idx = sdom_dfs.0;
            let ancestor = self.ancestor[dfs_idx];
            if sdom_idx < cfg.pre_order.len() {
                let sdom_block = cfg.pre_order[sdom_idx];
                let sdom_label = cfg.get_label(sdom_block);
                println!("{}\t{}\t{:?}", label.0, sdom_label.0, ancestor);
            } else {
                println!("{}\t{}\t{:?}", label.0, "???", ancestor);
            }
        }
    }
    pub fn pretty_print_immediate_dominators(&self, cfg: &CFG) {
        println!("Block\tImmediate-Dominator\tancestor");

        for dfs_idx in 0..cfg.pre_order.len() {
            let block = cfg.pre_order[dfs_idx]; // HashIndex
            let label = cfg.get_label(block);
            match self.i_dominators[dfs_idx] {
                Some(idom_dfs) => {
                    let ancestor = self.ancestor[dfs_idx];

                    let idom_block = cfg.pre_order[idom_dfs.0];
                    let idom_label = cfg.get_label(idom_block);
                    println!("{}\t{}\t{:?}", label.0, idom_label.0, ancestor);
                }
                None => {
                    println!("{}\t{}", label.0, "—");
                }
            }
        }
    }

    pub fn construct_dominator_tree(&mut self, cfg: &CFG) {
        for v_hash in cfg.pre_order.iter().skip(1) {
            let v_label = cfg.get_label(*v_hash);
            let v_dfs = cfg.dfs_num(v_label);

            let imm_dom = self.i_dominators[v_dfs.0];
            // let semi_dom = self.s_dominators[v_dfs.0];
            if let Some(imm_dom_unwrap) = imm_dom {
                self.dom_tree[imm_dom_unwrap.0].push(v_dfs);
            }
        }
    }

    pub fn pretty_print_dominator_tree(&self, cfg: &CFG) {
        println!("Block\tdominator children");

        for dfs_idx in 0..cfg.pre_order.len() {
            let block = cfg.pre_order[dfs_idx]; // HashIndex
            let label = cfg.get_label(block);

            print!("{}\t", label.0);
            let children = &self.dom_tree[dfs_idx];

            for child in children {
                let block = cfg.pre_order[child.0]; // HashIndex
                let label = cfg.get_label(block);
                // let block = cfg.pre_order[dfs_idx]; // HashIndex
                // let label = cfg.get_label(block);
                print!("{}\t", label.0);
            }
            println!();
        }
    }

    pub fn construct_dominance_frontiers(&mut self, cfg: &CFG) {
        for i in &mut self.dominance_frontier {
            i.clear();
        }

        for v_hash in cfg.pre_order.iter() {
            let v_label = cfg.get_label(*v_hash);
            let v_dfs = cfg.dfs_num(v_label);

            let block = cfg.get_block(v_label);
            if block.preds.len() >= 2 {
                if let Some(b_idom_dfs) = self.i_dominators[v_dfs.0] {
                    for p in block.preds.iter() {
                        let mut runner_dfs = cfg.dfs_num(p);
                        while runner_dfs.0 != b_idom_dfs.0 {
                            if runner_dfs.0 != v_dfs.0 {
                                self.dominance_frontier[runner_dfs.0].push(v_dfs);
                            }
                            let temp = self.i_dominators[runner_dfs.0];

                            if let Some(x) = temp {
                                runner_dfs = x;
                            } else {
                                break;
                            }
                        }
                    }
                }
            }
        }
    }

    pub fn pretty_print_dominance_frontier(&self, cfg: &CFG) {
        println!("Block\tfrontiers");

        for dfs_idx in 0..cfg.pre_order.len() {
            let block = cfg.pre_order[dfs_idx]; // HashIndex
            let label = cfg.get_label(block);

            print!("{}\t", label.0);
            let children = &self.dominance_frontier[dfs_idx];

            for child in children {
                let block = cfg.pre_order[child.0]; // HashIndex
                let label = cfg.get_label(block);
                // let block = cfg.pre_order[dfs_idx]; // HashIndex
                // let label = cfg.get_label(block);
                print!("{}\t", label.0);
            }
            println!();
        }
    }

    fn eval(&mut self, v: DFSNumber) -> DFSNumber {
        if let Some(anc) = self.ancestor[v.0] {
            self.compress(v);
            let lv = self.label[v.0];
            let lanc = self.label[anc.0];
            if self.s_dominators[lanc.0].0 < self.s_dominators[lv.0].0 {
                self.label[v.0] = lanc;
            }
            return self.label[v.0];
        }
        v
    }

    fn compress(&mut self, v: DFSNumber) {
        if let Some(anc) = self.ancestor[v.0] {
            if let Some(grand) = self.ancestor[anc.0] {
                self.compress(anc);
                if self.s_dominators[self.label[anc.0].0].0 < self.s_dominators[self.label[v.0].0].0
                {
                    self.label[v.0] = self.label[anc.0];
                }
                self.ancestor[v.0] = Some(grand);
            }
        }
    }

    fn link(&mut self, parent: DFSNumber, child: DFSNumber) {
        self.ancestor[child.0] = Some(parent);
    }

    fn get_label(&self, v: DFSNumber) -> DFSNumber {
        return self.label[v.0];
    }

    fn get_ancestor(&self, v: DFSNumber) -> Option<DFSNumber> {
        return self.ancestor[v.0];
    }

    fn get_semi_dominator(&self, v: DFSNumber) -> DFSNumber {
        return self.s_dominators[v.0];
    }

    fn get_immediate_dominator(&self, v: DFSNumber) -> Option<DFSNumber> {
        return self.i_dominators[v.0];
    }

    fn assign_label(&mut self, idx: DFSNumber, value: DFSNumber) {
        self.label[idx.0] = value;
    }

    fn assign_ancestor(&mut self, idx: DFSNumber, value: Option<DFSNumber>) {
        self.ancestor[idx.0] = value;
    }

    fn assign_semi_dominator(&mut self, idx: DFSNumber, value: DFSNumber) {
        self.s_dominators[idx.0] = value;
    }

    fn assign_immediate_dominator(&mut self, idx: DFSNumber, value: Option<DFSNumber>) {
        self.i_dominators[idx.0] = value;
    }
}
