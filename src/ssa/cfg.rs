use crate::ssa::block_builder::BlockBuilder;
use crate::types::hir_types::Label;
use crate::types::ssa_types::BasicBlock;
// use crate::types::ssa_types::{SSABasicBlock, SSAFunction, SSATerminator};
use core::num;
use std::collections::{HashMap, HashSet};

pub struct CFGNode {
    pub label: Label,
    pub preds: Vec<Label>,
    pub successors: Vec<Label>,
}

impl CFGNode {
    fn from_basic_block(block: &BasicBlock) -> CFGNode {
        return CFGNode {
            label: block.label.clone(),
            preds: block.preds.clone(),
            successors: block.terminator.successors(),
        };
    }
}

// dfs number is the number that corresponds the way that it got placed it is 1 indexed
#[derive(Debug, Clone, PartialEq, Copy, PartialOrd, Eq, Hash)]
pub struct DFSNumber(pub usize);

impl DFSNumber {
    pub fn less_then(&self, other: DFSNumber) -> bool {
        return (self.0 < other.0);
    }
}

// Hash index is 0 indexed
#[derive(Debug, Clone, PartialEq, Copy)]
pub struct HashIndex(pub usize);
pub struct CFG {
    pub blocks: Vec<CFGNode>, // indexed by arbitray value caused when flattening down list
    //
    pub(crate) label_to_index: HashMap<Label, HashIndex>, // lets you get the index into blocks from the label name
    index_to_label: Vec<Label>,                           // lets you get the label from the index

    pub entry_label: Label,
    // pub
    dfs_numbers: HashMap<Label, DFSNumber>, // uses the label to get the corresponding dfs_number in hashmap
    // dfs_label: HashMap<DFSNumber, Label>, // uses the dfs index to get the corresponding lbael in hashmap
    pub pre_order: Vec<HashIndex>, // pushes the arbritary index to order it
    pub post_order: Vec<Label>,
    dfs_parents: Vec<Option<HashIndex>>, //indexed by arbritary value ie index

    visited: Vec<bool>, //indexed by arbritary value ie index
}
// pub block_instrs: HashMap<Label, BasicBlock>,
// // anon_temp_counter: u32,
// // hir_to_ssa_temp: HashMap<TempId, SSATempId>,
// pub block_order: Vec<Label>,

impl CFG {
    pub fn from_blocks(
        block_instrs: &HashMap<Label, BasicBlock>,
        block_order: &Vec<Label>,
    ) -> Self {
        let mut label_to_index: HashMap<Label, HashIndex> = HashMap::new();
        let mut index_to_label = Vec::new();
        let mut raw_blocks = Vec::new();

        for (i, (label, block)) in block_instrs.iter().enumerate() {
            // println!("LABEL:{} HASH_INDEX:{}", label.0, i);
            label_to_index.insert(label.clone(), HashIndex(i));
            index_to_label.push(label.clone());
            raw_blocks.push(CFGNode::from_basic_block(block));
        }

        let n = raw_blocks.len();

        return CFG {
            blocks: raw_blocks,
            label_to_index,
            index_to_label,
            entry_label: block_order.first().unwrap().clone(),
            // dfs_label: HashMap::new(),
            dfs_numbers: HashMap::new(),
            pre_order: Vec::new(),
            post_order: Vec::new(),
            dfs_parents: vec![None; n],
            visited: vec![false; n],
        };
    }

    pub fn dfs_entry(&mut self) {
        if let Some(&entry_idx) = self.label_to_index.get(&self.entry_label) {
            self.dfs_visit(entry_idx, None);
        }
    }

    pub fn dfs(&mut self, entry_label: Label) {
        if let Some(&entry_idx) = self.label_to_index.get(&entry_label) {
            self.dfs_visit(entry_idx, None);
        }
    }

    fn dfs_visit(&mut self, idx: HashIndex, parent: Option<HashIndex>) {
        if self.visited[idx.0] {
            return;
        }
        self.visited[idx.0] = true;
        self.dfs_parents[idx.0] = parent;

        let dfs_number = DFSNumber(self.pre_order.len());
        let label = self.index_to_label[idx.0].clone();
        self.dfs_numbers.insert(label.clone(), dfs_number);
        self.pre_order.push(idx);

        for succ_label in &self.blocks[idx.0].successors.clone() {
            if let Some(&succ_idx) = self.label_to_index.get(&succ_label) {
                self.dfs_visit(succ_idx, Some(idx));
            }
        }

        self.post_order.push(label);
    }

    pub fn pretty_print_dfs(&self) {
        println!("label\tdfs#\tparent");
        for &idx in &self.pre_order {
            let label = &self.index_to_label[idx.0];
            let dfs = self
                .dfs_numbers
                .get(&label)
                .unwrap_or(&DFSNumber(usize::MAX));
            let parent_str = self.dfs_parents[idx.0]
                .map(|p| self.index_to_label[p.0].0.to_string())
                .unwrap_or("-1".to_string());
            println!("{}\t{}\t{}", label.0, dfs.0, parent_str);
        }
    }

    pub fn get_label(&self, idx: HashIndex) -> &Label {
        &self.index_to_label[idx.0]
    }

    // pub fn get_label_sage(&self, idx: HashIndex) -> Option<&Label> {
    //     &self.index_to_label
    // }

    pub fn get_dfs_parents(&self, idx: HashIndex) -> Option<HashIndex> {
        return self.dfs_parents[idx.0];
    }

    // pub fn get_dfs_label(&self, number: &DFSNumber) -> Option<&Label> {
    //     self.dfs_label.get(number)
    // }

    pub fn get_index(&self, label: &Label) -> Option<&HashIndex> {
        self.label_to_index.get(label)
    }

    pub fn get_block(&self, label: &Label) -> &CFGNode {
        &self.blocks[self.label_to_index[label].0]
    }

    pub fn dfs_num(&self, label: &Label) -> DFSNumber {
        self.dfs_numbers[label]
    }

    pub fn preds(&self, idx: usize) -> &[Label] {
        &self.blocks[idx].preds
    }
}
