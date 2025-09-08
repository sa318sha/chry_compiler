use std::collections::HashMap;

use itertools::Itertools;

use crate::{
    assembly::spill_slot_allocator::{self, SpillSlotAllocator}, lir::{lir::LIRProgram, live_range_analyis::LiveRange}, types::{
        lir_types::VirtualReg,
        x86_64::{RegName, RegSize, SpillSlot, TypedRegister, ALLOCATABLE},
    }
};

pub struct RegisterAllocation {
    active_registers: Vec<LiveRange>,
    pub assignments: HashMap<VirtualReg, TypedRegister>,
    pub spills: HashMap<VirtualReg, SpillSlot>,
    pub available: Vec<RegName>,
    spill_slot_allocator: SpillSlotAllocator,
}

// register_allocation

pub fn register_allocate_x86_64(
    lir_programs: Vec<LIRProgram>,
    live_ranges: &Vec<LiveRange>,
    // free_registers:
    // available: Vec
) -> (
    HashMap<VirtualReg, TypedRegister>,
    HashMap<VirtualReg, SpillSlot>,
) {
    let mut allocator = RegisterAllocation::new();
    allocator.linear_scan(&lir_programs, &live_ranges);
    return (allocator.assignments, allocator.spills);
}

impl RegisterAllocation {
    fn new() -> RegisterAllocation {
        return RegisterAllocation {
            spill_slot_allocator: SpillSlotAllocator { next_offset: 0 },
            active_registers: Vec::new(),
            assignments: HashMap::new(),
            spills: HashMap::new(),
            available: ALLOCATABLE.to_vec(),
        };
    }

    fn linear_scan(&mut self, lir_programs: &Vec<LIRProgram>, live_ranges: &Vec<LiveRange>) {
        for range in live_ranges.iter().sorted_by(|a, b| a.start.cmp(&b.start)) {
            self.expire_old_ranges(range.start);

            if self.active_registers.len() == 16 {
                //spill
                self.spill(range);
            } else {
                let phys_reg = self.choose_free_reg();
                self.assignments.insert(
                    range.virtual_reg.clone(),
                    TypedRegister {
                        name: phys_reg,
                        size: range.virtual_reg.reg_size.clone(),
                    },
                );

                self.active_registers.push(range.clone());
            }
            // use sorted range
        }
    }

    fn expire_old_ranges(&mut self, idx: usize) {
        let mut retained = Vec::new();

        for range in self.active_registers.drain(..) {
            if range.end < idx {
                if let Some(treg) = self.assignments.remove(&range.virtual_reg) {
                    self.available.push(treg.name);
                }
            } else {
                retained.push(range);
            }
        }

        self.active_registers = retained;
    }

    fn spill(&mut self, live_range: &LiveRange) {
        let (i, (max_range)) = self
            .active_registers
            .iter_mut()
            .enumerate()
            .max_by_key(|(_, (range))| range.end)
            .unwrap();

        if max_range.end > live_range.end {
            //spill max active
        } else {
            let spill_slot = self.spill_slot_allocator.fresh(&RegName::RBP, &RegSize::DWord);
            self.spills.insert(
                live_range.virtual_reg.clone(),
                spill_slot,
            );
        }
    }

    fn choose_free_reg(&mut self) -> RegName {
        if self.available.is_empty() {
            panic!();
        }
        return self.available.pop().unwrap();
    }
}

// pub fn register_allocation(lir_programs: &Vec<LIRProgram>, live_ranges: &Vec<LiveRange>) {
//     let mut active_registers = Vec::new();
//     let mut assignments: HashMap<VirtualReg, TypedRegister> = HashMap::new();
//     for range in live_ranges.iter().sorted_by(|a, b| a.start.cmp(&b.start)) {

//         // use sorted range
//     }
// }
