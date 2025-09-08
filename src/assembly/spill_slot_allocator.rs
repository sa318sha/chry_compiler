use crate::types::x86_64::{RegName, RegSize, SpillSlot};

#[derive(Clone, Copy)]
pub struct SpillSlotAllocator {
    pub next_offset: i32,
}

impl SpillSlotAllocator {
    pub fn fresh(&mut self, name: &RegName, size: &RegSize) -> SpillSlot {
        let offset = self.next_offset;
        self.next_offset -= 8;
        return SpillSlot {
            offset: offset,
            base: name.clone(),
            size: size.clone(),
        };
    }
}
