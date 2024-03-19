use core::{
    fmt::Debug,
    ops::{Index, IndexMut},
};

use aphelion_util::interrupt::Interrupt;

/// Look ma, no heap allocation!
#[derive(Debug, Clone, Copy)]
pub struct IntQueue {
    data:   [Interrupt; IntQueue::MAX_CAPACITY as usize],
    offset: u8,
    len:    u8,
}


impl IntQueue {
    pub const MAX_CAPACITY: u8 = 32;
    #[allow(clippy::cast_possible_truncation)]
    const fn loc(&self, idx: usize) -> usize {
        debug_assert!(idx < (self.len as usize));
        ((self.offset + (idx as u8)) % Self::MAX_CAPACITY) as usize
    }
    #[must_use]
    pub const fn offset(&self) -> usize { self.offset as usize }
    #[must_use]
    pub const fn len(&self) -> usize { self.len as usize }
    #[must_use]
    pub const fn is_empty(&self) -> bool { self.len == 0 }
    #[must_use]
    pub const fn new() -> Self { Self { data: [Interrupt(0); IntQueue::MAX_CAPACITY as usize], offset: 0, len: 0 } }
    #[must_use]
    pub const fn reached_capacity(&self) -> bool { self.len == Self::MAX_CAPACITY }
    pub fn pop_back(&mut self) -> Option<Interrupt> {
        (!self.is_empty()).then_some({
            let res = self[self.len as usize - 1];
            self.len -= 1;
            res
        })
    }
    pub fn pop_front(&mut self) -> Option<Interrupt> {
        (!self.is_empty()).then_some({
            let res = self[0];
            self.offset = (self.offset + 1) % Self::MAX_CAPACITY;
            self.len -= 1;
            res
        })
    }
    pub fn push_back(&mut self, int: Interrupt) -> bool {
        if self.len == Self::MAX_CAPACITY {
            false
        } else {
            debug_assert!(self.offset < Self::MAX_CAPACITY);
            self.len += 1;
            let len = self.len as usize;
            self[len - 1] = int;
            true
        }
    }
    pub fn push_front(&mut self, int: Interrupt) -> bool {
        if self.len == Self::MAX_CAPACITY {
            false
        } else {
            self.len += 1;
            self.offset = (self.offset + Self::MAX_CAPACITY - 1) % Self::MAX_CAPACITY;
            self[0] = int;
            true
        }
    }
    pub fn clear(&mut self) {
        self.offset = 0;
        self.len = 0;
    }
    pub fn total_clear(&mut self) { *self = Self::new(); }
}
impl Index<usize> for IntQueue {
    type Output = Interrupt;
    fn index(&self, index: usize) -> &Self::Output { &self.data[self.loc(index)] }
}
impl IndexMut<usize> for IntQueue {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output { &mut self.data[self.loc(index)] }
}
/* impl Debug for IntQueue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result { self.as_slice().fmt(f) }
} */
#[derive(Debug, Clone, Copy)]
pub struct Ic {
    pub ivt_base_address: u64,
    pub ret_addr:         u64,
    pub ret_status:       u64,
    pub queue:            IntQueue,
}
impl Ic {
    #[must_use]
    pub const fn new() -> Self { Self { ivt_base_address: 0, ret_addr: 0, ret_status: 0, queue: IntQueue::new() } }
}
