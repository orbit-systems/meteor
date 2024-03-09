use std::{
    borrow::{Borrow, BorrowMut},
    fmt::Debug,
    ops::{Deref, DerefMut, Index, IndexMut},
};

use aphelion_util::interrupt::Interrupt;

const INT_QUEUE_SIZE: usize = IntQueue::MAX_CAPACITY as usize * 2;
/// Look ma, no heap allocation!
#[derive(Debug, Clone, Copy)]
pub struct IntQueue {
    data:   [Interrupt; INT_QUEUE_SIZE],
    offset: u8,
    len:    u8,
}

impl IntQueue {
    pub const MAX_CAPACITY: u8 = 32;
    #[must_use]
    pub const fn max_capacity() -> usize { Self::MAX_CAPACITY as usize }
    #[must_use]
    pub const fn offset(&self) -> usize { self.offset as usize }
    #[must_use]
    pub const fn len(&self) -> usize { self.len as usize }
    #[must_use]
    pub const fn is_empty(&self) -> bool { self.len == 0 }
    #[must_use]
    pub const fn new() -> Self { Self { data: [Interrupt(0); INT_QUEUE_SIZE], offset: 0, len: 0 } }
    #[must_use]
    pub const fn as_slice(&self) -> &[Interrupt] { self.data.as_slice().split_at(self.offset()).1.split_at(self.len()).0 }
    fn copy_to_front(&mut self) {
        let (s1, s2) = self.data.split_at_mut(Self::max_capacity());
        s1.copy_from_slice(s2);
    }
    fn copy_to_back(&mut self) {
        let (s1, s2) = self.data.split_at_mut(Self::max_capacity());
        s2.copy_from_slice(s1);
    }
    #[must_use]
    pub fn as_slice_mut(&mut self) -> &mut [Interrupt] { &mut self.data[(self.offset as usize)..(self.offset + self.len) as usize] }
    pub fn pop_back(&mut self) -> Option<Interrupt> {
        (!self.is_empty()).then_some({
            self.len -= 1;
            self.data[self.offset() + self.len()]
        })
    }
    pub fn pop_front(&mut self) -> Option<Interrupt> {
        (!self.is_empty()).then_some({
            self.len -= 1;
            let res = self.data[self.offset()];
            if self.offset + 1 >= Self::MAX_CAPACITY {
                self.offset -= Self::MAX_CAPACITY - 1;
                self.copy_to_front();
            } else {
                self.offset += 1;
            }
            res
        })
    }
    pub fn push_back(&mut self, int: Interrupt) -> bool {
        if self.len == Self::MAX_CAPACITY {
            false
        } else {
            debug_assert!(self.offset < Self::MAX_CAPACITY);
            self.data[self.offset() + self.len()] = int;
            self.len += 1;
            true
        }
    }
    pub fn push_front(&mut self, int: Interrupt) -> bool {
        if self.len == Self::MAX_CAPACITY {
            false
        } else {
            self.len += 1;
            if self.offset == 0 {
                self.offset += Self::MAX_CAPACITY - 1;
                self.copy_to_back();
            } else {
                self.offset -= 1;
            }
            self.data[self.offset()] = int;
            true
        }
    }
    pub fn clear(&mut self) {
        self.offset = 0;
        self.len = 0;
    }
    pub fn total_clear(&mut self) { *self = Self::new(); }
}
impl Deref for IntQueue {
    type Target = [Interrupt];
    fn deref(&self) -> &Self::Target { self.as_slice() }
}
impl DerefMut for IntQueue {
    fn deref_mut(&mut self) -> &mut Self::Target { self.as_slice_mut() }
}
impl Borrow<[Interrupt]> for IntQueue {
    fn borrow(&self) -> &[Interrupt] { self.as_slice() }
}
impl BorrowMut<[Interrupt]> for IntQueue {
    fn borrow_mut(&mut self) -> &mut [Interrupt] { self.as_slice_mut() }
}
impl AsRef<[Interrupt]> for IntQueue {
    fn as_ref(&self) -> &[Interrupt] { self.as_slice() }
}
impl AsMut<[Interrupt]> for IntQueue {
    fn as_mut(&mut self) -> &mut [Interrupt] { self.as_slice_mut() }
}
impl Index<usize> for IntQueue {
    type Output = Interrupt;
    fn index(&self, index: usize) -> &Self::Output { &self.as_slice()[index] }
}
impl IndexMut<usize> for IntQueue {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output { &mut self.as_slice_mut()[index] }
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
