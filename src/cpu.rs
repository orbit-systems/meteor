use core::ops::{Index, IndexMut};

use aphelion_util::{helper::ops::BitAccess, instruction::Instruction, registers::Register};
use bitflags::bitflags;

macro_rules! nth_bit {
    ($v: expr) => {
        (1 << $v)
    };
}
// TODO: put this in the util lib
bitflags! {
    #[derive(Debug, Clone, Copy)]
    pub struct StFlag: u64 {
        const SIGN = nth_bit!(0);
        const ZERO = nth_bit!(1);
        const CARRY_BORROW = nth_bit!(2);
        const CARRY_BORROW_UNSIGNED = nth_bit!(3);
        const EQUAL = nth_bit!(4);
        const LESS = nth_bit!(5);
        const LESS_UNSIGNED = nth_bit!(6);
        const MODE = nth_bit!(7);

        const EXT_F = nth_bit!(31);
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Cpu {
    pub registers: Registers,
    pub cycle:     u64,
}
impl Cpu {
    #[must_use]
    pub const fn new() -> Self { Self { registers: Registers([0; 16]), cycle: 0 } }
    #[must_use]
    pub const fn get_flag(&self, flag: StFlag) -> bool { self.registers.get_flag(flag) }
    pub fn set_flag(&mut self, flag: StFlag, value: bool) { self.registers.set_flag(flag, value); }
}
impl Default for Cpu {
    fn default() -> Self { Self { registers: Registers::default(), cycle: 0 } }
}

// TODO: put this in the util lib
#[derive(Debug, Clone, Copy, Default)]
pub struct Registers(pub [u64; 16]);

impl Registers {
    #[must_use]
    pub const fn index(self, index: Register) -> u64 { self.0[index as usize] }
    #[must_use]
    pub const fn default() -> Self { Self([0; 16]) }
    #[must_use]
    pub const fn get_flag(&self, flag: StFlag) -> bool { (*self).index(Register::St) & flag.bits() != 0 }
    pub fn set_flag(&mut self, flag: StFlag, value: bool) {
        if value {
            self[Register::St] |= flag.bits();
        } else {
            self[Register::St] &= !flag.bits();
        }
    }
    #[must_use]
    pub const fn current_instr(&self) -> Instruction { Instruction(((*self).index(Register::St) >> 32) as u32) }
    pub fn set_current_instr(&mut self, instr: Instruction) { self[Register::St].write::<1>(instr.0); }
}
impl Index<Register> for Registers {
    type Output = u64;
    fn index(&self, index: Register) -> &Self::Output { &self.0[index as usize] }
}
impl IndexMut<Register> for Registers {
    fn index_mut(&mut self, index: Register) -> &mut Self::Output { &mut self.0[index as usize] }
}
