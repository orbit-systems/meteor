use std::ops::{Index, IndexMut};

use aphelion_util::{instruction::Instruction, registers::Register};

#[derive(Debug, Clone, Copy)]
pub struct Cpu {
    pub registers: Registers,
    pub instr:     Instruction,
    pub cycle:     u64,
}
impl Cpu {
    #[must_use]
    pub const fn new() -> Self { Self { registers: Registers([0; 16]), instr: Instruction(0), cycle: 0 } }
}
impl Default for Cpu {
    fn default() -> Self { Self { registers: Registers::default(), instr: Instruction(0), cycle: 0 } }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct Registers(pub [u64; 16]);

impl Registers {
    #[must_use]
    pub const fn index(self, index: Register) -> u64 { self.0[index as usize] }
    #[must_use]
    pub const fn default() -> Self { Self([0; 16]) }
}
impl Index<Register> for Registers {
    type Output = u64;
    fn index(&self, index: Register) -> &Self::Output { &self.0[index as usize] }
}
impl IndexMut<Register> for Registers {
    fn index_mut(&mut self, index: Register) -> &mut Self::Output { &mut self.0[index as usize] }
}
