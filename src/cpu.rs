use core::ops::{Index, IndexMut};

use aphelion_util::{
    helper::ops::{f16, BitAccess, Float},
    instruction::{
        instruction_set::{BranchCond, FloatPrecision},
        Instruction,
    },
    registers::Register,
};
use bitflags::bitflags;

macro_rules! nth_bit {
    ($v: expr) => {
        (1 << $v)
    };
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
    #[allow(clippy::fn_params_excessive_bools)]
    pub fn set_cmp(&mut self, equal: bool, less: bool, less_unsigned: bool, sign: bool, zero: bool) {
        self.set_flag(StFlag::EQUAL, equal);
        self.set_flag(StFlag::LESS, less);
        self.set_flag(StFlag::LESS_UNSIGNED, less_unsigned);
        self.set_flag(StFlag::SIGN, sign);
        self.set_flag(StFlag::ZERO, zero);
    }
}
impl Index<Register> for Registers {
    type Output = u64;
    fn index(&self, index: Register) -> &Self::Output { &self.0[index as usize] }
}
impl IndexMut<Register> for Registers {
    fn index_mut(&mut self, index: Register) -> &mut Self::Output { &mut self.0[index as usize] }
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
    #[must_use]
    pub const fn cond(&self, cond: BranchCond) -> bool {
        match cond {
            BranchCond::Bra => true,
            BranchCond::Beq => self.get_flag(StFlag::EQUAL),
            BranchCond::Bez => self.get_flag(StFlag::ZERO),
            BranchCond::Blt => self.get_flag(StFlag::LESS),
            BranchCond::Ble => self.get_flag(StFlag::LESS) || self.get_flag(StFlag::EQUAL),
            BranchCond::Bltu => self.get_flag(StFlag::LESS_UNSIGNED),
            BranchCond::Bleu => self.get_flag(StFlag::LESS_UNSIGNED) || self.get_flag(StFlag::EQUAL),
            BranchCond::Bne => !self.get_flag(StFlag::EQUAL),
            BranchCond::Bnz => !self.get_flag(StFlag::ZERO),
            BranchCond::Bge => !self.get_flag(StFlag::LESS),
            BranchCond::Bgt => !self.get_flag(StFlag::LESS) && !self.get_flag(StFlag::EQUAL),
            BranchCond::Bgeu => !self.get_flag(StFlag::LESS_UNSIGNED),
            BranchCond::Bgtu => !self.get_flag(StFlag::LESS_UNSIGNED) && !self.get_flag(StFlag::EQUAL),
        }
    }
    #[allow(clippy::fn_params_excessive_bools)]
    pub fn set_cmp(&mut self, equal: bool, less: bool, less_unsigned: bool, sign: bool, zero: bool) {
        self.registers.set_cmp(equal, less, less_unsigned, sign, zero);
    }
    #[allow(clippy::cast_possible_wrap)]
    pub fn set_cmp_u64(&mut self, a: u64, b: u64) { self.set_cmp(a == b, (a as i64) < (b as i64), a < b, (a as i64) < 0, a == 0); }
    pub fn set_cmp_float<F: Float>(&mut self, a: F, b: F) { self.set_cmp(a == b, a < b, a < b, a.is_sign_negative(), a.is_zero()); }
    pub fn set_cmp_float_bits<F: Float>(&mut self, a: u64, b: u64) { self.set_cmp_float(F::from_u64(a), F::from_u64(b)); }
    pub fn set_cmp_float_precision(&mut self, a: u64, b: u64, p: FloatPrecision) {
        match p {
            FloatPrecision::F16 => self.set_cmp_float_bits::<f16>(a, b),
            FloatPrecision::F32 => self.set_cmp_float_bits::<f32>(a, b),
            FloatPrecision::F64 => self.set_cmp_float_bits::<f64>(a, b),
        }
    }
}
impl Default for Cpu {
    fn default() -> Self { Self { registers: Registers::default(), cycle: 0 } }
}
