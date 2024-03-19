use aphelion_util::{
    helper::{
        ops::{self, AddResult, BitAccess},
        sign_extend,
    },
    instruction::{
        instruction_set::{FloatCastType, FloatPrecision, InstructionSet, LiType},
        Instruction,
    },
    interrupt::Interrupt,
    io::Port,
    nibble::Nibble,
    registers::Register,
};

use crate::{
    cpu::{Cpu, StFlag},
    ic::Ic,
    ioc::Ioc,
    mmu::{AccessMode, Mmu, Response},
};

pub trait Callback: Sized {
    fn should_stop<T: AsRef<[u8]> + AsMut<[u8]>>(&mut self, state: &State<T>) -> bool;
    fn on_run_started<T: AsRef<[u8]> + AsMut<[u8]>>(&mut self, state: &mut State<T>);
    fn on_run_ended<T: AsRef<[u8]> + AsMut<[u8]>>(&mut self, state: &mut State<T>);
    fn on_iteration_started<T: AsRef<[u8]> + AsMut<[u8]>>(&mut self, state: &mut State<T>);
    fn on_iteration_ended<T: AsRef<[u8]> + AsMut<[u8]>>(&mut self, state: &mut State<T>);
    fn on_instruction_loaded<T: AsRef<[u8]> + AsMut<[u8]>>(&mut self, state: &mut State<T>);
    fn on_dev_reveived<T: AsRef<[u8]> + AsMut<[u8]>>(&mut self, state: &mut State<T>, port: Port);
    fn on_output_received<T: AsRef<[u8]> + AsMut<[u8]>>(&mut self, state: &State<T>, port: Port, data: u64);
    fn send_input<T: AsRef<[u8]> + AsMut<[u8]>>(&mut self, state: &State<T>) -> (Port, u64);
}

#[derive(Debug, Clone, Copy)]
struct ProcMode;
impl ProcMode {
    // const KERNEL: bool = false;
    const USER: bool = true;
}

macro_rules! read {
    ($fn_name: ident, $phys_get: ident, $ty: ty) => {
        /// # Errors
        ///
        /// its straight forward dummy
        pub fn $fn_name(&self, addr: u64) -> Result<$ty, Response> {
            let addr = if self.cpu.get_flag(StFlag::MODE) == ProcMode::USER { self.mmu.translate_address(addr, AccessMode::Read)? } else { addr };
            self.mmu.$phys_get(addr)
        }
    };
}
macro_rules! read_to {
    ($fn_name: ident, $read: ident) => {
        /// # Errors
        ///
        /// its straight forward dummy
        pub fn $fn_name(&mut self, addr: u64, to: Register) -> Result<(), Response> {
            let v = self.$read(addr)?;
            // Not zeroing out! this is only used for lb etc
            self.regval_mut(to).write::<0>(v);
            Ok(())
        }
    };
}
macro_rules! read_to_signed {
    ($fn_name: ident, $read: ident, $by: expr) => {
        /// # Errors
        ///
        /// its straight forward dummy
        pub fn $fn_name(&mut self, addr: u64, to: Register) -> Result<(), Response> {
            let v = self.$read(addr)?;
            *self.regval_mut(to) = sign_extend::<$by>(v as u64);
            Ok(())
        }
    };
}
macro_rules! write {
    ($fn_name: ident, $phys_write: ident, $ty: ty) => {
        /// # Errors
        ///
        /// its straight forward dummy
        pub fn $fn_name(&mut self, addr: u64, value: $ty) -> Result<(), Response> {
            let addr = if self.cpu.get_flag(StFlag::MODE) == ProcMode::USER { self.mmu.translate_address(addr, AccessMode::Write)? } else { addr };
            self.mmu.$phys_write(addr, value)
        }
    };
}

#[derive(Debug, Clone)]
pub struct State<T: AsRef<[u8]> + AsMut<[u8]>> {
    pub mmu: Mmu<T>,
    pub ic:  Ic,
    pub cpu: Cpu,
    pub ioc: Ioc,
}
impl<T: AsRef<[u8]> + AsMut<[u8]>> State<T> {
    #[must_use]
    pub const fn current_instr(&self) -> Instruction { self.cpu.registers.current_instr() }
    pub fn set_current_instr(&mut self, instr: Instruction) { self.cpu.registers.set_current_instr(instr); }

    /// # Errors
    ///
    /// omg shut up about having errors doc   fucking compiler bich
    pub fn read_instruction(&self, addr: u64) -> Result<Instruction, Response> {
        let addr = if self.cpu.get_flag(StFlag::MODE) == ProcMode::USER { self.mmu.translate_address(addr, AccessMode::Execute)? } else { addr };
        self.mmu.phys_get_u32(addr).map(Instruction)
    }
    read! {read_u8, phys_get_u8, u8}
    read! {read_u16, phys_get_u16, u16}
    read! {read_u32, phys_get_u32, u32}
    read! {read_u64, phys_get_u64, u64}
    read_to! {read_to_u8, read_u8}
    read_to! {read_to_u16, read_u16}
    read_to! {read_to_u32, read_u32}
    read_to! {read_to_u64, read_u64}
    read_to_signed! {read_to_u8_signed, read_u8, 8}
    read_to_signed! {read_to_u16_signed, read_u16, 16}
    read_to_signed! {read_to_u32_signed, read_u32, 32}
    write! {write_u8, phys_write_u8, u8}
    write! {write_u16, phys_write_u16, u16}
    write! {write_u32, phys_write_u32, u32}
    write! {write_u64, phys_write_u64, u64}
    pub fn push_interrupt(&mut self, err: Interrupt) {
        if self.ic.queue.is_empty() {
            self.ic.ret_addr = self.cpu.registers[Register::Ip];
            self.ic.ret_status = self.cpu.registers[Register::St];
            self.cpu.set_flag(StFlag::MODE, ProcMode::USER);
        }
        let err = if self.ic.queue.reached_capacity() {
            // interrupt queue overflow
            self.ic.queue.clear();
            Interrupt::INTERRUPT_OVERFLOW
        } else {
            err
        };
        // hijack instruction pointer
        match self.mmu.phys_get_u64(self.ic.ivt_base_address + 8 * u64::from(err.0)) {
            Err(err) => {
                self.push_interrupt_from_mmu(err);
            }
            Ok(value) => {
                self.cpu.registers[Register::Ip] = value;
                self.ic.queue.push_back(err);
            }
        }
    }
    pub fn push_interrupt_from_mmu(&mut self, res: Response) {
        self.push_interrupt(match res {
            Response::AccViolation | Response::NoPerms | Response::OutOfBounds => Interrupt::ACCESS_VIOLATION,
            Response::Unaligned => Interrupt::UNALIGNED_ACCESS,
        });
    }
    #[must_use]
    pub const fn regval(&self, reg: Register) -> u64 { self.cpu.registers.index(reg) }
    pub fn regval_mut(&mut self, reg: Register) -> &mut u64 { &mut self.cpu.registers[reg] }
    /// BE CAREFUL! opposite order than normal assignment.
    pub fn regval_write(&mut self, from: Register, to: Register) { *self.regval_mut(to) = self.regval(from); }

    pub fn return_interrupt(&mut self) {
        if self.ic.queue.is_empty() {
            return;
        }
        let _ = self.ic.queue.pop_front();
        if self.ic.queue.is_empty() {
            self.cpu.registers[Register::Ip] = self.ic.ret_addr;
            self.cpu.registers[Register::St] = self.ic.ret_status;
        } else {
            // hijack instruction pointer
            let code = self.ic.queue[self.ic.queue.len() - 1].0;
            match self.mmu.phys_get_u64(self.ic.ivt_base_address + 8 * u64::from(code)) {
                Err(err) => self.push_interrupt_from_mmu(err),
                Ok(res) => self.cpu.registers[Register::Ip] = res,
            }
        }
    }
    fn proc_mode_is_user_then_invalid(&mut self) -> Result<(), Interrupt> {
        if self.cpu.get_flag(StFlag::MODE) == ProcMode::USER {
            Err(Interrupt::INVALID_OPERATION)
        } else {
            Ok(())
        }
    }
    pub fn push_stack(&mut self, data: u64) {
        *self.regval_mut(Register::Sp) -= 8;
        if let Err(err) = self.write_u64(self.regval(Register::Sp), data) {
            self.push_interrupt_from_mmu(err);
        }
    }
    pub fn push_stack_from(&mut self, reg: Register) { self.push_stack(self.regval(reg)); }
    pub fn pop_stack_to(&mut self, reg: Register) {
        match self.read_u64(self.regval(Register::Sp)) {
            Ok(val) => {
                *self.regval_mut(Register::Sp) += 8;
                *self.regval_mut(reg) = val;
            }
            Err(err) => self.push_interrupt_from_mmu(err),
        }
    }
    pub const fn load_address(&self, rs: Register, off: u8, rn: Register, sh: Nibble) -> u64 {
        self.regval(rs) + sign_extend::<8>(off as u64) + (self.regval(rn) << sh as u8)
    }
    #[allow(clippy::cast_possible_truncation)]
    #[allow(clippy::cast_sign_loss)]
    #[allow(clippy::cast_possible_wrap)]
    #[allow(clippy::too_many_lines)]
    /// # Errors
    ///
    /// is  obvious
    ///
    pub fn interpret_code(&mut self) -> Result<(), Interrupt> {
        use InstructionSet as I;
        let instr = self.current_instr().try_into_instruction_set().ok_or(Interrupt::INVALID_OPERATION)?;
        match instr {
            // system control
            I::Int { imm8 } => Err(imm8)?,
            I::Iret => {
                self.proc_mode_is_user_then_invalid()?;
                self.return_interrupt();
            }
            I::Ires => {
                self.proc_mode_is_user_then_invalid()?;
                self.return_interrupt();
            }
            I::Usr { rd } => {
                self.proc_mode_is_user_then_invalid()?;
                self.cpu.set_flag(StFlag::MODE, ProcMode::USER);
                self.cpu.registers[Register::Ip] = self.regval(rd);
            }
            // io
            I::Outr { rd, rs } => {
                self.proc_mode_is_user_then_invalid()?;
                self.ioc.send_out(Port(self.regval(rd) as u16), self.regval(rs));
            }
            I::Outi { imm16, rs } => {
                self.proc_mode_is_user_then_invalid()?;
                self.ioc.send_out(imm16, self.regval(rs));
            }
            I::Inr { rd, rs } => {
                self.proc_mode_is_user_then_invalid()?;
                *self.regval_mut(rd) = self.ioc.port_data(Port(self.regval(rs) as u16));
            }
            I::Ini { rd, imm16 } => {
                self.proc_mode_is_user_then_invalid()?;
                *self.regval_mut(rd) = self.ioc.port_data(imm16);
            }
            // control flow
            I::Jal { rs, imm16 } => {
                self.push_stack_from(Register::Ip);
                *self.regval_mut(Register::Ip) = self.regval(rs) + (sign_extend::<16>(imm16.into()) as i64 * 4) as u64;
            }
            I::Jalr { rd, rs, imm16 } => {
                self.regval_write(Register::Ip, rd);
                *self.regval_mut(Register::Ip) = self.regval(rs) + (sign_extend::<16>(imm16.into()) as i64 * 4) as u64;
            }
            I::Ret => self.pop_stack_to(Register::Ip),
            I::Retr { rs } => self.regval_write(rs, Register::Ip),
            I::Branch { cc, imm20 } => {
                if self.cpu.cond(cc) {
                    // wrapping add, since we don't want to panic.
                    *self.regval_mut(Register::Ip) = self.regval(Register::Ip).wrapping_add((sign_extend::<20>(imm20.into()) as i64 * 4) as u64);
                }
            }
            // stack
            I::Push { rs } => {
                self.push_stack_from(rs);
            }
            I::Pop { rd } => {
                self.pop_stack_to(rd);
            }
            I::Enter => {
                self.push_stack_from(Register::Fp);
                self.regval_write(Register::Sp, Register::Fp);
            }
            I::Leave => {
                self.regval_write(Register::Fp, Register::Sp);
                self.pop_stack_to(Register::Fp);
            }
            #[rustfmt::skip]
            I::Li { rd, func, imm } => {
                match func {
                    LiType::Lli   =>  self.regval_mut(rd).write::<0>(imm),
                    LiType::Llis  => *self.regval_mut(rd) = sign_extend::<16>(imm.into()),
                    LiType::Lui   =>  self.regval_mut(rd).write::<1>(imm),
                    LiType::Luis  => *self.regval_mut(rd) = sign_extend::<16>(imm.into()) << 16,
                    LiType::Lti   =>  self.regval_mut(rd).write::<2>(imm),
                    LiType::Ltis  => *self.regval_mut(rd) = sign_extend::<16>(imm.into()) << 32,
                    LiType::Ltui  =>  self.regval_mut(rd).write::<3>(imm),
                    LiType::Ltuis => *self.regval_mut(rd) = sign_extend::<16>(imm.into()) << 48,
                }
            },
            I::Lw { rd, rs, rn, sh, off } => {
                if let Err(err) = self.read_to_u64(self.load_address(rs, off, rn, sh), rd) {
                    self.push_interrupt_from_mmu(err);
                }
            }
            I::Lh { rd, rs, rn, sh, off } => {
                if let Err(err) = self.read_to_u32(self.load_address(rs, off, rn, sh), rd) {
                    self.push_interrupt_from_mmu(err);
                }
            }
            I::Lhs { rd, rs, rn, sh, off } => {
                if let Err(err) = self.read_to_u32_signed(self.load_address(rs, off, rn, sh), rd) {
                    self.push_interrupt_from_mmu(err);
                }
            }
            I::Lq { rd, rs, rn, sh, off } => {
                if let Err(err) = self.read_to_u16(self.load_address(rs, off, rn, sh), rd) {
                    self.push_interrupt_from_mmu(err);
                }
            }
            I::Lqs { rd, rs, rn, sh, off } => {
                if let Err(err) = self.read_to_u16_signed(self.load_address(rs, off, rn, sh), rd) {
                    self.push_interrupt_from_mmu(err);
                }
            }
            I::Lb { rd, rs, rn, sh, off } => {
                if let Err(err) = self.read_to_u8(self.load_address(rs, off, rn, sh), rd) {
                    self.push_interrupt_from_mmu(err);
                }
            }
            I::Lbs { rd, rs, rn, sh, off } => {
                if let Err(err) = self.read_to_u8_signed(self.load_address(rs, off, rn, sh), rd) {
                    self.push_interrupt_from_mmu(err);
                }
            }
            I::Sw { rs, off, rn, sh, rd } => {
                if let Err(err) = self.write_u64(self.load_address(rs, off, rn, sh), self.regval(rd)) {
                    self.push_interrupt_from_mmu(err);
                }
            }
            I::Sh { rs, off, rn, sh, rd } => {
                if let Err(err) = self.write_u32(self.load_address(rs, off, rn, sh), self.regval(rd) as u32) {
                    self.push_interrupt_from_mmu(err);
                }
            }
            I::Sq { rs, off, rn, sh, rd } => {
                if let Err(err) = self.write_u16(self.load_address(rs, off, rn, sh), self.regval(rd) as u16) {
                    self.push_interrupt_from_mmu(err);
                }
            }
            I::Sb { rs, off, rn, sh, rd } => {
                if let Err(err) = self.write_u8(self.load_address(rs, off, rn, sh), self.regval(rd) as u8) {
                    self.push_interrupt_from_mmu(err);
                }
            }
            // comparisons
            I::Cmpr { r1, r2 } => self.cpu.set_cmp_u64(self.regval(r1), self.regval(r2)),
            I::Cmpi { r1, s, imm } => {
                if s {
                    self.cpu.set_cmp_u64(sign_extend::<16>(imm.into()), self.regval(r1));
                } else {
                    self.cpu.set_cmp_u64(self.regval(r1), sign_extend::<16>(imm.into()));
                }
            }
            // arithmetic operations
            I::Addr { rd, r1, r2 } => self.arithmetic_r(Self::add, r1, r2, rd),
            I::Addi { rd, r1, imm16 } => self.arithmetic_i(Self::add, r1, imm16, rd),
            I::Subr { rd, r1, r2 } => self.arithmetic_r(Self::sub, r1, r2, rd),
            I::Subi { rd, r1, imm16 } => self.arithmetic_i(Self::sub, r1, imm16, rd),
            I::Imulr { rd, r1, r2 } => self.arithmetic_r(Self::imul, r1, r2, rd),
            I::Imuli { rd, r1, imm16 } => self.arithmetic_i(Self::imul, r1, imm16, rd),
            I::Idivr { rd, r1, r2 } => self.arithmetic_r(Self::idiv, r1, r2, rd)?,
            I::Idivi { rd, r1, imm16 } => self.arithmetic_i(Self::idiv, r1, imm16, rd)?,
            I::Umulr { rd, r1, r2 } => self.arithmetic_r(Self::umul, r1, r2, rd),
            I::Umuli { rd, r1, imm16 } => self.arithmetic_i(Self::umul, r1, imm16, rd),
            I::Udivr { rd, r1, r2 } => self.arithmetic_r(Self::udiv, r1, r2, rd)?,
            I::Udivi { rd, r1, imm16 } => self.arithmetic_i(Self::udiv, r1, imm16, rd)?,
            I::Remr { rd, r1, r2 } => self.arithmetic_r(Self::rem, r1, r2, rd),
            I::Remi { rd, r1, imm16 } => self.arithmetic_i(Self::rem, r1, imm16, rd),
            I::Modr { rd, r1, r2 } => self.arithmetic_r(Self::r#mod, r1, r2, rd),
            I::Modi { rd, r1, imm16 } => self.arithmetic_i(Self::r#mod, r1, imm16, rd),
            // bitwise operations
            I::Andr { rd, r1, r2 } => self.bitwise_r(Self::and, r1, r2, rd),
            I::Andi { rd, r1, imm16 } => self.bitwise_i(Self::and, r1, imm16, rd),
            I::Orr { rd, r1, r2 } => self.bitwise_r(Self::or, r1, r2, rd),
            I::Ori { rd, r1, imm16 } => self.bitwise_i(Self::or, r1, imm16, rd),
            I::Norr { rd, r1, r2 } => self.bitwise_r(Self::nor, r1, r2, rd),
            I::Nori { rd, r1, imm16 } => self.bitwise_i(Self::nor, r1, imm16, rd),
            I::Xorr { rd, r1, r2 } => self.bitwise_r(Self::xor, r1, r2, rd),
            I::Xori { rd, r1, imm16 } => self.bitwise_i(Self::xor, r1, imm16, rd),
            I::Shlr { rd, r1, r2 } => self.bitwise_r(Self::shl, r1, r2, rd),
            I::Shli { rd, r1, imm16 } => self.bitwise_i(Self::shl, r1, imm16, rd),
            I::Asrr { rd, r1, r2 } => self.bitwise_r(Self::asr, r1, r2, rd),
            I::Asri { rd, r1, imm16 } => self.bitwise_i(Self::asr, r1, imm16, rd),
            I::Lsrr { rd, r1, r2 } => self.bitwise_r(Self::lsr, r1, r2, rd),
            I::Lsri { rd, r1, imm16 } => self.bitwise_i(Self::lsr, r1, imm16, rd),
            I::Bitr { rd, r1, r2 } => self.bitwise_r(Self::bit, r1, r2, rd),
            I::Biti { rd, r1, imm16 } => self.bitwise_i(Self::bit, r1, imm16, rd),
            // float operations
            I::Fcmp { r1, r2, p } => self.cpu.set_cmp_float_precision(self.regval(r1), self.regval(r2), p),
            I::Fto { rd, rs, p } => *self.regval_mut(rd) = p.fto(self.regval(rs)),
            I::Ffrom { rd, rs, p } => *self.regval_mut(rd) = p.ffrom(self.regval(rs)),
            I::Fneg { rd, rs, p } => *self.regval_mut(rd) = p.fneg(self.regval(rs)),
            I::Fabs { rd, rs, p } => *self.regval_mut(rd) = p.fabs(self.regval(rs)),
            I::Fadd { rd, r1, r2, p } => *self.regval_mut(rd) = p.fadd(self.regval(r1), self.regval(r2)),
            I::Fsub { rd, r1, r2, p } => *self.regval_mut(rd) = p.fsub(self.regval(r1), self.regval(r2)),
            I::Fmul { rd, r1, r2, p } => *self.regval_mut(rd) = p.fmul(self.regval(r1), self.regval(r2)),
            I::Fdiv { rd, r1, r2, p } => *self.regval_mut(rd) = p.fdiv(self.regval(r1), self.regval(r2)),
            I::Fma { rd, r1, r2, p } => p.fma(self.regval(r1), self.regval(r2), self.regval_mut(rd)),
            I::Fsqrt { rd, r1, p } => *self.regval_mut(rd) = p.fsqrt(self.regval(r1)),
            I::Fmin { rd, r1, r2, p } => *self.regval_mut(rd) = p.fmin(self.regval(r1), self.regval(r2)),
            I::Fmax { rd, r1, r2, p } => *self.regval_mut(rd) = p.fmax(self.regval(r1), self.regval(r2)),
            I::Fsat { rd, r1, p } => *self.regval_mut(rd) = p.fsat(self.regval(r1)),
            I::Fcnv { rd, r1, p } => *self.regval_mut(rd) = p.cast(self.regval(r1)),
            I::Fnan { rd, r1, p } => *self.regval_mut(rd) = p.fnan(self.regval(r1)),
        }
        Ok(())
    }
    /* ops */
    const fn carry(&self) -> bool { self.cpu.get_flag(StFlag::CARRY_BORROW_UNSIGNED) }
    /* arithmetic */
    fn arithmetic_r<Ret>(&mut self, func: impl Fn(&mut Self, u64, u64, Register) -> Ret, r1: Register, r2: Register, rd: Register) -> Ret {
        func(self, self.regval(r1), self.regval(r2), rd)
    }
    fn arithmetic_i<Ret>(&mut self, func: impl Fn(&mut Self, u64, u64, Register) -> Ret, r1: Register, imm16: u16, rd: Register) -> Ret {
        func(self, self.regval(r1), sign_extend::<16>(imm16.into()), rd)
    }
    fn add(&mut self, a: u64, b: u64, to: Register) {
        let AddResult { result, unsigned_overflow, signed_overflow } = ops::add(a, b, self.carry());
        *self.regval_mut(to) = result;
        self.cpu.set_flag(StFlag::CARRY_BORROW_UNSIGNED, unsigned_overflow);
        self.cpu.set_flag(StFlag::CARRY_BORROW, signed_overflow);
    }
    fn sub(&mut self, a: u64, b: u64, to: Register) {
        let AddResult { result, unsigned_overflow, signed_overflow } = ops::sub(a, b, self.carry());
        *self.regval_mut(to) = result;
        self.cpu.set_flag(StFlag::CARRY_BORROW_UNSIGNED, unsigned_overflow);
        self.cpu.set_flag(StFlag::CARRY_BORROW, signed_overflow);
    }
    fn imul(&mut self, a: u64, b: u64, to: Register) { *self.regval_mut(to) = ops::imul(a, b); }
    fn idiv(&mut self, a: u64, b: u64, to: Register) -> Result<(), Interrupt> {
        *self.regval_mut(to) = ops::idiv(a, b).ok_or(Interrupt::DIVIDE_BY_ZERO)?;
        Ok(())
    }
    fn umul(&mut self, a: u64, b: u64, to: Register) { *self.regval_mut(to) = ops::umul(a, b); }
    fn udiv(&mut self, a: u64, b: u64, to: Register) -> Result<(), Interrupt> {
        *self.regval_mut(to) = ops::udiv(a, b).ok_or(Interrupt::DIVIDE_BY_ZERO)?;
        Ok(())
    }
    // maybe interrupt instead?
    fn rem(&mut self, a: u64, b: u64, to: Register) { *self.regval_mut(to) = ops::rem(a, b).unwrap_or(0); }
    fn r#mod(&mut self, a: u64, b: u64, to: Register) { *self.regval_mut(to) = ops::r#mod(a, b).unwrap_or(0); }
    /* bitwise */
    fn bitwise_r<Ret>(&mut self, func: impl Fn(&mut Self, u64, u64, Register) -> Ret, r1: Register, r2: Register, rd: Register) -> Ret {
        func(self, self.regval(r1), self.regval(r2), rd)
    }
    fn bitwise_i<Ret>(&mut self, func: impl Fn(&mut Self, u64, u64, Register) -> Ret, r1: Register, imm16: u16, rd: Register) -> Ret {
        func(self, self.regval(r1), imm16.into(), rd)
    }
    fn and(&mut self, a: u64, b: u64, to: Register) { *self.regval_mut(to) = ops::and(a, b); }
    fn or(&mut self, a: u64, b: u64, to: Register) { *self.regval_mut(to) = ops::or(a, b); }
    fn nor(&mut self, a: u64, b: u64, to: Register) { *self.regval_mut(to) = ops::nor(a, b); }
    fn xor(&mut self, a: u64, b: u64, to: Register) { *self.regval_mut(to) = ops::xor(a, b); }
    fn shl(&mut self, a: u64, b: u64, to: Register) { *self.regval_mut(to) = ops::shl(a, b); }
    fn asr(&mut self, a: u64, b: u64, to: Register) { *self.regval_mut(to) = ops::asr(a, b); }
    fn lsr(&mut self, a: u64, b: u64, to: Register) { *self.regval_mut(to) = ops::shr(a, b); }
    fn bit(&mut self, a: u64, b: u64, to: Register) { *self.regval_mut(to) = ops::bit(a, b); }

    /* end ops */
}

#[cfg(feature = "std")]
impl State<std::boxed::Box<[u8]>> {
    #[must_use]
    pub fn new_boxed(mem_cap: Option<std::num::NonZeroUsize>, image: &[u8]) -> Option<Self> {
        Some(Self { mmu: Mmu::new_boxed(mem_cap, image)?, ic: Ic::new(), cpu: Cpu::new(), ioc: Ioc::new() })
    }
}
impl<const N: usize> State<[u8; N]> {
    #[must_use]
    pub fn new(image: &[u8]) -> Option<Self> { Some(Self { mmu: Mmu::new([0; N], image)?, ic: Ic::new(), cpu: Cpu::new(), ioc: Ioc::new() }) }
}

#[derive(Debug, Clone)]
pub struct Emulator<T: AsRef<[u8]> + AsMut<[u8]>, U: Callback> {
    pub state:    State<T>,
    pub callback: U,
}

impl<T: AsRef<[u8]> + AsMut<[u8]>, U: Callback> Emulator<T, U> {
    pub const fn new(state: State<T>, callback: U) -> Self { Self { state, callback } }
    pub fn run(mut self) -> U {
        self.callback.on_run_started(&mut self.state);
        loop {
            if self.callback.should_stop(&self.state) {
                break;
            }
            self.callback.on_iteration_started(&mut self.state);
            'inner: {
                match self.state.read_instruction(self.state.regval(Register::Ip)) {
                    Ok(instr) => self.state.set_current_instr(instr),
                    Err(err) => {
                        self.state.push_interrupt_from_mmu(err);
                        break 'inner;
                    }
                }
                self.callback.on_instruction_loaded(&mut self.state);

                *self.state.regval_mut(Register::Ip) += 4;

                if let Err(err) = self.state.interpret_code() {
                    self.state.push_interrupt(err);
                }

                // TODO: do io stuff

                /* if self.state.ioc.out_pin {
                    match self.state.ioc.port {
                        /* TODO: default ports */
                        port => self.callback.on_output_received(&self.state, port, self.state.ioc.port_data(port)),
                    }
                    self.state.ioc.out_pin = false;
                } */

                self.state.cpu.registers[Register::Rz] = 0;
                if self.state.regval(Register::Sp) > self.state.regval(Register::Fp) {
                    self.state.push_interrupt(Interrupt::STACK_UNDERFLOW);
                }
                
                // receive input
            }

            self.callback.on_iteration_ended(&mut self.state);
            self.state.cpu.cycle += 1;
        }
        self.callback.on_run_ended(&mut self.state);
        self.callback
    }
}
