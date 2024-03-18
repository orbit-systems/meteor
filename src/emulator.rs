use aphelion_util::{
    helper::{ops::BitAccess, sign_extend},
    instruction::{instruction_set::InstructionSet, Instruction},
    interrupt::Interrupt,
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
    pub fn interpret_code(&mut self) -> Result<(), Interrupt> {
        use InstructionSet as I;
        let instr = self.current_instr().try_into_instruction_set().ok_or(Interrupt::INVALID_OPERATION)?;
        match instr {
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
            I::Outr { rd, rs } => {
                self.proc_mode_is_user_then_invalid()?;
                todo!()
            }
            _ => todo!(),
        }
        Ok(())
    }
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
                *self.state.regval_mut(Register::Ip) += 4;
                todo!()
            }

            self.callback.on_iteration_ended(&mut self.state);
            self.state.cpu.cycle += 1;
        }
        self.callback.on_run_ended(&mut self.state);
        self.callback
    }
}
