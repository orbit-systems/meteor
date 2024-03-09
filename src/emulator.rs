use crate::{cpu::Cpu, ic::Ic, ioc::Ioc, mmu::Mmu};

use std::num::NonZeroUsize;

pub trait Callback: Sized {
    fn new_emulator(self, mem_cap: Option<NonZeroUsize>, image: &[u8]) -> Option<Emulator<Self>> {
        Some(self.new_emulator_with_state(State::new(mem_cap, image)?))
    }
    #[must_use]
    fn new_emulator_with_state(self, state: State) -> Emulator<Self> { Emulator::new(state, self) }
}

#[derive(Debug, Clone)]
pub struct State {
    pub mmu: Mmu,
    pub ic:  Ic,
    pub cpu: Cpu,
    pub ioc: Ioc,
}

impl State {
    #[must_use]
    pub fn new(mem_cap: Option<NonZeroUsize>, image: &[u8]) -> Option<Self> {
        Some(Self { mmu: Mmu::new(mem_cap, image)?, ic: Ic::new(), cpu: Cpu::new(), ioc: Ioc::new() })
    }
}

#[derive(Debug, Clone)]
pub struct Emulator<T: Callback> {
    pub state:    State,
    pub callback: T,
}

impl<T: Callback> Emulator<T> {
    pub const fn new(state: State, callback: T) -> Self { Self { state, callback } }
}
