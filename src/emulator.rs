use crate::{cpu::Cpu, ic::Ic, ioc::Ioc, mmu::Mmu};

pub trait Callback: Sized {}

#[derive(Debug, Clone)]
pub struct State<T: AsRef<[u8]> + AsMut<[u8]>> {
    pub mmu: Mmu<T>,
    pub ic:  Ic,
    pub cpu: Cpu,
    pub ioc: Ioc,
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
}
