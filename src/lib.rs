#![warn(clippy::pedantic)]
#![no_std]

// TODO: make the aphelion-util library no_std

#[cfg(feature = "std")]
extern crate std;

pub mod cpu;
pub mod emulator;
pub mod ic;
pub mod ioc;
pub mod mmu;
