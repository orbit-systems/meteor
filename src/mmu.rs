#[derive(Debug, Clone)]
pub struct Mmu<T: AsRef<[u8]> + AsMut<[u8]>> {
    pub memory: T,
    pub page_table_base: u64,
}
#[derive(Debug, Clone, Copy)]
pub enum Response {
    AccViolation,
    NoPerms,
    OutOfBounds,
    Unaligned,
}
/// TODO: put this in the library instead
#[derive(Debug, Clone, Copy)]
pub enum AccessMode {
    Valid = 0,
    Override = 1,
    Read = 2,
    Write = 3,
    Execute = 4,
}

/// TODO: put this in the library instead
#[derive(Debug, Clone, Copy)]
pub struct Pde(pub u64);

impl Pde {
    #[must_use]
    pub const fn valid(self) -> bool {
        self.0 & (1 << 0) != 0
    }
    #[must_use]
    pub const fn r#override(self) -> bool {
        self.0 & (1 << 1) != 0
    }
    #[must_use]
    pub const fn read(self) -> bool {
        self.0 & (1 << 2) != 0
    }
    #[must_use]
    pub const fn write(self) -> bool {
        self.0 & (1 << 3) != 0
    }
    #[must_use]
    pub const fn execute(self) -> bool {
        self.0 & (1 << 4) != 0
    }
    #[must_use]
    pub const fn next(self) -> u64 {
        self.0 & (!0b11_1111_1111_1111)
    }
    #[must_use]
    pub const fn has_perm(self, mode: AccessMode) -> bool {
        self.0 & (mode as u64) != 0
    }
}

macro_rules! phys_get {
    ($fn_name: ident, $ty: ty) => {
        /// # Errors
        ///
        /// Either [`Response::OutOfBounds`] or [`Response::Unaligned`] is returned as error. They should be straight forward.
        ///
        pub fn $fn_name(&self, addr: u64) -> Result<$ty, Response> {
            self.phys_get_sized(addr)
                .map(|val| <$ty>::from_le_bytes(*val))
        }
    };
}
macro_rules! phys_write {
    ($fn_name: ident, $ty: ty) => {
        /// # Errors
        ///
        /// Either [`Response::OutOfBounds`] or [`Response::Unaligned`] is returned as error. They should be straight forward.
        ///
        pub fn $fn_name(&mut self, addr: u64, with: $ty) -> Result<(), Response> {
            self.phys_write_sized(addr, &with.to_le_bytes())
        }
    };
}
impl<T: AsRef<[u8]> + AsMut<[u8]>> Mmu<T> {
    #[must_use]
    pub fn new(zeroed: T, image: &[u8]) -> Option<Self> {
        let mem_cap = zeroed.as_ref().len();
        if image.len() > mem_cap {
            return None;
        }
        let mut mem = zeroed;
        mem.as_mut()[..image.len()].copy_from_slice(image);
        Some(Self {
            memory: mem,
            page_table_base: 0,
        })
    }

    pub fn size(&self) -> usize {
        self.memory.as_ref().len()
    }

    /// # Errors
    ///
    /// Either [`Response::OutOfBounds`] or [`Response::Unaligned`] is returned as error. They should be straight forward.
    ///
    pub fn phys_get_sized<const SIZE: usize>(&self, addr: u64) -> Result<&[u8; SIZE], Response> {
        let addr = usize::try_from(addr).map_err(|_| Response::OutOfBounds)?;
        if addr >= self.size() {
            Err(Response::OutOfBounds)
        } else if addr % SIZE != 0 {
            Err(Response::Unaligned)
        } else {
            let Ok(res) = self.memory.as_ref()[addr..(addr + SIZE)].try_into() else {
                unreachable!()
            };
            Ok(res)
        }
    }
    /// # Errors
    ///
    /// Either [`Response::OutOfBounds`] or [`Response::Unaligned`] is returned as error. They should be straight forward.
    ///
    pub fn phys_write_sized<const SIZE: usize>(
        &mut self,
        addr: u64,
        with: &[u8; SIZE],
    ) -> Result<(), Response> {
        let addr = usize::try_from(addr).map_err(|_| Response::OutOfBounds)?;
        if addr >= self.size() {
            Err(Response::OutOfBounds)
        } else if addr % SIZE != 0 {
            Err(Response::Unaligned)
        } else {
            self.memory.as_mut()[addr..(addr + SIZE)].copy_from_slice(with);
            Ok(())
        }
    }
    phys_get! {phys_get_u8, u8}
    phys_get! {phys_get_u16, u16}
    phys_get! {phys_get_u32, u32}
    phys_get! {phys_get_u64, u64}
    phys_write! {phys_write_u8, u8}
    phys_write! {phys_write_u16, u16}
    phys_write! {phys_write_u32, u32}
    phys_write! {phys_write_u64, u64}

    #[rustfmt::skip]
    /// # Errors
    ///
    /// The error should be straight fowarsd.
    ///
    pub fn translate_address(&self, r#virtual: u64, mode: AccessMode) -> Result<u64, Response> {
        let level_1_index =  r#virtual >> (11 + 11 + 11 + 11 + 14);
        let level_2_index = (r#virtual >> (     11 + 11 + 11 + 14)) & 0b00_0111_1111_1111;
        let level_3_index = (r#virtual >> (          11 + 11 + 14)) & 0b00_0111_1111_1111;
        let level_4_index = (r#virtual >> (               11 + 14)) & 0b00_0111_1111_1111;
        let level_5_index = (r#virtual >> (                    14)) & 0b00_0111_1111_1111;
        let level_6_index =  r#virtual                              & 0b11_1111_1111_1111;

        let mut auth_pde = None;
        let next = self.translate_address_level(self.page_table_base, level_1_index, mode, &mut auth_pde)?;
        let next = self.translate_address_level(next, level_2_index, mode, &mut auth_pde)?;
        let next = self.translate_address_level(next, level_3_index, mode, &mut auth_pde)?;
        let next = self.translate_address_level(next, level_4_index, mode, &mut auth_pde)?;
        let next = self.translate_address_level(next, level_5_index, mode, &mut auth_pde)?;
        Ok(next + level_6_index)
    }
    fn translate_address_level(
        &self,
        next: u64,
        level_index: u64,
        mode: AccessMode,
        auth_pde: &mut Option<Pde>,
    ) -> Result<u64, Response> {
        let pde = match self.phys_get_u64(next + level_index * 8) {
            Ok(pde) if Pde(pde).valid() => Pde(pde),
            _ => return Err(Response::AccViolation),
        };
        if pde.r#override() {
            *auth_pde = Some(pde);
        }
        if !(auth_pde.unwrap_or(pde).has_perm(mode)) {
            return Err(Response::NoPerms);
        }
        Ok(pde.next())
    }
}
pub mod std {
    #![cfg(feature = "std")]
    use super::Mmu;
    use std::{boxed::Box, num::NonZeroUsize, vec};

    impl Mmu<Box<[u8]>> {
        pub const MEM_PAGE_SIZE: usize = 0x4000;
        pub const MEM_DEFAULT_SIZE: usize = 4096 * Self::MEM_PAGE_SIZE;

        #[must_use]
        pub fn new_boxed(mem_cap: Option<NonZeroUsize>, image: &[u8]) -> Option<Self> {
            let mem_cap = mem_cap.map_or(Self::MEM_DEFAULT_SIZE, std::convert::Into::into);
            if image.len() > mem_cap {
                return None;
            }
            // perhaps there is a better way to do it
            let mut mem = vec![0u8; mem_cap];
            mem[..image.len()].copy_from_slice(image);
            Some(Self {
                memory: mem.into_boxed_slice(),
                page_table_base: 0,
            })
        }
    }
}
