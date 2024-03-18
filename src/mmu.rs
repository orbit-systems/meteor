#[derive(Debug, Clone)]
pub struct Mmu<T: AsRef<[u8]> + AsMut<[u8]>> {
    pub memory:          T,
    pub page_table_base: u64,
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
        Some(Self { memory: mem, page_table_base: 0 })
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
            Some(Self { memory: mem.into_boxed_slice(), page_table_base: 0 })
        }
    }
}
