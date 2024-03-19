use std::{fs::read, io, path::PathBuf, time::Instant};

use aphelion_util::{io::Port, registers::Register};
use clap::{
    error::{ErrorKind, RichFormatter},
    Parser,
};
use meteor_rs::emulator::{Callback, Emulator, State};
use thiserror::Error;

#[derive(Debug, Parser)]
#[command(
    version,
    about,
    long_about = None,
    arg_required_else_help = true,
)]
struct Args {
    path:       PathBuf,
    #[arg(short, long)]
    /// launch window with debug interface
    debug:      bool,
    #[arg(short, long, value_name = "INT", default_value_t = 0)]
    /// halt after cycle count has been reached (will run forever if unset)
    max_cycles: usize,
    #[arg(short = 'M', long, value_name = "INT", default_value_t = 1 << 26)]
    /// use a custom address space size; the maximum addressable byte will be [int]-1
    /// if not provided, defaults to 2^26 (64 MiB)
    memory:     usize,
    #[arg(short, long)]
    /// output benchmark info after execution is halted
    bench:      bool,
}

#[derive(Debug, Clone, Copy, Error)]
enum FileLoadError {
    #[error("no such file exists.")]
    NoSuchFile,
    #[error("path exists but is a directory, not a file.")]
    IsDirectory,
    #[error("no access.")]
    NoAccess,
    #[error("{0}")]
    Unknown(io::ErrorKind),
}

#[derive(Debug, Clone, Copy, Error)]
enum Error {
    #[error("failed to load file: {0}")]
    FileLoad(FileLoadError),
    #[error("could not initialize memory, file is likely too big. requires {file} bytes, but only {cap} bytes are available.")]
    CouldNotInitialize { cap: usize, file: usize },
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum PrintBuffLen {
    Zero  = 0,
    One   = 1,
    Two   = 2,
    Three = 3,
    Four  = 4,
}
impl PrintBuffLen {
    const fn incremented(self) -> Self {
        match self {
            Self::Zero => Self::One,
            Self::One => Self::Two,
            Self::Two => Self::Three,
            Self::Three => Self::Four,
            Self::Four => Self::Zero,
        }
    }
    fn increment(&mut self) { *self = self.incremented(); }
}
#[derive(Debug, Clone, Copy)]
struct PrintBuff {
    data: [u8; 4],
    len:  PrintBuffLen,
}
impl PrintBuff {
    const fn new() -> Self { Self { data: [0; 4], len: PrintBuffLen::Zero } }
    fn push(&mut self, byte: u8) -> Option<&str> {
        if self.len == PrintBuffLen::Four {
            self.len = PrintBuffLen::One;
        } else {
            self.len.increment();
        }

        self.data[self.len as usize - 1] = byte;
        match std::str::from_utf8(&self.data[0..{ self.len as usize }]) {
            Ok(val) => {
                self.len = PrintBuffLen::Zero;
                Some(val)
            }
            Err(_) => {
                if self.len == PrintBuffLen::Four {
                    Some("�")
                } else {
                    None
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
struct CliCallback {
    should_stop: bool,
    debug:       bool,
    max_cycles:  usize,
    print_buff:  PrintBuff,
    bench:       bool,
    now:         Instant,
}
impl CliCallback {
    const STDOUT: Port = Port(10);
    fn new(debug: bool, max_cycles: usize, bench: bool) -> Self {
        Self {
            should_stop: false,
            debug,
            max_cycles,
            print_buff: PrintBuff::new(),
            bench,
            now: Instant::now(),
        }
    }
}
impl Callback for CliCallback {
    fn should_stop<T: AsRef<[u8]> + AsMut<[u8]>>(
        &mut self, state: &meteor_rs::emulator::State<T>,
    ) -> bool {
        if self.max_cycles != 0 && state.cpu.cycle >= self.max_cycles as u64 {
            self.should_stop = true;
        }
        self.should_stop
    }
    fn on_instruction_loaded<T: AsRef<[u8]> + AsMut<[u8]>>(&mut self, state: &mut State<T>) {
        if self.debug {
            println!("{}", state.current_instr());
            println!(
                "\t                       ra: 0x{:016X} rb: 0x{:016X} rc: 0x{:016X}",
                state.regval(Register::Ra),
                state.regval(Register::Rb),
                state.regval(Register::Rc),
            );
            println!(
                "\trd: 0x{:016X} re: 0x{:016X} rf: 0x{:016X} rg: 0x{:016X}",
                state.regval(Register::Rd),
                state.regval(Register::Re),
                state.regval(Register::Rf),
                state.regval(Register::Rg),
            );
            println!(
                "\trh: 0x{:016X} ri: 0x{:016X} rj: 0x{:016X} rk: 0x{:016X}",
                state.regval(Register::Rh),
                state.regval(Register::Ri),
                state.regval(Register::Rj),
                state.regval(Register::Rk),
            );
            println!(
                "\tsp: 0x{:016X} fp: 0x{:016X} ip: 0x{:016X} st: 0x{:016X}",
                state.regval(Register::Sp),
                state.regval(Register::Fp),
                state.regval(Register::Ip),
                state.regval(Register::St),
            );
        }
    }

    fn on_output_received<T: AsRef<[u8]> + AsMut<[u8]>>(
        &mut self, _state: &State<T>, port: Port, data: u64,
    ) {
        #[allow(clippy::single_match)]
        match port {
            Self::STDOUT => {
                if let Some(str) = self.print_buff.push(data as u8) {
                    print!("{str}");
                }
            }
            _ => {}
        }
    }

    fn on_run_ended<T: AsRef<[u8]> + AsMut<[u8]>>(&mut self, state: &mut State<T>) {
        if self.bench {
            let elapsed = self.now.elapsed();
            let cycles = state.cpu.cycle;
            let persec = cycles as f64 / elapsed.as_secs_f64();
            let (s, ms) = (elapsed.as_secs(), elapsed.subsec_millis());
            println!("\ttime      : {s}.{ms:03}");
            println!("\tcycles    : {cycles}");
            println!("\tcycles/s  : {persec:.3}");
        }
    }
}

fn cli_main(Args { path, debug, max_cycles, memory, bench }: Args) -> Result<(), Error> {
    if path.is_dir() {
        Err(Error::FileLoad(FileLoadError::IsDirectory))?
    }
    let file = read(path).map_err(|err| match err.kind() {
        io::ErrorKind::NotFound => Error::FileLoad(FileLoadError::NoSuchFile),
        io::ErrorKind::PermissionDenied => Error::FileLoad(FileLoadError::NoAccess),
        err => Error::FileLoad(FileLoadError::Unknown(err)),
    })?;
    let Some(state) = State::new_boxed(memory.try_into().ok(), &file) else {
        Err(Error::CouldNotInitialize { cap: memory, file: file.len() })?
    };
    let callback = CliCallback::new(debug, max_cycles, bench);
    let emulator = Emulator::new(state, callback);
    let _ = emulator.run();
    Ok(())
}

fn main() {
    let args = Args::parse();
    if let Err(err) = cli_main(args) {
        println!("{}", clap::error::Error::<RichFormatter>::raw(ErrorKind::ValueValidation, err));
    }
}
