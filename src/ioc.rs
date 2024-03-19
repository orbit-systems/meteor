use aphelion_util::{interrupt::Interrupt, io::Port};
pub const NUM_PORTS: usize = Ioc::NUM_PORTS;
#[allow(clippy::cast_possible_truncation)]
pub const NUM_PORTS_U16: u16 = NUM_PORTS as u16;

#[derive(Debug, Clone, Copy)]
pub struct Ioc {
    pub in_pin:   bool,
    pub out_pin:  bool,
    pub port:     Port,
    pub binding:  [u8; NUM_PORTS],
    pub is_bound: [bool; NUM_PORTS],
    pub ports:    [u64; NUM_PORTS],
}

impl Ioc {
    pub const NUM_PORTS: usize = 256;
    #[must_use]
    pub const fn new() -> Self {
        Self { in_pin: false, out_pin: false, port: Port(0), binding: [0; NUM_PORTS], is_bound: [false; NUM_PORTS], ports: [0; NUM_PORTS] }
    }
    pub fn send_out(&mut self, port: Port, data: u64) {
        let port = Port(port.0 % NUM_PORTS_U16);
        self.out_pin = true;
        self.port = port;
        self.ports[port.0 as usize] = data;
    }
    #[must_use]
    pub const fn port_data(&self, port: Port) -> u64 { self.ports[port.0 as usize % NUM_PORTS] }
    pub fn bind_port(&mut self, port: Port, interrupt: Interrupt) {
        let port = port.0 as usize % NUM_PORTS;
        self.is_bound[port] = true;
        self.binding[port] = interrupt.0;
    }
}
impl Default for Ioc {
    fn default() -> Self {
        Self {
            in_pin:   false,
            out_pin:  false,
            port:     Port(0),
            binding:  [0; Self::NUM_PORTS],
            is_bound: [false; Self::NUM_PORTS],
            ports:    [0; Self::NUM_PORTS],
        }
    }
}
