

use aphelion_util::interrupt::Interrupt;
use meteor_rs::ic::IntQueue;

fn main() {
    let mut int_queue = IntQueue::new();
    println!("{int_queue:?}");
    println!("pushing back {:?}...", Interrupt::UNALIGNED_ACCESS);
    int_queue.push_back(Interrupt::UNALIGNED_ACCESS);
    println!("{int_queue:?}");
    println!("pushing back {:?}...", Interrupt::ACCESS_VIOLATION);
    int_queue.push_back(Interrupt::ACCESS_VIOLATION);
    println!("{int_queue:?}");
    println!("pushing front {:?}...", Interrupt::BREAK_POINT);
    int_queue.push_front(Interrupt::BREAK_POINT);
    println!("{int_queue:?}");
    println!("popping back...");
    let _ = int_queue.pop_back();
    println!("{int_queue:?}");
    println!("popping front...");
    let _ = int_queue.pop_front();
    println!("{int_queue:?}");
    println!();
    println!("B)");
    println!();
}
