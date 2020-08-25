
use in_and_out_logger::log_in_and_out;
use log::trace;
#[log_in_and_out]
fn main() {
    recursive_one(0)
}
#[log_in_and_out]
fn recursive_one(n: u32) {
    if n > 100 {
        return
    }
    recursive_two(n+1)
}

#[log_in_and_out]
pub fn recursive_two(n: u32) {
    recursive_one(n+1)
}