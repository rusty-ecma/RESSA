extern crate ress;
extern crate either;
use ress::{Scanner, Item};
pub mod node;
pub struct Parser {
    context: Context,
    config: Config,
    scanner: Scanner,
    parsed: Vec<node::Node>,
}

struct Context {
    pub is_assignment_target: bool,
    pub is_binding_element: bool,
    pub in_function_body: bool,
    pub in_iteration: bool,
    pub in_switch: bool,
}

impl Default for Context {
    fn default() -> Self {
        Context {
            is_assignment_target: false,
            is_binding_element: false,
            in_function_body: false,
            in_iteration: false,
            in_switch: false,
        }
    }
}


pub struct Config {
    pub allow_in: bool,
    pub allow_strict: bool,
    pub allow_yield: bool,
    pub allow_await: bool,
    pub allow_async: bool,
    pub strict: bool,
}

impl Default for Config {
    fn default() -> Self {
        Config {
            allow_in: false,
            allow_strict: false,
            allow_yield: false,
            allow_await: false,
            allow_async: false,
            strict: false,
        }
    }
}

impl Parser {
    pub fn new(text: &str) -> Parser {
        Parser {
            scanner: Scanner::new(text),
            config: Config::default(),
            context: Context::default(),
            parsed: vec![],
        }
    }
    pub fn parse(self) -> Vec<Item> {
        self.scanner.collect()
    }
}


#[cfg(test)]
mod tests {
}
