extern crate ress;
extern crate either;
use ress::{Scanner, Item};
pub mod node;
pub struct Parser {
    context: Context,
    config: Config,
    scanner: Scanner,
}

struct Context {
    is_assignment_target: bool,
    is_binding_element: bool,
    in_function_body: bool,
    in_iteration: bool,
    in_switch: bool,
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
    allow_in: bool,
    allow_strict: bool,
    allow_yield: bool,
    allow_await: bool,
    allow_async: bool,
    strict: bool,
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
        }
    }
    pub fn parse(self) -> Vec<Item> {
        self.scanner.collect()
    }
}


#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
