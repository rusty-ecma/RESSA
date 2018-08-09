use ress::{Item, Scanner};
use super::Line;



pub struct Resprima {
    config: Config,
    context: Context,
    scanner: Scanner,
    look_ahead: Item,
    tokens: Vec<Item>,
}

impl Resprima {
    pub fn new(text: String) -> Self {
        let mut s = Scanner::new(s);
        let la = s.next().unwrap();
        Self {
            config: Config::default(),
            context: Context::default(),
            scanner: s,
            look_ahead: la,
        }
    }

    pub fn next_token() -> Item {

    }

    pub fn parse_script() {
        unimplemented!()
    }

}