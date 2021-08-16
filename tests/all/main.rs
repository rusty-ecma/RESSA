extern crate env_logger;
#[macro_use]
extern crate log;
extern crate ress;
extern crate ressa;
#[macro_use]
extern crate lazy_static;

mod comment_handler;
mod ecma262;
mod es_tokens;
mod major_libs;
mod snippets;
#[cfg(feature = "moz_central")]
mod spider_monkey;

use std::{fs::read_to_string, io::Error};

#[derive(Clone, Copy, Debug)]
enum Lib {
    Jquery,
    Angular,
    React,
    ReactDom,
    Vue,
    Moment,
    Dexie,
    Everything(EverythingVersion),
}
#[derive(Clone, Copy, Debug)]
enum EverythingVersion {
    Es5,
    Es2015Module,
    Es2015Script,
}

impl EverythingVersion {
    fn file_name(&self) -> &str {
        match self {
            EverythingVersion::Es5 => "es5.js",
            EverythingVersion::Es2015Script => "es2015-script.js",
            EverythingVersion::Es2015Module => "es2015-module.js",
        }
    }
}

impl Lib {
    pub fn path(&self) -> String {
        match self {
            Lib::Jquery => "node_modules/jquery/dist/jquery.js".into(),
            Lib::Angular => "node_modules/angular/angular.js".into(),
            Lib::React => "node_modules/react/umd/react.development.js".into(),
            Lib::ReactDom => "node_modules/react-dom/umd/react-dom.development.js".into(),
            Lib::Vue => "node_modules/vue/dist/vue.js".into(),
            Lib::Moment => "node_modules/moment/moment.js".into(),
            Lib::Dexie => "node_modules/dexie/dist/dexie.js".into(),
            Lib::Everything(kind) => format!("node_modules/everything.js/{}", kind.file_name()),
        }
    }

    pub fn min_path(&self) -> Option<String> {
        match self {
            Lib::Jquery => Some("node_modules/jquery/dist/jquery.min.js".into()),
            Lib::Angular => Some("node_modules/angular/angular.min.js".into()),
            Lib::React => Some("node_modules/react/umd/react.production.min.js".into()),
            Lib::ReactDom => Some("node_modules/react-dom/umd/react-dom.production.min.js".into()),
            Lib::Vue => Some("node_modules/vue/dist/vue.js".into()),
            Lib::Moment => Some("node_modules/moment/min/moment.min.js".into()),
            Lib::Dexie => Some("node_modules/dexie/dist/dexie.min.js".into()),
            _ => None,
        }
    }
}

pub fn get_js_file(path: impl AsRef<::std::path::Path>) -> Result<String, Error> {
    let path = path.as_ref();
    if !path.exists() {
        npm_install()?;
        if !path.exists() {
            panic!("npm install failed to make {:?} available", path);
        }
    }
    read_to_string(path)
}

pub fn npm_install() -> Result<(), Error> {
    let mut c = ::std::process::Command::new("npm");
    c.arg("i");
    c.output()?;
    Ok(())
}

fn format_error(js: &str, e: &ressa::Error) -> Option<String> {
    let start = e.position()?;
    let column = js.lines().nth(start.line.saturating_sub(1))?.len();
    let end = ress::Position {
        line: start.line,
        column,
    };
    hilight_position(js, &ress::SourceLocation { start, end })
}
fn hilight_position(js: &str, location: &ress::SourceLocation) -> Option<String> {
    let line_count = js.lines().count();
    if line_count < 5 {
        return Some(js.to_string());
    }
    let skip = location.start.line.saturating_sub(2);
    Some(
        js.lines()
            .enumerate()
            .skip(skip)
            .take(5)
            .map(|(i, l)| {
                if i + 1 == location.start.line {
                    let whitespace = " ".repeat(location.start.column);
                    let arrows = "^".repeat(location.end.column - location.start.column);
                    format!("{}\n{}{}\n", l, whitespace, arrows)
                } else {
                    format!("{}\n", l)
                }
            })
            .collect(),
    )
}
