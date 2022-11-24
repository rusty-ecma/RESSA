use docopt::Docopt;
use serde::Deserialize;
use std::{
    error::Error,
    ffi::OsStr,
    fs::{read_to_string, write},
    path::PathBuf,
};

use ressa::Parser;

static USAGE: &str = "
js_to_json

Usage:
    js_to_json <path> [options]
    js_to_json -h | --help

Options:
    -h --help       show this screen
    -p --pretty     prettify json output
    -o --out PATH   Where to save the json
";

#[derive(Deserialize)]
struct Args {
    arg_path: PathBuf,
    flag_pretty: bool,
    flag_out: Option<PathBuf>,
}

fn main() -> Result<(), Box<dyn Error>> {
    let args: Args = Docopt::new(USAGE)
        .and_then(|o| o.deserialize())
        .unwrap_or_else(|e| e.exit());
    if args.arg_path.exists() {
        let json = gen_json(args.arg_path, args.flag_pretty)?;
        if let Some(mut out) = args.flag_out {
            if out.is_dir() {
                out.push("ressa.json");
            }
            write(out, json.as_bytes())?;
        } else {
            println!("{}", json);
        }
    } else {
        eprintln!("{}", USAGE);
    }
    Ok(())
}

fn gen_json(from: PathBuf, pretty: bool) -> Result<String, Box<dyn Error>> {
    let js = read_to_string(&from)?;
    let mut p = Parser::builder()
        .js(&js)
        .module(from.extension() == Some(&OsStr::new(".mjs")))
        .build()?;
    let ast = p.parse()?;
    let ret = if pretty {
        serde_json::to_string_pretty(&ast)?
    } else {
        serde_json::to_string(&ast)?
    };
    Ok(ret)
}
