#![cfg(feature = "test_262")]
#[macro_use]
extern crate serde_derive;

use ress::prelude::*;
use ressa::Parser;
use std::{
    error::Error,
    path::{Path, PathBuf},
};
use flate2::read::GzDecoder;


type Tok<'a> = Token<&'a str>;
type It<'a> = Item<Tok<'a>>;
type Res<T> = Result<T, Box<dyn Error>>;

struct Test262Runner<'a> {
    desc: Description,
    js: &'a str,
}

#[derive(Debug)]
struct E262(String);
impl ::std::fmt::Display for E262 {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(f, "{}", self.0)
    }
}
impl Error for E262 {}
impl E262 {
    pub fn new(s: &str) -> Box<Self> {
        Box::new(Self(s.to_string()))
    }
}

impl<'a> Test262Runner<'a> {
    pub fn new(js: &'a str) -> Res<Self> {
        let desc = Self::find_desc_comment(js)?;
        Ok(Self {
            desc,
            js,
        })
    }
    fn find_desc_comment(js: &str) -> Res<Description> {
        for item in ress::Scanner::new(js) {
            if let Token::Comment(comment) = &item?.token {
                if comment.is_multi_line() && comment.content.starts_with("---") {
                    let trimmed = comment.content.trim_matches('-');
                    let ret = serde_yaml::from_str(trimmed)?;
                    return Ok(ret)
                }
            }
        }
        Err(E262::new("no description comment found"))
    }

    pub fn run_strict(&self) -> Res<()> {
        if let Some(flags) = &self.desc.flags {
            if !flags.iter().any(|f| f == &Flag::Module || f == &Flag::NoStrict) {
                self.run_script(&format!("'use strict'\n{}", self.js))?;
            } 
        } 
        Ok(())
    }

    pub fn run(&self) -> Res<()> {
        if let Some(flags) = &self.desc.flags {
            if !flags.iter().any(|f| f == &Flag::OnlyStrict) {
                if flags.iter().any(|f| f == &Flag::Module) {
                    self.run_mod(&self.js)?;
                } else {
                    self.run_script(&self.js)?;
                }
            } 
        }
        Ok(())
    }

    fn run_script(&self, js: &str) -> Res<()> {
        self.run_(false, js)?;
        Ok(())
    }

    fn run_mod(&self, js: &str) -> Res<()> {
        self.run_(true, js)?;
        Ok(())
    }

    fn run_(&self, module: bool, js: &str)  -> Res<()> {
        let mut p = Parser::builder().module(module).js(js).build()?;
        match p.parse() {
            Ok(_) => {
                if let Some(n) = &self.desc.negative {
                    if &n.phase == &Phase::Parse {
                        return Err(E262::new("Unexpected successful parsing"))
                    }
                }
            },
            Err(e) => {
                if let Some(n) = &self.desc.negative {
                    if &n.phase == &Phase::Parse {
                        return Ok(())
                    }
                }
                return Err(Box::new(e))
            }
        }
        Ok(())
    }
}


#[derive(Debug, Deserialize)]
struct Description {
    negative: Option<Negative>,
    includes: Vec<String>,
    flags: Option<Vec<Flag>>,
    locale: Option<String>,
}

#[derive(Debug, Deserialize)]
struct Negative {
    phase: Phase,
    kind: String,
}

#[derive(Debug, Deserialize, PartialEq)]
#[serde(rename_all = "camelCase", untagged)]
enum Phase {
    Parse,
    Early,
    Resolution,
    Runtime,
}

#[derive(Debug, Deserialize, PartialEq)]
#[serde(untagged)]
enum Flag {
    #[serde(rename = "camelCase")]
    OnlyStrict,
    #[serde(rename = "camelCase")]
    NoStrict,
    #[serde(rename = "camelCase")]
    Module,
    #[serde(rename = "camelCase")]
    Raw,
    #[serde(rename = "camelCase")]
    Async,
    #[serde(rename = "camelCase")]
    Generated,
    CanBlockIsFalse,
    CanBlockIsTrue,
}

static URL: &str = "https://github.com/tc39/test262/tarball/master";

#[test]
fn run_against_ecma262() -> Res<()> {
    let path = Path::new("./test262");
    if !path.exists() {
        get_repo(path)?;
    }
    walk(&path)
}

fn walk(path: &Path) -> Res<()> {
    for entry in walkdir::WalkDir::new(path) {
        let entry = entry?;
        let current = entry.path();
        if !current.is_dir() {
            let test = if let Some(ext) = current.extension() {
                ext == "js"
            } else {
                false
            };
            if !test {
                continue;
            }
            let contents = ::std::fs::read_to_string(&current)?;
            let handler = Test262Runner::new(&contents)?;
            if let Err(e) = handler.run() {
                panic!("Error in non-strict for {}\n\t{}", current.display(), e);
            }
            if let Err(e) = handler.run_strict() {
                panic!("Error in strict for {}\n\t{}", current.display(), e);
            }
        }
    }
    Ok(())
}

fn get_repo(path: &Path) -> Res<()> {
    let _ = ::std::fs::create_dir_all(path);
    let mut response = reqwest::get(
        URL,
    )?;
    let mut buf = Vec::new();
    response
        .copy_to(&mut buf)?;
        
    let gz = GzDecoder::new(buf.as_slice());
    let mut t = tar::Archive::new(gz);
    let mut target: Option<PathBuf> = None;
    for entry in t.entries()? {
        let mut entry = entry?;
        let p = entry.path()?.into_owned();
        if target.is_none() && format!("{}", p.display()).starts_with("tc39") {
            target = Some(p.clone().to_path_buf().join("test"));
        }
        let stripped = if let Some(ref target) = target {
            if p.starts_with(target) {
                if let Ok(p2) = p.strip_prefix(target) {
                    eprintln!("stripped {}", p.display());
                    Some(p2.clone())
                } else {
                    eprintln!("failed to strip {}", p.display());
                    None
                }
            } else {
                None
            }
        } else {
            None
        };
        if let Some(p2) = stripped {
            entry.unpack(path.join(p2))?;
        }
    }
    Ok(())
}