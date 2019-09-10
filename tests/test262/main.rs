#![cfg(feature = "test_262")]
use serde::{
    Deserialize, 
};

use ress::prelude::*;
use ressa::Parser;
use std::{
    error::Error,
    path::{Path, PathBuf},
};
use flate2::read::GzDecoder;


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
        if !self.desc.flags.iter().any(|f| f == &Flag::Module || f == &Flag::NoStrict) {
            self.run_script(&format!("'use strict'\n{}", self.js))?;
        } 
        Ok(())
    }

    pub fn run(&self) -> Res<()> {
        if !self.desc.flags.iter().any(|f| f == &Flag::OnlyStrict) {
            if self.desc.flags.iter().any(|f| f == &Flag::Module) {
                self.run_mod(&self.js)?;
            } else {
                self.run_script(&self.js)?;
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
    #[serde(default)]
    includes: Vec<String>,
    #[serde(default)]
    flags: Vec<Flag>,
    #[serde(default)]
    locale: Vec<String>,
}

#[derive(Debug, Deserialize)]
struct Negative {
    phase: Phase,
    #[serde(alias = "type")]
    kind: Option<String>,
}

#[derive(Debug, PartialEq, Deserialize)]
#[serde(rename_all = "camelCase")]
enum Phase {
    Parse,
    Early,
    Resolution,
    Runtime,
}

#[derive(Debug, Deserialize, PartialEq)]
#[serde(rename_all = "camelCase")]
enum Flag {
    OnlyStrict,
    NoStrict,
    Module,
    Raw,
    Async,
    Generated,
    CanBlockIsFalse,
    CanBlockIsTrue,
    Unknown(String)
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
            let handler = match Test262Runner::new(&contents) {
                Ok(h) => h,
                Err(e) => panic!("Error creating runner for {}\n\t{}",  current.display(), e),
            };
            eprintln!("{}\n{:?}", current.display(), handler.desc);
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
                    Some(p2.clone())
                } else {
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

#[test]
fn yam() {
    let yaml = "es6id: B.3.3.2
flags: [onlyStrict]
info: |
    B.3.3.2 Changes to GlobalDeclarationInstantiation

    1. 1. Let strict be IsStrict of script
    2. If strict is *false*, then
       [...]";
    let res: serde_yaml::Mapping = serde_yaml::from_str(yaml).expect("failed to parse yaml");
    eprintln!("{:?}", res);
    let yaml2 = "[onlyStrict]";
    let res2: Vec<Flag> = serde_yaml::from_str(yaml2).expect("failed to parse yaml2");
    eprintln!("{:?}", res2);
}