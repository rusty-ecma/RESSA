#![cfg(feature = "test_262")]
use serde::{
    Deserialize, 
};

use ressa::Parser;
use std::{
    error::Error,
    path::{Path, PathBuf},
};
use flate2::read::GzDecoder;
use indicatif::{ParallelProgressIterator, ProgressBar, ProgressStyle};
use rayon::iter::{ParallelIterator, IntoParallelRefIterator};


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
    pub fn new(s: &str) -> Self {
        Self(s.to_string())
    }
    pub fn boxed(s: &str) -> Box<Self> {
        Box::new(Self::new(s))

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
        let start = js.find("/*---");
        if let Some(start_idx) = start {
            let ending = &js[start_idx+5..];
            if let Some(end_idx) = ending.find("---*/") {
                let trimmed = &ending[..end_idx];
                let ret = if trimmed.contains('\r') {
                    serde_yaml::from_str(&trimmed.replace("\r\n", "\n").replace('\r', "\n"))?
                } else {
                    serde_yaml::from_str(trimmed)?
                };
                return Ok(ret)
            }
        }
        Err(E262::boxed("no description comment found"))
    }
    pub fn clone_desc(&self) -> Description {
        self.desc.clone()
    }
    pub fn run_strict(&self) -> Res<()> {
        if !self.desc.flags.iter().any(|f| f == &Flag::Module || f == &Flag::NoStrict) {
            self.run_script(&format!("'use strict'\n{}", self.js))?;
        } 
        Ok(())
    }

    pub fn run(&self) -> Result<(), E262> {
        if !self.desc.flags.iter().any(|f| f == &Flag::OnlyStrict) {
            if self.desc.flags.iter().any(|f| f == &Flag::Module) {
                self.run_mod(&self.js)?;
            } else {
                self.run_script(&self.js)?;
            }
        } 
        Ok(())
    }

    fn run_script(&self, js: &str) -> Result<(), E262> {
        self.run_(false, js)?;
        Ok(())
    }

    fn run_mod(&self, js: &str) -> Result<(), E262> {
        self.run_(true, js)?;
        Ok(())
    }

    fn run_(&self, module: bool, js: &str)  -> Result<(), E262> {
        match ::std::panic::catch_unwind::<_, Result<(), E262>>(|| {
            let mut p = Parser::builder().module(module).js(js).build().map_err(|e| E262::new(&format!("{:?}", e)))?;

            match p.parse() {
                Ok(_) => {
                    if let Some(n) = &self.desc.negative {
                        if &n.phase == &Phase::Parse {
                            Err(E262::new("Unexpected successful parsing"))
                        } else {
                            Ok(())
                        }
                    } else {
                        Ok(())
                    }
                },
                Err(e) => {
                    if let Some(n) = &self.desc.negative {
                        if &n.phase == &Phase::Parse {
                            return Ok(())
                        } else {
                            Err(E262::new(&format!("{:?}", e)))
                        }
                    } else {
                            Err(E262::new(&format!("{:?}", e)))
                    }
                }
            }
        }) {
            Ok(inner) => inner,
            Err(e) => Err(E262::new(&format!("{:?}", e)))
        }
    }
}


#[derive(Debug, Deserialize, Clone, Default)]
struct Description {
    info: Option<String>,
    description: Option<String>,
    negative: Option<Negative>,
    #[serde(default)]
    includes: Vec<String>,
    #[serde(default)]
    flags: Vec<Flag>,
    #[serde(default)]
    locale: Vec<String>,
}

#[derive(Debug, Deserialize, Clone)]
struct Negative {
    phase: Phase,
    #[serde(alias = "type")]
    kind: Option<String>,
}

#[derive(Debug, PartialEq, Deserialize, Clone, Copy)]
#[serde(rename_all = "camelCase")]
enum Phase {
    Parse,
    Early,
    Resolution,
    Runtime,
}

#[derive(Debug, Deserialize, PartialEq, Clone, Copy)]
#[serde(rename_all = "camelCase")]
enum Flag {
    OnlyStrict,
    NoStrict,
    Module,
    Raw,
    Async,
    Generated,
    #[serde(alias = "CanBlockIsFalse")]
    CanBlockIsFalse,
    #[serde(alias = "CanBlockIsTrue")]
    CanBlockIsTrue,
}

static URL: &str = "https://github.com/tc39/test262/tarball/master";

#[test]
fn test262() -> Res<()> {
    let path = Path::new("./test262");
    if !path.exists() {
        get_repo(path)?;
    }
    let failures = walk(&path)?;
    for (msg, desc) in &failures {
        println!("----------\n{},\n{:#?}\n----------", msg, desc)
    }
    let len = failures.len();
    if len > 0 {
        panic!("{} failures in test262", len);
    }
    Ok(())
}

fn walk(path: &Path) -> Res<Vec<(String, Description)>> {
    let wd = walkdir::WalkDir::new(path).into_iter().filter_map(filter_mapper);
    let ct = wd.count();
    let pb = ProgressBar::new(ct as u64);
    let sty = ProgressStyle::default_bar()
        .template("[{elapsed_precise}] {bar:40.cyan/darkgrey} {pos:>7}/{len:7} {msg}")
        .progress_chars("█▓▒░  ");
    pb.set_style(sty);
    
    let wd: Vec<PathBuf> = walkdir::WalkDir::new(path)
                            .into_iter()
                            .filter_map(filter_mapper)
                            .collect();
    let ret = wd.par_iter()
        .progress_with(pb)
        .filter_map(test_mapper)
        .flatten()
        .collect();
    Ok(ret)
}

fn filter_mapper(e: Result<walkdir::DirEntry, walkdir::Error>) -> Option<PathBuf> {
    let entry = e.ok()?;
    let path = entry.path();
    if path.is_dir() {
        None
    } else {
        let ext = path.extension()?;
        if ext == "js" {
            let file_name = path.file_name()?;
            let file_name = file_name.to_str()?;
            if file_name.ends_with("_FIXTURE.js") {
                None
            } else {
                Some(path.to_path_buf())
            }
        } else {
            None
        }
    }
}
type Pair = (String, Description);
type PairVec = Vec<Pair>;
type OptPairVec = Option<PairVec>;
fn test_mapper(path: &PathBuf) -> OptPairVec {
    let contents = if let Ok(contents) = ::std::fs::read_to_string(path) {
        contents
    } else {
        return None;
    };
    let handler = match Test262Runner::new(&contents) {
        Ok(handler) => handler,
        Err(e) => {
            return Some(vec![(format!("Unable to create handler for {}\n\t{}", path.display(), e), Description::default())]);
        },
    };
    let mut ret = vec![];
    if let Err(e) = handler.run_strict() {
        ret.push((format!("Strict failure for {}\n{}", path.display(), e), handler.clone_desc()))
    }
    if let Err(e) = handler.run() {
        ret.push((format!("Non-strict failure for {}\n{}", path.display(), e), handler.clone_desc()))
    }
    if ret.is_empty() {
        None
    } else {
        Some(ret)
    }
}

fn report_mapper(pb: &mut ProgressBar, (ct, result): (usize, OptPairVec)) -> OptPairVec {
    pb.inc(1);
    if result.is_some() {
        pb.println(&format!("Error Count: {}", ct));
    }
    result
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