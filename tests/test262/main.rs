#![cfg(feature = "test_262")]
use serde::{
    Deserialize,
    Serialize,
};

use ressa::Parser;
use std::{
    error::Error,
    path::{Path, PathBuf},
};
use flate2::read::GzDecoder;
use indicatif::{
    ParallelProgressIterator, 
    ProgressIterator,
    ProgressBar, 
    ProgressStyle, 
    ProgressDrawTarget
};
use rayon::iter::{ParallelIterator, IntoParallelRefIterator};

static SKIPPED_FEATURES: &[&str] = &[
    "IsHTMLDDA",
    "Reflect.construct",
    "String.prototype.endsWith",
    "Array.prototype.flat",
    "template",
    "Symbol.toStringTag",
    "Intl.DateTimeFormat-formatRange",
    "super",
    "Symbol.split",
    "regexp-named-groups",
    "Symbol.iterator",
    "DataView.prototype.getFloat32",
    "globalThis",
    "object-rest",
    "class-fields-public",
    "DataView.prototype.getUint32",
    "Symbol.prototype.description",
    "WeakRef",
    "Int32Array",
    "Uint8ClampedArray",
    "String.fromCodePoint",
    "class-static-methods-private",
    "SharedArrayBuffer",
    "Intl.DateTimeFormat-fractionalSecondDigits",
    "Uint8Array",
    "rest-parameters",
    "DataView.prototype.getInt8",
    "Intl.DateTimeFormat-datetimestyle",
    "DataView.prototype.setUint8",
    "String.prototype.trimStart",
    "caller",
    "Uint16Array",
    "Symbol.asyncIterator",
    "destructuring-binding",
    "BigInt",
    "Symbol.hasInstance",
    "DataView.prototype.getInt16",
    "arrow-function",
    "string-trimming",
    "class-methods-private",
    "optional-catch-binding",
    "dynamic-import",
    "let",
    "FinalizationGroup",
    "Symbol",
    "Float32Array",
    "import.meta",
    "Reflect.set",
    "WeakSet",
    "tail-call-optimization",
    "class",
    "String.prototype.matchAll",
    "export-star-as-namespace-from-module",
    "Proxy",
    "top-level-await",
    "Symbol.unscopables",
    "DataView.prototype.getInt32",
    "Symbol.search",
    "Intl.NumberFormat-unified",
    "Symbol.species",
    "numeric-separator-literal",
    "Object.fromEntries",
    "cross-realm",
    "object-spread",
    "default-parameters",
    "DataView.prototype.getFloat64",
    "optional-chaining",
    "Symbol.isConcatSpreadable",
    "Symbol.toPrimitive",
    "String.prototype.trimEnd",
    "Array.prototype.values",
    "regexp-lookbehind",
    "TypedArray",
    "destructuring-assignment",
    "Reflect.setPrototypeOf",
    "regexp-dotall",
    "u180e",
    "Intl.RelativeTimeFormat",
    "proxy-missing-checks",
    "DataView.prototype.getUint16",
    "async-iteration",
    "Intl.ListFormat",
    "Intl.DateTimeFormat-quarter",
    "computed-property-names",
    "regexp-unicode-property-escapes",
    "Reflect",
    "class-fields-private",
    "Symbol.match",
    "Intl.DateTimeFormat-dayPeriod",
    "generators",
    "async-functions",
    "Object.is",
    "Promise.allSettled",
    "Symbol.replace",
    "well-formed-json-stringify",
    "Intl.Locale",
    "class-static-fields-public",
    "ArrayBuffer",
    "Set",
    "new.target",
    "Intl.Segmenter",
    "Promise.prototype.finally",
    "hashbang",
    "Int8Array",
    "const",
    "WeakMap",
    "Array.prototype.flatMap",
    "DataView",
    "for-of",
    "Float64Array",
    "Atomics",
    "Symbol.matchAll",
    "json-superset",
    "String.prototype.includes",
    "class-static-fields-private",
    "Map",
];
type Res<T> = Result<T, Box<dyn Error>>;

struct Test262Runner<'a> {
    desc: Description,
    js: &'a str,
}

#[derive(Debug, Clone)]
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
    pub fn from(e: impl Error) -> Self {
        Self(format!("{}", e))
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
    fn find_desc_comment(js: &str) -> Result<Description, E262> {
        let start_idx = js.find("/*---").ok_or_else(|| E262::new("Unable to find comment start"))?;
        let ending = js.get(start_idx+5..).ok_or_else(|| E262::new("Invalid start index"))?;
        let end_idx = ending.find("---*/").ok_or_else(|| E262::new("Unable to find comment end"))?;
        let trimmed = ending.get(..end_idx).ok_or_else(|| E262::new("Invalid end index"))?;
        let ret = if trimmed.contains('\r') {
            serde_yaml::from_str(&trimmed
                .replace("\r\n", "\n")
                .replace('\r', "\n"))
                .map_err(E262::from)?
        } else {
            serde_yaml::from_str(trimmed)
                    .map_err(E262::from)?
        };
        Ok(ret)
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
        let mut p = Parser::builder()
                        .module(module)
                        .js(js)
                        .build()
                        .map_err(|e| E262::new(&format!("{:?}", e)))?;
        let first_err = p.find(|r| r.is_err());
        match first_err {
            None => if let Some(n) = &self.desc.negative {
                if &n.phase == &Phase::Parse {
                    Err(E262::new("Unexpected successful parsing"))
                } else {
                    Ok(())
                }
            } else {
                Ok(())
            },
            Some(Err(e)) => {
                if let Some(n) = &self.desc.negative {
                    if &n.phase == &Phase::Parse {
                        Ok(())
                    } else {
                        Err(E262::new(&format!("{:?}", e)))
                    }
                } else {
                        Err(E262::new(&format!("{:?}", e)))
                }
            },
            Some(Ok(_)) => Ok(())
        }
    }
}


#[derive(Debug, Deserialize, Clone, Default, Serialize)]
struct Description {
    id: Option<String>,
    esid: Option<String>,
    es5id: Option<String>,
    es6id: Option<String>,
    info: Option<String>,
    description: Option<String>,
    negative: Option<Negative>,
    #[serde(default)]
    includes: Vec<String>,
    #[serde(default)]
    flags: Vec<Flag>,
    #[serde(default)]
    locale: Vec<String>,
    #[serde(default)]
    features: Vec<String>,
}

#[derive(Debug, Deserialize, Clone, Serialize)]
struct Negative {
    phase: Phase,
    #[serde(alias = "type")]
    kind: Option<String>,
}

#[derive(Debug, PartialEq, Deserialize, Clone, Copy, Serialize)]
#[serde(rename_all = "camelCase")]
enum Phase {
    Parse,
    Early,
    Resolution,
    Runtime,
}

#[derive(Debug, Deserialize, PartialEq, Clone, Copy, Serialize)]
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
#[derive(Debug, Serialize)]
struct TestFailure {
    pub path: PathBuf,
    pub strict: Option<String>,
    pub not_strict: Option<String>,
    pub runner: Option<String>,
    pub desc: Description,
    pub js: String,
}

impl TestFailure {
    pub fn is_failure(&self) -> bool {
        self.strict.is_some() 
            || self.not_strict.is_some()
            || self.runner.is_some()
    }
    pub fn to_markdown(&self) -> String {
        let flags: Vec<String> = self.desc.flags.iter().map(|f| format!("{:?}", f)).collect();
        let features: Vec<String> = self.desc.features.iter().map(|f| format!("{:?}", f)).collect();
        let desc = if let Some(ref inner) = self.desc.description {
            inner.to_string()
        } else {
            "__not provided__".to_string()
        };
        let info = if let Some(ref inner) = self.desc.info {
            inner.to_string()
        } else {
            "__not provided__".to_string()
        };
        let (ran, runner) = if let Some(ref inner) = self.runner {
            (false, format!("{}", inner))
        } else {
            (true, "passed".to_string())
        };
        let (strict, not) = if !ran {
            ("not run".to_string(), "not run".to_string())
        } else {
            let strict = if let Some(ref inner) = self.strict {
                inner.to_string()
            } else {
                "passed".to_string()
            };
            let not = if let Some(ref inner) = self.not_strict {
                inner.to_string()
            } else {
                "passed".to_string()
            };
            (strict, not)
        };
        let mut id = if let Some(ref id) = self.desc.id {
            format!("{} (id) ", id)
        } else {
            String::new()
        };
        if let Some(ref i) = self.desc.esid {
            id.push_str(&format!("{} (esid) ", i));
        }
        if let Some(ref i) = self.desc.es5id {
            id.push_str(&format!("{} (es5id) ", i));
        }
        if let Some(ref i) = self.desc.es6id {
            id.push_str(&format!("{} (es6id) ", i));
        }
        format!("# {id}
## Description
{desc}

### flags
{flags}

### features
{features}

### Info
{info}

## Results
### runner
{runner}

### strict
{strict}

### not_strict
{not}

```js
{js}
```
", 
id=id,
desc=desc, 
info=info, 
flags=flags.join(", "),
features=features.join(", "),
runner=runner,
strict=strict,
not=not,
js=self.js
)
    }
}
#[test]
fn test262() -> Res<()> {
    let pb = ProgressBar::new_spinner();
    let sty = ProgressStyle::default_bar()
        .template("{bar:40.cyan/blue} {pos:>7}/{len:7} {msg}")
        .progress_chars("█▓▒░  ");
    // ::std::panic::set_hook(Box::new(|_| {}));
    let path = Path::new("./test262");
    if !path.exists() {
        get_repo(path)?;
    }
    pb.set_style(sty.clone());
    let (ct, paths) = get_paths(&path);
    pb.set_length(ct as u64);
    let failures: Vec<TestFailure> = paths.par_iter()
        .progress_with(pb)
        .filter_map(test_mapper)
        .collect();

    let write_failures = if let Ok(env_var) = ::std::env::var("RESSA_WRITE_FAILURES") {
        println!("RESSA_WRITE_FAILURES={}", env_var);
        env_var != "0"
    } else {
        println!("RESSA_WRITE_FAILURES does not exist");
        false
    };
    let write_failures = true;
    let len = failures.len();
    
    if write_failures {
        println!("getting ready to write failures");
        let base_path = PathBuf::from("failures");
        let base_path = base_path.join("test262");
        if base_path.exists() {
            let _ = ::std::fs::remove_dir_all(&base_path);
        }
        let keep_writing = base_path.exists() || if let Ok(_) = ::std::fs::create_dir_all(&base_path) {
            println!("created directory");
            true
        } else {
            println!("failed to create directory");
            false
        };
        if keep_writing {
            for failure in &failures {
                let new_path = failure.path.with_extension("md");
                if let Some(file_name) = new_path.file_name() {
                    let new_path = base_path.join(file_name);
                    // we really don't care if this fails
                    let _ = ::std::fs::write(&new_path, failure.to_markdown());
                }
            }
        }
    }
    println!("found {} failures", len);
    if len > 0 {
        panic!("{} failures in test262", len);
    }
    Ok(())
}

fn get_paths(path: &Path) -> (usize, Vec<PathBuf>) {
    let wd = walkdir::WalkDir::new(path)
        .into_iter()
        .filter_map(filter_mapper);
    let ct = wd.count();
    let wd = walkdir::WalkDir::new(path)
                            .into_iter()
                            .filter_map(filter_mapper)
                            .collect();
    (ct, wd)
}

fn walk(path: &Path) -> Res<Vec<TestFailure>> {
    let wd = walkdir::WalkDir::new(path)
        .into_iter()
        .filter_map(filter_mapper);
    let ct = wd.count();
    
    let pb = ProgressBar::new(ct as u64);
    let sty = ProgressStyle::default_bar()
        .template("{bar:40.cyan/blue} {pos:>7}/{len:7} {percent} {per_sec}")
        .progress_chars("█▓▒░  ");
    pb.set_style(sty);
    
    pb.set_draw_target(ProgressDrawTarget::stdout());
    let wd: Vec<PathBuf> = walkdir::WalkDir::new(path)
                            .into_iter()
                            .filter_map(filter_mapper)
                            .collect();
    let ret = wd.iter()
        .progress_with(pb)
        .filter_map(test_mapper)
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

fn test_mapper(path: &PathBuf) -> Option<TestFailure> {
    let contents = if let Ok(contents) = ::std::fs::read_to_string(path) {
        contents
    } else {
        return None;
    };
    let handler = match Test262Runner::new(&contents) {
        Ok(handler) => handler,
        Err(e) => {
            return Some(TestFailure {
                desc: Description::default(),
                path: path.clone(),
                strict: None,
                not_strict: None,
                runner: Some(format!("{}", e)),
                js: contents
            });
        },
    };
    let mut ret = TestFailure {
        desc: handler.clone_desc(),
        path: path.clone(),
        strict: None,
        not_strict: None,
        runner: None,
        js: contents.clone(),
    };
    if ret.desc.features.iter().any(|f| SKIPPED_FEATURES.iter().any(|f2| f == f2)) {
        return None;
    }
    if let Err(e) = handler.run_strict() {
        ret.strict = Some(format!("{}", e));
    }
    if let Err(e) = handler.run() {
        ret.not_strict = Some(format!("{}", e));
    }
    if ret.is_failure() {
        Some(ret)
    } else {
        None
    }
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

#[test]
fn yeild_in_strict_mode() {
    let _ = env_logger::try_init();
    let js = "'use strict'
var yield = 1;";
    let mut p = Parser::builder().js(js).build().expect("faile to create parser"); 
    match p.parse() {
        Err(e) => println!("{}", e),
        _ => panic!("Unexpected successful parse of yield as identifier"),
    }
    let js = "'use strict'
var \\u0079ield = 123;";
    let mut p = Parser::builder().js(js).build().expect("faile to create parser"); 
    match p.parse() {
        Err(e) => println!("{}", e),
        _ => panic!("Unexpected successful parse of escaped yield as identifier"),
    }
}