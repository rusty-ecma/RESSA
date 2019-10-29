#![cfg(feature = "test_262")]

use serde::{Deserialize, Serialize};

use flate2::read::GzDecoder;
use indicatif::{ParallelProgressIterator, ProgressBar, ProgressStyle};
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};
use ressa::Parser;
use std::{
    error::Error,
    path::{Path, PathBuf},
};

static SKIPPED_FEATURES: &[&str] = &[
    "coalesce-expression",
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
enum E262 {
    General(String),
    Success(String),
}
impl ::std::fmt::Display for E262 {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match self {
            E262::General(ref s) => write!(f, "{}", s),
            E262::Success(_) => write!(f, "Unexpected Successful Parsing"),
        }
    }
}
impl Error for E262 {}
impl E262 {
    pub fn new(s: &str) -> Self {
        E262::General(s.to_string())
    }
    pub fn from(e: impl Error) -> Self {
        Self::General(format!("{}", e))
    }
}

impl<'a> Test262Runner<'a> {
    pub fn new(js: &'a str) -> Res<Self> {
        let desc = Self::find_desc_comment(js)?;
        Ok(Self { desc, js })
    }
    fn find_desc_comment(js: &str) -> Result<Description, E262> {
        let start_idx = js
            .find("/*---")
            .ok_or_else(|| E262::new("Unable to find comment start"))?;
        let ending = js
            .get(start_idx + 5..)
            .ok_or_else(|| E262::new("Invalid start index"))?;
        let end_idx = ending
            .find("---*/")
            .ok_or_else(|| E262::new("Unable to find comment end"))?;
        let trimmed = ending
            .get(..end_idx)
            .ok_or_else(|| E262::new("Invalid end index"))?;
        let ret = if trimmed.contains('\r') {
            serde_yaml::from_str(&trimmed.replace("\r\n", "\n").replace('\r', "\n"))
                .map_err(E262::from)?
        } else {
            serde_yaml::from_str(trimmed).map_err(E262::from)?
        };
        Ok(ret)
    }
    pub fn clone_desc(&self) -> Description {
        self.desc.clone()
    }
    pub fn run_strict(&self) -> Result<(), E262> {
        if !self
            .desc
            .flags
            .iter()
            .any(|f| f == &Flag::Module || f == &Flag::NoStrict)
        {
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

    fn run_(&self, module: bool, js: &str) -> Result<(), E262> {
        let mut p = Parser::builder()
            .module(module)
            .js(js)
            .build()
            .map_err(|e| E262::new(&format!("{:?}", e)))?;
        match p.parse() {
            Ok(program) => {
                if let Some(n) = &self.desc.negative {
                    if &n.phase == &Phase::Parse {
                        Err(E262::Success(format!("```ron\n{:#?}\n```", program)))
                    } else {
                        Ok(())
                    }
                } else {
                    Ok(())
                }
            }
            Err(e) => {
                if let Some(n) = &self.desc.negative {
                    if &n.phase == &Phase::Parse {
                        Ok(())
                    } else {
                        Err(E262::from(e))
                    }
                } else {
                    Err(E262::from(e))
                }
            }
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
    #[serde(alias = "non-deterministic")]
    NonDeterministic,
}

static URL: &str = "https://github.com/tc39/test262/tarball/master";
#[derive(Debug, Serialize)]
struct TestFailure {
    pub path: PathBuf,
    pub strict: TestStatus,
    pub not_strict: TestStatus,
    pub runner: TestStatus,
    pub desc: Description,
    pub js: String,
}
#[derive(Debug, Serialize, PartialEq)]
enum TestStatus {
    Success,
    Failure(String),
    NotRun,
}
impl TestStatus {
    pub fn is_failure(&self) -> bool {
        if let TestStatus::Failure(_) = self {
            true
        } else {
            false
        }
    }
}
impl ::std::fmt::Display for TestStatus {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match self {
            TestStatus::Failure(ref s) => write!(f, "{}", s),
            TestStatus::NotRun => write!(f, "Not Run"),
            TestStatus::Success => write!(f, "Passed"),
        }
    }
}
impl TestFailure {
    pub fn is_failure(&self) -> bool {
        self.strict.is_failure() || self.not_strict.is_failure() || self.runner.is_failure()
    }
    pub fn to_markdown(&self) -> String {
        let flags: Vec<String> = self.desc.flags.iter().map(|f| format!("{:?}", f)).collect();
        let features: Vec<String> = self
            .desc
            .features
            .iter()
            .map(|f| format!("{:?}", f))
            .collect();
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
        let runner = format!("{}", self.runner);
        let strict = format!("{}", self.strict);
        let not = format!("{}", self.not_strict);
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
        format!(
            "# {id}
## Description
{desc}

### flags
{flags}

### features
{features}

### Info
```
{info}
```

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
            id = id,
            desc = desc,
            info = info,
            flags = flags.join(", "),
            features = features.join(", "),
            runner = runner,
            strict = strict,
            not = not,
            js = self.js
        )
    }
    fn get_first_id(&self, fallback: &str) -> String {
        if let Some(ref i) = self.desc.esid {
            i.to_string()
        } else if let Some(ref i) = self.desc.es5id {
            i.to_string()
        } else if let Some(ref i) = self.desc.es6id {
            i.to_string()
        } else if let Some(ref i) = self.desc.description {
            i.replace(" ", "_")
        } else {
            fallback.to_string()
        }
    }

    pub fn as_list_item(&self, path: &impl AsRef<std::path::Path>) -> String {
        let mut href = format!("{}", path.as_ref().display());
        if href.starts_with("\\\\?") {
            href = format!("\\\\{}", &href[3..]);
        }
        let mut html = format!(
            r#"<li><a class="test-name" href="{}">{}</a> - <div class="additional-info">"#,
            href,
            self.get_first_id("unknown")
        );
        if let TestStatus::Failure(_) = self.runner {
            html.push_str(r#"<span class="not-run-error">!!!</span>"#)
        }
        if let Some(ref d) = self.desc.description {
            html.push_str(&format!(r#"<div class="description">{}</div>"#, d));
        }
        if !self.desc.features.is_empty() {
            html.push_str(r#"<div class="feature title"><span>features</span></div>"#)
        }
        for feat in &self.desc.features {
            html.push_str(&format!(
                r#"<div class="feature"><span>{:?}</span></div>"#,
                feat
            ));
        }
        html.push_str("</div></li>");
        html
    }
}
static SKIP_COUNT: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);
#[test]
fn test262() -> Res<()> {
    let pb = ProgressBar::new_spinner();
    let sty = ProgressStyle::default_bar()
        .template("{bar:40.cyan/blue} {pos:>7}/{len:7} {msg}")
        .progress_chars("█▓▒░  ");
    let path = Path::new("./test262");
    if !path.exists() {
        get_repo(path)?;
    }
    pb.set_style(sty.clone());
    let (ct, paths) = get_paths(&path);
    pb.set_length(ct as u64);
    let failures: Vec<TestFailure> = paths
        .par_iter()
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
    let len = failures.len();
    let total_run = ct - SKIP_COUNT.load(std::sync::atomic::Ordering::Relaxed) as usize;
    let fail_rate = len as f32 / total_run as f32;
    let report = format!(
        "Failed {} of {} test262, fail rate of {:02.2}%",
        len,
        total_run,
        fail_rate * 100.0
    );
    if write_failures {
        println!("getting ready to write failures");
        let base_path = PathBuf::from("failures");
        let base_path = base_path.join("test262");
        if base_path.exists() {
            let _ = ::std::fs::remove_dir_all(&base_path);
        }
        let keep_writing = base_path.exists()
            || if let Ok(_) = ::std::fs::create_dir_all(&base_path) {
                println!("created directory");
                true
            } else {
                println!("failed to create directory");
                false
            };
        let base_path = ::std::fs::canonicalize(&base_path)?;
        if keep_writing {
            use std::io::Write;
            let root_path = base_path.join("test262.html");
            let mut root_file = ::std::io::BufWriter::new(::std::fs::File::create(&root_path)?);
            let head = b"<html>
            <head>
                <title>ressa test 262 failures</title>
                
                <style>
* {
  font-family: sans-serif; 
}
body {
    max-width: 800px;
    margin: auto;
}
quote {
  font-weight: bold;
  color: ghostwhite;
  background: grey;
  padding: 5px 10px;
  margin-bottom: 15px;
}
ul {
  display: flex;
  flex-flow: column;
}
li > a {
  font-weight: bold;
}
li > .additional-info {
  margin-bottom: 10px;
  border: 1px solid black;
  max-width: 800px;
}
</style>
<link rel=\"stylesheet\" href=\"http://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.15.10/styles/default.min.css\">

            </head>
            <body>";
            root_file.write_all(head)?;
            root_file.write_all(b"<h1>Failures</h1><ul>")?;
            root_file.write_all(format!("<quote>{}</quote>", report).as_bytes())?;
            for failure in &failures {
                use std::fs::File;
                use std::io::{BufWriter, Write};
                let new_path = failure.path.with_extension("html");
                if let Some(file_name) = new_path.file_name() {
                    let new_path = base_path.join(file_name);
                    root_file.write_all(failure.as_list_item(&new_path).as_bytes())?;
                    let md = failure.to_markdown();
                    let parser = pulldown_cmark::Parser::new(&md);
                    let mut f = BufWriter::new(File::create(&new_path)?);
                    f.write_all(head)?;
                    pulldown_cmark::html::write_html(&mut f, parser)?;
                    f.write_all(
                        format!(
                            "<script>{}
document.addEventListener('DOMContentLoaded', (event) => {{
  document.querySelectorAll('pre code').forEach((block) => {{
    hljs.highlightBlock(block);
  }});
}});
</script>",
                            include_str!("./hilight.js")
                        )
                        .as_bytes(),
                    )?;
                    f.write_all(b"</body></html>")?;
                }
            }
            root_file.write_all(b"</ul></body></html>")?;
            let root_str = format!("{}", root_path.display());
            if root_str.starts_with("\\\\?") {
                println!("file://{}", &root_str[3..].replace('\\', "/"))
            } else {
                println!("file:{}", root_str.replace('\\', "/"));
            }
        }
    }
    if len > 0 {
        panic!("{}", report);
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
                strict: TestStatus::NotRun,
                not_strict: TestStatus::NotRun,
                runner: TestStatus::Failure(format!("{}", e)),
                js: contents,
            });
        }
    };
    let mut ret = TestFailure {
        desc: handler.clone_desc(),
        path: path.clone(),
        strict: TestStatus::NotRun,
        not_strict: TestStatus::NotRun,
        runner: TestStatus::Success,
        js: contents.clone(),
    };
    if ret
        .desc
        .features
        .iter()
        .any(|f| SKIPPED_FEATURES.iter().any(|f2| f == f2))
    {
        SKIP_COUNT.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        return None;
    }
    if let Err(e) = handler.run_strict() {
        let s = match e {
            E262::General(ref general) => general.to_string(),
            E262::Success(ref tree) => tree.to_string(),
        };
        ret.strict = TestStatus::Failure(s);
    } else {
        ret.strict = TestStatus::Success;
    }
    if let Err(e) = handler.run() {
        let s = match e {
            E262::General(ref general) => general.to_string(),
            E262::Success(ref tree) => tree.to_string(),
        };
        ret.not_strict = TestStatus::Failure(s);
    } else {
        ret.not_strict = TestStatus::Success;
    }
    if ret.is_failure() {
        Some(ret)
    } else {
        None
    }
}

fn get_repo(path: &Path) -> Res<()> {
    let _ = ::std::fs::create_dir_all(path);
    let mut response = reqwest::get(URL)?;
    let mut buf = Vec::new();
    response.copy_to(&mut buf)?;

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
    let mut p = Parser::builder()
        .js(js)
        .build()
        .expect("faile to create parser");
    match p.parse() {
        Err(e) => println!("{}", e),
        _ => panic!("Unexpected successful parse of yield as identifier"),
    }
    let js = "'use strict'
var \\u0079ield = 123;";
    let mut p = Parser::builder()
        .js(js)
        .build()
        .expect("faile to create parser");
    match p.parse() {
        Err(e) => println!("{}", e),
        _ => panic!("Unexpected successful parse of escaped yield as identifier"),
    }
}
