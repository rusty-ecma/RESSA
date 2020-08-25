#![cfg(feature = "test_262")]

use indicatif::{ParallelProgressIterator, ProgressBar, ProgressStyle};
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};
use ressa::Parser;
use serde::{Deserialize, Serialize};
use std::fs::File;
use std::io::BufWriter;
use std::{
    error::Error,
    path::{Path, PathBuf},
};
static INCLUDED_FEATURES: &[&str] = &[
    "IsHTMLDDA",
    "String.prototype.endsWith",
    "Array.prototype.flat",
    "template",
    "super",
    "for-of",
    "Float64Array",
    "Reflect.construct",
    "Symbol.toStringTag",
    "Intl.DateTimeFormat-formatRange",
    "Symbol.split",
    "DataView.prototype.getFloat32",
    "globalThis",
    "DataView.prototype.getUint32",
    "Symbol.prototype.description",
    "WeakRef",
    "Int32Array",
    "Uint8ClampedArray",
    "String.fromCodePoint",
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
    "Symbol.hasInstance",
    "DataView.prototype.getInt16",
    "string-trimming",
    "optional-catch-binding",
    "FinalizationGroup",
    "Float32Array",
    "Reflect.set",
    "WeakSet",
    "tail-call-optimization",
    "String.prototype.matchAll",
    "Symbol.unscopables",
    "DataView.prototype.getInt32",
    "Symbol.search",
    "Intl.NumberFormat-unified",
    "Symbol.species",
    "Object.fromEntries",
    "cross-realm",
    "DataView.prototype.getFloat64",
    "Symbol.isConcatSpreadable",
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
    "Intl.ListFormat",
    "Intl.DateTimeFormat-quarter",
    "Reflect",
    "Symbol.match",
    "Intl.DateTimeFormat-dayPeriod",
    "Object.is",
    "Promise.allSettled",
    "Symbol.replace",
    "well-formed-json-stringify",
    "Intl.Locale",
    "ArrayBuffer",
    "Set",
    "Intl.Segmenter",
    "Promise.prototype.finally",
    "Int8Array",
    "WeakMap",
    "Array.prototype.flatMap",
    "DataView",
    "Atomics",
    "Symbol.matchAll",
    "String.prototype.includes",
    "Map",
    // "coalesce-expression",
    // "regexp-named-groups",
    // "Symbol.iterator",
    // "object-rest",
    // "class-fields-public",
    // "class-static-methods-private",
    // "Symbol.asyncIterator",
    // "destructuring-binding",
    // "BigInt",
    // "arrow-function",
    // "class-methods-private",
    // "dynamic-import",
    // "let",
    // "Symbol",
    // "import.meta",
    // "class-static-fields-public",
    // "class",
    // "export-star-as-namespace-from-module",
    // "Proxy",
    // "top-level-await",
    // "numeric-separator-literal",
    // "object-spread",
    // "default-parameters",
    // "optional-chaining",
    // "Symbol.toPrimitive",
    // "async-iteration",
    // "computed-property-names",
    // "regexp-unicode-property-escapes",
    // "class-fields-private",
    // "generators",
    // "async-functions",
    // "new.target",
    // "hashbang",
    // "const",
    // "json-superset",
    // "class-static-fields-private",
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
            E262::General(ref s) => write!(f, "General Error:\n{}", s),
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
            self.run_script(&format!("'use strict';\n{}", self.js))?;
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
            .map_err(|e| E262::new(&format!("Error constructing parser{:?}", e)))?;
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

#[derive(Debug, Serialize, Clone)]
struct TestFailure {
    pub path: PathBuf,
    pub strict: TestStatus,
    pub not_strict: TestStatus,
    pub runner: TestStatus,
    pub desc: Description,
    pub js: String,
}
#[derive(Debug, Serialize, PartialEq, Clone)]
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
            js = self.js,
        )
    }
    pub fn get_first_id(&self, fallback: &str) -> String {
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
        let mut li_class = "failure-container".to_string();
        let neg = if let Some(Negative {
            phase: Phase::Parse,
            ..
        }) = &self.desc.negative
        {
            li_class.push_str(" negative");
            true
        } else {
            li_class.push_str(" positive");
            false
        };
        let mut html = format!(
            r#"<li class="{}">
<a class="test-name" href="{}">{}</a> - <div class="additional-info">"#,
            li_class,
            href,
            self.path.display()
        );
        if let TestStatus::Failure(_) = self.runner {
            html.push_str(r#"<span class="not-run-error">!!!</span>"#)
        }
        if !neg {
            let mut error_text = r#"<div class="error-text">"#.to_string();
            if let TestStatus::Failure(ref msg) = self.strict {
                error_text.push_str(&format!(r#"<span class="strict">{}</span>"#, msg));
            }
            if let TestStatus::Failure(ref msg) = self.not_strict {
                error_text.push_str(&format!(r#"<span class="not-strict">{}</span>"#, msg));
            }
            error_text.push_str("</div>");
            html.push_str(&error_text);
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
        panic!("unable to run this test without the test262 test suite see CONTRIBUTING.md for more information");
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
        let mut collected = std::collections::HashMap::new();
        let mut feature_count = std::collections::HashMap::new();
        for failure in failures {
            for feat in &failure.desc.features {
                *feature_count.entry(feat.clone()).or_insert(0) += 1;
            }
            let id = failure.get_first_id("unknown");
            let sames = collected.entry(id).or_insert(vec![]);
            sames.push(failure.clone());
        }
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
            let root_path = base_path.join("index.html");
            let mut root_file = ::std::io::BufWriter::new(::std::fs::File::create(&root_path)?);
            let head = format!("<html>
            <head>
                <title>ressa test 262 failures</title>
                
                <style>{}</style>
<link rel=\"stylesheet\" href=\"http://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.15.10/styles/default.min.css\">

            </head>
            <body>", include_str!("./style.css"));
            root_file.write_all(head.as_bytes())?;
            root_file.write_all(format!("<h1>Failures</h1><quote>{}</quote><button id=\"remove-neg-button\">positive only</button><ul id=\"feature-counts-list\">", report).as_bytes())?;

            for (name, ct) in feature_count {
                root_file.write_all(format!("<li class=\"feature-count-entry\"><span class=\"feature-name\">{}</span><span class=\"feature-count\">{}</span></li>", name, ct).as_bytes())?;
            }
            root_file.write_all(b"</ul><ul>").unwrap();
            let mut sorted = collected
                .into_iter()
                .collect::<Vec<(String, Vec<TestFailure>)>>();
            sorted.sort_by(|(_, lhs), (_, rhs)| rhs.len().cmp(&lhs.len()));
            for (id, list) in sorted {
                root_file.write_all(
                    format!(
                        "<li class=\"single-error-list\"><h2>{} ({})</h2><ol>",
                        id,
                        list.len()
                    )
                    .as_bytes(),
                )?;
                for (i, fail) in list.iter().enumerate() {
                    if let Some(file_name) = fail.path.file_stem() {
                        let file_name = format!("{}{}.html", file_name.to_str().unwrap(), i);
                        let new_path = base_path.join(&file_name);
                        root_file.write(fail.as_list_item(&file_name).as_bytes())?;
                        let md = fail.to_markdown();
                        let parser = pulldown_cmark::Parser::new(&md);
                        let mut f = BufWriter::new(File::create(&new_path)?);
                        f.write_all(head.as_bytes())?;
                        pulldown_cmark::html::write_html(&mut f, parser)?;
                        f.write_all(br#"<script src="//cdn.jsdelivr.net/gh/highlightjs/cdn-release@9.16.2/build/highlight.min.js"></script>"#)?;
                        f.write_all(
                            format!("<script>{}</script>", include_str!("./addLineNumbers.js"))
                                .as_bytes(),
                        )?;
                        f.write_all(b"</body></html>")?;
                    }
                }
                root_file.write_all(b"</ol></li>")?;
            }
            root_file.write_all(
                format!(
                    "</ul><script>{}</script></body></html>",
                    include_str!("./removeNegative.js")
                )
                .as_bytes(),
            )?;
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
    let ct = walkdir::WalkDir::new(path)
        .into_iter()
        .filter_map(filter_mapper)
        .count();
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
    if !ret
        .desc
        .features
        .iter()
        .all(|f| INCLUDED_FEATURES.iter().any(|f2| f == f2))
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

#[test]
fn yam() {
    let yaml = "es6id: asdf
flags: [onlyStrict]
info: |
    data
    data
    data";
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

#[cfg(feature = "test_262_parser")]
mod parser {

    #[test]
    fn test_262_parser() {
        let path = Path::new("./test262-parser");
        if !path.exists() {
            panic!("Unable to run this test without the test262-parser test suite, see CONTRIBUTING.md for more information");
        }
        let (total, paths) = get_paths(&path);
        let (early, fail, pass, pass_explicit) = categorize_paths(&paths);
        let earlies = run_category(&early, true);
        let fails = run_category(&fail, true);
        let passes = run_category(&pass, false);
        let explicits = run_category(&pass_explicit, false);
        if !report_errors(total, &earlies, &fails, &passes, &explicits) {
            panic!("Error in 262 parser tests");
        }
    }
    fn report_errors(
        total: usize,
        earlies: &[String],
        fails: &[String],
        passes: &[String],
        explicits: &[String],
    ) -> bool {
        if earlies.is_empty() || fails.is_empty() || passes.is_empty() || explicits.is_empty() {
            eprintln!("passed 100% of {} test", total);
            return true;
        }
        let mut fail_ct = report_cat_errors("earlies", earlies);
        fail_ct += report_cat_errors("fails", fails);
        fail_ct += report_cat_errors("passes", passes);
        fail_ct += report_cat_errors("explicits", explicits);
        eprintln!(
            "failed {} of {} ({:02.2}%)",
            fail_ct,
            total,
            (fail_ct as f32 / total as f32) * 100f32
        );
        return false;
    }

    fn report_cat_errors(name: &str, cat: &[String]) -> usize {
        if cat.is_empty() {
            return 0;
        }
        eprintln!("----------\n{}\n----------\n", name);
        for (i, msg) in cat.iter().enumerate() {
            eprintln!("{}: {}", i, msg);
        }
        eprintln!("\n----------\n");
        cat.len()
    }

    fn run_category(paths: &[PathBuf], should_fail: bool) -> Vec<String> {
        let mut ret = Vec::new();
        for path in paths.iter() {
            let js = std::fs::read_to_string(&path)
                .expect(&format!("failed to read {}", path.display()));
            let is_mod = format!("{}", path.display()).contains("module");
            let mut p = match Parser::builder().js(&js).module(is_mod).build() {
                Ok(p) => p,
                Err(e) => {
                    if should_fail {
                        ret.push(format!("{}\n\n{}", path.display(), e));
                    }
                    continue;
                }
            };
            match p.parse() {
                Ok(p) => {
                    if should_fail {
                        ret.push(format!("{}\n{:#?}", path.display(), p));
                    }
                }
                Err(e) => {
                    if !should_fail {
                        ret.push(format!("{}\n{}", path.display(), e))
                    }
                }
            }
        }
        ret
    }

    fn categorize_paths(
        paths: &[PathBuf],
    ) -> (Vec<PathBuf>, Vec<PathBuf>, Vec<PathBuf>, Vec<PathBuf>) {
        paths
            .into_iter()
            .fold((vec![], vec![], vec![], vec![]), |mut acc, path| {
                if let Some(parent) = path.parent() {
                    let parent_str = format!("{}", parent.display());
                    if parent_str.ends_with("early") {
                        acc.0.push(path.to_path_buf());
                    } else if parent_str.ends_with("fail") {
                        acc.1.push(path.to_path_buf());
                    } else if parent_str.ends_with("pass") {
                        acc.2.push(path.to_path_buf());
                    } else if parent_str.ends_with("pass-explicit") {
                        acc.3.push(path.to_path_buf());
                    }
                }
                acc
            })
    }
}
