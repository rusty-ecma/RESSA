#![cfg(test)]
use flate2::read::GzDecoder;
use rayon::prelude::*;
use ressa::{Error, Parser};
use std::path::Path;

static mut COUNT: usize = 0;
static mut FAILURES: usize = 0;

#[test]
fn moz_central() {
    let moz_central_path = Path::new("./moz-central");
    if !moz_central_path.exists() {
        get_moz_central_test_files(&moz_central_path);
    }
    walk(&moz_central_path);
    unsafe {
        if FAILURES > 0 {
            panic!("Some spider_monkey tests failed to parse");
        }
    }
}

fn walk(path: &Path) {
    let files = path
        .read_dir()
        .unwrap()
        .map(|e| e.unwrap().path())
        .collect::<Vec<_>>();

    files.par_iter().for_each(|path| {
        unsafe {
            if COUNT > 0 && COUNT % 100 == 0 {
                println!("Status Update {}/{}", FAILURES, COUNT);
            }
        }
        if path.is_file() {
            if let Some(ext) = path.extension() {
                if ext == "js" {
                    if let Err(e) = run(path) {
                        let loc = match &e {
                            Error::InvalidGetterParams(ref pos)
                            | Error::InvalidSetterParams(ref pos)
                            | Error::NonStrictFeatureInStrictContext(ref pos, _)
                            | Error::OperationError(ref pos, _)
                            | Error::Redecl(ref pos, _)
                            | Error::UnableToReinterpret(ref pos, _, _)
                            | Error::UnexpectedToken(ref pos, _) => {
                                format!("{}:{}:{}", path.display(), pos.line, pos.column)
                            }
                            _ => format!("{}", path.display()),
                        };
                        eprintln!("Parse Failure {}\n\t{}", e, loc);
                        if let Ok(op) = ::std::process::Command::new("./node_modules/.bin/esparse")
                            .arg(path)
                            .output()
                        {
                            if !op.status.success() {
                                eprintln!("possible new whitelist item:\n\t{}", path.display());
                            }
                        }
                        unsafe { FAILURES += 1 }
                    }
                }
            }
        } else {
            walk(&path)
        }
    });
}

fn run(file: &Path) -> Result<(), Error> {
    unsafe { COUNT += 1 }
    if file.ends_with("gc/bug-1459860.js")
        || file.ends_with("basic/testBug756918.js")
        || file.ends_with("basic/bug738841.js")
        || file.ends_with("ion/bug1331405.js")
        || file.ends_with("basic/testThatGenExpsActuallyDecompile.js")
        || file.ends_with("jaeger/bug672122.js")
        || file.ends_with("gc/bug-924690.js")
        || file.ends_with("auto-regress/bug732719.js")
        || file.ends_with("auto-regress/bug740509.js")
        || file.ends_with("auto-regress/bug521279.js")
        || file.ends_with("auto-regress/bug701248.js")
        || file.ends_with("auto-regress/bug1390082-1.js")
        || file.ends_with("auto-regress/bug680797.js")
        || file.ends_with("auto-regress/bug521163.js")
        || file.ends_with("auto-regress/bug1448582-5.js")
        || file.ends_with("tests/backup-point-bug1315634.js")
        || file.ends_with("auto-regress/bug650574.js")
        || file.ends_with("baseline/setcall.js")
        //FIXME binint support
        // // || file.ends_with("ion/bug1526840.js")
        // || file.ends_with("ion/bug1528818.js")
        // || file.ends_with("cacheir/bug1526872.js")
        // Anonymous Object with getter/setter number properties
        || file.ends_with("jit-test/tests/arrays/sort-update-types.js")
        || file.ends_with("jit-test/tests/collections/WeakMap-constructor-arraylike-exception.js")
        || file.ends_with("jit-test/tests/baseline/getter_setter.js")
    {
        return Ok(());
    }
    let contents = ::std::fs::read_to_string(file)?;
    if contents.starts_with("// |jit-test| error: SyntaxError")
        || contents.starts_with("|")
        || contents.starts_with("// |jit-test| error:SyntaxError")
    {
        return Ok(());
    }
    if contents.starts_with("// |jit-test| module") {
        return Ok(()); //these all contain restricted word import as an ident
    }
    for part in Parser::new(&contents)? {
        let _part = part?;
    }
    Ok(())
}

fn get_moz_central_test_files(path: &Path) {
    let mut response = reqwest::get(
        "https://hg.mozilla.org/mozilla-central/archive/tip.tar.gz/js/src/jit-test/tests/",
    )
    .expect("Failed to get zip of moz-central");
    let mut buf = Vec::new();
    response
        .copy_to(&mut buf)
        .expect("failed to copy to BzDecoder");
    let gz = GzDecoder::new(buf.as_slice());
    let mut t = tar::Archive::new(gz);
    t.unpack(path).expect("Failed to unpack gz");
}
