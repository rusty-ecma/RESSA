#![cfg(test)]
use ressa::{Builder, Error};
use std::path::Path;
use walkdir::WalkDir;
#[cfg(windows)]
static ESPARSE: &str = "node_modules/.bin/esparse.cmd";
#[cfg(not(windows))]
static ESPARSE: &str = "node_modules/.bin/esparse";

#[test]
fn moz_central() {
    let moz_central_path = Path::new("./moz-central");
    if !moz_central_path.exists() {
        panic!("Unable to run this test without the files in ./moz-central see CONTRIBUTING.md for more information");
    }
    let failures = walk(&moz_central_path);
    let fail_count = failures
        .iter()
        .filter(|(_, white_list)| !white_list)
        .count();
    for (msg, _) in failures.iter().filter(|(_, white_list)| *white_list) {
        println!("W-{}", msg);
    }
    if fail_count > 0 {
        eprintln!("----------");
        eprintln!("FAILURES");
        eprintln!("----------");
        for (msg, _) in failures.iter().filter(|(_, white_list)| !white_list) {
            eprintln!("{}", msg);
        }
        panic!("Failed to parse {} moz_central files", fail_count);
    }
}

fn walk(path: &Path) -> Vec<(String, bool)> {
    let mut ret = Vec::new();
    for file_path in WalkDir::new(path).into_iter() {
        let file_path = file_path.expect(&format!("Error for file {}", path.display()));
        if file_path.path().is_file() {
            let test = if let Some(ext) = file_path.path().extension() {
                ext == "js"
            } else {
                false
            };
            if !test {
                continue;
            }
            if let Err(e) = run(&file_path.path()) {
                let loc = match &e {
                    Error::UnexpectedToken(ref pos, _)
                    | Error::UnableToReinterpret(ref pos, _, _)
                    | Error::Redecl(ref pos, _)
                    | Error::OperationError(ref pos, _)
                    | Error::InvalidGetterParams(ref pos)
                    | Error::InvalidSetterParams(ref pos)
                    | Error::NonStrictFeatureInStrictContext(ref pos, _)
                    | Error::InvalidImportError(ref pos)
                    | Error::InvalidExportError(ref pos)
                    | Error::InvalidUseOfContextualKeyword(ref pos, _)
                    | Error::TryWithNoCatchOrFinally(ref pos)
                    | Error::InvalidCatchArg(ref pos)
                    | Error::ThrowWithNoArg(ref pos)
                    | Error::UnknownOptionalLabel(ref pos, _, _)
                    | Error::InvalidOptionalLabel(ref pos)
                    | Error::UseOfModuleFeatureOutsideOfModule(ref pos, _) => format!(
                        "{}:{}:{}",
                        &file_path.path().to_str().unwrap(),
                        pos.line,
                        pos.column
                    ),
                    _ => format!("{}", file_path.path().display()),
                };
                let mut msg = format!("Parse Failure {}\n\t\"{}\"", e, loc);
                let white_list = match ::std::process::Command::new(ESPARSE)
                    .arg(file_path.path())
                    .output()
                {
                    Ok(op) => {
                        if !op.status.success() {
                            let mut msg2 = format!(
                                "esparse failure: \nstderr: {:?}",
                                String::from_utf8_lossy(&op.stderr)
                            );
                            msg2.push_str(&format!(
                                "stdout: {:?}",
                                String::from_utf8_lossy(&op.stdout)
                            ));
                            Some(msg2)
                        } else {
                            let name = file_path.file_name();
                            let failures_path = Path::new("failures");
                            if !failures_path.exists() {
                                std::fs::create_dir_all(&failures_path)
                                    .expect("Failed to create out path for failures");
                            }
                            let mut out_path = failures_path.join(name);
                            out_path.set_extension("json");
                            ::std::fs::write(
                                &out_path,
                                String::from_utf8_lossy(&op.stdout).to_string(),
                            )
                            .expect(&format!("failed to wirte {}", out_path.display()));
                            None
                        }
                    }
                    Err(e) => {
                        panic!("failed to exec esparse {}", e);
                    }
                };
                let white_list = if let Some(msg2) = white_list {
                    msg.push_str(&format!("\n{}", msg2));
                    true
                } else {
                    false
                };
                ret.push((msg, white_list));
            }
        }
    }
    ret
}

fn run(file: &Path) -> Result<(), Error> {
    let mut contents = ::std::fs::read_to_string(file)?;
    if contents.starts_with("|") {
        // bad comment
        contents = format!("//{}", contents);
    }
    if let Some(first) = contents.lines().next() {
        if first.contains("SyntaxError") {
            return Ok(());
        }
        if first.contains("error:InternalError")
        /*--> in last line*/
        {
            contents = contents.replace("-->", "//");
        }
    }
    let module = contents.starts_with("// |jit-test| module");
    let b = Builder::new();
    let parser = b.js(&contents).module(module).build()?;
    for part in parser {
        let _part = part?;
    }
    Ok(())
}
