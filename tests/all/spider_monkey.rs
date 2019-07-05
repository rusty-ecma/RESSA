#![cfg(test)]
use flate2::read::GzDecoder;
use ressa::{Error, Parser};
use std::path::Path;

#[test]
fn moz_central() {
    let moz_central_path = Path::new("./moz-central");
    if !moz_central_path.exists() {
        get_moz_central_test_files(&moz_central_path);
    }
    let failures = walk(&moz_central_path);
    let fail_count = failures
        .iter()
        .filter(|(_, white_list)| !white_list)
        .count();
    for (msg, white_list) in failures {
        let prefix = if white_list { "W- " } else { "" };
        eprintln!("{}{}", prefix, msg);
    }
    if fail_count > 0 {
        panic!("Failed to parse {} moz_central files", fail_count);
    }
}

fn walk(path: &Path) -> Vec<(String, bool)> {
    let mut ret = Vec::new();
    for file_path in path.read_dir().unwrap().map(|e| e.unwrap().path()) {
        if file_path.is_file() {
            let test = if let Some(ext) = file_path.extension() {
                ext == "js"
            } else {
                false
            };
            if !test {
                continue;
            }
            if let Err(e) = run(&file_path) {
                let loc = match &e {
                    Error::InvalidGetterParams(ref pos)
                    | Error::InvalidSetterParams(ref pos)
                    | Error::NonStrictFeatureInStrictContext(ref pos, _)
                    | Error::OperationError(ref pos, _)
                    | Error::Redecl(ref pos, _)
                    | Error::UnableToReinterpret(ref pos, _, _)
                    | Error::UnexpectedToken(ref pos, _) => format!(
                        "{}:{}:{}",
                        &file_path.to_str().unwrap(),
                        pos.line,
                        pos.column
                    ),
                    _ => format!("{}", file_path.display()),
                };
                let mut msg = format!("Parse Failure {}\n\t{}", e, loc);
                let white_list = match ::std::process::Command::new(
                    "C:\\Users\\rmasen\\projects\\ressa\\node_modules\\.bin\\esparse.cmd",
                )
                .arg(file_path.clone())
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
                            if let Some(name) = file_path.file_name() {
                                let mut out_path =
                                    Path::new("C:\\Users\\rmasen\\projects\\ressa\\failures")
                                        .join(name);
                                out_path.set_extension("json");
                                ::std::fs::write(
                                    &out_path,
                                    String::from_utf8_lossy(&op.stdout).to_string(),
                                )
                                .unwrap();
                            }
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
        } else if file_path.is_dir() {
            ret.extend(walk(&file_path))
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
        if first.contains("error:InternalError")
        /*--> in last line*/
        {
            contents = contents.replace("-->", "//");
        }
    }
    let module = contents.starts_with("// |jit-test| module");
    let mut b = ressa::Builder::new();
    let parser = b.js(&contents).module(module).build()?;
    for part in parser {
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
