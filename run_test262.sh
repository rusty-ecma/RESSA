export RUST_MIN_STACK=9999999
export RESSA_WRITE_FAILURES=1
cargo test262 || node ./prepFailures.js && rsync -r ./failures/test262/ rfm@45.55.78.145:~/projects/wiredforge.com/public/test262/