If you are interested in contributing, I would be super happy for the help. This is not my
full time job so please be patient if I am slow to respond.

If you are running into any issues, please open an issue before opening a pull request.

If you want to start in on an existing issue, please make a comment on it so we don't have
two people working on the same issue


# Testing
There are a few sets of JavaScript files that are required to run the tests in this repository. The first set can be easily acquired by running `npm install` in the root of this project. An additional test is also available behind a feature flag `moz_central` that requires the JIT Test files from the FireFox repository, the expectation is that these will exist in the folder `moz-central` in the root of this project. To get these files you can either manually download and unzip them by following [this link](https://hg.mozilla.org/mozilla-central/archive/tip.zip/js/src/jit-test/tests/) or you can execute the following command.

```sh
curl https://hg.mozilla.org/mozilla-central/archive/tip.zip/js/src/jit-test/tests/ --output moz-central.zip
unzip -q moz-central.zip -d moz-central
```

To run these tests simple execute the following command.

```sh
cargo test --features moz_central -- moz_central
```