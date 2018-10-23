# RESSA
> Rust EcmaScript Syntax Analyzer

This project is designed to parse javascript text into an Abstract Syntax Tree (AST), based on [ESTREE](https://github.com/estree/estree) however it doesn't match quite yet.

In its current state it is minimally functional, there are a few big features that are still missing but it was able to successfully parse the three tests files in [everything.js](https://github.com/michaelficarra/everything.js). The biggest missing pieces is a more robust error handling/reporting scheme, some moderately significant changes to the AST data structures to bring them closer to the ESTREE specification and some performance tweaks.


## Current Performance
> WARNING: EXTREMELY NAIVE BENCHMARKS

| test name | bench time | +/- | size |
|---|---|---|---|
| angular1.js | 2.30 s | 629.24 ms | 1.10 mb |
| angular1.min.js | 298.48 ms | 133.10 ms | 155.19 kb |
| jquery.js | 512.97 ms | 193.62 ms | 265.38 kb |
| jquery.min.js | 151.37 ms | 38.78 ms | 84.89 kb |
| react.js | 38.49 ms | 2.92 ms | 57.70 kb |
| react.min.js | 10.63 ms | 497958.00 ns | 7.05 kb |
| reactDom.js | 1.49 s | 672.39 ms | 626.54 kb |
| reactDom.min.js | 157.81 ms | 66.25 ms | 94.27 kb |
| vue.js | 633.09 ms | 285.20 ms | 282.52 kb |
| vue.min.js | 154.84 ms | 16.89 ms | 84.43 kb |
