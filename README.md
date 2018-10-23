# RESSA
> Rust EcmaScript Syntax Analyzer

This project is designed to parse javascript text into an Abstract Syntax Tree (AST), based on [ESTREE](https://github.com/estree/estree) however it doesn't match quite yet.

In its current state it is minimally functional, there are a few big features that are still missing but it was able to successfully parse the three tests files in [everything.js](https://github.com/michaelficarra/everything.js). The biggest missing pieces is a more robust error handling/reporting scheme, some moderately significant changes to the AST data structures to bring them closer to the ESTREE specification and some performance tweaks.

