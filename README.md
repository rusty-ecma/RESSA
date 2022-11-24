# RESSA

[![Rust](https://github.com/rusty-ecma/RESSA/workflows/Rust/badge.svg?branch=featureless_test262)](https://github.com/rusty-ecma/RESSA/actions)

[![crates.io](https://img.shields.io/crates/v/ressa.svg)](https://crates.io/crates/ressa)
[![last commit master](https://img.shields.io/github/last-commit/FreeMasen/RESSA.svg)](https://github.com/FreeMasen/RESSA/commits/master)

> Rust EcmaScript Syntax Analyzer

This project is part of a series of crates designed to enable developers to create JavaScript development tools using the Rust programming language. [Rusty ECMA Details](#rusty-ecma-details)

The two major pieces that users will interact with are the `Parser` struct and the enums defined by [`resast`](https://github.com/FreeMasen/resast)

## `Parser`

The parser struct will be the main way to convert text into an `AST`.
Conveniently `Parser` implements `Iterator` over `Result<ProgramPart, Error>`,
this means that you can evaluate your JS in pieces from top to bottom.

> Note: By default the `Parser` will not be able to handle js module features,
> [see the module example](./examples/simple_module.rs) for details on how to parse js modules

### Iterator Example

```rust
use resast::prelude::*;
use ressa::*;

fn main() {
    let js = "function helloWorld() { alert('Hello world'); }";
    let p = Parser::new(&js).unwrap();
    let f = ProgramPart::decl(Decl::Func(Func {
        id: Some(Ident::from("helloWorld")),
        params: vec![],
        body: FuncBody(vec![ProgramPart::Stmt(Stmt::Expr(Expr::Call(CallExpr {
            callee: Box::new(Expr::ident_from("alert")),
            arguments: vec![Expr::Lit(Lit::String(StringLit::Single(Cow::Owned(
                "Hello world".to_string(),
            ))))],
        })))]),
        generator: false,
        is_async: false,
    }));
    for part in p {
        assert_eq!(part.unwrap(), f);
    }
}
```

Another way to interact with a `Parser` would be to utilize the `parse` method. This method will iterate over all of the found `ProgramParts` and collect them into a `Program`,

### Parse Example

```rust
use ressa::{
    Parser,
};
use resast::ref_tree::prelude::*;
fn main() {
    let js = "
function Thing() {
    return 'stuff';
}
";
    let mut parser = Parser::new(js).expect("Failed to create parser");
    let program = parser.parse().expect("Unable to parse text");
    match program {
        Program::Script(_parts) => println!("found a script"),
        Program::Mod(_parts) => println!("found an es6 module"),
    }
}
```
Once you get to the inner `parts` of a `Program` you have a `Vec<ProgramPart>` which will operate the same as the [iterator example](#iterator-example)

# Rusty ECMA Details

## The Rust ECMA Crates

- [RESS](https://github.com/freemasen/ress) - Tokenizer or Scanner
- [RESSA](https://github.com/freemasen/ressa) - Parser
- [RESAST](https://github.com/freemasen/resast) - AST
- [RESW](https://github.com/freemasen/resw) - Writer

## Why So Many?

While much of what each crate provides is closely coupled with the other crates, the main goal is to provide the largest amount of customizability. For example, someone writing a fuzzer would only need the `RESAST` and `RESW`, it seems silly to require that they also pull in `RESS` and `RESSA` needlessly.
