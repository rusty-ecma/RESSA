# RESSA
> Rust EcmaScript Syntax Analyzer

This project is designed to parse javascript text into an Abstract Syntax Tree (AST), based on [ESTREE](https://github.com/estree/estree) (there is still some work todo to map directly to the [ESTREE structure](#a-couple-of-notes))


The two major pieces that users will interact with are the `Parser` struct and the enums defined in the `node` module.

## `Parser`

The parser struct will be the main way to convert text into an `AST`.
Conveniently `Parser` implements `Iterator` over `Result<ProgramPart, Error>`,
this means that you can evaluate your JS in pieces from top to bottom. These pieces will be discussed in more detail in the [node section](#node).

### Iterator Example
```rust
extern crate ressa;
use ressa::{
    Parser,
    node::*,
};
fn main() {
    let js = "
function Thing() {
    return 'stuff';
}
";
    let parser = Parser::new(js).expect("Failed to create parser");
    for part in parser {
        let part = part.expect("Error parsing part");
        match part {
            ProgramPart::Decl(decl) => match decl {
                Declaration::Function(f) => {
                    if let Some(ref id) = f.id {
                        assert_eq!(id, "Thing");
                    }
                    assert!(f.params.len() == 0);
                    assert!(!f.generator);
                    assert!(!f.is_async);
                    for part in f.body {
                        match part {
                            ProgramPart::Statement(stmt) => match stmt {
                                Statement::Return(expr) => {
                                    if let Some(expr) = expr {
                                        match expr {
                                            Expression::Literal(lit) => match lit {
                                                Literal::String(value) => assert_eq!(value, String::from("'stuff'")),
                                                _ => ()
                                            },
                                            _ => ()
                                        }
                                    }
                                },
                                _ => (),
                            },
                            _ => ()
                        }
                    }
                },
                _ => ()
            },
            _ => (),
        }
    }
}
```

Another way to interact with a `Parser` would be to utilize the `parse` method. This method will iterate over all of the found `ProgramParts` and collect them into a `Program`,

### Parse Example
```rust
extern crate ressa;
use ressa::{
    Parser,
    node::*,
};

fn main() {
    let js = "
function Thing() {
    return 'stuff';
}
";
    let mut parser = Parser::new(js).expect("Failed to create parser");
    let program = parser.parse().expect("Unable to parse text");
    match program {
        Program::Script(parts) => println!("found a script"),
        Program::Module(parts) => println!("found an es6 module"),
    }
}
```
Once you get to the inner `parts` of a `Program` you have a `Vec<ProgramPart>` which will operate the same as the [iterator example](#iterator-example)
## `Node`
The `node` module houses a collection of `enums` and `structs` that represent the static meaning of JS text. The primary entry point is going to be the `ProgramPart` enum. There are 3 types of `ProgramParts` a

- `Directive` - This is a literal value at the top of a scope (i.e. `'use strict'`)
- `Declaration` - This defines something in the top level of scope
    - `Variable` - `var`, `let` or `const` statements
    - `Function` - a named function definition in the top scope
    - `Class` - a named class in the top scope
    - `Import` - importing of assets from another file (ES6 Modules only)
    - `Export` - exporting of assets from this file (ES6 Modules only)
- `Statement` - This is the primary unit of js, see the node module for additional details

### A couple of notes
1. Currently this module defines some structures that are never used, most notable the `Node` struct and `Item` enum. This is because I am still working through how I am going to fully reconcile the inheritance based approach that [ESTREE](https://github.com/estree/estree) uses to define their AST and the trait based approach that rust uses for composition.
2. In the not so distant future, I intend to break this module into its own crate to allow for more modularity.
3. My goal is to have this AST, when serialized to JSON, match the output of [esprima](https://github.com/jquery/esprima)