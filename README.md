# openapi3-code-generator
[![CircleCI](https://circleci.com/gh/Haskell-OpenAPI-Code-Generator/Haskell-OpenAPI-Client-Code-Generator.svg?style=svg)](https://circleci.com/gh/Haskell-OpenAPI-Code-Generator/Haskell-OpenAPI-Client-Code-Generator)

## How to use?
1. install [stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)
1. `stack install openapi3-code-generator`
1. `openapi3-code-generator-exe my_specification.yml`

An `out` directory is created with the generated code. Hint you can use `--output-dir` to specify another output directory.
You can use `openapi3-code-generator --help` to list all CLI options.

If `openapi3-code-generator-exe` is not found, you may not have added the cabal bin to your `PATH`. Execute `~/bin/openapi3-code-generator-exe` instead.

## How to run from source
1. `stack build`
1. `stack run -- --help`

## Example package
In the folder `example` is a package that uses the generated code from `specifications/petstore.yml`.
You can run `stack test` inside the `example` directory to run the code, it calls the server "https://petstore.swagger.io/v2" with some sample data.

https://github.com/Haskell-OpenAPI-Code-Generator/Stripe-Haskell-Library uses this code generator to generate
a Stripe API client.

## Documentation
The documentation for the code can be found at https://hackage.haskell.org/package/openapi3-code-generator
This package was created as part of a bachelor thesis. This thesis can be found here TODO.

## Large specifications
For large specifications some modules (CyclicTypes.hs for example) can get pretty big. It may be necessary to use `--fast` with `stack build --fast` to build the code.

## Module structure of the generated code.
All symbols are globally unique and are reexported in the module `OpenAPI` (Module name can be changed with `--module-name`).
To reduce compile time, the code is split up into multiple modules.
Mainly for every operation and for every schema.
Schemas with cyclic dependencies are are in the module `OpenAPI.CyclicTypes`.

## Troubleshooting naming conflicts
Naming conflicts can happen, sometimes a little manual adjustment is needed.
With the following options naming conflicts can be resolved.

- `property-type-suffix`
- `response-type-suffix`
- `response-body-type-suffix`
- `request-body-type-suffix`
- `use-numbered-variant-constructors`
- `convert-to-camel-case`

