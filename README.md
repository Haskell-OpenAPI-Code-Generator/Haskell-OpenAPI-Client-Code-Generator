# openapi3-code-generator
[![CircleCI](https://circleci.com/gh/Haskell-OpenAPI-Code-Generator/Haskell-OpenAPI-Client-Code-Generator.svg?style=svg)](https://circleci.com/gh/Haskell-OpenAPI-Code-Generator/Haskell-OpenAPI-Client-Code-Generator)

## How to use?
1. install [cabal](https://www.haskell.org/cabal/)
1. `cabal update`
1. `cabal install openapi3-code-generator`
1. `openapi3-code-generator-exe my_specification.yml`

An `out` directory is created with the generated code. Hint you can use `--output-dir` to specify another output directory.
You can use `openapi3-code-generator --help` to list all CLI options.

If `openapi3-code-generator-exe` is not found, you may not have added the cabal bin to your `PATH`. Execute `~/.local/bin/openapi3-code-generator-exe` instead.

## How to run from source
1. install [stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)
1. `stack run -- --help`

## Example package
In the folder `example` is a package that uses the generated code from `specifications/petstore.yml`.
You can run `stack test` inside the `example` directory to run the code, it calls the server "https://petstore.swagger.io/v2" with some sample data.

https://github.com/Haskell-OpenAPI-Code-Generator/Stripe-Haskell-Library uses this code generator to generate
a Stripe API client.

## Documentation
The documentation for the code can be found at https://hackage.haskell.org/package/openapi3-code-generator
This package was created as part of a bachelor thesis. [public thesis](https://github.com/Haskell-OpenAPI-Code-Generator/Haskell-OpenAPI-Client-Code-Generator/blob/master/thesis.pdf)

## Large specifications
For large specifications some modules (CyclicTypes.hs for example) can get pretty big. It may be necessary to use `--fast` with `stack build --fast` to build the code.

## Module structure of the generated code.
All symbols are globally unique and are reexported in the module `OpenAPI` (Module name can be changed with `--module-name`).
To reduce compile time, the code is split up into multiple modules.
Mainly for every operation and for every schema.

## Troubleshooting naming conflicts
Naming conflicts can happen, sometimes a little manual adjustment is needed.
With the following options naming conflicts can be resolved.
It can happen, that the names get so long, that they are longer than the file system supports.

- `property-type-suffix`
- `response-type-suffix`
- `response-body-type-suffix`
- `request-body-type-suffix`
- `use-numbered-variant-constructors`
- `convert-to-camel-case`

## Limitations
The following features are not supported
- links
- callbacks
- Only references to `components` are supported
- Only JSON is supported for both sending and receiving data. `application/x-www-form-urlencoded` can only be used to send data.
- Some circular references in the schemas. For example if an `allOf` contains itself
- Parameters not in `path` or `query`
- `additionalProperties`
- `not` schemas
- `writeOnly` and `readOnly`
- `multipleOf`
- `maximum`
- `exclusiveMaximum`
- `minimum`
- `exclusiveMinimum`
- `minLength`
- `maxItems`
- `minItems`
- `uniqueItems`
- `maxProperties`
- `minProperties`
- `xml`

## Development
Re-generate the code found in the [example](./example/) directory if you made changes to the output generator.
You can do this using the [example-configuration.yml](./example-configuration.yml):

``` bash
openapi3-code-generator-exe --configuration example-configuration.yml
```

Also make sure that the tests there run fine:

``` bash
cd example
stack test
```

Also adapt golden tests:

``` bash
nix-build ci.nix -A test-golden-generate
rm -rf golden-output
mkdir -p golden-output
cp -R result/* golden-output
chmod -R 764 golden-output
```
