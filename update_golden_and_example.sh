#!/usr/bin/env bash

system=$(nix eval --impure --raw --expr 'builtins.currentSystem')

nix build .\#checks.${system}.testGoldenGenerate
rm -rf testing/golden-output
mkdir -p testing/golden-output
cp -R result/* testing/golden-output
chmod -R 764 testing/golden-output


nix build .\#checks.${system}.exampleGenerate
rm -rf example/generatedCode
mkdir -p example/generatedCode
cp -R result/* example/generatedCode
chmod -R 764 example/generatedCode
