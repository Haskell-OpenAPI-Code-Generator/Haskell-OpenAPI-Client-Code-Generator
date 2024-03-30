#!/usr/bin/env bash

nix build .\#checks.x86_64-linux.testGoldenGenerate
rm -rf testing/golden-output
mkdir -p testing/golden-output
cp -R result/* testing/golden-output
chmod -R 764 testing/golden-output


nix build .\#checks.x86_64-linux.exampleGenerate
rm -rf example/generatedCode
mkdir -p example/generatedCode
cp -R result/* example/generatedCode
chmod -R 764 example/generatedCode
