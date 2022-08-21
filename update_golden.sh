#!/bin/bash

nix-build ci.nix -A test-golden-generate
rm -rf testing/golden-output
mkdir -p testing/golden-output
cp -R result/* testing/golden-output
chmod -R 764 testing/golden-output
