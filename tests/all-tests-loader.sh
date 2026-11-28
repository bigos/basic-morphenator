#!/usr/bin/bash

echo "Running all tests"
echo

sbcl --load ./all-tests-loader.lisp
