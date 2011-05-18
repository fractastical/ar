#!/bin/bash
#set -e -v

cd tests
mzscheme ar-test.ss
mzscheme ac-test.ss
cd ..

arc run-tests
sudo ./arc run-tests "tests/io.arc (root)"
