#!/bin/bash
#set -e -v

#./run-tests
#mzscheme "as.ss" "run-tests"

#cd arubic
#mzscheme "arubic" "run-tests.arc"
#cd ..

cd tests
mzscheme "arc-test.ss"
mzscheme "ar-test.ss"
mzscheme "ac-test.ss"
mzscheme "strings-test.ss"
cd ..
