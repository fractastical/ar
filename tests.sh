#!/bin/bash
#set -e -v

cd tests
mzscheme ar-test.ss
mzscheme ac-test.ss
mzscheme arc-test.ss
mzscheme io-test.ss
sudo `which mzscheme` io-test-root.ss
mzscheme strings-test.ss
cd ..
