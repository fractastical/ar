#!/bin/bash
set -e -v
mzscheme "tests/ar-test.ss"
mzscheme "tests/ac-test.ss"
mzscheme "tests/arc-test.ss"
mzscheme "tests/strings-test.ss"
