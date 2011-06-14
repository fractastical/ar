#! /usr/bin/env arc

(use arc cwd io test-by-example)

(def test-file (x)
  (w/infile file x
    (example-test (runtime '(arc))
                  (allchars file))))

(w/cwd "tests"
  (each x (dir ".")
    (test-file x)))
