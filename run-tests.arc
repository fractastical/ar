#! /usr/bin/env arc

(use arc io tester) ; test-by-example)

#|(w/cwd "../old/lib/"
  (load "tester.arc"))|#

#|(def test-file (x)
  (w/infile file x
    (example-test (runtime '(arc))
                  (allchars file))))|#

(w/cwd "tests"
  (each x (dir ".")
    (test-file x)))
