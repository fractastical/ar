#! /usr/bin/env arc

(use arubic io tester) ; test-by-example)

#|(w/cwd "../old/lib/"
  (load "tester.arc"))|#

#|(def test-file (x)
  (w/infile file x
    (example-test (runtime '(arc))
                  (allchars file))))|#

(w/cwd "tests"
  (each x (dir ".")
    (w/eval (new-namespace)
      (test-file x))))
