#!/usr/bin/env mzscheme
#lang scheme/load

(require scheme/cmdline)

(define run-repl #f)
(define files-to-load '())

(command-line
 #:once-each
 (("--repl")
  "run the REPL even when specifying files"
  (set! run-repl #t))

 #:args files
 (set! files-to-load files))

(define srcdir*
  (path->string
   (let-values (((base _2 _3)
                 (split-path (normalize-path
                              (find-system-path 'run-file)))))
     base)))

(namespace-require `(file ,(string-append srcdir* "ac.ss")))

(let ((arc (new-arc)))
  (aload arc (string-append srcdir* "core.arc")
             (string-append srcdir* "base.arc")
             (string-append srcdir* "arc.arc")
             (string-append srcdir* "arc3.1/backcompat.arc")
             (string-append srcdir* "arc3.1/strings.arc"))
; this should work, but says "undefined variable: _"
;  ((get arc 'load) (string-append srcdir* "arc3.1/strings.arc"))
  (for-each (get arc 'load) files-to-load)
  (when (or run-repl (null? files-to-load))
    ((get arc 'load) (string-append srcdir* "repl.arc"))
    (noprint ((get arc 'repl)))))
