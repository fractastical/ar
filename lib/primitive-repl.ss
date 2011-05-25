;; This is a simple REPL (it doesn't even catch errors) which doesn't
;; rely on Arc being up and running, and so is useful for working on
;; the compiler.
;;
;; Run arc to get the full featured REPL.

#lang scheme

(require "ac.ss")

(let ((arc (new-arc)))
  ((g ar-load) "core.arc"
               "base.arc"
               "arc.arc")
  (let loop ()
    (display "arc> ")
    (let ((v ((g ar-read) (current-input-port))))
      (unless (eof-object? v)
        (let ((expr (toarc v)))
          (let ((val ((get arc 'eval) expr)))
            (write (ar-deep-fromarc val))
            (newline)))
        (loop)))))
