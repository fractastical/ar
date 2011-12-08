(= repl-eof-value (uniq))

(def repl ()
  ;; This causes Ctrl+C to return to the REPL, rather than aborting.
  ;; Technique was taken from Racket's (read-eval-print-loop) which
  ;; I found in /usr/lib/plt/collects/scheme/private/misc.ss
  (racket-call-with-continuation-prompt
    (fn ()
      (on-err (fn (c)
                (prn "error: " (details c))
                (repl))
              (fn ()
                (let expr ;; This is to make GNU readline work
                          ((racket-current-prompt-read))
                          (if (is expr racket-eof)
                        (prn)
                      (let expr (ac-deep-toarc (racket-syntax->datum expr))
                        (let val (eval expr)
                          (write val)
                          (prn)
                          ;; TODO: ew
                          (sref namespace val  'that)
                          (sref namespace expr 'thatexpr)
                          (repl))))))))
    (racket-default-continuation-prompt-tag)
    (fn args (repl))))

(repl)
