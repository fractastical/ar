(= repl-eof-value (uniq))

(def repl ()
  ;; This causes Ctrl+C to return to the REPL, rather than aborting.
  ;; Technique was taken from Racket's (read-eval-print-loop) which
  ;; I found in /usr/lib/plt/collects/scheme/private/misc.ss
  (%get.racket-call-with-continuation-prompt
    (fn ()
      (on-err (fn (c)
                (prn "error: " (details c))
                (repl))
              (fn ()
                (let expr ;; This is to make GNU readline work
                          ((%get.racket-current-prompt-read))
                  (if (is expr %get.racket-eof)
                        (prn)
                      (let expr (%get.racket-syntax->datum expr) ;(ac-deep-toarc )
                        ;; > (readline) ;Fee fi fo fum
                        ;; " ;Fee fi fo fum"
                        (readline)
                        (let val (eval expr)
                          (write val)
                          (prn)
                          ;; TODO: ew
                          (sref namespace val  'that)
                          (sref namespace expr 'thatexpr)
                          (repl))))))))
    (%get.racket-default-continuation-prompt-tag)
    (fn args (repl))))

(repl)
