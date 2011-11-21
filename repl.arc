(= repl-eof-value (uniq))

;(parameterize (racket-readline-prompt #"> ")
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
                ;(disp "> ")
                (let expr ;(racket-readline "> ")
                          ;(read (stdin) repl-eof-value)
                          ;; This is to make GNU readline work
                          ((racket-current-prompt-read))
                          #|(let in ((racket-current-get-interaction-input-port))
                            ((racket-current-read-interaction) (racket-object-name in) in))|#

                  (if ;(in expr ':a repl-eof-value)
                      (is expr racket-eof)
                        (prn)
                      (let expr (ac-deep-toarc (racket-syntax->datum expr))
                        ;(readline)
                        (let val (eval expr)
                          (write val)
                          (prn)
                          (= that      val
                             thatexpr  expr)
                          (repl))))))))
    (racket-default-continuation-prompt-tag)
    (fn args (repl))))

#|(def repl ()
  (let repl-loop ()
    ;; This prompt catches all error escapes, including from read and print.
    (racket-call-with-continuation-prompt
      (fn ()
        (let expr ((racket-current-prompt-read))
          (unless (is expr racket-eof) ;(eof-object? expr)
            (racket-call-with-values
              (fn ()
                ;; This prompt catches escapes during evaluation.
                ;; Unlike the outer prompt, the handler prints
                ;; the results.
                (racket-call-with-continuation-prompt
                  (fn ()
                    (let val (eval expr)
                      (= that      val
                         thatexpr  expr)
                      ;val
                      ))))
              (fn results (map (racket-current-print) results)
                ;(racket-for-each (racket-current-print) results)
                ))
            ;; Abort to loop. (Calling `repl-loop' directory would not be a tail call.)
            (racket-abort-current-continuation (racket-default-continuation-prompt-tag)))))
      (racket-default-continuation-prompt-tag)
      (fn args (repl)))))|#


(repl)
