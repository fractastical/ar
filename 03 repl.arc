;; http://arclanguage.org/item?id=10344
(let interactive (%:terminal-port? (current-input-port))
  (when interactive
    (%:namespace-require 'readline/rep-start))

  (def repl ()
    ;; This causes Ctrl+C to return to the REPL, rather than aborting.
    ;; Technique was taken from Racket's (read-eval-print-loop) which
    ;; I found in /usr/share/racket/collects/racket/private/misc.ss
    (%.call-with-continuation-prompt
      (fn ()
        (on-err (fn (c)
                  ;; http://arclanguage.org/item?id=10344
                  (w/stdout (stderr)
                    (prn "error: " details.c))
                  ;; Abort to loop. (Calling `repl` directly would not be a tail call.)
                  (%:abort-current-continuation (default-continuation-prompt-tag)))
                (fn ()
                  (let expr (if interactive
                                ;; This is to make GNU readline work
                                (let it ((%.current-prompt-read))
                                  (when %.syntax?.it
                                    (%.syntax->datum it)))
                                (read))
                    (if expr
                          (let that? (in expr 'that 'thatexpr)
                            (unless that?
                              (%.assign-global-raw (%.namespace) 'thatexpr expr))
                            (let val eval.expr
                              (unless that?
                                (%.assign-global-raw (%.namespace) 'that val))
                              (when interactive
                                (write val)
                                (prn))
                              ;; Abort to loop. (Calling `repl` directly would not be a tail call.)
                              (%:abort-current-continuation (default-continuation-prompt-tag))))
                        interactive
                          (prn))))))
      (%.default-continuation-prompt-tag)
      (fn args (repl)))))

(repl)
