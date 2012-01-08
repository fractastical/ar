;; http://arclanguage.org/item?id=10344
(let interactive (%:terminal-port? (current-input-port))
  (when interactive
    (%:namespace-require 'readline/rep-start)
    ;(%:dynamic-require 'readline/rep-start #f)
    )

  #|(def repl ()
    (% (let repl-loop ()
         ;; This prompt catches all error escapes, including from read and print.
         (call-with-continuation-prompt
           (lambda ()
             (let ((v ((current-prompt-read))))
               (unless (eof-object? v)
                 (call-with-values
                   (lambda ()
                     ;; This prompt catches escapes during evaluation.
                     ;; Unlike the outer prompt, the handler prints
                     ;; the results.
                     (call-with-continuation-prompt
                      (lambda ()
                        (let ((w (cons '#%top-interaction v)))
                          ((current-eval) (if (syntax? v)
                                              (namespace-syntax-introduce
                                               (datum->syntax #f w v))
                                              w))))))
                   (lambda results (for-each (current-print) results)))
                 ;; Abort to loop. (Calling `repl-loop' directory would not be a tail call.)
                 (abort-current-continuation (default-continuation-prompt-tag)))))
           (default-continuation-prompt-tag)
           (lambda args (repl-loop))))))|#
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
                  ;(repl)
                  ;; Abort to loop. (Calling `repl` directly would not be a tail call.)
                  (%:abort-current-continuation (default-continuation-prompt-tag))
                  )
                (fn ()
                  (let expr (if interactive
                                ;; This is to make GNU readline work
                                (let it ((%.current-prompt-read))
                                  ;; TODO: make false? in ac.rkt return #t for %.eof
                                  ;(and it (%.syntax->datum it))
                                  (when %.syntax?.it
                                    (%.syntax->datum it))
                                  ;it
                                  )
                                ;; TODO: make false? in ac.rkt return #t for %.eof
                                ;; TODO: maybe make read return %.eof by default?
                                (read)
                                ;(read (stdin) %.eof)
                                )
                        ;; TODO: make false? in ac.rkt return #t for %.eof
                    (if expr ;(%.eof-object? expr)
                          ;; > (readline) ;Fee fi fo fum
                          ;; " ;Fee fi fo fum"
                          ;(readline)
                          (let that? (in expr 'that 'thatexpr)
                            ;; TODO: ew, used to prevent assigning things to the
                            ;;       name 'that:
                            ;;
                            ;;       > do
                            ;;       #<mac:do>
                            ;;       > do
                            ;;       #<mac:that>
                            (unless that?
                              (%.assign-global-raw (%.namespace) 'thatexpr expr))
                            (let val eval.expr
                              (when interactive
                                (write val)
                                (prn))
                              (unless that?
                                (%.assign-global-raw (%.namespace) 'that val)
                                ;(sref (%.namespace) val  'that)
                                ;(sref (%.namespace) expr 'thatexpr)
                                ;(= that      val)
                                ;(= thatexpr  expr)
                                )
                              ;(repl)
                              ;; Abort to loop. (Calling `repl` directly would not be a tail call.)
                              (%:abort-current-continuation (default-continuation-prompt-tag))
                              ))
                        interactive
                          (prn))))))
      (%.default-continuation-prompt-tag)
      (fn args #;(prn args) (repl))))
  )

(repl)
