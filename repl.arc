;; http://arclanguage.org/item?id=10344
(let interactive t ;(%.terminal-port? (stdin))
  #|(when interactive
    (% (namespace-require 'readline)))|#

  (def repl ()
    ;; This causes Ctrl+C to return to the REPL, rather than aborting.
    ;; Technique was taken from Racket's (read-eval-print-loop) which
    ;; I found in /usr/lib/plt/collects/scheme/private/misc.ss
    (%.call-with-continuation-prompt
      (fn ()
        (on-err (fn (c)
                  ;; http://arclanguage.org/item?id=10344
                  (w/stdout (stderr)
                    (prn "error: " (details c)))
                  (repl))
                (fn ()
                  (let expr (if interactive
                                ;; This is to make GNU readline work
                                (let it ((%.current-prompt-read))
                                  ;; TODO: make false? in ac.rkt return #t for %.eof
                                  ;(and it (%.syntax->datum it))
                                  (if (%.eof-object? it)
                                      it
                                      (%.syntax->datum it)))
                                ;; TODO: make false? in ac.rkt return #t for %.eof
                                ;; TODO: maybe make read return %.eof by default?
                                ;(read)
                                (read (stdin) %.eof))
                        ;; TODO: make false? in ac.rkt return #t for %.eof
                    (if (%.eof-object? expr)
                        (when interactive (prn))
                        ;; > (readline) ;Fee fi fo fum
                        ;; " ;Fee fi fo fum"
                        ;(readline)
                        (let val (eval expr)
                          (when interactive
                            (write val)
                            (prn))
                          ;(= that      val)
                          ;(= thatexpr  expr)
                          ;; TODO: ew, used to prevent assigning things to the
                          ;;       name 'that:
                          ;;
                          ;;       > do
                          ;;       #<mac:do>
                          ;;       > do
                          ;;       #<mac:that>
                          (sref namespace val  'that)
                          (sref namespace expr 'thatexpr)
                          (repl)))))))
      (%.default-continuation-prompt-tag)
      (fn args (repl)))))

(repl)
