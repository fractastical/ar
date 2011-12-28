(def repl ()
  ;; This causes Ctrl+C to return to the REPL, rather than aborting.
  ;; Technique was taken from Racket's (read-eval-print-loop) which
  ;; I found in /usr/lib/plt/collects/scheme/private/misc.ss
  (%.call-with-continuation-prompt
    (fn ()
      (on-err (fn (c)
                (prn "error: " (details c))
                (repl))
              (fn ()
                (let expr ;; This is to make GNU readline work
                          ((%.current-prompt-read))
                  (if (%.eof-object? expr)
                        (prn)
                      (let expr (%.syntax->datum expr)
                        ;; > (readline) ;Fee fi fo fum
                        ;; " ;Fee fi fo fum"
                        ;(readline)
                        (let val (eval expr)
                          (write val)
                          (prn)
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
                          (repl))))))))
    (%.default-continuation-prompt-tag)
    (fn args (repl))))

(repl)
