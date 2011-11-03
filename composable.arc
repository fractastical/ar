(use racket)

(def abort-current-continuation/keep-prompt (tag thunk)
  ((racket-call-with-continuation-prompt
     (fn ()
       ((racket-call-with-current-continuation
          (fn (k) (fn () k))
          tag)))
     tag)
   thunk))

(def call-with-shift (f (o tag))
  (if tag (racket-call-with-composable-continuation
            (fn (k)
              (abort-current-continuation/keep-prompt
                tag
                (fn ()
                  (f (fn vals
                       (racket-call-with-continuation-prompt
                         (fn () (apply k vals))
                         tag
                         racket-#f))))))
            tag)
          (call-with-shift f (racket-default-continuation-prompt-tag))))

(mac shift (x y)
  `(call-with-shift (fn (,x) ,y)))

(mac reset (x)
  `(racket-call-with-continuation-prompt (fn () ,x)))
