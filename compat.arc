(alias acons cons?)
(alias alist list?)

(def call-w/stdout (port thunk)
  (parameterize (racket-current-output-port port) (thunk)))

(def call-w/stdin (port thunk)
  (parameterize (racket-current-input-port port) (thunk)))

;; lib/strings.arc
;(= num comma)


;; TODO: compat.arc should be in a separate namespace
#|(def readline ((o str (stdin)))
  (awhen (readc str)
    (tostring
      (writec it)
      (whiler c (readc str) [in _ nil #\newline]
        (writec c)))))|#

;(= assoc (reverse-args assoc))


; setforms returns (vars get set) for a place based on car of an expr
;  vars is a list of gensyms alternating with expressions whose vals they
;   should be bound to, suitable for use as first arg to withs
;  get is an expression returning the current value in the place
;  set is an expression representing a function of one argument
;   that stores a new value in the place
(def setforms (place)
  (w/uniq (u v)
    (let place ssexpand-full.place
      (if (cons? place)
            (iflet f (setter car.place)
              (list (list u cadr.place)
                    (list car.place u)
                    `(fn (,v) ,(apply f u v cddr.place)))
              ; assumed to be data structure in fn position
              (let argsyms (map [uniq] cdr.place)
                (list (join (list u car.place)
                            (mappend list argsyms cdr.place))
                      `(,u ,@argsyms)
                      `(fn (,v) (sref ,u ,v ,car.argsyms)))))
          (list (list u place)
                u
                `(fn (,v) (assign ,place ,v)))))))
