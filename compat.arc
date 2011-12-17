(alias acons cons?)
(alias alist list?)

(def call-w/stdout (port thunk)
  (parameterize (racket-current-output-port port) (thunk)))

(def call-w/stdin (port thunk)
  (parameterize (racket-current-input-port port) (thunk)))

;; lib/strings.arc
;(= num commafy)


;; TODO: compat.arc should be in a separate namespace
#|(def readline ((o str (stdin)))
  (awhen (readc str)
    (tostring
      (writec it)
      (whiler c (readc str) [in _ nil #\newline]
        (writec c)))))|#

;(= assoc (reverse-args assoc))

#|(def setforms (place)
  (w/uniq (u v)
    (let place expand-full.place
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
                      `(fn (,v) (,sref ,u ,v ,car.argsyms)))))
          (list (list u place)
                u
                `(fn (,v) (,assign ,place ,v)))))))|#
