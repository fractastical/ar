;(= assoc (reverse-args assoc))

(redef assoc (x y)
  (orig y x))


(redef readline ((o str (stdin)))
  (awhen (readc str)
    (tostring
      (writec it)
      (whiler c (readc str) [in _ nil #\newline]
        (writec c)))))


;; lib/strings.arc
(alias num commafy)


(redef table ((o init))
  (let h (make-table)
    (when init (init h))
    h))


(redef setforms (place)
  (w/uniq u
    (let (bind get set) (orig place)
      (list bind get #`(fn (u) ,(set u))))))


(= setter (obj))

(mac defset (name parms . body)
  (w/uniq gexpr
    #`(sref setter
            (fn (gexpr)
              (let parms (cdr gexpr) . body))
            ',name)))

(defset car (x)
  (w/uniq g
    (list (list g x)
          `(car ,g)
          `(fn (val) (scar ,g val)))))

(defset cdr (x)
  (w/uniq g
    (list (list g x)
          `(cdr ,g)
          `(fn (val) (scdr ,g val)))))

(defset caar (x)
  (w/uniq g
    (list (list g x)
          `(caar ,g)
          `(fn (val) (scar (car ,g) val)))))

(defset cadr (x)
  (w/uniq g
    (list (list g x)
          `(cadr ,g)
          `(fn (val) (scar (cdr ,g) val)))))

(defset cddr (x)
  (w/uniq g
    (list (list g x)
          `(cddr ,g)
          `(fn (val) (scdr (cdr ,g) val)))))


;; TODO: make sure this works as expected
(extend sref-mac (f . args) (setter f)
  (apply it args))

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
