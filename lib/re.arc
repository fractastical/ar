(def racket->string (x)
  (racket-bytes->string/utf-8 (racket-car x)))

(mac pref (x y)
  `(= ,x (+ ,y ,x)))

#|(defrule ac (racket-eq? s (racket #f))
  nil)|#

#|(defrule ac (racket-bytes? s)
  s)|#

(def make-matcher (cont str (o next))
  (fn (re (o next next))
    (when next
      (pref re "^"))
    (= next t)
    (let x (racket-regexp-match re str)
      (if (isnt x (racket #f))
            (racket->string x)
            (cont nil)))))


(= modes* (table))

(mac defmode (name parms . body)
  `(= (modes* ',name) (fn ,parms ,@body)))

(defmode drain (str body)
  (w/uniq (x tmp)
    `(let ,tmp nil
       (withs (re-input* (instring ,str)
               re        (,make-matcher idfn re-input*)
               re        (fn (s)
                           (re s nil)))
         ((afn ()
            (let ,x (do ,@body)
              (if ,x (push ,x ,tmp))
              (if (peekc re-input*)
                    (self))))))
       (rev ,tmp))))

(mac w/target (str . body)
  (iflet x (and (cons? str)
                (modes* (car str)))
    (x (cadr str) body)
    (w/uniq c
      `(ccc (fn (,c)
         (withs (re-input* (instring ,str)
                 re        (,make-matcher ,c re-input*))
           (,c (do ,@body))))))))
