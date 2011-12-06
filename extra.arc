(defset assoc (x y)
  (w/uniq g
    (list (list g x)
          `(assoc ,g ,y)
          `(fn (val) (scar (assoc-ref ,g ,y) val)))))

(defset alref (x y)
  (w/uniq g
    (list (list g x)
          `(alref ,g ,y)
          `(fn (val) (scar (cdr:assoc ,g ,y) val)))))


#|
;; TODO: inefficient
(def zip args
  ;; TODO: this causes it to stop when all the lists are empty.
  ;;       however, in Python, it stops when the first list is
  ;;       empty. I'm not sure which is the better semantic.
  ;;       to get the Python behavior, just change `all` to `some`
  (if (all no args)
        nil
      (cons (apply list (map car args))
            (apply zip  (map cdr args)))))|#

(def zip args
  ;; Faster than the above: 395ms compared to 695ms
  ;; TODO: how fast is map applied to multiple lists?
  (apply map list args))


(mac rloop (name parms . body)
  (let (l r) (apply zip pair.parms)
    `((rfn ,name ,l ,@body) ,@r)))

(mac aloop (parms . body)
  `(rloop self ,parms ,@body))


(mac afneach (parms x . body)
  (w/uniq u
    `((afn (,u)
        (whenlet ,parms ,u
          ,@body))
      ,x)))


(def maplast (f xs)
  (if (no cdr.xs)
        (f car.xs)
      (do (f car.xs)
          (maplast f cdr.xs))))


(mac catcherr (expr)
  `(on-err (fn (c) (details c))
           (fn () ,expr nil)))

(def xml-encode (s)
  (multisubst '(("&" "&amp;")
                ("<" "&lt;")) s))


(mac extend (name parms test . body)
  (w/uniq u
    `(let orig ,name
       (= ,name (fn ,u
                  ;(%nocompile (racket-displayln ,u))
                  #|(apply (fn ,parms
                           (aif ,test
                                  (do ,@body)
                                (apply orig ,u)))
                         ,u)|#
                  (let ,parms ,u
                    (aif ,test
                           (do ,@body)
                         (%nocompile (,apply orig ,u))))
                  )))))

#|(mac extend (name parms test . body)
  (w/uniq (k a u)
    `(let orig ,name
       (= ,name (racket-make-keyword-procedure
                  (fn (,k ,a . ,u)
                    ;(prn ',name " " ,u)
                    (let ,parms ,u
                      (aif ,test
                             (do ,@body)
                           (racket-keyword-apply orig ,k ,a (racket-mlist->list ,u))))))))))|#


#|(mac extend (name parms test . body)
  (w/uniq u
    `(let orig ,name
       (= ,name (fn ,u
                  (aif (apply (fn ,parms ,test) ,u)
                         (apply (fn ,parms ,@body) ,u)
                       (apply orig ,u)))))))|#

#|(mac extend (name parms test . body)
  (w/uniq u
    `(let orig ,name
       (= ,name (fn ,u
                  ;(prn ',name " " ,u)
                  (aif (apply (fn ,parms ,test) ,u)
                         (apply (fn ,parms ,@body) ,u)
                       (apply orig ,u)))))))|#


(= defcall-types* (table))

(mac defcall (name parms . body)
  `(= (defcall-types* ',name) (fn ,parms ,@body)))

(extend ac-apply-non-fn (x . args)
  (%nocompile (orig defcall-types* (type x)))
  (apply it x args))


(mac %eval body
  (eval `(do ,@body)))
