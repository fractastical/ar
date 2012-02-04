(mac redef (name parms . body)
  `(let orig ,name
     (= ,name (fn ,parms ,@body))))

(mac remac (name parms . body)
  `(let orig (rep ,name)
     (= ,name (annotate 'mac (fn ,parms ,@body)))))

(mac extend (name parms test . body)
  (w/uniq (args a b)
    `(withs (orig  ,name
             ,a    (fn ,parms ,test)
             ,b    (fn ,(cons 'it parms) ,@body))
       (= ,name (fn ,args
                  (aif (apply ,a ,args)
                       (apply ,b it ,args)
                       (apply orig ,args)))))))

#|(mac extend (name parms test . body)
  (w/uniq args
    `(redef ,name ,args
       (aif (apply ,(fn ,parms ,test) ,args)
            (apply ,(fn ,parms ,@body) ,args)
            (apply orig ,args)))))|#


(= ref-types* (obj))

(extend ref (x . args) (orig ref-types* (type x))
  (apply it x args))

(mac defcall (type parms . body)
  `(= (ref-types* ',type) (fn ,parms ,@body)))


(def eachfn (f xs)
  (each x xs f.x))


(def hash args
  (listtab pair.args))


(def assoc-ref (xs key)
  (if (atom xs)
        nil
      (and (acons (car xs)) (is (caar xs) key))
        (car xs)
      (assoc-ref (cdr xs) key)))


(def debug args
  ;(apply prn (intersperse " " args))
  nil)


(mac %no (x)
  (cons %.nocompile x))


(def var (x)
  ((%.namespace) x))

(extend sref (x v k) (is x var)
  (sref (%.namespace) v k))


(mac %require args
  `(% (parameterize ((current-namespace nu-namespace))
        (namespace-require ,@(map (fn (x) `',x) args)))))

(mac require-rename args
  `((% namespace-require) ,@(map (fn ((x y)) `'(rename racket/base ,y ,x)) pair.args)))
