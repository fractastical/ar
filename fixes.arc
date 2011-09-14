;=============================================================================
;  This file fixes various flaws that I have discovered in ar or Arc
;=============================================================================

(mac remac (name parms . body)
  `(let orig (rep ,name)
     (= (sig ',name) ',parms)
     (= ,name (annotate 'mac (fn ,parms ,@body)))))

(remac redef (name args . body)
  `(let orig ,name
     (= (sig ',name) ',args)
     (= ,name (fn ,args ,@body))))

(remac safeset (var val)
  `(do (redefine-warning ',var)
       (= ,var ,val)))

(remac extend (name arglist test . body)
  (w/uniq args
    `(let orig ,name
       (= ,name (fn ,args
                  ;; TODO: hacky
                  (apply (fn ,arglist
                           (aif ,test
                                  (do ,@body)
                                (apply orig ,args)))
                         ,args))))))


(redef sym args
  (coerce (apply string args) 'sym))


;; TODO: should really be in ac.arc...
(redef ac-assign1 (a b1 env)
  (unless (ar-tnil racket-symbol?.a)
    (err "First arg to assign must be a symbol" a))
  (let result (ac b1 env)
    ;(debug a result "\n")
    `(racket-begin ,(if (ac-lex? a env)
                          (list 'racket-set! a result)
                        (ac-global-assign a result))
                   ,result)))

;; TODO: should really be in ac.arc...
(redef ar-apply-non-fn (x args)
  ;(debug type.x args)
  (case type.x
    list   (racket-mlist-ref x car.args)
    string (racket-string-ref x car.args)
    table  (racket-hash-ref x car.args cadr.args)
           (orig x args)))


(redef empty (seq)
  (or (no seq)
      (is seq '||)
      (is len.seq 0)))


(redef dedup (xs (o f idfn))
  (collect:let h (obj)
    (each x xs
      (let k f.x
        (unless h.k
          (yield x)
          (= h.k t))))))


(redef isa (x y . args)
  ;(assign x (type x))
  (zap type x)
  (or (is x y)
      ;; can't use `some`, because that causes an infinite loop
      (if args (reclist (^:is x car._) args))))


;; TODO: should this use dopair...?
(remac in (x . choices)
  (if (cdr choices)
        (w/uniq g
          `(let ,g ,x
             (or ,@(map1 (fn (c) `(is ,g ,c)) choices))))
      `(is ,x ,(car choices))))


(redef expand=list (terms)
  (if (cddr terms)
        `(do ,@(mapeach (p v) (pair terms)
                 (expand= p v)))
      (apply expand= terms)))


(redef avg ns (/ (apply + ns) (len ns)))


(extend coerce (x y . r) (and (isa x 'table)
                              (is y 'cons))
  (tablist x))

(extend coerce (x y . r) (and (isa x 'cons)
                              (is y 'table))
  (listtab x))


(remac zap (f x . args)
  `(= ,x (,f ,x ,@args)))
