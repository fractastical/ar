(use object runtime)

; should be in arc.arc
(mac mapeach (var val . body)
  `(map (fn (,var) ,@body) ,val))


#|
; look into replacing these with ar equivalents
(def racket-get (name k (o d fail))
  (racket-namespace-variable-value
    k
    racket-#t
    (fn () d)
    name))

(def racket-set (name k (o v))
  ((ail-code namespace-set-variable-value!)
    k
    v
    racket-#t
    name))|#

(def racket-del (name k)
  ((ail-code namespace-undefine-variable!)
    k
    name))


;(let defn runtime* ;((this-namespace) 'racket-namespace*)
(= __built-ins* (object call  (fn (k (o d fail))
                                (runtime* k d))

                        set   (fn (k (o v))
                                (= (runtime* k) v))

                        del   (fn (k)
                                (racket-del runtime* k))

                        print (fn () "#<namespace __built-ins*>")))


(implicit namespace)


(defcall mac (x . args)
  (eval (apply (rep x) args) namespace))


(def new-namespace ((o parent __built-ins*))
  (let child (table)
    (object parent parent

            call   (fn (k (o d fail))
                     (isnt/fail val (get-attribute child k)
                       #|(if (in (type val) 'fn)
                             (fn args
                               (w/namespace self
                                 (apply val args)))
                           val)|#
                       val
                       (self<-parent k d)))

            keys   (fn ()
                     (keys child))

            set    (fn (k (o v))
                     (let self self
                       (set-attribute child k
                         (case (type v)
                           fn  (fn args
                                 (apply call-w/self self v args))
                                      #|(w/namespace self
                                        (apply v args))|#
                           mac (annotate 'mac (fn args
                                 (w/namespace self
                                   (eval (apply (rep v) args)))))
                               v)))
                     #|(when (in (type v) 'fn)
                       (= v (fn args
                              (w/namespace self
                                (apply v args)))))|#
                     )

            del    (fn (k)
                     (del-attribute child k))

            print  (fn () "#<namespace>"))))


(def lookup-in-namespace (k it)
  (isnt/fail val (it k fail)
    val
    (err "undefined variable:" k)))

#|
; look into replacing this with an ar equivalent
(mac racket-q (x)
  `(ail-code (racket-quasiquote
               (racket-quote
                 (racket-unquote ,x)))))|#

(extend ac-global (k) namespace
  `(,lookup-in-namespace (racket-quote ,k) ,it)) ;(racket-q

(extend ac-global-assign (k v) namespace
  `(,sref ,it ,v (racket-quote ,k))) ;(racket-q

(extend eval (x (o env)) (isa env 'table)
  (w/namespace env (orig x)))


(mac w/eval (x . body)
  `(eval '(do ,@body) ,x))

(mac import args
  (w/uniq env
    `(do ,@(mapeach (n v) (pair args)
             (if (isa n 'cons)
                   `(let ,env (new-namespace)
                      (w/namespace ,env (load ,v))
                      (= ,@(mappend (fn (x)
                                      `(,x (,env ',x)))
                                    n))))
                 `(let ,env (new-namespace)
                    (w/namespace ,env (load ,v))
                    (= ,n ,env)))
         nil)))
