(load "object.arc")

(def racket-get (name k (o d fail))
  (racket-namespace-variable-value
    k
    racket-#t
    (fn () d)
    name))

(def racket-set (name k (o v))
  ((racket namespace-set-variable-value!)
    k
    v
    racket-#t
    name))

(def racket-del (name k)
  ((racket namespace-undefine-variable!)
    k
    name))


(let defn ((this-namespace) 'racket-namespace*)
  (= __built-ins*
    (object call  (fn (k (o d fail))
                    (racket-get defn k)) ; d

            set   (fn (k (o v))
                    (racket-set defn k v))

            del   (fn (k)
                    (racket-del defn k))

            print (fn () "#<namespace __built-ins*>"))))


(implicit namespace)


(def new-namespace ((o parent __built-ins*))
  (let child (table)
    (object parent parent

            call   (fn (k (o d fail))
                     (isnt/fail val (get-attribute child k)
                       (if (in (type val) 'fn)
                             (fn args
                               (w/namespace self
                                 (apply val args)))
                             val)
                       (self<-parent k d)))

            keys   (fn ()
                     (keys child))

            set    (fn (k (o v))
                     #|(when (in (type v) 'fn)
                       (= v (fn args
                              (w/namespace self
                                (apply v args)))))|#
                     (set-attribute child k v))

            del    (fn (k)
                     (del-attribute child k))

            print  (fn () "#<namespace>"))))


(def lookup-in-namespace (k it)
  (isnt/fail val (it k fail)
    val
    (err "undefined variable:" k)))

(mac racket-q (x)
  `(racket (racket-quasiquote
             (racket-quote
               (racket-unquote ,x)))))

(extend ac-global (k) namespace
  `(,lookup-in-namespace ,(racket-q k) ,it))

(extend ac-global-assign (k v) namespace
  `(,sref ,it ,v ,(racket-q k)))

(extend eval (x (o env)) (isa env 'table)
  (w/namespace env (orig x)))


; should be in arc.arc
(mac mapeach (var val . body)
  `(map (fn (,var) ,@body) ,val))

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
