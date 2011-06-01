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
                     ;(prn "grabbing " k)
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


#|(ac-def ac-var-ref (s env)
  (if ((g ar-true) ((g ac-lex?) s env))
       s
       ((g ac-global) s)))|#

#|(defrule ac-global-assign (ac-defined-var a)
  `(,(or (cadr it) (ac-not-assignable a)) ,b))|#

#|(extend ac-var-ref (x env) namespace
  )

(extend ac-assign ((x y) env) namespace
  )|#

(mac racket-q (x)
  `(racket (racket-quasiquote
             (racket-quote
               (racket-unquote ,x)))))

#|(extend ac-global (k) namespace
  ;(prn "grabbing " k)
  ;(let val (it k fail)
    ;(racket-q val))
  (isnt/fail val (it k fail)
    (racket-q val)
    (err "undefined variable:" k)))|#

#|(#(tagged mac #<procedure>) g1953 (apply #<namespace> (racket-quote qux) fail nil)
  g1953
  (#<procedure:error> undefined variable: (racket-quote qux)))

(apply #<namespace> (racket-quote qux) ,fail nil)|#

#|(let ,(racket-q val) (,it ,(racket-q k))
  (if ,(racket-q val)
    ,(racket-q val)
    (,err "undefined variable:" ,(racket-q k))))|#

(def lookup-in-namespace (k it)
  (isnt/fail val (it k fail)
    val
    (err "undefined variable:" k)))

(extend ac-global (k) namespace
  ;(prn k " " it " " (it k))
  ;(w/uniq val
    #|(prn `(,isnt/fail ,val (apply ,it ,(racket-q k) fail nil)
           ,val
           (,err "undefined variable:" ,(racket-q k))))|#
  `(,lookup-in-namespace ,(racket-q k) ,it))
    #|`(do (,prn "ac-global " ,(racket-q k) " " ,it " "
                       (,lookup-in-namespace ,(racket-q k) ,it))
         ;(prn (racket-quote (,it ,(racket-q k))))
         ))|#

         #|
         (,eval (,iflet ,(racket-q val) (racket-quote (,it ,(racket-q k)))
                  ,(racket-q val)
                  (,err "undefined variable:" ,(racket-q k) " " ,(racket-q val)))))))|#
         #|(,eval '(,isnt/fail ,(racket-q val) (,it ,(racket-q k) ,(racket-q fail))
                   ,(racket-q val)
                   (,err "undefined variable:" ,(racket-q k)))
           ))))|#
  #|(fn (k)
    (isnt/fail val (it k fail)
      val
      (err "undefined variable:" k))))|#
  #|(w/uniq val
    (',isnt/fail ,val (',it ',k fail)
     ',val
     ;(racket-q val)
     (,err "undefined variable:" ',k)))))|#

;; should use ac-global and current-env instead
#|
(extend ac-var-ref (k env) namespace
  (prn k " " env)
  (if (ac-lex? k env)
        k
      (isnt/fail val (it k fail)
        (racket-q val))))
        ;(unless env
          ;(err "undefined variable:" k)))))
|#

(extend ac-global-assign (k v) namespace
  ;(prn k " " v)
  ;(prn `(,= (,it ',k) ,v))
  ;`(do (,= (,namespace ,k) ,v) nil)
  ;nil
  ;(= (it k) nil)
  `(,sref ,it ,v ,(racket-q k)))

#|(extend ac-assign1 (k v env) namespace
  ;(prn k " " v " " env)
  (let v (eval v)
    (= (it k) v)
    (racket-q v)))|#
      #|(racket-quasiquote
        (racket-quote
          (racket-unquote v))))))|#
  #|(racket
    (racket-quasiquote
      (racket-quote
        (racket-unquote v)))))|#
  ;`(racket-quote ,v))
    ;)
    ;`(do ;(,= (,it ',k) ,v)
         ;',v)
  #|
  (unless (symbol? a)
    ((g err) "First arg to assign must be a symbol" a))
  (let ((result (gensym)))
    (arc-list 'racket-let
              (arc-list (arc-list result ((g ac) b1 env)))
              (if ((g ar-true) ((g ac-lex?) a env))
                   (arc-list 'racket-set! a result)
                   ((g ac-global-assign) a result))
              result))))|#

(extend eval (x (o env)) (isa env 'table)
  (w/namespace env (orig x)))

#|(extend ac-assign1 (a b1 env) (isa env 'table)
  (prn a)
  (prn b1)
  )

(extend eval (x (o env)) (isa env 'table)
  (orig x (obj )))|#

#|(defrule ac-global (ac-defined-var v)
  `(,(car it)))|#


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
