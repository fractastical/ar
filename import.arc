;(use runtime)
;(use object)
(use utils fail)

#|(let orig ac-call
  (def ac-call (fn args env)
    (debug "ac-call" fn args env)
    (orig fn args env)))|#

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

#|(def racket-del (name k)
  ((ail-code namespace-undefine-variable!)
    k
    name))|#


;; should be in runtime.arc
#|(extend ar-apply-non-fn (x args) (and (isa x 'runtime)
                                      (ac-zeroarg*:car args))
  (debug "zeroarg" x args)
  ((orig x args)))|#

(implicit namespace (annotate 'namespace list.runtime*))

(def new-namespace ((o parent namespace))
  ;; TODO: ew
  (unless (acons rep.parent)
    (zap list parent))
  ;(debug "new-namespace" parent)
  (annotate 'namespace (cons (racket-make-base-empty-namespace) rep.parent)))


;; TODO: hacky
(extend ar-apply-non-fn (x args) (isa x 'mac)
  (zap rep:rep x)
  ;(debug x)
  (eval (apply car.x args) cadr.x))

;; TODO: incredibly hacky
(extend ar-apply-non-fn (x args) (isa x 'mac-wrapper)
  (apply (car rep.x) args))

;; TODO: incredibly hacky
(remac mac (name parms . body)
  `(do (sref sig ',parms ',name)
       (safeset ,name
         (annotate 'mac
           (annotate 'mac-wrapper
             (list (fn ,parms ,@body)
                   ',namespace))))))

#|(remac mac (name parms . body)
  `(do (sref sig ',parms ',name)
       (safeset ,name (annotate 'mac (object type      'fn
                                             call      (fn ,parms ,@body)
                                             namespace namespace)))))|#




;; TODO: use runtime.arc
(extend ar-apply-non-fn (x args) (ar-tnil:racket-namespace? x)
  (let (k d) args
    (let x (racket-namespace-variable-value
             k
             (ail-code #t) ;; TODO: racket-#t
             d
             x)
      ;(debug "ar-apply-non-fn" k x)
      ;; TODO: ew
      (if ac-zeroarg*.k
            (x)
          x))))

(extend ar-apply-non-fn (x args) (isa x 'namespace)
  ;(debug x args)
  (let (k d) args
    ((afn (x)
       ;(debug "ar-apply-non-fn" x)
       (if x (car.x k (fn () (self cdr.x)))
             d))
     rep.x)))

(extend keys (x) (isa x 'namespace)
  (ar-toarc:racket-namespace-mapped-symbols:car rep.x))

(extend len (x) (isa x 'namespace) (len:keys x))

(extend maptable (f tab) (isa tab 'namespace)
  (do (each k keys.tab
        (f k (tab k)))
      tab))

(extend sref (x v k) (isa x 'namespace)
  (zap rep x)
  ;(debug "sref" x k v)
  #|(case type.v
    mac (= v (annotate 'mac (fn args (apply ) ))))|#
  ((ail-code racket-namespace-set-variable-value!)
    k
    v
    (ail-code #t)
    car.x)
  v)

#|(extend dref (x k . rest) (isa x 'namespace)
  ;(debug x k)
  (let n x.k
    ((ail-code racket-namespace-undefine-variable!)
      k
      (car rep.x))
    n))|#

(extend dref (x k . rest) (isa x 'namespace)
  ((ail-code racket-namespace-undefine-variable!)
    k
    (car rep.x))
  x)

(extend print (primitive x port) (isa x 'namespace)
  (disp "#<namespace>" port))


#|(def lookup-in-namespace (it k)
  ;(debug it cdr.it)
  (racket-namespace-variable-value
    k
    (ail-code #t)
    (fn ()
      (if it (lookup-in-namespace cadr.it k)
             (err "undefined variable:" k)))
    car.it))|#

(def lookup-in-namespace (it k)
  ;(debug "lookup" it k)
  (isnt/fail val (it k fail)
    (do ;(debug "get" rep.it rep.namespace k val)
        val)
    (err "undefined variable:" k)))

(extend ac-global (k) namespace
  ;(debug "ac-global" k)
  `(,lookup-in-namespace ,it (racket-quote ,k))) ;(racket-q

(extend ac-global-assign (k v) namespace
  ;(debug "set" it rep.it k v)
  `(,sref ,it ,v (racket-quote ,k)))

;; TODO: (isa x 'table 'namespace)
(extend eval (x (o env)) (in type.env 'table 'namespace) ;(table? env)
  (w/namespace env orig.x))


#|
(def set-in-namespace (it k v)
  ((ail-code racket-namespace-set-variable-value!)
    k
    v
    (ail-code #t)
    car.it))

;; TODO: should this use the value or name of lookup-in-namespace ?
(extend ac-global (k) namespace
  `(,lookup-in-namespace ,(ar-tunnel it) (racket-quote ,k)))

(extend ac-global-assign (k v) namespace
  `(,set-in-namespace ,(ar-tunnel:it) (racket-quote ,k) ,v))
|#


(mac w/eval (x . body)
  `(eval '(do ,@body) ,x))

(mac import args
  (w/uniq env
    `(do ,@(mapeach x args
             `(let ,env (new-namespace)
                (w/eval ,env (use ,x))
                (= ,x ,env)))
         nil)))

(mac import-as args
  (w/uniq env
    `(do ,@(mapeach (n v) pair.args
             `(let ,env (new-namespace)
                ;(dletif namespace ,env (load ,v))
                (w/namespace ,env (load ,v))
                (= ,@(if (cons? n)
                           (mappend (fn (x)
                                      `(,x (,env ',x)))
                                    n)
                         (list n env)))))
         nil)))

(mac load-ver args
  (w/uniq (x env)
    `(do ,@(mapeach (n v) (pair args)
             `(let ,env (new-namespace)
                (dletif namespace ,env (load ,n))
                (each ,x (,env ',v)
                  (eval `(= ,,x (,,env ,,x))))))
         nil)))

(mac import-ver args
  (w/uniq (x env)
    `(do ,@(mapeach (n v) (pair args)
             `(let ,env (new-namespace)
                (dletif namespace ,env (load ,n))
                (= ,n (new-namespace))
                (each ,x (,env ',v)
                  (= (,n ,x) (,env ,x)))))
         nil)))

#|`(let ,env (new-namespace)
                    (w/namespace ,env (load ,v))
                    (= ,n ,env))|#
