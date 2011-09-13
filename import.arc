(use del fail)

(implicit namespace (annotate 'namespace list.runtime*))

(use runtime)

(def new-namespace ((o parent namespace))
  ;; TODO: ew
  (unless (acons rep.parent)
    (zap list parent))
  (annotate 'namespace (cons (racket-make-empty-namespace) rep.parent)))

#|
;; TODO: hacky
(extend ar-apply-non-fn (x args) (isa x 'mac)
  )|#

(defcall mac (x . args)
  (debug "apply mac!" x args)
  (zap rep:rep x)
  (if acons.x
        (eval (apply car.x args) cadr.x)
      (eval (apply x args) runtime*)))

#|(redef ac-mac-call (m args env)
  ;(debug "ac-mac-call!" m args env)
  (zap rep m)
  (if acons.m
        (eval (apply car.m args) cadr.m)
      (orig m args env)))|#

;; TODO: incredibly hacky
(defcall mac-wrapper (x . args)
;(extend ar-apply-non-fn (x args) (isa x 'mac-wrapper)
  ;(debug "apply wrapper!" x args)
  (apply (car rep.x) args))

;; TODO: incredibly hacky
; (remac mac
(= mac (annotate 'mac (fn (name parms . body)
  `(do (sref sig ',parms ',name)
       (safeset ,name
         (annotate 'mac
           (annotate 'mac-wrapper
             (list (fn ,parms ,@body)
                   ',namespace))))))))

(defcall namespace (x k (o d))
  ((afn (x)
     ;(debug car.x k d)
     #|(isnt/fail x (car.x k fail)
       x
       (self cdr.x))|#
     (if x (car.x k (fn () (self cdr.x)))
           d))
   rep.x))

(extend keys (x) (isa x 'namespace)
  (ar-toarc:racket-namespace-mapped-symbols:car rep.x))

(extend len (x) (isa x 'namespace) (len:keys x))

(extend maptable (f tab) (isa tab 'namespace)
  (each k keys.tab
    (f k tab.k))
  tab)

(extend sref (x v . rest) (isa x 'namespace)
  ;(debug x k x!ac-var-assigner*.k)
  ;(parameter? x.k)
  ;(debug "namespace sref" rep.x v rest)
  (apply orig (car rep.x) v rest)
  v)

(extend dref (x k . rest) (isa x 'namespace)
  ((ail-code racket-namespace-undefine-variable!)
    k
    (car rep.x))
  x)

(extend print (primitive x port) (isa x 'namespace)
  (disp "#<namespace>" port))


(def lookup-in-namespace (it k)
  ;(debug rep.it k (it k))
  (isnt/fail val (it k fail)
    (do ;(debug "get" rep.it k val parameter?.val)
        val)
    (err "undefined variable:" k)))

(extend ar-var (k (o d)) namespace
  ;(debug "ar-var" it k d)
  (it k d))

(extend ac-global (k) namespace
  ;; TODO: should this use the value or name of lookup-in-namespace ?
  (let x `(,lookup-in-namespace ,it (racket-quote ,k))
    (if ac-zeroarg*.k  `(,x)
                        x)))

(extend ac-global-assign (k v) namespace
  `(,sref ,it ,v (racket-quote ,k)))

;; TODO: (isa x 'table 'namespace)
(extend eval (x (o env)) (in type.env 'table 'namespace) ;; TODO: (table? env)
  (w/namespace env orig.x))


(mac w/eval (x . body)
  `(eval '(do ,@body) ,x))

(mac import args
  (w/uniq env
    `(do ,@(map (fn (x)
                  `(let ,env (new-namespace)
                     (w/eval ,env (use ,x))
                     (= ,x ,env)))
                args)
         nil)))

(mac import-as args
  (w/uniq env
    `(do ,@(map (fn ((n v))
                  `(let ,env (new-namespace)
                     ;; TODO: (dletif namespace ,env (load ,v)) ?
                     (w/namespace ,env (load ,v))
                     (= ,@(if cons?.n
                                (map (fn (x)
                                       `(,x (,env ',x)))
                                     n)
                              (list n env)))))
                pair.args)
         nil)))

(mac load-ver args
  (w/uniq (x env)
    `(do ,@(map (fn ((n v))
                  `(let ,env (new-namespace)
                     (dletif namespace ,env (load ,n))
                     (each ,x (,env ',v)
                       (eval `(= ,,x (,,env ,,x))))))
                pair.args)
         nil)))

(mac import-ver args
  (w/uniq (x env)
    `(do ,@(map (fn ((n v))
                  `(let ,env (new-namespace)
                     (dletif namespace ,env (load ,n))
                     (= ,n (new-namespace))
                     (each ,x (,env ',v)
                       (= (,n ,x) (,env ,x)))))
                pair.args)
         nil)))
