(use utils del fail)

(implicit namespace (annotate 'namespace list.runtime*))

(def new-namespace ((o parent namespace))
  ;; TODO: ew
  (unless (acons rep.parent)
    (zap list parent))
  (annotate 'namespace (cons (racket-make-base-empty-namespace) rep.parent)))


;; TODO: hacky
(extend ar-apply-non-fn (x args) (isa x 'mac)
  (zap rep:rep x)
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


;; TODO: use runtime.arc
(extend ar-apply-non-fn (x args) (ar-tnil:racket-namespace? x)
  (let (k d) args
    (let x (racket-namespace-variable-value
             k
             (ail-code #t) ;; TODO: racket-#t
             d
             x)
      ;; TODO: ew
      (if ac-zeroarg*.k
            (x)
          x))))

(extend ar-apply-non-fn (x args) (isa x 'namespace)
  (let (k d) args
    ((afn (x)
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
  ((ail-code racket-namespace-set-variable-value!)
    k
    v
    (ail-code #t)
    car.x)
  v)

(extend dref (x k . rest) (isa x 'namespace)
  ((ail-code racket-namespace-undefine-variable!)
    k
    (car rep.x))
  x)

(extend print (primitive x port) (isa x 'namespace)
  (disp "#<namespace>" port))


(def lookup-in-namespace (it k)
  (isnt/fail val (it k fail)
    val
    (err "undefined variable:" k)))

(extend ac-global (k) namespace
  ;; TODO: should this use the value or name of lookup-in-namespace ?
  `(,lookup-in-namespace ,it (racket-quote ,k)))

(extend ac-global-assign (k v) namespace
  `(,sref ,it ,v (racket-quote ,k)))

;; TODO: (isa x 'table 'namespace)
(extend eval (x (o env)) (in type.env 'table 'namespace) ;; TODO: (table? env)
  (w/namespace env orig.x))


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
                ;; TODO: (dletif namespace ,env (load ,v)) ?
                (w/namespace ,env (load ,v))
                (= ,@(if cons?.n
                           (mappeach x n
                             `(,x (,env ',x)))
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
