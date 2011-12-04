(make-w/ load-suffix*)
(make-w/ load-paths*)
(make-w/ namespace)
#|(implicit load-paths* (list cwd (abspath) (abspath "lib/")))
(implicit load-suffix* ".arc")

(def load-file-dir (x)
  (car:mem (fn (y)
             (file-exists (joinpath y x)))
           load-paths*))|#

(make-implicit cwd
  (racket-make-derived-parameter (%nocompile racket-current-directory)
    (fn (v) (zap string v)
            (if empty.v
                  (racket-current-directory)
                (racket-expand-user-path v)))
    (fn (v) (racket-path->string v))))

(def expandpath (x)
  (zap string x)
  (if empty.x
        x
      (racket-path->string:racket-expand-user-path x)))

(def joinpath args
  (racket-path->string:apply racket-build-path
    (aloop (x   args
            acc nil)
      (if (no x)
            rev.acc
          (let c (expandpath car.x)
            (if (is c.0 #\/)
                  (self cdr.x (cons c nil))
                (self cdr.x (cons c acc))))))))

;; TODO: ew
(redef load-file-dir (x)
  ;; this is just (car:mem [file-exists (joinpath _ x)] load-paths*)
  (find [file-exists:joinpath _ x] load-paths*))
#|  (aloop (xs load-paths*)
    (if (no xs)
          nil
        (file-exists:joinpath car.xs x)
          (car xs)
        (self cdr.xs))))
|#


(def new-namespace args
  (annotate 'namespace
            (cons (racket-make-empty-namespace)
                  (mappend (fn (x)
                             (let x (rep x)
                               (if (cons? x)
                                     x
                                   (list x))))
                           args))))

;; TODO: should use object.arc
;; TODO: extend should work with keyword args
(defcall namespace (x k (o d))
;(extend ac-apply-non-fn (x k (o d)) (%nocompile (isa x (racket-quote namespace)))
  ;(%nocompile (racket-displayln x))
  (let self (%nocompile nil)
    (assign self (fn (x)
                   (if x ((%nocompile (car x))
                          k
                          (fn ()
                            (self (%nocompile (cdr x)))))
                         (if (%nocompile (fn? d))
                               (d)
                             d))))
    (self (%nocompile (rep x))))

  #|(aloop (x (%nocompile (rep x)))
    )|#
  #|(aloop (x rep.x)
    (car.x k (fn ()
               (if cdr.x  (self cdr.x)
                          d))))|#
    #|(namespace-get car.x car.args
      (fn ()
        (if cdr.x  cadr.args
                   (self cdr.x))))|#
  )

(extend sref (x v k) (isa x 'namespace)
  ;(prn x)
  (sref (car rep.x) v k))


;(implicit namespace)
#|
(unless ac-direct-globals
  #|
  (extend ac-lookup-global (space k) (%nocompile (namespace))
    ;(%nocompile (prn k " " it))
    ;(%nocompile (racket-displayln k))
    ;(%nocompile (racket-displayln it))
    #|
    ;; TODO: w/fail and isnt/fail and fail?
    (withs (fail  (%nocompile (uniq))
            v     (it k fail))
      (if (%nocompile (is v fail))
            (%nocompile (err "undefined variable:" k))
          v))|#
          ;; TODO: shouldn't this be wrapped in %nocompile...?
    (it k (ac-undefined-var k)))
  |#
#|
  (extend ac-global-assign (a b) namespace
    (let x (it a)
      ;; This allows annotate to assign a name to functions

      ;; This implements implicit parameters
      ;; TODO: this should probably do the check at runtime,
      ;;       rather than at compile time
      `(racket-parameterize ((ac-assign-name (racket-quote ,a)))
         ,(if (ac-tnil:racket-parameter? x)
                (list x (ac-compile b))
              `(,sref ,it ,(ac-compile b) (racket-quote ,a))))))|#
  )|#

(mac eval-w/ (x . body)
  `(w/namespace ,x
     (maplast eval ',body)))


(mac import args
  `(do ,@(map (fn (x)
                (let x (load-normalize-path string.x)
                  ;; TODO: use w/cwd ...?
                  `(do (w/cwd ,load-file-dir.x
                       ;`(parameterize (racket-current-directory ,load-file-dir.x))
                         (load ,x))
                       (namespace (new-namespace namespace)))))
              args)
       ;(namespace (new-namespace namespace))
       nil))
