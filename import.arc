#|(parameter load-paths* (list cwd (abspath) (abspath "lib/")))
(parameter load-suffix* ".arc")

(def load-file-dir (x)
  (car:mem (fn (y)
             (file-exists (joinpath y x)))
           load-paths*))|#

(make-parameter cwd
  (racket-make-derived-parameter racket-current-directory
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


(def make-path->string (converter)
  (fn (x)
    (zap string x)
    (unless empty.x
      (zap converter x)
      (when ac-tnil.x
        (racket-path->string x)))))

(= dirname  (make-path->string racket-path-only:expandpath))
(= basename (make-path->string racket-file-name-from-path:expandpath))


(def abspath ((o x))
  ;(joinpath cwd x)
  (racket-path->string:racket-path->complete-path expandpath.x))


;; TODO: ew
;; TODO: throw an error when the file can't be found
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


;(parameter namespace)
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

      ;; This implements parameters
      ;; TODO: this should probably do the check at runtime,
      ;;       rather than at compile time
      `(racket-parameterize ((ac-assign-name (racket-quote ,a)))
         ,(if (ac-tnil:racket-parameter? x)
                (list x (ac-compile b))
              `(,sref ,it ,(ac-compile b) (racket-quote ,a))))))|#
  )|#

(parameter load-automatic-namespaces*)

#|(extend ac-global-assign-defined (x a b) (and load-automatic-namespaces*
                                              (nor (in type.x 'parameter 'alias)
                                                   (in a      'thatexpr  'that)))
  ;(zap new-namespace namespace)
  ;(orig nil 'namespace (new-namespace namespace))
  (ac-namespace (new-namespace (ac-namespace)))
  (load-automatic-namespaces* nil)
  (orig x a b))|#


;; TODO: not sure about this
(let u (uniq)
  (extend bound (x) (isa namespace 'namespace)
    (isnt (namespace-get (car rep.namespace) x u) u)))

(remac safeset (var val)
  (if load-automatic-namespaces*
        `(do (when (bound ',var)
               (zap new-namespace namespace)
               (disp "*** creating new namespace to avoid redefining " stderr)
               (disp ',var stderr)
               (disp #\newline stderr))
             (= ,var ,val))
      (orig var val)))


(mac eval-w/ (x . body)
  `(w/namespace ,x
     (maplast eval ',body)))

(mac w/arc3 body
  `(eval-w/ arc3-namespace ,@body))


#|(= imported-namespaces*
   ;; TODO: ew
   (listtab:list (list (joinpath exec-dir* "compiler.arc") arc3-namespace)
                 (list (joinpath exec-dir* "core.arc")     arc3-namespace)
                 (list (joinpath exec-dir* "ssyntax.arc")  arc3-namespace)
                 (list (joinpath exec-dir* "compat.arc")   arc3-namespace)
                 (list (joinpath exec-dir* "arc.arc")      arc3-namespace)
                 (list (joinpath exec-dir* "extra.arc")    arc3-namespace)
                 (list (joinpath exec-dir* "import.arc")   arc3-namespace)))|#

(def importfn (args)
  (w/load-paths* load-paths*
    (each x args
      (if (dirname x)
            ;; TODO: use dont
            (do (push abspath.x load-paths*)
                nil)
          (withs (x     (load-normalize-path string.x)
                  path  abspath.x)
            (prn "loading: " path)
            ;; TODO: use w/cwd ...?
            ;`(parameterize (racket-current-directory ,load-file-dir.x))
            (w/cwd load-file-dir.x
              (w/load-automatic-namespaces* t (load x))
              #|(aif imported-namespaces*.path
                                ;; TODO: should namespace come first or second...?
                     ;; TODO: fix the huge explosion of namespaces when you
                     ;;       import a module that was already imported
                     ;(namespace (new-namespace namespace it))
                     nil
                   (do (load x)
                       ;(zap new-namespace namespace)
                       #|(= namespace (new-namespace
                         (= imported-namespaces*.path namespace)))|#
                       ))|#
              ))))))

(mac import args
  #|
  ;; TODO: use dont
  `(do
       ;(namespace (new-namespace namespace))
       nil)|#
  ;`(importfn ,@(map string args))
  `(importfn ',args))
