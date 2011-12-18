(make-parameter cwd
  (racket-make-derived-parameter racket-current-directory
    (fn (v) (zap string v)
            (if empty.v
                  (racket-current-directory)
                (racket-expand-user-path v)))
    (fn (v) (string v))))

(def expandpath (x)
  (zap string x)
  (if empty.x
        x
      (string:racket-expand-user-path x)))

(def joinpath args
  (string:apply racket-build-path
    (aloop (x   args
            acc nil)
      (if (no x)
            nrev.acc
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
        (string x)))))

(= dirname  (make-path->string racket-path-only:expandpath))
(= basename (make-path->string racket-file-name-from-path:expandpath))


(def abspath ((o x))
  (string:racket-path->complete-path expandpath.x))


;; TODO: ew
;; TODO: throw an error when the file can't be found
(redef load-file-dir (x)
  ;; this is just (car:mem [file-exists (joinpath _ x)] load-paths*)
  (find [file-exists:joinpath _ x] load-paths*))


(parameter namespace-load-type* 'inherit)

(def new-namespace args
  (let args (mappend listify:rep args)
    (case namespace-load-type*
      ;package (apply namespace-package args)
      inherit (apply namespace-inherit args)
      ;copy    (apply namespace-copy    args)
              (err "unknown namespace creation type:" namespace-load-type*))))

(mac w/new-namespace (x . body)
  #`(w/namespace (new-namespace x) ,@body))


#|(def namespace-copy1 ((o x   (racket-current-namespace))
                      (o new (racket-make-empty-namespace)))
  (each n (racket-namespace-mapped-symbols x) ;(racket-list->mlist )
    (namespace-set new n (namespace-get x n)))
  new)

(def namespace-copy args
  (let new (racket-make-base-empty-namespace)
    ;(prn (racket-namespace-mapped-symbols new))
    (each x nrev.args
      (namespace-copy1 x new))
    (parameterize (racket-current-namespace new)
      (ac-require-base))
    new))|#


(def namespace-inherit args
  (annotate 'namespace (cons (racket-make-empty-namespace) args)))

;; TODO: should use object.arc
;; TODO: extend should work with keyword args
(defcall namespace (x k (o d))
  ;; TODO: use afn or aloop
  (let self nil ;(%nocompile )
    (= self (fn (x)
              (if x ((car x) ;(%nocompile )
                     k
                     (fn ()
                       (self (cdr x)))) ;(%nocompile )
                    (if (fn? d) ;(%nocompile )
                          (d)
                        d))))
    (self (rep x))) ;(%nocompile )
  )

(extend sref (x v k) (isa x 'namespace)
  (sref (car rep.x) v k))

(extend print (primitive x port) (isa x 'namespace)
  (disp "#<namespace (len " port)
  (disp (len rep.x) port)
  (disp ")>" port))

#|
;; TODO: need a better name
(def namespace-partition ((o x))
  (case namespace-load-type*
    ;; TODO: a little hacky
    inherit (if x (= namespace x)
                  (zap new-namespace namespace))
    copy    (if x (do (racket-current-namespace x)
                      (= namespace x))
                  (let new (apply namespace-copy (listify rep.namespace))
                    (racket-current-namespace new)
                    (= namespace new)
                    new))
            (err "unknown namespace load type:" namespace-load-type*)))|#


(parameter load-automatic-namespaces*)

;; TODO: not sure about this
(let u (uniq)
  (extend bound (x) (isa namespace 'namespace)
    (isnt (namespace-get (car rep.namespace) x u) u)))

(redef redefine-warning (var)
  (if load-automatic-namespaces*
        (do (zap new-namespace namespace)
            ;(namespace-partition)
            (disp "*** creating new namespace to avoid redefining " stderr)
            (disp var stderr)
            (disp #\newline stderr))
      (orig var)))

#|(remac safeset (var val)
  (if load-automatic-namespaces*
        #`(do (when (bound ',var)
                                   ;; TODO: does namespace need to be quoted?
                (zap new-namespace 'namespace)
                ;(namespace-partition)
                (disp "*** creating new namespace to avoid redefining " stderr)
                (disp ',var stderr)
                (disp #\newline stderr))
              (= var val))
      (orig var val)))|#


(mac eval-w/ (x . body)
  #`(w/namespace x
      (maplast eval ',body)))

(mac w/arc3 body
  #`(eval-w/ arc3-namespace ,@body))


;; TODO: could use a better name
(def namespace-fn (x (o f idfn))
  (annotate 'namespace (f:rep x)))

;; TODO: could use a better name
(def namespace-wrap (x (o f last))
  (namespace-fn x (fn (x) (cons f.x x)))
  )


(parameter imported-paths*
  ;; TODO: ew
  (listtab:list (list (joinpath exec-dir* "01 compiler.arc") arc3-namespace)
                (list (joinpath exec-dir* "02 core.arc")     arc3-namespace)
                (list (joinpath exec-dir* "03 ssyntax.arc")  arc3-namespace)
                (list (joinpath exec-dir* "compat.arc")   arc3-namespace)
                (list (joinpath exec-dir* "04 arc.arc")      arc3-namespace)
                (list (joinpath exec-dir* "05 extra.arc")    arc3-namespace)
                (list (joinpath exec-dir* "06 import.arc")   arc3-namespace)))

(def importfn1 (x)
  (if (basename x)
        (w/load-automatic-namespaces* t
          (ac-with-find-file string.x
            (fn (x)
              (let path abspath.x
                    ;; TODO: fix this
                (if ((imported-paths*) path)
                      (debug " skipping:" x)
                    (do (debug " loading: " x)
                        (= ((imported-paths*) path) t)
                        (load x)))))))
      ;; TODO: use dont
      (do (push abspath.x load-paths*)
          nil)
      ))

(def importfn (args)
  (w/load-paths* load-paths*
    (each x args
      (importfn1 x))))

(mac import args
  #`(importfn ',args))

(mac reimport args
  #`(w/imported-paths* (table)
      (importfn ',args)))
