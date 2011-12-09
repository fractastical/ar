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


(def new-namespace args
  (annotate 'namespace
            (cons (racket-make-empty-namespace)
                  (mappend (fn (x)
                             (let x (rep x)
                               (if (cons? x)
                                     x
                                   (list x))))
                           args))))

(mac w/new-namespace (x . body)
  #`(w/namespace (new-namespace x)
      ,@body))

;; TODO: should use object.arc
;; TODO: extend should work with keyword args
(defcall namespace (x k (o d))
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
  )

(extend sref (x v k) (isa x 'namespace)
  (sref (car rep.x) v k))

(extend print (primitive x port) (isa x 'namespace)
  (disp "#<namespace (len " port)
  (disp (len rep.x) port)
  (disp ")>" port))


(parameter load-automatic-namespaces*)

;; TODO: not sure about this
(let u (uniq)
  (extend bound (x) (isa namespace 'namespace)
    (isnt (namespace-get (car rep.namespace) x u) u)))

(remac safeset (var val)
  (if load-automatic-namespaces*
        #`(do (when (bound ',var)
                                   ;; TODO: does namespace need to be quoted?
                (zap new-namespace 'namespace)
                (disp "*** creating new namespace to avoid redefining " stderr)
                (disp ',var stderr)
                (disp #\newline stderr))
              (= var val))
      (orig var val)))


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
  (listtab:list (list (joinpath exec-dir* "compiler.arc") arc3-namespace)
                (list (joinpath exec-dir* "core.arc")     arc3-namespace)
                (list (joinpath exec-dir* "ssyntax.arc")  arc3-namespace)
                (list (joinpath exec-dir* "compat.arc")   arc3-namespace)
                (list (joinpath exec-dir* "arc.arc")      arc3-namespace)
                (list (joinpath exec-dir* "extra.arc")    arc3-namespace)
                (list (joinpath exec-dir* "import.arc")   arc3-namespace)))

(def importfn1 (x)
  (if (dirname x)
        ;; TODO: use dont
        (do (push abspath.x load-paths*)
            nil)
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
