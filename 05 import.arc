;=============================================================================
;  from 06 extra.arc
;=============================================================================

;; TODO: should probably be in core.arc, under Types
(def listify (x)
  (if (cons? x) x (list x)))


(mac extend (name parms test . body)
  (w/uniq u
    #`(let 'orig name
        (= name (fn u
                  ;; TODO: (apply (fn ,parms ...) ,u) ?
                  (let parms u
                    (aif test (do ,@body)
                              (apply 'orig u)))
                 )))))


;; TODO: macro for generating these
(= defcall-types* (obj))

(mac defcall (name parms . body)
  #`(= (defcall-types* ',name) (fn parms ,@body)))

(extend ac-apply-non-fn (x . args)
  (orig defcall-types* (type x))
  (apply it x args))


;=============================================================================
;  from script.arc
;=============================================================================

(make-parameter cwd
  (racket-make-derived-parameter racket-current-directory
    (fn (v) (zap string v)
            (if empty.v
                  (%get.racket-current-directory)
                (%get.racket-expand-user-path v)))
    (fn (v) (string v))))

(def extension (x)
  (let x (%get.racket-filename-extension string.x)
    (%if x (string x))))

(def expandpath (x)
  (zap string x)
  (if empty.x
        x
      ;; TODO: different ssyntax priorities
      (string (%get.racket-expand-user-path x))))

(def abspath ((o x))
  ;; TODO: different ssyntax priorities
  (string (%get.racket-path->complete-path expandpath.x)))

(def joinpath args
  (string:apply %get.racket-build-path
    (awith (x    args
            acc  nil)
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

                               ;; TODO: different ssyntax priorities
(= dirname  (make-path->string (compose %get.racket-path-only           expandpath)))
(= basename (make-path->string (compose %get.racket-file-name-from-path expandpath)))


;=============================================================================
;  namespaces
;=============================================================================

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


#|(def empty-namespace ()
  (%get.racket-make-empty-namespace))

(def namespace-copy1 ((o x   (%get.racket-current-namespace))
                      (o new (empty-namespace)))
  (each n (%get.racket-namespace-mapped-symbols x) ;(racket-list->mlist )
    ;; TODO; use = and remove namespace-get...?
    (namespace-set new n (namespace-get x n)))
  new)

(def namespace-copy args
  (let new (empty-namespace) ;(%get.racket-make-base-empty-namespace)
    ;(prn (%get.racket-namespace-mapped-symbols new))
    (each x nrev.args
      (namespace-copy1 x new))
                  ;; TODO: not sure about this
    (parameterize (%get.racket-current-namespace new)
      (ac-require-base))
    new))|#


(def namespace-inherit args
  (annotate 'namespace (cons (empty-namespace) args))) ;(%get.racket-make-empty-namespace)

;; TODO: should use object.arc
;; TODO: extend should work with keyword args
(defcall namespace (x k (o d))
  ;; TODO: use alet
  (let self nil ;(% )
    (= self (fn (x)
              (if x ((car x) ;(% )
                     k
                     (fn ()
                       (self (cdr x)))) ;(% )
                    (if (fn? d) ;(% )
                          (d)
                        d))))
    (self (rep x))) ;(% )
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
    copy    (if x (do (%get.racket-current-namespace x)
                      (= namespace x))
                  (let new (apply namespace-copy (listify rep.namespace))
                    (%get.racket-current-namespace new)
                    (= namespace new)
                    new))
            (err "unknown namespace load type:" namespace-load-type*)))|#


(parameter load-automatic-namespaces*)

;; TODO: not sure about this
(let u (uniq)
  (extend bound (x) (isa namespace 'namespace)
          ;; TODO: remove namespace-get...?
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


;=============================================================================
;  import
;=============================================================================

(parameter imported-paths*
  ;; TODO: ew
  (make-table (joinpath exec-dir* "01 compiler.arc") arc3-namespace
              (joinpath exec-dir* "02 core.arc")     arc3-namespace
              (joinpath exec-dir* "03 ssyntax.arc")  arc3-namespace
              ;(joinpath exec-dir* "compat.arc")      arc3-namespace
              (joinpath exec-dir* "04 arc.arc")      arc3-namespace
              (joinpath exec-dir* "05 import.arc")   arc3-namespace
              (joinpath exec-dir* "06 extra.arc")    arc3-namespace))


(parameter load-paths*
            ;; TODO: different ssyntax priorities
  (list cwd ;(string (%get.racket-current-directory))
        exec-dir*
        (joinpath exec-dir* "lib")
        ;(joinpath exec-dir* "apps")
        (joinpath exec-dir* "lang")
        ;; TODO: different ssyntax priorities
        ;(string (%get.racket-build-path exec-dir* "lib"))
        ;; TODO: different ssyntax priorities
        ;(string (%get.racket-build-path exec-dir* "lang"))
        ))

(parameter load-suffix* ".arc")


;; TODO: ew
;; TODO: throw an error when the file can't be found
(def load-file-dir (x)
  ;; this is just (car:mem [file-exists (joinpath _ x)] load-paths*)
  (find [file-exists:joinpath _ x] load-paths*))

#|(def load-file-dir (x)
  ;; this is just (find [file-exists:joinpath _ x] load-paths*)
  (alet xs load-paths*
    (if (no xs)
          ;; TODO: should this be nil?
          nil
        (%get.racket-file-exists? (%get.racket-build-path (car xs) x))
          (car xs)
        (self (cdr xs)))))|#

(def load-normalize-path (x)
  (if (extension x) ;racket-filename-
        x
      (string x load-suffix*)))

(def call-w/find-file (x f)
                ;; TODO: %get.racket-port-count-lines-enabled
  (parameterize (racket-port-count-lines-enabled #t)
    (let y (load-normalize-path x)
      (iflet it (load-file-dir y)
        (f (joinpath it y) y)
        (iflet it (and (isnt x y)
                       (load-file-dir x))
          (f (joinpath it x) x)
          (err:string "file \"" x "\" was not found"))))))


(def importfn1 (x)
  (if (basename x)
        (call-w/find-file string.x
          (fn (path name)
            (let path abspath.path
                  ;; TODO: fix this
              (if ((imported-paths*) path)
                    (debug " skipping:" name)
                  (do (debug " loading: " name)
                      (= ((imported-paths*) path) t)
                      (w/load-automatic-namespaces* t
                        (load path)))))))
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
  #`(w/imported-paths* (obj)
      (importfn ',args)))
