(use defcall)

(extend type (x) (ar-tnil:racket-namespace? x) 'runtime)

;; TODO: use runtime.arc
(defcall runtime (x k (o d))
  ;; TODO: hacky
  (or= d (fn () nil))
  ;(debug "runtime" x k d)
  (ail-code:racket-namespace-variable-value
    k
    #t
    #|(if fn?.d
          d
        (fn () d))|#
    d
    ;(or d 5) ;(fn () nil)
    ;d
    x))
  #|(let x
    ;(debug "runtime" x k d parameter?.k ac-zeroarg*.k)
    ;; TODO: ew
    (if parameter?.x
        ;ac-zeroarg*.k
          (x)
        x)
        )|#

(extend sref (x v k . rest) (isa x 'runtime)
  ;(debug namespace)
  ;; TODO: hacky
  (aif ac-var-assigner*.k
       ;parameter?.n
         (do ;(debug "var-assigner" it k v)
         (it v))
       (do ;(debug "runtime sref" x v k rest)
       (ail-code:racket-namespace-set-variable-value!
         k
         v
         #t ;racket-#t
         x)))
  v)

;; TODO: bound
#|(with (orig bound default (list 'default))
  (redef bound (name (o runtime))
    (if runtime
          (isnt (racket-namespace-variable-value
                   name (ail-code #t) (fn () default) runtime)
                 default)
          (orig name))))|#

#|(def runtime ((o uses) (o usepath (usepath*)))
  (let runtime (new-runtime usepath)
    (each item uses
      (runtime!use-apply item))
    runtime))|#
