#|(def foo (x y)
  (+ x y z))

(list '(x y)
      '(+ x y z)
      (obj z (fn () z)))

(fn (a b c)
  (+ a b c))

(fn (a b c)
  (parameterize (outer-env    current-env
                 current-env  (join (list 'a a
                                          'b b
                                          'c c)
                                    current-env))
    (+ a b c)))|#


#|(parameter current-env)

(def some-fn (x)
  (parameterize (current-env (join (list 'x x) current-env))
    (x)))

(def some-fexpr (env)
  (fn ()
    (prn (cadr:mem 'a env))
    (prn (cadr:mem 'a current-env))
    nil))

(let a 5
  (parameterize (current-env (join (list 'a a) current-env))
    (some-fn (some-fexpr current-env))))|#


;; TODO: try implementing this in compiler.arc to see how slow it is

;(= current-env (uniq))

;(= namespace.current-env nil)

;(sref (ac-namespace) nil env)
;(sref namespace nil env)

#|(def make-current-env (x)
  (let x (mappend (fn (x)
                    (unless uniq?.x
                      ;; TODO: use quote rather than racket-quote...?
                      #`(('racket-quote x) x)))
                  (dedup x))
                    ;; TODO: conswhen? consif?
    #`(current-env ,(when x (cons list x)))))|#

(def make-current-env (x)
             ;; TODO: this should probably be a function somewhere, like in
             ;;       extra.arc
  (cons list (mappend (fn (x) #`(',x x))
                      (dedup x))))
#|        (list join

              env)|#

(def make-fexpr-fn (u args x)
  #`(fn args (apply (rep u) x args)))

(mac fx-wrap (x (o env (make-current-env (ac-local-env))))
  (make-fexpr-fn x (uniq) env))

#|(mac fexpr (name parms . body)
  ;; TODO: macro to generate this
  #`(do (sref sig ',parms ',name)
        (safeset name (annotate ''fexpr (fn parms ,@body)))))|#

#|(mac fexpr (name parms . body)
  (w/uniq (u args)
    #`(let u (fx parms . body)
        (make-inline-fn name
          (fn args
            ;(prn (ac-local-env))
            ;(%eval (prn (make-current-env (ac-local-env))))

            ;; TODO: is it faster at compile-time / runtime to map quote over
            ;;       the args, or call apply like this:
            ;;       #`(apply u (list ,@(make-current-env (ac-local-env))) ',args)
            ;;       ?
            (list* (rep u) (cons list (make-current-env (ac-local-env)))
                           ;; TODO: unquote the fn...?
                           (map (fn (u) (list quote u)) args))
            ;(list* u ',current-env args)
            )
          (fn ()
            u
            ;(make-fexpr-fn u ',args (make-current-env (ac-local-env)))
            ))
        u)))|#

;; TODO: use objects for this
#|(object (type () 'fexpr)

        (ref (x . args)
          (apply (rep x) nil args))

        (print (primitive x port)
          (print-w/name x "#<fexpr" ":" ">" port))

        ;; TODO: better name than this
        (ac-macro? (x)
          ;; TODO: is it faster at compile-time / runtime to map quote over
          ;;       the args, or call apply like this:
          ;;       #`(apply u (list ,@(make-current-env (ac-local-env))) ',args)
          ;;       ?
          (list* (rep x) (make-current-env (ac-local-env))
                         ;; TODO: unquote the fn...?
                         (map (fn (u) (list quote u)) args))))|#

(defcall fexpr (x . args)
  (apply (rep x) nil args))

(defmac fexpr (x . args)
  ;; TODO: is it faster at compile-time / runtime to map quote over
  ;;       the args, or call apply like this:
  ;;       #`(apply u (list ,@(make-current-env (ac-local-env))) ',args)
  ;;       ?
  (list* (rep x) (make-current-env (ac-local-env))
                 ;; TODO: unquote the fn...?
                 (map (fn (u) (list quote u)) args)))

(defprint fexpr (primitive x port)
  (print-w/name x "#<fexpr" ":" ">" port))


(mac fx (parms . body)
  #`(annotate ''fexpr (fn parms . body)))

(mac fexpr (name parms . body)
  #`(safeset name (fx parms . body)))

#|(extend eval (x (o runtime)) (do (prn x " " runtime) (cons? runtime))
  (w/uniq u
    (let v (plref runtime x u)
      (if (is v u)
            (orig x)
          v))))|#


;; TODO: more granular extending
#|(%
(ac-def ac-fn-args (x)
  (racket-cond
    ;; TODO: make it work with rest args too
    ((racket-symbol? x)
      (ac-add-to ac-local-env x)
      (ac-fn-body (ac-args (ac-fn-body)))
      x)
    (racket-else
      (racket-parameterize ((ac-fn-let* nil))
                     ;; This is one of two changes
        (racket-let ((env (ac-local-env))
                     (x   (ac-fn-normal-args x)))
          ;; This is two of two changes
          (racket-unless (racket-eq? env (ac-local-env))
            (ac-add-to ac-fn-let* ((ac-var (racket-quote make-current-env))
                                   (ac-local-env)))
            (ac-add-to ac-local-env current-env))

          (ac-fn-body (racket-if (ac-true (ac-fn-let*))
                                   (list (list* (racket-quote racket-let*)
                                                (ac-fn-let*)
                                                (ac-args (ac-fn-body))))
                                 (ac-args (ac-fn-body))))
          x)))))
)|#


;(dynamic ac-fn-args ac-fn-args)

#|(redef ac-fn-args (x)
  (if (sym? x)
        (do (ac-fn-body `((racket-let ,(ac-fn-rest-args x)
                            ,@(ac-args (ac-fn-body)))))
            x)
    (parameterize (ac-fn-let* nil)
      (withs (env (ac-local-env)
              x   (ac-fn-normal-args x))
        (unless (is env (ac-local-env))
          (ac-add-to ac-fn-let* (make-current-env (ac-local-env))))

        (ac-fn-body (if (ac-fn-let*)
                           `((racket-let* ,(ac-fn-let*)
                               ,@(ac-args (ac-fn-body))))
                         (ac-args (ac-fn-body))))
        x))))|#
