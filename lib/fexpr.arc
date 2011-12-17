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

(= current-env (uniq))

(= namespace.current-env nil)

;(sref (ac-namespace) nil env)
;(sref namespace nil env)

(def make-current-env (x)
  (let x (mappend (fn (x)
                    (unless uniq?.x
                      ;; TODO: use quote rather than racket-quote...?
                      #`(('racket-quote x) x)))
                  (dedup x))
                    ;; TODO: conswhen? consif?
    #`(current-env ,(when x (cons list x)))))
#|        (list racket-mappend

              env)|#

#|(mac fexpr (name parms . body)
  ;; TODO: macro to generate this
  #`(do (sref sig ',parms ',name)
        (safeset name (annotate ''fexpr (fn parms ,@body)))))|#

(mac fexpr (name parms . body)
  (w/uniq (u args)
    #`(let u (fn parms ,@body)
        (mac name args
          (list* u ',current-env args)))))


;; TODO: more granular extending
;; TODO: use ac-def
(%nocompile
(racket-define (ac-fn-args x)
  (racket-cond
    ;; TODO: make it work with rest args too
    ((racket-symbol? x)
      (ac-fn-body (list (list* (racket-quote racket-let)
                               (ac-fn-rest-args x)
                               (ac-args (ac-fn-body)))))
      x)
    (racket-else
      (racket-parameterize ((ac-fn-let* nil))
                      ;; This is one of two changes
        (racket-let* ((env (ac-local-env))
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
)

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
