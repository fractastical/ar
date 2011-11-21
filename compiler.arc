(racket-define t            (racket-quote t))
(racket-define nil          racket-null)

(racket-define ac-local-env (racket-make-parameter nil))

;; TODO: does this need to be a Racket macro...?
(racket-define-syntax-rule (ac-w/local-env x ...body)
  (racket-parameterize ((ac-local-env (racket-mappend (ac-local-env) x)))
    ...body))

;=============================================================================
;  Hash Tables
;=============================================================================

(racket-define sig (racket-make-hash))


;=============================================================================
;  Mapping/Iteration
;=============================================================================

(racket-define (map1 f xs)
  (racket-if (racket-mpair? xs)
               (cons (f (car xs)) (map1 f (cdr xs)))
             xs))

;=============================================================================
;  Symbols
;=============================================================================

(racket-define uniq  racket-gensym)

(racket-define (ac-var x (def nil))
  (namespace-get namespace x def))

(racket-define bound
  (racket-let ((undef (uniq)))
    (racket-lambda (name)
      (ac-tnil
        (racket-not (racket-eq? (ac-var name undef) undef))))))


;=============================================================================
;  Types
;=============================================================================

(racket-define (annotate type rep)
  (racket-vector (racket-quote tagged) type rep))

(racket-define (ac-exint? x)
  (racket-and (racket-integer? x) (racket-exact? x)))

(racket-define (ac-tagged? x)
  (racket-and (racket-vector? x)
              (racket-eq? (racket-vector-ref x 0) (racket-quote tagged))))

(racket-define (type x)
  (racket-cond
    ((ac-tagged? x)           (racket-vector-ref x 1))
    ((racket-mpair? x)        (racket-quote cons))
    ((racket-null? x)         (racket-quote sym))
    ((racket-symbol? x)       (racket-quote sym))
    ((racket-parameter? x)    (racket-quote parameter))
    ((racket-procedure? x)    (racket-quote fn))
    ((racket-char? x)         (racket-quote char))
    ((racket-string? x)       (racket-quote string))
    ((ac-exint? x)            (racket-quote int))
    ((racket-number? x)       (racket-quote num))
    ((racket-hash? x)         (racket-quote table))
    ((racket-output-port? x)  (racket-quote output))
    ((racket-input-port? x)   (racket-quote input))
    ((racket-exn? x)          (racket-quote exception))
    ((racket-thread? x)       (racket-quote thread))
    ((racket-thread-cell? x)  (racket-quote thread-cell))
    ((racket-semaphore? x)    (racket-quote semaphore))
    ((racket-keyword? x)      (racket-quote keyword))
    (racket-else              (racket-quote unknown))))

(racket-define (rep x)
  (racket-if (ac-tagged? x)
               (racket-vector-ref x 2)
             x))

(racket-define (ac-deep-toarc x)
  (racket-cond
    ((racket-pair? x)
     (cons (ac-deep-toarc (racket-car x))
           (ac-deep-toarc (racket-cdr x))))
    (racket-else
     (racket-when (racket-mpair? x)
       (err "Racket mpair passed to ac-deep-toarc" x))
     x)))

(racket-define (ac-deep-fromarc x)
  (racket-cond
    ((racket-mpair? x)
     (racket-cons (ac-deep-fromarc (car x))
                  (ac-deep-fromarc (cdr x))))
    (racket-else
     (racket-when (racket-pair? x)
       (err "Racket pair passed to ac-deep-fromarc" x))
     x)))


;=============================================================================
;  Exceptions/Errors
;=============================================================================

(racket-define err  racket-error)

(racket-define (on-err errf f)
  (racket-with-handlers ((racket-exn:fail? errf))
    (f)))

(racket-define (details c)
  (racket-exn-message c))


;=============================================================================
;  print
;=============================================================================

(racket-define (print-w/list primitive x port)
  (racket-cond
    ((ac-no (cdr x))
      (print primitive (car x) port)
      (disp ")" port))
    ((racket-mpair? (cdr x))
      (print primitive (car x) port)
      (disp " " port)
      (print-w/list primitive (cdr x) port))
    (racket-else
      (print primitive (car x) port)
      (disp " . " port)
      (print primitive (cdr x) port)
      (disp ")" port))))

(racket-define (print primitive x port)
  (racket-cond
    ((racket-mpair? x)
      (disp "(" port)
      (print-w/list primitive x port))
    #|((racket-eq? (type x) (racket-quote fn))
      (disp "#<fn>" port))|#
    ((racket-eq? (type x) (racket-quote mac))
      (disp "#<mac>" port))
    ((ac-no x)
      (disp "nil" port))
    (racket-else
      (primitive x port)))
  nil)


;=============================================================================
;  I/O
;=============================================================================

(racket-define (sread input (eof nil))
  (racket-let ((v (racket-read input)))
    (racket-if (racket-eof-object? v)
                 eof
               (ac-deep-toarc v))))


;=============================================================================
;  Predicates
;=============================================================================

(racket-define (ac-tnil x)
  (racket-if x t nil))

(racket-define (ac-no x)
  (racket-eq? x nil))

(racket-define (ac-nil x)
  (racket-if (ac-no x) #f x))

(racket-define (ac-true x)
  (racket-not (ac-no x)))

(racket-define (ac-bool x)
  (ac-tnil (ac-no x)))

(racket-define (ac-caris x y)
  (racket-and (racket-mpair? x)
              (racket-eq? (car x) y)))


;=============================================================================
;  Lists
;=============================================================================

(racket-define cons  racket-mcons)
(racket-define list  racket-mlist)

(racket-define (car x)
  (racket-if (ac-no x)
               nil
             (racket-mcar x)))

(racket-define (cdr x)
  (racket-if (ac-no x)
               nil
             (racket-mcdr x)))

(racket-define (cadr x)
  (car (cdr x)))

(racket-define (cddr x)
  (cdr (cdr x)))

(racket-define (list* . args)
  (racket-cond
    ((ac-no args)
      nil)
    ((ac-no (racket-cdr args))
     (racket-car args))
    ((ac-no (racket-cddr args))
     (cons (racket-car args) (racket-cadr args)))
    (racket-else
     (cons (racket-car args) (racket-apply list* (racket-cdr args))))))

#|(racket-define (ac-length xs)
  (racket-let loop ((x xs) (n 0))
    (racket-cond
      ((racket-mpair? x)   (loop (cdr x) (racket-+ n 1)))
      ((racket-eq? x nil)  n)
      (racket-else         (err "len expects a proper list" xs)))))|#

#|(racket-define (ac-length2 x)
  (racket-cond
    ((ac-no x)          0)
    ((racket-mpair? x)  (racket-+ 1 (ac-length2 (cdr x))))
    (racket-else        (err "len expects a proper list"))))|#

(racket-define (len x)
  (racket-cond
    ((racket-string? x) (racket-string-length x))
    ((racket-hash? x)   (racket-hash-count x))
    (racket-else        ;(ac-length x)
                        (racket-mlength x)
                        )))

(racket-define (ac-mappend f x)
  (apply racket-mappend (map1 f x)))


#|(racket-define (ac-mlist->list x)
  (racket-cond
    ((racket-mpair? x)
      (racket-cons (racket-mcar x) (ac-mlist->list (racket-mcdr x))))
    (racket-else x)))|#


;=============================================================================
;  apply
;=============================================================================

(racket-define (ac-arg-list* args)
  ;; TODO: figure out how to avoid the mlist->list call
  ;(racket-apply list* (racket-mlist->list args))
  (racket-mlist->list (racket-apply list* args))
  ;(racket-mlist->list (racket-apply list* args))
  #|(racket-display args)
  (racket-newline)
  (racket-cond
    ((racket-null? args)
      nil)
    ((racket-null? (racket-cdr args))
     (racket-car args))
    ((racket-null? (racket-cddr args))
     (racket-cons (racket-car args) (racket-cadr args)))
    (racket-else
     (racket-cons (racket-car args) (ac-arg-list* (racket-cdr args)))))|#
     )

#|(racket-define (ac-arg-list* as)
  (racket-let next ((as as) (accum (racket-list)))
    (racket-cond
     ((racket-null? as)
      accum)
     ((racket-null? (racket-cdr as))
      (racket-append accum (racket-mlist->list (racket-car as))))
     (racket-else
      (next (racket-cdr as)
            (racket-append accum (racket-list (racket-car as))))))))|#

(racket-define (ac-apply-non-fn x args)
  (racket-cond
    ((racket-mpair? x)
     (racket-mlist-ref x (car args)))
    ((racket-string? x)
     (racket-string-ref x (car args)))
    ((racket-hash? x)
     (racket-hash-ref x (car args) (cadr args)))
    (racket-else
     (err "Function call on inappropriate object" x args))))

(racket-define (ac-apply f . racket-arg-list)
  ;(racket-display racket-arg-list)
  ;(racket-newline)
  (racket-if (racket-procedure? f)
               (racket-apply f racket-arg-list)
             (ac-apply-non-fn f (racket-list->mlist racket-arg-list))))

;; TODO: why is apply very slow compared to ar and Arc 3.1? fix it
(racket-define (apply f . args)
  ;(racket-display (racket-apply list* args))
  ;(racket-display (racket-pair? (racket-cdr args)))
  ;(racket-display args)
  ;(racket-display (racket-car args))
  ;(racket-newline)
  (racket-apply ac-apply f (ac-arg-list* args))
  #|(racket-if (racket-pair? (racket-cdr args))
               (racket-apply ac-apply f (ac-arg-list* args))
             (racket-apply ac-apply f (racket-mlist->list (racket-car args))))|#

  ;time: 2556 msec.
  ;(racket-apply ac-apply (racket-car args))
  ;(racket-apply ac-apply f (racket-apply racket-list* args))
  ;(racket-apply ac-apply f (racket-mlist->list (racket-apply racket-list* args)))
  ;(racket-apply ac-apply f (racket-mlist->list (racket-apply list* args)))
  )


;=============================================================================
;  Function/Macro calls
;=============================================================================

(racket-define ac-functional-position? (racket-make-parameter nil))

(racket-define (ac-lex? x)
  (racket-let self ((env (ac-local-env)))
    (racket-and (racket-mpair? env)
                (racket-or (racket-eq? x (car env))
                           (self (cdr env))))))

;; TODO: make keyword arguments supported
(racket-define (ac-funcall0 f)
  (racket-if (racket-procedure? f)
               (f)
             (ac-apply f)))

(racket-define (ac-funcall1 f arg1)
  (racket-if (racket-procedure? f)
               (f arg1)
             (ac-apply f arg1)))

(racket-define (ac-funcall2 f arg1 arg2)
  (racket-if (racket-procedure? f)
               (f arg1 arg2)
             (ac-apply f arg1 arg2)))

(racket-define (ac-funcall3 f arg1 arg2 arg3)
  (racket-if (racket-procedure? f)
               (f arg1 arg2 arg3)
             (ac-apply f arg1 arg2 arg3)))

(racket-define (ac-funcall4 f arg1 arg2 arg3 arg4)
  (racket-if (racket-procedure? f)
               (f arg1 arg2 arg3 arg4)
             (ac-apply f arg1 arg2 arg3 arg4)))

(racket-define (ac-macro? f)
  (racket-cond
    ((racket-eq? (type f) (racket-quote mac))
     (rep f))
    ((racket-symbol? f)
     (racket-let ((v (ac-var f)))
       (racket-if (racket-eq? (type v) (racket-quote mac))
                    (rep v)
                  #f)))
    (racket-else #f)))

(racket-define (ac-mac-call m args)
  (ac-compile (apply m args)))

(racket-define (ac-args args)
  (map1 (racket-lambda (expr)
          (ac-compile expr))
        args))

(racket-define (ac-call f args)
  (racket-let* ((g  (racket-not (ac-lex? f)))
                (m  (racket-and g (ac-macro? f)))
                (f  (racket-parameterize ((ac-functional-position? t))
                      (ac-compile f))))
    (racket-cond
      (m (ac-mac-call m args))
      ;; if we're about to call a literal fn such as ((fn (a b) ...) 1 2)
      ;; then we know we can just call it in Racket and we don't
      ;; have to use ac-apply
      ((ac-caris f (racket-quote racket-lambda))
       (cons f (ac-args args)))
      #|
      ;; if it's a global function, don't bother calling ac-apply or ac-funcall
      ((racket-and g
                   (racket-symbol? f)
                   (racket-procedure? (namespace-get namespace f nil)))
       (cons f (ac-args args)))|#
      (racket-else
       (cons (racket-case (len args)
               ((0) ac-funcall0)
               ((1) ac-funcall1)
               ((2) ac-funcall2)
               ((3) ac-funcall3)
               ((4) ac-funcall4)
               (racket-else ac-apply))
             (cons f (ac-args args)))))))


;=============================================================================
;  assign
;=============================================================================

(racket-define (ac-local-assign a b)
  #|(racket-let ((result (uniq)))
    (list (racket-quote racket-let)
          (list (list result (ac-compile b)))
          (list (racket-quote racket-set!) a result)
          result))|#
  (list (racket-quote racket-set!) a b))

(racket-define (ac-global-assign a b)
  (list (racket-quote racket-set!) a b))

(racket-define (ac-assign1 a b1)
  (racket-unless (racket-symbol? a)
    (err "First arg to assign must be a symbol" a))

  (racket-let ((result (ac-compile b1)))
    (racket-if (ac-lex? a)
                 (ac-local-assign a result)
               (ac-global-assign a result))))

(racket-define (ac-assignn x)
  (racket-if (ac-no x)
               nil
             ;; TODO: why does Arc 3.1 call ac-macex here?
             (cons (ac-assign1 (car x) (cadr x))
                   (racket-if (ac-no (cddr x))
                                (list (car x))
                              (ac-assignn (cddr x))))))

(racket-define (ac-assign x)
  (cons (racket-quote racket-begin)
        (ac-assignn x)))

(racket-define assign (annotate (racket-quote mac)
                        (racket-lambda args
                          (cons ac-assign (racket-list->mlist args)))))


;=============================================================================
;  fn
;=============================================================================

(racket-define ac-fn-required-args?       (racket-make-parameter t))
(racket-define ac-fn-excess-args?         (racket-make-parameter nil))
(racket-define ac-fn-rigid-destructuring? (racket-make-parameter nil))

;; TODO: tests for this
(racket-define ac-fn-optional-on-nil?     (racket-make-parameter nil))

(racket-define (ac-fn-keyword-args x default)
  (list x (list (racket-string->symbol (racket-keyword->string x))
                default)))

(racket-define (ac-fn-optional-args x body)
  (racket-let ((c (car x))
               (default (racket-let ((x (cdr x)))
                          (racket-if (ac-no x)
                            (racket-quote nil)
                            (racket-begin (racket-display (ac-local-env))
                            (racket-newline)
                            (ac-compile (car x)))))))
    (racket-if (racket-keyword? c)
                 (ac-fn-keyword-args c default)
               (list (list c default)))))

;; TODO: unit tests verifying the minimalness of the destructuring code output
(racket-define (ac-fn-destructuring-args u x body)
  (racket-let ((rigid (ac-fn-rigid-destructuring?)))
    (racket-let self ((x x))
      (racket-cond
        ((ac-no x)                            ;; end of the argument list
          nil)
        ((racket-symbol? x)                   ;; dotted rest args
          (list (list x u)))
        ((ac-caris (car x) (racket-quote o))  ;; optional args
          (racket-let ((c (car x)))
            ;(racket-display body)
            ;(racket-newline)
            #|(racket-set! body (list (list* (racket-quote racket-let)
                                           (list (list (cadr c)
                                                       (ac-compile
                                                         (list (racket-quote if)
                                                               (cadr c)
                                                               (cadr c)
                                                               (caddr c)))))
                                           body)))|#
            ;; TODO: ew, code duplication
            ;; TODO: hacky
            (cons (racket-if (ac-true (ac-fn-optional-on-nil?))
                               (cons (list (cadr c) (list (racket-quote car) u))
                                     (list (cadr c) (ac-compile (list (racket-quote if)
                                                                      (cadr c)
                                                                      (cadr c)
                                                                      (caddr c)))))
                             (list (cadr c) (ac-compile (list (racket-quote if)
                                                              (list (racket-quote cdr) u)
                                                              (list (racket-quote car) u)
                                                              (caddr c)))))
                  (racket-if (ac-no (cdr x))
                               nil
                             (cons (list u (list (racket-quote cdr) u))
                                   (self (cdr x)))))
            #|(list (list (cadr x)
                        (list (racket-quote or) (list (racket-quote car) u)
                                                (caddr x))))|#
                                                ))
        ((racket-mpair? (car x))              ;; destructuring args
          (racket-let ((v (uniq)))
            (racket-mappend (list (list v (list (racket-quote car) u)))
                            (ac-fn-destructuring-args v (car x) body)
                            (racket-if (ac-no (cdr x))
                                         nil
                                       (list (list u (list (racket-quote cdr) u))))
                            (self (cdr x)))))
        (racket-else                          ;; normal args
          (cons (list (car x) (list (racket-quote car) u))
                (racket-if (ac-no (cdr x))
                             nil
                           (cons (list u (list (racket-quote cdr) u))
                                 (self (cdr x))))))))))


#|(racket-define (ac-fn-destructuring-args u x body)
  (racket-let ((rigid (ac-fn-rigid-destructuring?)))
                ;; TODO: replace with apply after I make it faster
    (list (list racket-apply
                ;apply
                ;(racket-quote apply)
                (racket-parameterize ((ac-fn-required-args? rigid)
                                      (ac-fn-excess-args?   (ac-bool rigid)))
                  (cons (racket-quote racket-lambda)
                        (ac-fn-args x body))
                  #|(ac-compile (list* (racket-quote fn)
                                     x
                                     body))|#
                              )
                ;u
                (list racket-mlist->list u)
                ))))|#

#|(racket-define (ac-fn-rest-args x body)
  (cons (list (racket-quote racket-set!) x (list racket-list->mlist x))
        body))|#

(racket-define (ac-fn-rest-args x body)
  (list (list x (list racket-list->mlist x)))
    ;(racket-display (ac-deep-fromarc x))
    ;(racket-newline)
   )

(racket-define (ac-fn-required-args x body)
  (racket-if (ac-true (ac-fn-required-args?))
               x
             (list x (racket-quote nil))))

(racket-define (ac-fn-end-of-args x body)
  (racket-if (ac-true (ac-fn-excess-args?))
               (uniq)
             nil))

(racket-define (ac-fn-normal-args x body)
  (racket-let ((l nil))
    (cons (racket-let self ((x x))
            ;; TODO: hacky
            (racket-when (racket-mpair? x)
              (racket-set-mcar! x (ssexpand (car x))))

            (racket-cond
              ((ac-no x)                            ;; end of the argument list
                (ac-fn-end-of-args x body))
              ((racket-symbol? x)                   ;; dotted rest args
                (racket-set! l (racket-mappend (ac-fn-rest-args x body) l))
                x)
              ((ac-caris (car x) (racket-quote o))  ;; optional args
                ;(ac-fn-optional-args (cdr (car x)) body)
                (racket-let* ((c (car x))
                              (n (cadr c))
                              (d (car (cddr c))))
                  ;(racket-display l)
                  ;(racket-newline)
                  #|(cons (list (cadr c) (car (cddr c))) ; (racket-quote nil)
                        (self (cdr x)))|#

                  ;; TODO: really hacky
                  (racket-if (ac-true (ac-fn-optional-on-nil?))
                               (racket-begin
                                 (racket-unless (ac-no d)
                                   (racket-set! l
                                     (cons (list n (ac-compile (list (racket-quote if)
                                                                     n
                                                                     n
                                                                     d)))
                                           l)))
                                 (racket-mappend (list (list n (racket-quote nil)))
                                                 (self (cdr x))))
                               (racket-mappend (ac-fn-optional-args (cdr c) body)
                                               (self (cdr x))))

                  #|(racket-mappend (ac-fn-optional-args (car c) body)
                                  (self (cdr x)))|#
                        )
                                )
              ((racket-keyword? (car x))            ;; keyword args
                (racket-mappend (ac-fn-keyword-args (car x)
                                                    (racket-quote nil))
                                (self (cdr x))))
              ((racket-mpair? (car x))              ;; destructuring args
                (racket-let ((u (uniq)))
                  ;(racket-set! body (ac-fn-destructuring-args u (car x) body))
                  #|(racket-display y)
                  (racket-newline)
                  (racket-newline)|#
                  ;(racket-set! body (ac-fn-destructuring-args u (car x) body))
                  #|(racket-set! body (list (list* (racket-quote racket-let*)
                                                 (ac-fn-destructuring-args u (car x) body)
                                                 body)))|#
                  (racket-set! l
                    (racket-mappend
                      (ac-fn-destructuring-args u (car x) body)
                      l))
                  ;(racket-display l)
                  ;(racket-newline)
                  (cons (ac-fn-required-args u body)
                        (ac-w/local-env nil ;(list (racket-quote a))
                          (self (cdr x))))))
              (racket-else                          ;; normal args
                (cons (ac-fn-required-args (car x) body)
                      (self (cdr x))))))
          (racket-let ((x
          (racket-if (ac-true l)
                       (list (list* (racket-quote racket-let*)
                                    l
                                    body))
                     body)))
            #|(racket-display x)
            (racket-newline)
            (racket-newline)|#
            x))))

(racket-define (ac-fn-args x body)
  (racket-when (ac-no body)
    (racket-set! body (list (racket-quote nil))))

  #|(racket-when (ac-no body)
    (racket-set! body (list nil)))|#

  (racket-cond
    ((racket-symbol? x)
      ;(cons x (ac-fn-rest-args x body))
      (cons x (list (list* (racket-quote racket-let)
                           (ac-fn-rest-args x body)
                           body)))
      )
    (racket-else
      (ac-fn-normal-args x body))))


#|(racket-define (ac-arglist-normal x)
  (racket-cond
    ((racket-keyword? x)
      (racket-string->symbol (racket-keyword->string x))) ;; (sym x)
    (racket-else
      x)))

;; TODO: ew, hacky
(racket-define (ac-arglist x)
  (racket-cond
    ((ac-no x)
      nil)
    ((racket-symbol? x)
      (list x))
    ((ac-caris (car x) (racket-quote o))
      (cons (ac-arglist-normal (cadr (car x)))
            (ac-arglist (cdr x))))
    ((racket-mpair? (car x))
      (racket-mappend (ac-arglist (car x))
                      (ac-arglist (cdr x))))
    (racket-else
      (cons (ac-arglist-normal (car x))
            (ac-arglist (cdr x))))))|#

(racket-define (ac-fn x)
  ;; TODO: add env support back in, but better
  ;(racket-set! env (racket-mappend (ac-arglist (cadr x)) env))
  (cons (racket-quote racket-lambda)
        (ac-fn-args (cadr x)
                    (ac-args (cddr x)))))

(racket-define fn (annotate (racket-quote mac)
                    (racket-lambda (parms . body)
                      (list* ac-fn parms (racket-list->mlist body)))))


;=============================================================================
;  if
;=============================================================================

(racket-define (ac-if args)
  (racket-cond
   ((ac-no args)
    (racket-quote nil))
   ((ac-no (cdr args))
    (ac-compile (car args)))
   (racket-else
    (list (racket-quote racket-if)
          (list ac-true (ac-compile (car args)))
          (ac-compile (cadr args))
          (ac-if (cddr args))))))

(racket-define if (annotate (racket-quote mac)
                    (racket-lambda args
                      (cons ac-if (racket-list->mlist args)))))


;=============================================================================
;  quote
;=============================================================================

(racket-define (ac-quote x)
  #|(racket-when (racket-eq? x (racket-quote nil))
    (racket-set! x nil))|#

  #|(list (list (racket-quote racket-quote)
              (racket-lambda () x)))|#

  (list (racket-lambda () x))
  ;(racket-display x)
  ;(racket-newline)
  #|(list ac-deep-toarc
        (list (racket-quote racket-quote)
              x))|#
              )

(racket-define quote (annotate (racket-quote mac)
                       (racket-lambda (x)
                         (list ac-quote x))))


;=============================================================================
;  quasiquote
;=============================================================================
; qq-expand takes an Arc list containing a quasiquotation expression
; (the x in `x), and returns an Arc list containing Arc code.  The Arc
; code, when evaled by Arc, will construct an Arc list, the
; expansion of the quasiquotation expression.
;
; This implementation is Alan Bawden's quasiquotation expansion
; algorithm from "Quasiquotation in Lisp"
; http://repository.readscheme.org/ftp/papers/pepm99/bawden.pdf
;
; You can redefine qq-expand in Arc if you want to implement a
; different expansion algorithm.

(racket-define (qq-expand-list x)
  (racket-cond
   ((ac-caris x (racket-quote unquote))
    (list list (cadr x)))
    ;(list (racket-quote list) (cadr x)))
   ((ac-caris x (racket-quote unquote-splicing))
    (cadr x))
   ((ac-caris x (racket-quote quasiquote))
    (qq-expand-list (qq-expand (cadr x))))
   ((racket-mpair? x)
    (list list (qq-expand-pair x)))
    ;(list (racket-quote list) (qq-expand-pair x)))
   (racket-else
    (list quote (list x)))))
    ;(list (racket-quote quote) (list x)))))

(racket-define (qq-expand-pair x)
  (list racket-mappend
        ;(racket-quote racket-mappend)
        (qq-expand-list (car x))
        (qq-expand      (cdr x))))

(racket-define (qq-expand x)
  (racket-cond
   ((ac-caris x (racket-quote unquote))
    (cadr x))
   ((ac-caris x (racket-quote unquote-splicing))
    (err "illegal use of ,@ in non-list quasiquote expansion"))
   ((ac-caris x (racket-quote quasiquote))
    (qq-expand (qq-expand (cadr x))))
   ((racket-mpair? x)
    (qq-expand-pair x))
   (racket-else
    (list quote x))))
    ;(list (racket-quote quote) x))))

(racket-define quasiquote (annotate (racket-quote mac)
                            (racket-lambda args
                              (racket-apply qq-expand args))))
                              ;(racket-apply qq-expand (racket-list->mlist args)))))


;=============================================================================
;  assignment
;=============================================================================

(racket-define (sref com val ind)
  (racket-cond
   ((racket-hash? com)
    (racket-if (ac-no val)
                 (racket-hash-remove! com ind)
               (racket-hash-set! com ind val)))
   ((racket-string? com)
    (racket-string-set! com ind val))
   ((racket-mpair? com)
    (racket-set-mcar! (racket-mlist-tail com ind) val))
   (racket-else
    (err "Can't set reference" com ind val)))
  val)


;=============================================================================
;  is
;=============================================================================

(racket-define (ac-pairwise pred lst)
  (racket-cond
    ((ac-no lst)       t)
    ((ac-no (cdr lst)) t)
    ((ac-true (pred (car lst) (cadr lst)))
     (ac-pairwise pred (cdr lst)))
    (racket-else nil)))

(racket-define (ac-binary bin reduce)
  (racket-case-lambda
    ((x y) (bin x y))
    (args  (reduce bin (racket-list->mlist args)))))

(racket-define (is2 a b)
  (ac-tnil (racket-or (racket-eqv? a b) ;; TODO: should this use racket-eq?
                      (racket-and (racket-string? a)
                                  (racket-string? b)
                                  (racket-string=? a b)))))

(racket-define is (ac-binary is2 ac-pairwise))


;=============================================================================
;  %nocompile/%compile
;=============================================================================

(racket-define (ac-nocompile x)
  (racket-let ((x (racket-let self ((x x))
                    (ac-mappend (racket-lambda (x)
                                  (racket-cond
                                    ((ac-caris x (racket-quote %compile))
                                      (map1 (racket-lambda (x)
                                              (ac-compile x))
                                            (cdr x)))
                                    ((racket-mpair? x)
                                      (list (self x)))
                                    (racket-else
                                      (list x))))
                                x))))
    (racket-if (ac-no (cdr x))
                 (car x)
               (cons (racket-quote racket-begin) x))))

(racket-define %nocompile (annotate (racket-quote mac)
                            (racket-lambda args
                              (cons ac-nocompile (racket-list->mlist args)))))


;=============================================================================
;  ssyntax
;=============================================================================

(racket-define (ssexpand x) x)
(racket-define (ssyntax  x) nil)


;=============================================================================
;  Variables
;=============================================================================

(racket-define (ac-local-var x)
  x)

(racket-define (ac-lookup-global x)
  x)

(racket-define (ac-lookup-global-arg x)
  ;; This implements implicit parameters
  (racket-if (racket-parameter? x)
               (x)
             (ac-lookup-global x)))

(racket-define (ac-global-var x)
  (racket-if (ac-true (ac-functional-position?))
               (list ac-lookup-global x)
             (list ac-lookup-global-arg x)))


;=============================================================================
;  compiler/eval
;=============================================================================

(racket-define (ac-compile x)
  (racket-cond
    ((ac-caris x ac-nocompile)
      (ac-nocompile (cdr x)))
    ((ac-caris x ac-assign)
      (ac-assign (cdr x)))
    ((ac-caris x ac-fn)
      (ac-fn x))
    ((ac-caris x ac-if)
      (ac-if (cdr x)))
    ((ac-caris x ac-quote)
      (ac-quote (cadr x)))
    ((racket-mpair? x)
      (ac-call (car x) (cdr x)))
    ((racket-eq? x nil)
     (racket-quote nil))
    ((ac-true (ssyntax x))
     (ac-compile (ssexpand x)))
    ((racket-symbol? x)
     (racket-if (ac-lex? x)
                  (ac-local-var x)
                (ac-global-var x)))
    (racket-else x)))

(racket-define (eval x (runtime nil))
  (racket-eval (ac-deep-fromarc (ac-compile x))
               (racket-if (ac-no runtime)
                            namespace
                          runtime)))


#|(racket-define (+-2 (x 0) (y 0))
  (racket-cond
    ((racket-number? x)
      (racket-+ x y))
    ((racket-or (racket-char? x)
                (racket-string? x))
      (string x y))
    ((racket-mlist? x)
      (racket-mappend x y))))|#


;=============================================================================
;  load
;=============================================================================

(racket-define (ac-eval-all in runtime)
  (racket-let ((x (sread in)))
    (racket-if (ac-no x)
                 nil
               (racket-begin (eval x)
                             (ac-eval-all in runtime)))))
