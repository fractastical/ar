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

(racket-define stdin   racket-current-input-port)
(racket-define stdout  racket-current-output-port)
(racket-define stderr  racket-current-error-port)

(racket-define (ac-disp x port)
  (racket-display x port)
  (racket-flush-output port))

(racket-define (ac-write x port)
  (racket-write x port)
  (racket-flush-output port))

(racket-define (disp x (port (stdout)))
  (print ac-disp x port))

(racket-define (write x (port (stdout)))
  (print ac-write x port))

(racket-define (sread input (eof nil))
  (racket-let ((v (racket-read input)))
    (racket-if (racket-eof-object? v)
                 eof
               (ac-deep-toarc v))))


;=============================================================================
;  Predicates
;=============================================================================

(racket-define t    (racket-quote t))
(racket-define nil  racket-null)

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

(racket-define (ac-lex? v env)
  (racket-and (racket-mpair? env)
              (racket-or (racket-eq? v (car env))
                         (ac-lex? v (cdr env)))))

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

(racket-define (ac-mac-call m args env)
  (ac-compile (apply m args) env))

(racket-define (ac-args args env)
  (map1 (racket-lambda (expr)
          (ac-compile expr env))
        args))

(racket-define (ac-call f args env)
  (racket-let* ((g  (racket-not (ac-lex? f env)))
                (m  (racket-and g (ac-macro? f)))
                (f  (racket-parameterize ((ac-functional-position? t))
                      (ac-compile f env))))
    (racket-cond
      (m (ac-mac-call m args env))
      ;; if we're about to call a literal fn such as ((fn (a b) ...) 1 2)
      ;; then we know we can just call it in Racket and we don't
      ;; have to use ac-apply
      ((ac-caris f (racket-quote racket-lambda))
       (cons f (ac-args args env)))
      #|
      ;; if it's a global function, don't bother calling ac-apply or ac-funcall
      ((racket-and g
                   (racket-symbol? f)
                   (racket-procedure? (namespace-get namespace f nil)))
       (cons f (ac-args args env)))|#
      (racket-else
       (cons (racket-case (len args)
               ((0) ac-funcall0)
               ((1) ac-funcall1)
               ((2) ac-funcall2)
               ((3) ac-funcall3)
               ((4) ac-funcall4)
               (racket-else ac-apply))
             (cons f (ac-args args env)))))))


;=============================================================================
;  assign
;=============================================================================

(racket-define (ac-local-assign a b)
  #|(racket-let ((result (uniq)))
    (list (racket-quote racket-let)
          (list (list result (ac-compile b env)))
          (list (racket-quote racket-set!) a result)
          result))|#
  (list (racket-quote racket-set!) a b))

(racket-define (ac-global-assign a b)
  (list (racket-quote racket-set!) a b))

(racket-define (ac-assign1 a b1 env)
  (racket-unless (racket-symbol? a)
    (err "First arg to assign must be a symbol" a))

  (racket-let ((result (ac-compile b1 env)))
    (racket-if (ac-lex? a env)
                 (ac-local-assign a result)
               (ac-global-assign a result))))

(racket-define (ac-assignn x env)
  (racket-if (ac-no x)
               nil
             ;; TODO: why does Arc 3.1 call ac-macex here?
             (cons (ac-assign1 (car x) (cadr x) env)
                   (racket-if (ac-no (cddr x))
                                (list (car x))
                              (ac-assignn (cddr x) env)))))

(racket-define (ac-assign x env)
  (cons (racket-quote racket-begin)
        (ac-assignn x env)))

(racket-define assign (annotate (racket-quote mac)
                        (racket-lambda args
                          (cons ac-assign (racket-list->mlist args)))))


;=============================================================================
;  fn
;=============================================================================

(racket-define ac-fn-required-args?       (racket-make-parameter t))
(racket-define ac-fn-excess-args?         (racket-make-parameter nil))
(racket-define ac-fn-rigid-destructuring? (racket-make-parameter nil))

(racket-define (ac-fn-keyword-args x default)
  (list x (list (racket-string->symbol (racket-keyword->string x))
                default)))

(racket-define (ac-fn-optional-args x body env)
  (racket-let ((c (car x))
               (default (racket-let ((x (cdr x)))
                          (racket-if (ac-no x)
                            (racket-quote nil)
                            (ac-compile (car x) env)))))
    (racket-if (racket-keyword? c)
                 (ac-fn-keyword-args c default)
               (list (list c default)))))


#|
;; TODO: unit tests verifying the minimalness of the destructuring code output
(racket-define (ac-fn-destructuring-args u x body env)
  (racket-let self ((x x))
    (racket-cond
      ((ac-no x)
        nil)
      ((racket-mpair? (car x))
        (racket-let ((v (uniq)))
          (racket-mappend (list (list v (list (racket-quote car) u)))
                          (ac-fn-destructuring-args v (car x) body env)
                          (racket-if (ac-no (cdr x))
                                       nil
                                     (list (list u (list (racket-quote cdr) u))))
                          (self (cdr x)))))
      (racket-else
        (cons (list (car x) (list (racket-quote car) u))
              (racket-if (ac-no (cdr x))
                           nil
                         (cons (list u (list (racket-quote cdr) u))
                               (self (cdr x)))))))))|#


(racket-define (ac-fn-destructuring-args u x body env)
  (racket-let ((rigid (ac-fn-rigid-destructuring?)))
                ;; TODO: replace with apply after I make it faster
    (list (list racket-apply
                ;apply
                ;(racket-quote apply)
                (racket-parameterize ((ac-fn-required-args? rigid)
                                      (ac-fn-excess-args?   (ac-bool rigid)))
                  (cons (racket-quote racket-lambda)
                        (ac-fn-args x body env))
                  #|(ac-compile (list* (racket-quote fn)
                                     x
                                     body)
                              env)|#
                              )
                ;u
                (list racket-mlist->list u)
                ))))

#|(racket-define (ac-fn-rest-args x body env)
  (cons (list (racket-quote racket-set!) x (list racket-list->mlist x))
        body))|#

(racket-define (ac-fn-rest-args x body env)
  (racket-let ((x
  (list (list* (racket-quote racket-let)
               (list (list x (list racket-list->mlist x)))
               body))))
    ;(racket-display (ac-deep-fromarc x))
    ;(racket-newline)
    x
         ))

(racket-define (ac-fn-required-args x body env)
  (racket-if (ac-true (ac-fn-required-args?))
               x
             (list x (racket-quote nil))))

(racket-define (ac-fn-end-of-args x body env)
  (racket-if (ac-true (ac-fn-excess-args?))
               (uniq)
             nil))

(racket-define (ac-fn-normal-args x body env)
  (cons (racket-let self ((x x))
          ;; TODO: hacky
          (racket-when (racket-mpair? x)
            (racket-set-mcar! x (ssexpand (car x))))

          (racket-cond
            ((racket-eq? x nil)                   ;; end of the argument list
              (ac-fn-end-of-args x body env))
            ((racket-symbol? x)                   ;; dotted rest args
              (racket-set! body (ac-fn-rest-args x body env))
              x)
            ((ac-caris (car x) (racket-quote o))  ;; optional args
              (racket-mappend (ac-fn-optional-args (cdr (car x)) body env)
                              (self (cdr x))))
            ((racket-keyword? (car x))            ;; keyword args
              (racket-mappend (ac-fn-keyword-args (car x)
                                                  (racket-quote nil))
                              (self (cdr x))))
            ((racket-mpair? (car x))              ;; destructuring args
              (racket-let ((u (uniq)))
                (racket-set! body (ac-fn-destructuring-args u (car x) body env))
                #|(racket-set! body (list (list* (racket-quote racket-let*)
                                               (ac-fn-destructuring-args u (car x) body env)
                                               body)))|#
                (cons (ac-fn-required-args u body env)
                      (self (cdr x)))))
            (racket-else                          ;; normal args
              (cons (ac-fn-required-args (car x) body env)
                    (self (cdr x))))))
        body))

(racket-define (ac-fn-args x body env)
  (racket-when (ac-no body)
    (racket-set! body (list (racket-quote nil))))

  #|(racket-when (ac-no body)
    (racket-set! body (list nil)))|#

  (racket-cond
    ((racket-symbol? x)
     (cons x (ac-fn-rest-args x body env)))
    (racket-else
     (ac-fn-normal-args x body env))))


;; TODO: ew, hacky
(racket-define (ac-arglist a)
  (racket-cond
   ((ac-no a)
     nil)
   ((racket-symbol? a)
     (list a))
   ((racket-and (racket-symbol? (cdr a))
                (ac-true (cdr a)))
     (list (car a) (cdr a)))
   (racket-else
     (cons (car a) (ac-arglist (cdr a))))))

(racket-define (ac-fn x env)
  (racket-set! env (racket-mappend (ac-arglist (cadr x)) env))
  (cons (racket-quote racket-lambda)
        (ac-fn-args (cadr x)
                    (ac-args (cddr x) env)
                    env)))

(racket-define fn (annotate (racket-quote mac)
                    (racket-lambda (parms . body)
                      (list* ac-fn parms (racket-list->mlist body)))))


;=============================================================================
;  if
;=============================================================================

(racket-define (ac-if args env)
  (racket-cond
   ((ac-no args)
    (racket-quote nil))
   ((ac-no (cdr args))
    (ac-compile (car args) env))
   (racket-else
    (list (racket-quote racket-if)
          (list ac-true (ac-compile (car args) env))
          (ac-compile (cadr args) env)
          (ac-if (cddr args) env)))))

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
  ;(racket-display x)
  ;(racket-newline)
  (list ac-deep-toarc
        (list (racket-quote racket-quote)
              x)))

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

(racket-define (ac-nocompile x env)
  (racket-let ((x (racket-let self ((x x))
                    (ac-mappend (racket-lambda (x)
                                  (racket-cond
                                    ((ac-caris x (racket-quote %compile))
                                      (map1 (racket-lambda (x)
                                              (ac-compile x env))
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

(racket-define (ac-local-var x env)
  x)

(racket-define (ac-lookup-global-fn x)
  x)

(racket-define (ac-lookup-global-arg x)
  ;; This implements implicit parameters
  (racket-if (racket-parameter? x)
               (x)
             (ac-lookup-global-fn x)))

(racket-define (ac-global-var x env)
  (racket-if (ac-true (ac-functional-position?))
               (ac-lookup-global-fn x)
             (list ac-lookup-global-arg x)))


;=============================================================================
;  compiler/eval
;=============================================================================

(racket-define (ac-compile x env)
  (racket-cond
    ((ac-caris x ac-nocompile)
      (ac-nocompile (cdr x) env))
    ((ac-caris x ac-assign)
      (ac-assign (cdr x) env))
    ((ac-caris x ac-fn)
      (ac-fn x env))
    ((ac-caris x ac-if)
      (ac-if (cdr x) env))
    ((ac-caris x ac-quote)
      (ac-quote (cadr x)))
    ((racket-mpair? x)
      (ac-call (car x) (cdr x) env))
    ((racket-eq? x nil)
     (racket-quote nil))
    ((ac-true (ssyntax x))
     (ac-compile (ssexpand x) env))
    ((racket-symbol? x)
     (racket-if (ac-lex? x env)
                  (ac-local-var x env)
                (ac-global-var x env)))
    (racket-else x)))

(racket-define (eval x (runtime nil))
  (racket-eval (ac-deep-fromarc (ac-compile x nil))
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
