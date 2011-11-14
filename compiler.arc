#|(racket-define -         racket--)
(racket-define /         racket-/)
(racket-define *         racket-*)
(racket-define inside    racket-get-output-string)
(racket-define instring  racket-open-input-string)
(racket-define outstring racket-open-output-string)
(racket-define +         racket-+)|#



#|(racket-define-syntax-rule (assign x y)
  (racket-set! x y))

(racket-define-syntax-rule (fn parms ...body)
  (racket-lambda parms ...body))|#

#|(racket-define-syntax-rule (ac-name-toracket x y)
  (racket-define-syntax-rule (x ...body)
    (y ...body)))

;(ac-name-toracket assign           racket-set!)
(ac-name-toracket quote            racket-quote)
(ac-name-toracket quasiquote       racket-quasiquote)
(ac-name-toracket unquote          racket-unquote)
(ac-name-toracket unquote-splicing racket-unquote-splicing)|#



#|(racket-define-syntax-rule (quasiquote x)
  (qq-expand x))|#

#|(ail-code (ac-extend ac (s env) (caris s 'quasiquote)
  (racket-let ((expansion (qq-expand (cadr s))))
    (ac expansion env))))|#

#|(racket-define-syntax-rule (quote x)
  (racket-quote x))

(racket-define-syntax-rule (quasiquote x)
  (racket-quasiquote x))|#

;(racket-define quote            racket-quote)
;(racket-define unquote          racket-unquote)
;(racket-define unquote-splicing racket-unquote-splicing)
;(racket-define quasiquote       racket-quasiquote)
;(racket-define-syntax-parameter quote (fn (x) #'racket-quote))


;(racket-define car       racket-mcar)
;(racket-define cdr       racket-mcdr)


#|(racket-current-readtable (racket-make-readtable (racket-current-readtable) #\( 'terminating-macro
                            (fn (ch port src line col pos)
                              (racket-list->mlist (racket-read/recursive port #\( #f)))))|#

#|(assign do (annotate 'mac
             (fn args `((fn () ,@args)))))|#

#|(racket-define (ac-r/list-toarc x)
  (racket-cond
    ((racket-pair? x)
     (racket-mcons (racket-car x) (ac-r/list-toarc (racket-cdr x))))
    (racket-else x)))|#

#|(racket-define ac-deep-toarc   (ac-make-deep racket-pair? cons racket-car racket-cdr racket-mpair?))
(racket-define ac-deep-fromarc (ac-make-deep racket-mpair? racket-cons car cdr racket-pair?))|#

#|(racket-define (ac-combine-args as)
   (racket-let next ((as as) (accum (racket-list)))
     (racket-cond
      ((racket-null? as)
       accum)
      ((racket-null? (racket-cdr as))
       (racket-append accum (ac-list-fromarc (racket-car as))))
      (racket-else
       (next (racket-cdr as)
             (racket-append accum (racket-list (racket-car as))))))))|#

#|(racket-define (ac-assign1 a b1 env)
  (racket-unless (racket-symbol? a)
    (err "First arg to assign must be a symbol" a))
  (racket-let ((result (uniq)))
    (list (racket-quote racket-let)
          (list (list result (ac-compile b1 env)))
          (racket-if (ac-lex? a env)
                       (list (racket-quote racket-set!) a result)
                     (ac-global-assign a result))
          result)))|#

#|(ail-code (ac-extend ac (s env) (caris s (racket-quote quote))
  (racket-let ((v (cadr s)))
    (list (list (racket-quote racket-quote) (racket-lambda () v))))))|#

;(= foo '(1 2 3))
;(= (foo 0) 5)

;foo => '(5 2 3)

;(racket-set! foo (quote 1 2 3))
;(racket-set-mcar! foo 5)
;foo -> ((quote (racket-lambda () '(1 2 3))))

#|(racket-not (racket-eq? (racket-cddr args) racket-null))
(racket-else
     )|#

;(racket-current-eval      eval)
;(racket-current-readtable nu-read)

;(nu-load namespace "arc.arc")

#|(racket-define-syntax-rule (quote x)
  (ac-deep-toarc (racket-quote x)))|#

#|(racket-define ac-assign (uniq))
(racket-define ac-quote  (uniq))
(racket-define ac-fn     (uniq))
(racket-define ac-if     (uniq))|#


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
      (primitive x port))))


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
  (print ac-disp x port)
  nil)

(racket-define (write x (port (stdout)))
  (print ac-write x port)
  nil)

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

(racket-define (ac-true x)
  (racket-not (ac-no x)))

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

(racket-define (ac-length xs)
  (racket-let loop ((x xs) (n 0))
    (racket-cond
      ((racket-mpair? x)   (loop (cdr x) (racket-+ n 1)))
      ((racket-eq? x nil)  n)
      (racket-else         (err "len expects a proper list" xs)))))

#|(racket-define (ac-length2 x)
  (racket-cond
    ((ac-no x)          0)
    ((racket-mpair? x)  (racket-+ 1 (ac-length2 (cdr x))))
    (racket-else        (err "len expects a proper list"))))|#

(racket-define (len x)
  (racket-cond
    ((racket-string? x) (racket-string-length x))
    ((racket-hash? x)   (racket-hash-count x))
    (racket-else        (ac-length x))))

(racket-define (ac-mappend f x)
  (apply racket-mappend (map1 f x))) ;(racket-mlist->list


;=============================================================================
;  apply
;=============================================================================

(racket-define (ac-apply-non-fn x args)
  (racket-cond
    ((racket-mpair? x)
     (racket-mlist-ref x (car args)))
    ((racket-string? x)
     (racket-string-ref x (car args) (cadr args)))
    ((racket-hash? x)
     (racket-hash-ref x (car args) (cadr args)))
    (racket-else
     (err "Function call on inappropriate object" x args))))

(racket-define (ac-apply f . racket-arg-list)
  (racket-if (racket-procedure? f)
               (racket-apply f racket-arg-list)
             (ac-apply-non-fn f (racket-list->mlist racket-arg-list))))

(racket-define (apply f . args)
  ;; TODO: figure out how to avoid the mlist->list call
  (racket-apply ac-apply f (racket-mlist->list (racket-apply list* args))))


;=============================================================================
;  Function/Macro calls
;=============================================================================

(racket-define (ac-funcall0 fn)
  (racket-if (racket-procedure? fn)
               (fn)
             (ac-apply fn)))

(racket-define (ac-funcall1 fn arg1)
  (racket-if (racket-procedure? fn)
               (fn arg1)
             (ac-apply fn arg1)))

(racket-define (ac-funcall2 fn arg1 arg2)
  (racket-if (racket-procedure? fn)
               (fn arg1 arg2)
             (ac-apply fn arg1 arg2)))

(racket-define (ac-funcall3 fn arg1 arg2 arg3)
  (racket-if (racket-procedure? fn)
               (fn arg1 arg2 arg3)
             (ac-apply fn arg1 arg2 arg3)))

(racket-define (ac-funcall4 fn arg1 arg2 arg3 arg4)
  (racket-if (racket-procedure? fn)
               (fn arg1 arg2 arg3 arg4)
             (ac-apply fn arg1 arg2 arg3 arg4)))

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
  (racket-let ((m (racket-and (racket-not (ac-lex? f env))
                              (ac-macro? f)))
               (f (ac-compile f env)))
    (racket-cond
      (m (ac-mac-call m args env))
      ;; if we're about to call a literal fn such as ((fn (a b) ...) 1 2)
      ;; then we know we can just call it in Racket and we don't
      ;; have to use ac-apply
      ((ac-caris f (racket-quote racket-lambda))
       (cons f (ac-args args env)))
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

(racket-define (ac-lex? v env)
  (racket-and (racket-mpair? env)
              (racket-or (racket-eq? v (car env))
                         (ac-lex? v (cdr env)))))

(racket-define (ac-local-assign a b)
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
             ;; todo: why does Arc 3.1 call ac-macex here?
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

#|(racket-define (ac-destructuring-args? x)
  ;; TODO: clunky
  (racket-cond
    ((ac-no x) #f)
    ((racket-symbol? x) #f)
    ((racket-and (racket-mpair? (car x))
                 (racket-not (ac-caris (car x) (racket-quote o))))
     #t)
    (racket-else (ac-destructuring-args? (cdr x)))))|#

(racket-define (ac-fn-args1 x)
  (racket-cond
    ((ac-caris x (racket-quote o))  (list (cadr x)
                                          (racket-let ((x (cddr x)))
                                            (racket-if (ac-no x)
                                              (racket-quote nil)
                                              (car x)))))
    (racket-else                    x)))

(racket-define (ac-fn-rest-args x body env)
  (cons (list (racket-quote racket-set!) x (list racket-list->mlist x))
        body))

(racket-define (ac-fn-complex-args x body env)
  (cons (racket-let self ((x x))
          (racket-cond
            ((racket-eq? x nil)
              nil)
            ((racket-symbol? x)
              (racket-set! body (ac-fn-rest-args x body env))
              x)
            ((ac-caris (car x) (racket-quote o))
              (cons (racket-let ((x (car x)))
                      (list (cadr x)
                            (racket-let ((x (cddr x)))
                              (racket-if (ac-no x)
                                (racket-quote nil)
                                (car x)))))
                    (self (cdr x))))
            (racket-else
              (cons (car x) (self (cdr x))))))
        body))

(racket-define (ac-fn-args x body env)
  (racket-when (ac-no body)
    (racket-set! body (list (racket-quote nil))))
  (racket-cond
    ((racket-symbol? x)
     (cons x (ac-fn-rest-args x body env)))
    #|((ac-destructuring-args? x)
     (ac-fn-destructure x body env))|#
    (racket-else
     (ac-fn-complex-args x body env)
     ;(cons (map1 ac-fn-args1 x) body)
     )))

(racket-define (ac-fn x env)
  ;(racket-display (eval (list* (racket-quote racket-lambda) (cadr x) (cddr x))))
  ;(racket-newline)
  (cons (racket-quote racket-lambda)
        (ac-fn-args (cadr x)
                    (ac-args (cddr x) env)
                    env)))

(racket-define fn (annotate (racket-quote mac)
                    (racket-lambda (parms . body)
                      #|(racket-display parms)
                      (racket-display body)
                      (racket-newline)|#
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
  (racket-when (racket-eq? x (racket-quote nil))
    (racket-set! x nil))

  (list (list (racket-quote racket-quote)
              (racket-lambda () x))))

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
    (list (racket-quote list) (cadr x)))
   ((ac-caris x (racket-quote unquote-splicing))
    (cadr x))
   ((ac-caris x (racket-quote quasiquote))
    (qq-expand-list (qq-expand (cadr x))))
   ((racket-mpair? x)
    (list (racket-quote list) (qq-expand-pair x)))
   (racket-else
    (list (racket-quote quote) (list x)))))

(racket-define (qq-expand-pair x)
  (list (racket-quote racket-mappend)
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
    (list (racket-quote quote) x))))

(racket-define quasiquote (annotate (racket-quote mac)
                            (racket-lambda args
                              (apply qq-expand (racket-list->mlist args)))))


;=============================================================================
;  is
;=============================================================================

(racket-define (ac-pairwise pred lst)
  (racket-cond
    ((ac-no lst) t)
    ((ac-no (cdr lst)) t)
    ((racket-not (racket-eq? (pred (car lst) (cadr lst)) nil))
     (ac-pairwise pred (cdr lst)))
    (racket-else nil)))

(racket-define (is2 a b)
  (ac-tnil (racket-or (racket-eqv? a b)
                      (racket-and (racket-string? a)
                                  (racket-string? b)
                                  (racket-string=? a b)))))

(racket-define is (racket-case-lambda
                    ((x y) (is2 x y))
                    (args  (ac-pairwise is2 (racket-list->mlist args)))))

#|(racket-define (is . args)
  (ac-pairwise is2 (racket-list->mlist args)))|#


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
;  %nocompile/%compile
;=============================================================================

#|(racket-define (ac-nocompile x env)
  (cons (racket-quote racket-begin)
        (racket-let self ((x x))
          (racket-cond
            ((ac-caris x (racket-quote %compile))
              (map1 (racket-lambda (x) (ac-compile x env)) (cdr x)))
            ((racket-mpair? x)
              (racket-display (car x))
              (racket-newline)
              (racket-if (racket-mpair? (car x))
                           (racket-mappend (self (car x))
                                           (self (cdr x)))
                         (map1 self x)))
            (racket-else x))))) ;(cons (car x) (self (cdr x)))|#

(racket-define (ac-nocompile x env)
  (cons (racket-quote racket-begin)
        (racket-let self ((x x))
          (ac-mappend (racket-lambda (x)
                        (racket-cond
                          ((ac-caris x (racket-quote %compile))
                            (map1 (racket-lambda (x)
                                    (ac-compile x env))
                                  (cdr x)))
                          ((racket-mpair? x)
                            (list (self x)))
                          (racket-else (list x))))
                      x))))

#|(mappend (fn (x)
           (if (caris x '%compile)
                 (map (fn (x) (ac-compile x env)) (cdr x))
               (cons? x)
                 (list (map self x))))
         x)|#

(racket-define %nocompile (annotate (racket-quote mac)
                            (racket-lambda args
                              (cons ac-nocompile (racket-list->mlist args)))))


;=============================================================================
;  compiler/eval
;=============================================================================

(racket-define (ac-compile x env)
  #|(racket-display x)
  (racket-newline)|#
  (racket-cond
    #|((ac-caris x ac-quote)
      (racket-let ((v (cadr x)))
        (list (list (racket-quote racket-quote) (racket-lambda () v)))))|#
    #|((racket-symbol? x)
      x)|#
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
    (racket-else x)))

(racket-define (eval x (runtime nil))
  (racket-eval (ac-deep-fromarc (ac-compile x nil))
               (racket-if (ac-no runtime)
                            namespace
                          runtime)))


;=============================================================================
;  load
;=============================================================================

(racket-define (ac-eval-all in runtime)
  (racket-let ((x (sread in)))
    (racket-if (ac-no x)
                 nil
               (racket-begin (eval x)
                             (ac-eval-all in runtime)))))

#|(racket-define (ac-load filename)
  (racket-call-with-input-file filename ac-eval-all))|#

(ac-load "core.arc")
(ac-load "repl.arc")


;=============================================================================
;  REPL
;=============================================================================

#|(racket-define repl-eof-value (uniq))

(racket-define (repl)
  (on-err (racket-lambda (c)
            ;(debug "repl" c)
            (disp "error: ")
            (disp (details c))
            (racket-newline)
            (repl))
          (racket-lambda ()
            (disp "arc> ")
            (racket-let ((expr (sread (stdin) repl-eof-value)))
              (racket-if (racket-eq? expr repl-eof-value)
                           (racket-newline)
                         (racket-begin (racket-read-line)
                                       (racket-let ((val (eval expr)))
                                         (write val)
                                         (racket-newline)
                                         (racket-set! that val)
                                         (racket-set! thatexpr expr)
                                         (repl))))))))

(repl)|#
