;#lang s-exp '#%kernel

(racket-define (ac-prn . args)
  (racket-for-each (racket-lambda (x)
                     (racket-display x)
                     (racket-display " "))
                   args)
  (racket-newline)
  (racket-car args))

;(#%require (only racket/base #%app #%datum #%top #%top-interaction))
#|(#%require (prefix-all-except racket- racket/base
             #%app #%datum #%top #%top-interaction))|#

;(#%require (only racket/base namespace-require/copy))
;(#%require (prefix racket- racket/base))

;(racket-require (racket-prefix-in racket- racket/base))

;(#%require (prefix racket- racket/base))
#|(racket-require (racket-rename-in
                  (racket-prefix-in racket- racket/mpair)
                  [racket-mcons cons]
                  [racket-mlist list]))|#

(racket-namespace-require/copy (racket-quote (prefix racket- racket/mpair)))
(racket-namespace-require/copy (racket-quote (prefix racket- racket/path)))
(racket-namespace-require/copy (racket-quote (prefix racket- racket/system)))

#|(racket-require (racket-prefix-in racket- racket/mpair))
(racket-require (racket-prefix-in racket- racket/path))
(racket-require (racket-prefix-in racket- racket/system))|#

#|(racket-provide (racket-all-defined-out)
                (racket-all-from-out racket/base
                                     racket/mpair
                                     racket/path
                                     racket/system))|#


#|(racket-displayln cons?)
(racket-displayln module->namespace)
(racket-displayln racket-mcons)|#

;(racket-displayln compiler)
;(racket-displayln module->namespace)
#|(racket-displayln (module->namespace (racket-list (racket-quote quote)
                                                  (syntax->datum (namespace-module-identifier)))))|#

;(racket-displayln (racket-module->namespace (racket-list (racket-quote quote) (racket-quote compiler))))
;(racket-displayln (racket-namespace-mapped-symbols))

#|(racket-define-syntax-rule (ac-parameter name x)
  (racket-let ((x (racket-make-parameter x)))
    (racket-define name (annotate (racket-quote alias) (list x x)))))|#

;(ac-lookup-global arc3-namespace 'namespace)


(racket-define t            (racket-quote t))
(racket-define nil          racket-null)
;(racket-define nil          (racket-quote nil))

;; TODO: tests for the fn environment
(racket-define ac-local-env      (racket-make-parameter nil))
;(racket-define ac-assign-name    (racket-make-parameter #f))

#|
;; Setting this to `t` makes code 60% faster, but disables namespace functionality
;; Setting this to `t` makes code 28% faster, but disables namespace functionality
(racket-define ac-direct-globals (racket-make-parameter nil))|#

#|
;; TODO: does this need to be a Racket macro...?
(racket-define-syntax-rule (ac-w/local-env x ...body)
  (racket-parameterize ((ac-local-env (racket-mappend (ac-local-env) x)))
    ...body))|#

(racket-define arc3-namespace (racket-current-namespace))
(racket-define ac-namespace   (racket-make-parameter arc3-namespace))
;(racket-define namespace racket-current-namespace)

(racket-define (namespace-get runtime varname (default nil))
  (racket-let ((default (racket-if (racket-procedure? default)
                                   #|(racket-or (racket-not default)
                                              )|#
                                     default
                                   (racket-lambda () default))))

    (racket-namespace-variable-value varname #f default runtime)

    #|(racket-namespace-variable-value varname #f
      (racket-lambda ()
        (racket-namespace-variable-value varname #t default runtime))
      runtime)|#
  ))

(racket-define (namespace-set runtime varname value)
  (racket-namespace-set-variable-value! varname value #f runtime))

;=============================================================================
;  Hash Tables
;=============================================================================

(racket-define sig       (racket-make-hash))
(racket-define ac-names  (racket-make-hash))


;=============================================================================
;  Predicates
;=============================================================================

(racket-define (ac-tnil x)
  (racket-if x t nil))

#|(racket-define (ac-#f x)
  (racket-if x x nil))|#

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
;  Mapping/Iteration
;=============================================================================

(racket-define (map1 f xs)
  (racket-if (racket-mpair? xs)
               (cons (f (car xs)) (map1 f (cdr xs)))
             xs))

(racket-define (dottedmap1 f xs)
  (racket-cond ((racket-mpair? xs)
                 (cons (f (car xs)) (dottedmap1 f (cdr xs))))
               ((ac-no xs)
                 xs)
               (racket-else
                 (f xs))))


;=============================================================================
;  Symbols
;=============================================================================

(racket-define ac-uniq-counter* (racket-make-parameter 1))

(racket-define (uniq (name (racket-quote g)) (num nil))
  (racket-let ((num (racket-if (ac-true num)
                                 num
                               (racket-let ((num (ac-uniq-counter*)))
                                 (ac-uniq-counter* (racket-+ (ac-uniq-counter*) 1))
                                 num))))
    (racket-string->uninterned-symbol
      ;; TODO: should use (string ...) rather than the Racket functions
      (racket-string-append (racket-symbol->string name)
                            (racket-number->string num)))))

(racket-define (ac-var x (def nil))
  ;(namespace-get arc3-namespace x def)
  ;(ac-apply-non-fn (namespace) nil nil (list x def))
  ;(namespace-get (namespace) x def)
  (ac-apply-non-fn (ac-namespace) x def)
  )

(racket-define bound
  (racket-let ((undef (uniq)))
    (racket-lambda (name)
      ;(racket-displayln (((ac-var name undef))))
      (ac-tnil
        (racket-not (racket-eq? (ac-var name undef) undef))))))


;=============================================================================
;  Types
;=============================================================================

#|(struct tagged (type value) #:mutable #:prefab
  #:auto-value 'no)

(racket-make-struct-type 'annotate #f 2 0 #f
  nil
  'prefab)|#

#|(racket-define-values (struct:tagged
                       annotate
                       ac-tagged?
                       ac-tagged-ref
                       ac-tagged-set!)
  (racket-make-struct-type (racket-quote annotate) #f 2 0 #f
    #|(racket-list (racket-cons prop:name
                              (racket-lambda (x)
                                (racket-display x))))|#
    nil
    ;(racket-quote prefab)
    #f
    #f
    nil
    #f
    ;(racket-quote annotate)
    ))|#

(racket-struct ac-tagged (type rep)
  ;#:prefab
  ;#:transparent
  #:constructor-name annotate
  #:mutable
  #|#:guard (racket-lambda (type rep name)
            (racket-let ((n (ac-assign-name)))
              (racket-if (racket-and n
                                     (racket-procedure? rep)
                                     (racket-not (racket-parameter? rep)))
                           (racket-values type (racket-procedure-rename rep n))
                         (racket-values type rep))))|#
  ;#:property racket-prop:set!-transformer
  ;           (racket-lambda (x) (racket-display x))
  )

#|(racket-struct ac-hash (rep)
  )|#

;(set-ac-tagged-type! x ...)
;(set-ac-tagged-rep! x ...)

;(racket-define annotate make-ac-tagged)

#|(racket-define annotate
  (racket-procedure-rename make-ac-tagged
                           (racket-quote annotate)))|#

#|(racket-define (annotate type rep)
  (racket-vector (racket-quote tagged) type rep))

(racket-define (ac-tagged? x)
  (racket-and (racket-vector? x)
              (racket-eq? (racket-vector-ref x 0) (racket-quote tagged))))|#

(racket-define (ac-exint? x)
  (racket-and (racket-integer? x) (racket-exact? x)))

(racket-define (type x)
  (racket-cond
    ((ac-tagged? x)           (ac-tagged-type x)) ;(racket-vector-ref x 1)
    ((racket-mpair? x)        (racket-quote cons))
    ((racket-null? x)         (racket-quote sym))
    ((racket-symbol? x)       (racket-quote sym))
    ;((racket-parameter? x)    (racket-quote parameter))
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
               (ac-tagged-rep x)
               ;(racket-vector-ref x 2)
             x))

(racket-define (ac-deep-toarc x)
  (racket-cond
    ((racket-pair? x)
      (cons (ac-deep-toarc (racket-car x))
            (ac-deep-toarc (racket-cdr x))))
    ((racket-string? x)
      (racket-string-copy x))
    ((racket-mpair? x)
      (err "Racket mpair passed to ac-deep-toarc" x))
    (racket-else x)))

(racket-define (ac-deep-fromarc x)
  (racket-cond
    ((racket-mpair? x)
     (racket-cons (ac-deep-fromarc (car x))
                  (ac-deep-fromarc (cdr x))))
    ((racket-pair? x)
      (err "Racket pair passed to ac-deep-fromarc" x))
    (racket-else x)))


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
      (racket-display ")" port))
    ((racket-mpair? (cdr x))
      (print primitive (car x) port)
      (racket-display " " port)
      (print-w/list primitive (cdr x) port))
    (racket-else
      (print primitive (car x) port)
      (racket-display " . " port)
      (print primitive (cdr x) port)
      (racket-display ")" port))))

(racket-define (name x)
  #|(racket-if (ac-tagged? x)
               (name (rep x))|#
  (racket-or (racket-hash-ref ac-names x #f)
             #|(racket-and (racket-or (racket-procedure? x)
                                    (racket-port? x))
                         )|#
             (racket-object-name x)
             nil)
             ;)
             )

(racket-define (print-w/name x l m r port)
  (racket-let ((x (name x)))
    (racket-display l port)
    (racket-when (ac-true x)
      (racket-display m port)
      (racket-display x port))
    (racket-display r port)))

(racket-define (print primitive x port)
  (racket-cond
    ((ac-no x)
      (racket-display "nil" port))
    ((racket-mpair? x)
      (racket-display "(" port)
      (print-w/list primitive x port))
    ((racket-eq? (type x) (racket-quote fn))
      (print-w/name x "#<fn" ":" ">" port))
    ((racket-eq? (type x) (racket-quote mac))
      (print-w/name x "#<mac" ":" ">" port))
    ((ac-tagged? x)
      (racket-display "#(" port)
      (racket-display "tagged " port)
      (print primitive (type x) port)
      (racket-display " " port)
      (print primitive (rep x) port)
      (racket-display ")" port))
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

#|(racket-define mmappend
  (racket-case-lambda
    (() racket-null)
    ((a) a)
    ((a b) (racket-let loop ((a a))
             (racket-if (racket-mpair? a)
                          (racket-mcons (racket-mcar a) (loop (racket-mcdr a)))
                        b)))
    ((a . l) (mmappend a (racket-apply mmappend l)))))|#

#|(racket-define (join . args)
  (racket-if (ac-no args)
               nil
             (racket-let ((a (racket-car args)))
               (racket-cond
                 ((racket-mpair? a)
                    (cons (car a) (racket-apply join (cdr a) (racket-cdr args))))
                 ((ac-no a)
                   (racket-apply join (racket-cdr args)))
                 (racket-else
                   a)))))|#

;(racket-display (join (list 1 2 3) (list 4 5) (list 6) (list 7 8 9) nil (list 10)))
;(racket-display (join (list 1 2 3) (list 4 5) (list 6) (list 7 8 9) nil (racket-quote b)))
;(racket-display (join (list 1 2 3) (list 4 5) (cons 6 (racket-quote a)) (list 7 8 9) nil (racket-quote b)))
;(racket-newline)

#|((1 2 3) (4 5) (6) (7 8 9) nil (10))
  ((1 2 3) (4 5) (6) (7 8 9) nil 'b)
  ((1 2 3) (4 5) (6 . b) (7 8 9) nil . b)
'(1 2 3 4 5 6 7 8 9 10)|#

#|((1 2 3) (4 5) (6) (7 8 9) nil (10))
(1 2 3)
(cons 1 2 3)|#

;((1 2 3) (4 5) (6) (7 8 9) nil (10))

;(cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (cons 6 (cons 7 (cons 8 (cons 9 (cons 10 nil))))))))))

(racket-define (ac-mappend f x)
  ;(racket-display (dottedmap1 f x))
  ;(racket-newline)
  ;(apply join (dottedmap1 f x))
  ;(racket-display x)
  ;(racket-newline)
  (apply racket-mappend (dottedmap1 f x))
  )


#|(racket-define (ac-mlist->list x)
  (racket-cond
    ((racket-mpair? x)
      (racket-cons (racket-mcar x) (ac-mlist->list (racket-mcdr x))))
    (racket-else x)))|#


(racket-define (caar xs)
  (car (car xs)))

(racket-define (assoc-ref al key)
  (racket-cond
    ((racket-not (racket-mpair? al))
      nil)
    ((racket-and (racket-mpair? (car al)) (racket-eq? (caar al) key))
      al)
    (racket-else
      (assoc-ref (cdr al) key))))

(racket-define (assoc al key)
  (car (assoc-ref al key)))

(racket-define (alref al key)
  (cadr (assoc al key)))


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

#|(racket-define (ac-apply-non-fn x k (d nil))
  ;(racket-displayln x)
  (racket-cond
    ((racket-namespace? x)
      (namespace-get x k d))
    ((racket-eq? (type x) (racket-quote parameter))
      ((rep x)))
    ((racket-mpair? x)
      (racket-if (racket-number? k)
                   (racket-mlist-ref x k)
                 ;; TODO: should alref be defined in compiler.arc?
                 (alref x k)))
    ((racket-string? x)
      (racket-string-ref x k))
    ((racket-hash? x)
      (racket-hash-ref x k d))
    (racket-else
      (err "function call on inappropriate object" x k d))))|#

(racket-define ac-apply-non-fn
  (racket-case-lambda
    [(x)     (racket-if (racket-eq? (type x) (racket-quote parameter))
                          ((rep x))
                        (err "function call on inappropriate object" x))]
    [(x k)   (racket-cond
               ((racket-namespace? x)
                 (namespace-get x k))
               ((racket-string? x)
                 (racket-string-ref x k))
               ((racket-hash? x)
                 (racket-hash-ref x k nil))
               ((racket-mpair? x)
                 (racket-if (racket-number? k)
                              (racket-mlist-ref x k)
                            ;; TODO: should alref be defined in compiler.arc?
                            (alref x k)))
               ((racket-eq? (type x) (racket-quote parameter))
                 ((rep x) k)
                 k)
               (racket-else
                 (err "function call on inappropriate object" x k)))]
    [(x k d) (racket-cond
               ((racket-namespace? x)
                 (namespace-get x k d))
               ((racket-hash? x)
                 (racket-hash-ref x k d))
               (racket-else
                 (err "function call on inappropriate object" x k d)))]))

(racket-define ac-apply
  ;; TODO: ew
  ;(racket-procedure-rename
  (racket-make-keyword-procedure
    (racket-lambda (kw kw-val f . racket-arg-list)
      (racket-cond
        #|((racket-parameter? f)
          (racket-keyword-apply (f) kw kw-val racket-arg-list))|#
        ((racket-procedure? f)
          (racket-keyword-apply f kw kw-val racket-arg-list))
        (racket-else
          (racket-keyword-apply ac-apply-non-fn kw kw-val f racket-arg-list)))))
    ;(racket-quote ac-apply))
  )

(racket-hash-set! ac-names ac-apply (racket-quote ac-apply))


;; TODO: why is apply very slow compared to ar and Arc 3.1? fix it
(racket-define apply
  (racket-make-keyword-procedure
    (racket-lambda (kw kw-val f . args)
      ;(racket-display (racket-apply list* args))
      ;(racket-display (racket-pair? (racket-cdr args)))
      ;(racket-display args)
      ;(racket-display (racket-car args))
      ;(racket-newline)
      (racket-keyword-apply ac-apply kw kw-val f (ac-arg-list* args))
      #|(racket-if (racket-pair? (racket-cdr args))
                   (racket-apply ac-apply f (ac-arg-list* args))
                 (racket-apply ac-apply f (racket-mlist->list (racket-car args))))|#

      ;time: 2556 msec.
      ;(racket-apply ac-apply (racket-car args))
      ;(racket-apply ac-apply f (racket-apply racket-list* args))
      ;(racket-apply ac-apply f (racket-mlist->list (racket-apply racket-list* args)))
      ;(racket-apply ac-apply f (racket-mlist->list (racket-apply list* args)))
  )))

(racket-hash-set! ac-names apply (racket-quote apply))


;=============================================================================
;  Function/Macro calls
;=============================================================================

(racket-define ac-functional-position? (racket-make-parameter #f))
;(racket-define ac-call-args            (racket-make-parameter nil))

(racket-define (ac-lex? x)
  (racket-let self ((env (ac-local-env)))
    (racket-and (racket-mpair? env)
                (racket-or (racket-eq? x (car env))
                           (self (cdr env))))))

;; TODO: should ac-funcall* accept keyword args...?
(racket-define (ac-funcall0 f)
  (racket-if (racket-procedure? f)
               (f)
             (ac-apply-non-fn f)))

(racket-define (ac-funcall1 f arg1)
  (racket-cond
    ;; this implements parameters
    #|((racket-parameter? f)
      ((f) arg1))|#
    ((racket-procedure? f)
      (f arg1))
    (racket-else
      (ac-apply-non-fn f arg1))))

(racket-define (ac-funcall2 f arg1 arg2)
  (racket-cond
    ;; this implements parameters
    #|((racket-parameter? f)
      ((f) arg1 arg2))|#
    ((racket-procedure? f)
      (f arg1 arg2))
    (racket-else
      (ac-apply-non-fn f arg1 arg2))))

(racket-define (ac-funcall3 f arg1 arg2 arg3)
  (racket-cond
    ;; this implements parameters
    #|((racket-parameter? f)
      ((f) arg1 arg2 arg3))|#
    ((racket-procedure? f)
      (f arg1 arg2 arg3))
    (racket-else
      (ac-apply-non-fn f arg1 arg2 arg3))))

(racket-define (ac-funcall4 f arg1 arg2 arg3 arg4)
  (racket-cond
    ;; this implements parameters
    #|((racket-parameter? f)
      ((f) arg1 arg2 arg3 arg4))|#
    ((racket-procedure? f)
      (f arg1 arg2 arg3 arg4))
    (racket-else
      (ac-apply-non-fn f arg1 arg2 arg3 arg4))))

(racket-define (ac-macro? f)
  (racket-cond
    ((racket-symbol? f)
      (racket-let ((v (ac-var f)))
        (racket-if (racket-eq? (type v) (racket-quote mac))
                     v
                   #f)))
    ((racket-eq? (type f) (racket-quote mac))
      f)
    (racket-else #f)))

(racket-define (ac-mac-call m args)
  (racket-if
    (racket-and (ac-functional-position?)
                (racket-or (racket-eq? m compose)
                           (racket-eq? m complement)))
      (cons m args)
    (ac-compile (apply (rep m) args))))

(racket-define (ac-args args)
  (racket-parameterize ((ac-functional-position? #f))
    (ac-mappend (racket-lambda (x)
                  (racket-let ((c (ac-compile x)))
                    (racket-if (ac-caris c ac-splice)
                                 (cdr c)
                               (list c))))
                args)
    ;(map1 ac-compile args)
                ))

(racket-define (ac-return-apply x)
  (racket-let loop ((x x) (n 0))
    (racket-cond
      ((racket-> n 4)
        ac-apply)
      ((ac-no x)
        (racket-case n
          ((0) ac-funcall0)
          ((1) ac-funcall1)
          ((2) ac-funcall2)
          ((3) ac-funcall3)
          ((4) ac-funcall4)))
      ((racket-keyword? (car x))
        ac-apply)
      (racket-else
        (loop (cdr x) (racket-+ n 1))))))

(racket-define compose    (uniq))
(racket-define complement (uniq))

(racket-define (ac-decompose fns args)
  (racket-cond
    ((ac-no fns)
      ;; TODO
      nil);`((fn vals (car vals)) ,@args)
    ((ac-no (cdr fns))
      (cons (car fns) args))
    (racket-else
      (list (car fns) (ac-decompose (cdr fns) args)))))

;(ac-caris f (rep compose))
;(ac-caris f (rep complement))

(racket-define (ac-call f args)
  ;; TODO: ew, mutation
  ;(racket-set! f (ssexpand f))
  ;(racket-displayln (rep f))
  (racket-let* ((g  (racket-not (ac-lex? f)))
                #|(c  (racket-and (racket-mpair? f)
                                (ac-macro? (car f))))|#
                (m  (racket-and g (ac-macro? f))))
    (racket-if m
      (ac-mac-call m args)
      (racket-let ((f  (racket-parameterize ((ac-functional-position? #t)
                                             ;(ac-call-args            args)
                                             )
                         (ac-compile f))))
        #|(racket-when (racket-and (racket-mpair? f) (cadr f))
          (racket-displayln f))|#
        ;(ac-prn f)
        (racket-cond
          ;; optimization for compose and complement in functional position
          ;; (and:or 3) => ((compose and or) 3) => (and (or 3))
          ((ac-caris f compose)
            (ac-compile (ac-decompose (cdr f) args)))
          ;; (~and 3 nil) => ((complement and) 3 nil) => (no (and 3 nil))
          ((ac-caris f complement)
            (ac-compile (list (racket-quote no)
                              (cons (car (cdr f)) args))))
          (racket-else
            (racket-let ((args  (ac-args args)))
              (racket-cond
                ;; if we're about to call a literal fn such as ((fn (a b) ...) 1 2)
                ;; then we know we can just call it in Racket and we don't
                ;; have to use ac-apply
                ((racket-or (racket-procedure? f)
                            (ac-caris f (racket-quote racket-lambda)))
                  (cons f args))
                #|(racket-else
                  (cons f args))|#
                #|
                ;; if it's a global function, don't bother calling ac-apply or ac-funcall
                ((racket-and g
                             (racket-symbol? f)
                             (racket-procedure? (namespace-get (ac-namespace) f nil)))
                 (cons f (ac-args args)))|#
                (racket-else
                  ;; TODO: ew, mutation
                  (racket-when (ac-caris f ac-splice)
                    (racket-set! args (racket-mappend (cddr f) args))
                    (racket-set! f    (cadr f)))

                  (cons (ac-return-apply args)
                        (cons f args)))
                ))))))))


;=============================================================================
;  Variables
;=============================================================================

(racket-define (ac-local-var x)
  x)

(racket-define (ac-undefined-var x)
  (racket-lambda ()
    (err "undefined variable:" x)))

(racket-define (ac-lookup-global-raw space x)
  (ac-apply-non-fn space x (ac-undefined-var x)))

(racket-define (ac-lookup-global x)
  ;(racket-displayln x)
  ;(racket-eval (racket-cons (racket-quote #%top) x) arc3-namespace)
  ;(racket-eval x arc3-namespace)
  ;x
  ;(racket-namespace-variable-value x #f #f arc3-namespace)
  #|(racket-namespace-variable-value x #f
    (racket-lambda ()
      (racket-namespace-variable-value x #t #f arc3-namespace))
    arc3-namespace)|#
  ;(ac-apply space x (ac-undefined-var x))

  ;(racket-displayln x)

  ;(racket-let ((x (ac-lookup-global-raw space x)))
  (racket-if (racket-eq? (type x) (racket-quote alias))
               ((car (rep x)))
             x)
               ;)

  #|(racket-if (racket-eq? space (ac-namespace))
               ;;
             (racket-let ((v (ac-lookup-global-raw (ac-namespace) x)))
               (racket-if (racket-eq? (type v) (racket-quote alias))
                            ((car (rep v)))
                          (racket-let ((x (ac-lookup-global-raw space x)))
                            (racket-if (racket-eq? (type x) (racket-quote alias))
                                         ((car (rep x)))
                                       x)))))|#

  ;; slower than using ac-var
  ;(ac-apply-non-fn space nil nil (list x (ac-undefined-var x)))
  ;(namespace-get space x (ac-undefined-var x))
  #|(racket-let ((x (ac-apply-non-fn space x (ac-undefined-var x))))
    (racket-if (racket-parameter? x)
                 (x)
               x))|#
  #|(racket-if (racket-eq? space (ac-namespace))
               (ac-apply-non-fn space x (ac-undefined-var x))
             (racket-let ((v (ac-apply-non-fn (ac-namespace) x)))
               (racket-if (racket-eq? (type v) (racket-quote alias))
                          ;(racket-parameter? v)
                            ;v
                            (racket-begin (racket-displayln v)
                            ((car (rep v))))
                          (ac-apply-non-fn space x (ac-undefined-var x)))))|#
  )

(racket-define (ac-lookup-global-arg x)
  ;(ac-lookup-global space x)
  ;(racket-displayln x)
  (racket-let ((x (ac-lookup-global x)))
    (racket-if (racket-eq? (type x) (racket-quote parameter))
                 ((rep x))
               x))
  #|(racket-let ((x (ac-lookup-global space x)))
    x
    ;; This implements parameters
    #|(racket-if (racket-parameter? x)
                 (x)
               x)|#
               )|#
  )

(racket-define (ac-global-var x)
  (racket-let* ((name (ac-namespace))
                                 ;; woot optimizations
                (x    (racket-if (racket-eq? name (racket-current-namespace))
                                   x
                                 (list ac-lookup-global-raw
                                       name
                                       (list (racket-quote racket-quote) x)))))
    (racket-if (ac-functional-position?)
                 (list ac-lookup-global x)
               (list ac-lookup-global-arg x))))


;=============================================================================
;  assign
;=============================================================================

(racket-define (ac-local-assign a b)
  #|(racket-let ((result (uniq)))
    (list (racket-quote racket-let)
          (list (list result (ac-compile b)))
          (list (racket-quote racket-set!) a result)
          result))|#
  (list (racket-quote racket-set!)
        a
        (ac-compile b)))

(racket-define (ac-global-assigner x a b)
  (racket-when (racket-or (ac-tagged? b)
                          (racket-procedure? b))
    (racket-hash-set! ac-names b a))

  (racket-cond
    ;; This implements parameters
    ((racket-eq? (type x) (racket-quote parameter))
      ((rep x) b))
    ((racket-eq? (type x) (racket-quote alias))
      ((cadr (rep x)) b))
    (racket-else
      (sref (ac-namespace) b a)))
  b)

(racket-define (ac-global-assign-undefined x a b)
  (ac-global-assigner x a b))

(racket-define (ac-global-assign-defined x a b)
  (ac-global-assigner x a b))

(racket-define ac-global-assign-raw
  (racket-let ((u (uniq)))
    (racket-lambda (a b)
                      ;; TODO: should this be ac-var or ac-lookup-global?
      (racket-let ((x (ac-var a u)))
        (racket-if (racket-eq? x u)
                     (ac-global-assign-undefined x a b)
                   (ac-global-assign-defined x a b))))))

(racket-define (ac-global-assign a b)
  ;; This allows annotate to assign a name to functions
  ;; TODO: figure out a way to get rid of this
  #|(list (racket-quote racket-parameterize)
        (list (list (racket-quote ac-assign-name)
                    (list (racket-quote racket-quote) a)))
        )|#
  (list ac-global-assign-raw
        (list (racket-quote racket-quote) a)
        (ac-compile b)))

(racket-define (ac-assign1 a b)
  (racket-unless (racket-symbol? a)
    (err "first arg to assign must be a symbol" a))

  (racket-if (ac-lex? a)
               (ac-local-assign a b)
             (ac-global-assign a b)))

(racket-define (ac-assignn x)
  (racket-if (ac-no x)
               nil
             ;; TODO: why does Arc 3.1 call ac-macex here?
             (cons (ac-assign1 (car x) (cadr x))
                              ;; TODO: ew
                   (racket-if (racket-and (ac-no (cddr x))
                                          (ac-lex? (car x)))
                                (list (ac-compile (car x)))
                              (ac-assignn (cddr x))))))

(racket-define (ac-assign x)
  (racket-let ((x (ac-assignn x)))
    (racket-if (ac-no (cdr x))
                 (car x)
               (cons (racket-quote racket-begin) x))))

;; TODO: make this prettier
(racket-define assign (annotate (racket-quote mac)
                        (racket-lambda args
                          (cons ac-assign (racket-list->mlist args)))))

(racket-hash-set! ac-names assign (racket-quote assign))


;=============================================================================
;  fn
;=============================================================================

;; TODO: tests for this
(racket-define ac-fn-optional-on-nil?     (racket-make-parameter #f))

(racket-define ac-fn-required-args?       (racket-make-parameter #t))
(racket-define ac-fn-excess-args?         (racket-make-parameter #f))
(racket-define ac-fn-rigid-destructuring? (racket-make-parameter #f))

(racket-define ac-fn-body                 (racket-make-parameter nil))
(racket-define ac-fn-let*                 (racket-make-parameter nil))

(racket-define (ac-add-to x y)
  (x (racket-mappend (x) (list y))))

(racket-define (ac-keyword->symbol x)
  (racket-string->symbol (racket-keyword->string x)))

(racket-define (ac-fn-keyword-args x default)
  (racket-let ((c (ac-keyword->symbol x)))
    (ac-add-to ac-local-env c)
    (list x (list c default))))

(racket-define (ac-fn-optional-args n default)
  ;; TODO: hacky
  (racket-parameterize ((ac-functional-position? #f))
    (racket-let ((default (ac-compile default)))
      (racket-if (racket-keyword? n)
                   (ac-fn-keyword-args n default)
                 (racket-begin (ac-add-to ac-local-env n)
                               (list (list n default)))))))

;; TODO: unit tests verifying the minimalness of the destructuring code output
;; TODO: huge hacky function
(racket-define (ac-fn-destructuring-args u x)
  (racket-let ((rigid (ac-fn-rigid-destructuring?)))
    (racket-let self ((x x))
      (racket-cond
        ((ac-no x)                            ;; end of the argument list
          nil)
        ((racket-symbol? x)                   ;; dotted rest args
          (ac-add-to ac-local-env x)
          (list (list x u)))
        ((ac-caris (car x) (racket-quote o))  ;; optional args
          (racket-let* ((c (car x))
                        (n (cadr c))
                        (d (car (cddr c))))
            ;; TODO: hacky
            (racket-set! n (ssexpand n))
            (ac-fn-optional-args n d)
            ;(ac-add-to ac-local-env n)
            (racket-if (racket-keyword? n)
                                     ;; TODO: code duplication
                         (cons (list (ac-keyword->symbol n)
                                     ;; TODO: ew, ac-funcall2
                                     (list ac-funcall2 u (list (racket-quote racket-quote)
                                                               (ac-keyword->symbol n))
                                                         d))
                               (self (cdr x)))
                       ;; TODO: hacky
                       (cons (racket-if (ac-fn-optional-on-nil?)
                                          (cons (list n (list car u))
                                                (list n (ac-compile (list if
                                                                          n
                                                                          n
                                                                          d))))
                                        (list n (ac-compile (list if
                                                                  (list (racket-quote %nocompile)
                                                                        (list cdr u))
                                                                  (list (racket-quote %nocompile)
                                                                        (list car u))
                                                                  d))))
                             ;; TODO: ew, code duplication
                             (racket-if (ac-no (cdr x))
                                          nil
                                        (cons (list u (list cdr u))
                                              (self (cdr x))))))))
        ((racket-mpair? (car x))              ;; destructuring args
          (racket-let ((v (uniq)))
            (racket-mappend (list (list v (list car u)))
                            (ac-fn-destructuring-args v (car x))
                            (racket-if (ac-no (cdr x))
                                         nil
                                       (list (list u (list cdr u))))
                            (self (cdr x)))))
        (racket-else                          ;; normal args
          (racket-let ((n (car x)))
            ;; TODO: hacky
            (racket-set! n (ssexpand n))
            (ac-fn-optional-args n nil)
            ;(ac-add-to ac-local-env n)
            (racket-if (racket-keyword? n)
                                     ;; TODO: code duplication
                         (cons (list (ac-keyword->symbol n)
                                     ;; TODO: ew, ac-funcall1
                                     (list ac-funcall1 u (list (racket-quote racket-quote)
                                                               (ac-keyword->symbol n))))
                               (self (cdr x)))
                       ;; TODO: ew, duplication
                       (cons (list n (list car u))
                             (racket-if (ac-no (cdr x))
                                          nil
                                        (cons (list u (list cdr u))
                                              (self (cdr x))))))))))))

(racket-define (ac-fn-rest-args x)
  (ac-add-to ac-local-env x)
  ;(ac-local-env (racket-mappend (ac-local-env) (list x)))
  (list (list x (list racket-list->mlist x))))

(racket-define (ac-fn-required-args x)
  (racket-if (ac-fn-required-args?)
               x
             (list x (racket-quote nil))))

(racket-define (ac-fn-end-of-args x)
  (racket-if (ac-fn-excess-args?)
               (uniq)
             nil))

(racket-define (ac-fn-normal-args x)
  ;; TODO: hacky
  (racket-when (racket-mpair? x)
    (racket-set-mcar! x (ssexpand (car x))))

  (racket-cond
    ((ac-no x)                            ;; end of the argument list
      (ac-fn-end-of-args x))
    ((racket-symbol? x)                   ;; dotted rest args
      (ac-fn-let* (racket-mappend (ac-fn-rest-args x) (ac-fn-let*)))
      x)
    ((ac-caris (car x) (racket-quote o))  ;; optional args
      (racket-let* ((c (car x))
                    (n (cadr c))
                    (d (car (cddr c))))
        ;; TODO: hacky
        (racket-set! n (ssexpand n))
        ;; TODO: really hacky
        (racket-if (ac-fn-optional-on-nil?)
                     (racket-begin
                       (racket-unless (ac-no d)
                         (ac-fn-let* (cons (list n (ac-compile (list if
                                                                     n
                                                                     n
                                                                     d)))
                                           (ac-fn-let*))))
                       (racket-mappend (list (list n (racket-quote nil)))
                                       (ac-fn-normal-args (cdr x))))
                     (racket-mappend (ac-fn-optional-args n d)
                                     (ac-fn-normal-args (cdr x))))
              ))
    ((racket-keyword? (car x))            ;; keyword args
      (racket-mappend (ac-fn-keyword-args (car x) (racket-quote nil))
                      (ac-fn-normal-args (cdr x))))
    ((racket-mpair? (car x))              ;; destructuring args
      (racket-let ((u (uniq)))
        (ac-fn-let* (racket-mappend
                      (ac-fn-destructuring-args u (car x))
                      (ac-fn-let*)))
        (cons (ac-fn-required-args u)
              (ac-fn-normal-args (cdr x)))))
    (racket-else                          ;; normal args
      (ac-add-to ac-local-env (car x))
      (cons (ac-fn-required-args (car x))
            (ac-fn-normal-args (cdr x))))))


(racket-define (ac-fn-args x)
  #|(racket-when (ac-no body)
    (racket-set! body (list nil)))|#
  (racket-cond
    ((racket-symbol? x)
      ;(cons x (ac-fn-rest-args x body))
      (ac-fn-body (list (list* (racket-quote racket-let)
                               (ac-fn-rest-args x)
                               (ac-args (ac-fn-body)))))
      x)
    (racket-else
      (racket-parameterize ((ac-fn-let* nil))
        (racket-let ((x (ac-fn-normal-args x)))
          (ac-fn-body (racket-if (ac-true (ac-fn-let*))
                                   (list (list* (racket-quote racket-let*)
                                                (ac-fn-let*)
                                                (ac-args (ac-fn-body))))
                                 (ac-args (ac-fn-body))))
          x)))))


#|(racket-define (ac-arglist-normal x)
  (racket-cond
    ((racket-keyword? x)
      (ac-keyword->symbol x)) ;; (sym x)
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

(racket-define (ac-fn parms body)
  ;; TODO: add env support back in, but better
  ;(racket-set! env (racket-mappend (ac-arglist (cadr x)) env))
  (cons (racket-quote racket-lambda)
        (racket-parameterize ((ac-fn-body (racket-if (ac-no body)
                                                       (list (racket-quote nil))
                                                     body)))
          (cons (racket-parameterize ((ac-local-env (ac-local-env)))
                ;ac-w/local-env nil
                  ;; TODO: remove this let
                  (racket-let ((x (ac-fn-args parms)))
                    ;(racket-display (ac-local-env))
                    ;(racket-newline)
                    x))
                (ac-fn-body)))))

;; TODO: make this prettier
(racket-define fn (annotate (racket-quote mac)
                    (racket-lambda (parms . body)
                      (list* ac-fn parms (racket-list->mlist body)))))

(racket-hash-set! ac-names fn (racket-quote fn))


;=============================================================================
;  if
;=============================================================================

(racket-define (ac-if args)
  (racket-cond
    #|((ac-no args)
      (racket-quote nil))|#
    ((ac-no (cdr args))
      (ac-compile (car args)))
    (racket-else
      (list (racket-quote racket-if)
            (list ac-true (ac-compile (car args)))
            (ac-compile (cadr args))
            (ac-if (cddr args))))))

;; TODO: make this prettier
(racket-define if (annotate (racket-quote mac)
                    (racket-lambda args
                      (cons ac-if (racket-list->mlist args)))))

(racket-hash-set! ac-names if (racket-quote if))


;=============================================================================
;  quote
;=============================================================================

#|
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

;; TODO: make this prettier
(racket-define quote (annotate (racket-quote mac)
                       (racket-procedure-rename
                         ;; Could pass ac-quote directly, but then it wouldn't
                         ;; work if somebody overwrites ac-quote later
                         (racket-lambda (x)
                           (cons ac-quote x))
                         (racket-quote quote))))|#

(racket-define quote (annotate (racket-quote mac)
                       (racket-lambda (x)
                         ;; TODO: not sure about the %nocompile part: is it
                         ;;       fast enough?
                         ;(list %nocompile (list (racket-lambda () x)))
                         (list %nocompile
                           (list (racket-procedure-rename (racket-lambda () x)
                                                          (racket-quote quoted)))))))

(racket-hash-set! ac-names quote (racket-quote quote))

#|

(mac quote (x)
  (list (fn () x)))

|#


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
    ;; TODO: don't hardcode the symbol unquote
    ((ac-caris x (racket-quote unquote))
      (list list (cadr x)))
      ;(list (racket-quote list) (cadr x)))
    ;; TODO: don't hardcode the symbol unquote-splicing
    ((ac-caris x (racket-quote unquote-splicing))
      (cadr x))
    ;; TODO: don't hardcode the symbol quasiquote
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
    ;; TODO: don't hardcode the symbol unquote
    ((ac-caris x (racket-quote unquote))
      (cadr x))
    ;; TODO: don't hardcode the symbol unquote-splicing
    ((ac-caris x (racket-quote unquote-splicing))
      (err "illegal use of ,@ in non-list quasiquote expansion"))
    ;; TODO: don't hardcode the symbol quasiquote
    ((ac-caris x (racket-quote quasiquote))
      (qq-expand (qq-expand (cadr x))))
    ((racket-mpair? x)
      (qq-expand-pair x))
    (racket-else
      (list quote x))))
    ;(list (racket-quote quote) x))))

;; TODO: make this prettier
(racket-define quasiquote (annotate (racket-quote mac)
                            (racket-lambda args
                              (racket-apply qq-expand args))))
                              ;(racket-apply qq-expand (racket-list->mlist args)))))

(racket-hash-set! ac-names quasiquote (racket-quote quasiquote))


;=============================================================================
;  assignment
;=============================================================================

(racket-define (sref com val ind)
  (racket-cond
    ((racket-namespace? com)
      (namespace-set com ind val))
    ((racket-hash? com)
      (racket-if (ac-no val)
                   (racket-hash-remove! com ind)
                 (racket-hash-set! com ind val)))
    ((racket-string? com)
      (racket-string-set! com ind val))
    ((racket-mpair? com)
      (racket-if (racket-number? ind)
                   (racket-set-mcar! (racket-mlist-tail com ind) val)
                 (racket-if (ac-no val)
                              ;; TODO: should assoc-ref be defined in compiler.arc?
                              (racket-let ((x (assoc-ref com ind)))
                                (racket-when (ac-true x)
                                  (racket-set-mcar! x (cadr x))
                                  (racket-set-mcdr! x (cddr x))))
                            ;; TODO: should assoc be defined in compiler.arc?
                            (racket-let ((x (assoc com ind)))
                              (racket-cond
                                ((ac-true x)
                                  (racket-set-mcar! (cdr x) val))
                                (racket-else
                                  ;; This is needed to prevent cyclic lists
                                  ;; TODO: should idfn be defined in compiler.arc?
                                  (racket-let ((x (racket-mmap (racket-lambda (x) x) com)))
                                    (racket-set-mcar! com (list ind val))
                                    (racket-set-mcdr! com x))))))))
    (racket-else
      (err "can't set reference" com ind val)))
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

(racket-hash-set! ac-names is (racket-quote is))


;=============================================================================
;  %nocompile/%compile
;=============================================================================

(racket-define (ac-nocompile x)
  (racket-let ((x (racket-let self ((x x))
                    ;(racket-display x)
                    ;(racket-newline)

                    (ac-mappend (racket-lambda (x)
                                  (racket-cond
                                    ;; TODO: don't hardcode the symbol %compile
                                    ((ac-caris x (racket-quote %compile))
                                      ;(map1 ac-compile (cdr x))
                                      (ac-args (cdr x)))
                                    ((racket-mpair? x)
                                      (list (self x)))
                                    (racket-else
                                      (list x))))
                                x))))
    (racket-if (ac-no (cdr x))
                 (car x)
               (cons (racket-quote racket-begin) x))))

;; TODO: make this prettier
(racket-define %nocompile (annotate (racket-quote mac)
                            (racket-lambda args
                              (cons ac-nocompile (racket-list->mlist args)))))

(racket-hash-set! ac-names %nocompile (racket-quote %nocompile))


;=============================================================================
;  %splice
;=============================================================================

(racket-define ac-splice (uniq))

;; TODO: make this prettier
(racket-define %splice (annotate (racket-quote mac)
                         (racket-lambda args
                           (cons ac-splice (racket-list->mlist args)))))

(racket-hash-set! ac-names %splice (racket-quote %splice))

;(foo (%splice 1 2 3))
;(foo 1 2 3)

#|
;=============================================================================
;  %eval
;=============================================================================

(racket-define %eval (annotate (racket-quote mac)
                       (racket-procedure-rename
                         (racket-lambda (x)
                           (eval x))
                         (racket-quote %eval))))|#


;=============================================================================
;  ssyntax
;=============================================================================

(racket-define (ssexpand x) x)
(racket-define (ssyntax  x) nil)


;=============================================================================
;  compiler/eval
;=============================================================================

(racket-define (ac-compile x)
  ;(racket-display x)
  ;(racket-newline)
  ;(ac-prn x)
  (racket-cond
    ((ac-no x)
      (racket-quote nil))
    #|((ac-caris x ac-quote)
      (ac-quote (cdr x)))|#
    ((ac-caris x ac-fn)
      (ac-fn (cadr x) (cddr x)))
    ((ac-caris x ac-if)
      (ac-if (cdr x)))
    ((ac-caris x ac-assign)
      (ac-assign (cdr x)))
    #|((ac-caris x ac-compose)
      (ac-compile (ac-compose (cdr x))))
    ((ac-caris x ac-complement)
      (ac-compile (ac-complement (cadr x))))|#
    ((ac-caris x ac-nocompile)
      (ac-nocompile (cdr x)))
    ((ac-caris x ac-splice)
      x)
    ((racket-mpair? x)
      (ac-call (car x) (cdr x)))
    ((ac-true (ssyntax x))
      (ac-compile (ssexpand x)))
    ((racket-symbol? x)
      (racket-if (ac-lex? x)
                   (ac-local-var x)
                 (ac-global-var x)))
    (racket-else x)))

(racket-define (eval x (runtime nil))
  ;(racket-displayln (ac-compile x))
  (racket-eval (ac-deep-fromarc (ac-compile x))
               (racket-if (ac-no runtime)
                            (racket-current-namespace) ;arc3-namespace ;(ac-namespace)
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

#|(racket-define exec-dir*
  (racket-path->string
    (racket-path-only
      (racket-normalize-path
        (racket-find-system-path (racket-quote run-file))))))|#

(racket-define ac-load-paths*
  (racket-make-parameter
    (list (racket-path->string (racket-current-directory))
          exec-dir*
          (racket-path->string (racket-build-path exec-dir* "lib")))))

(racket-define ac-load-suffix* (racket-make-parameter ".arc"))


(racket-define (load-file-dir x)
  ;; this is just (find [file-exists:joinpath _ x] load-paths*)
  (racket-let loop ((xs (ac-load-paths*)))
    (racket-cond
      ((racket-null? xs)
        ;; TODO: should this be nil?
        nil
        ;(racket-current-directory)
        )
      ((racket-file-exists? (racket-build-path (car xs) x))
        (car xs))
      (racket-else
        (loop (cdr xs))))))

#|(racket-define (load-file-path x)
  ;; this is just (find [file-exists:joinpath _ x] load-paths*)
  (racket-let loop ((xs (ac-load-paths*)))
    (racket-cond
      ((racket-null? xs)
        ;; TODO: should this be nil?
        nil
        ;(racket-current-directory)
        )
      ((racket-file-exists? (racket-build-path (car xs) x))
        (racket-build-path (car xs) x))
      ((racket-file-exists? (racket-build-path (car xs) (load-normalize-path x)))
        (racket-build-path (car xs) (load-normalize-path x)))
      (racket-else
        (loop (cdr xs))))))|#

(racket-define (load-normalize-path x)
  (racket-if (racket-filename-extension x)
               x
             (racket-string-append x (ac-load-suffix*))))

#|(racket-define (ac-with-find-file x f)
  (racket-let ((it (load-file-dir x)))
    (racket-if (ac-true it)
                 (racket-parameterize ((racket-current-directory it))
                   (f x))
               (racket-let ((x (load-normalize-path x)))
                 (racket-parameterize ((racket-current-directory (load-file-dir x)))
                   (f x))))))|#

(racket-define (ac-with-find-file x f)
  (racket-parameterize ((racket-port-count-lines-enabled  #t))
    (racket-let* ((y  (load-normalize-path x))
                  (it (load-file-dir y)))
      (racket-if (ac-true it)
                   (racket-parameterize ((racket-current-directory it))
                     (f y))
                 (racket-parameterize ((racket-current-directory (load-file-dir x)))
                   (f x))))))


(racket-define (ac-eval-all in)
  (racket-let ((x (sread in)))
    (racket-if (ac-no x)
                 nil
               (racket-begin (eval x)
                             (ac-eval-all in)))))

(racket-define (ac-load x)
  ;(racket-let ((x (load-normalize-path x)))
  #|(racket-parameterize ((racket-compile-allow-set!-undefined #t)
                        ;(racket-port-count-lines-enabled     #t)
                        ;(racket-current-directory            (load-file-dir x))
                        ;(racket-compile-enforce-module-constants #f)
                        )
    )|#
    ;(racket-displayln (racket-namespace-mapped-symbols))
    ;(racket-displayln (racket-compile-allow-set!-undefined))
  (ac-with-find-file x
    (racket-lambda (x)
      (racket-call-with-input-file x
        (racket-lambda (in)
          (ac-eval-all in))))))

;(racket-display (ac-compile (ac-deep-toarc (racket-quote (foo (%splice 1 2 3))))))
;(racket-newline)
