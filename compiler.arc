;#lang s-exp '#%kernel

(racket-define (ac-prn . args)
  (racket-for-each (racket-lambda (x)
                     (racket-display x)
                     (racket-display " "))
                   args)
  (racket-newline)
  (racket-car args))

;(#%require (only racket/base #%app #%datum #%top #%top-interaction))
;(#%require (prefix-all-except racket- racket/base
;             #%app #%datum #%top #%top-interaction))

;(#%require (only racket/base namespace-require/copy))
;(#%require (prefix racket- racket/base))
;(racket-require (racket-rename-in
;                  (racket-prefix-in racket- racket/mpair)
;                  [racket-mcons cons]
;                  [racket-mlist list]))

;(racket-require (racket-prefix-in racket- racket/base))

;(#%require (prefix racket- racket/base))
(racket-namespace-require/copy (racket-quote (prefix racket- racket/mpair)))
(racket-namespace-require/copy (racket-quote (prefix racket- racket/path)))
(racket-namespace-require/copy (racket-quote (prefix racket- racket/system)))

;(racket-require (racket-prefix-in racket- racket/mpair))
;(racket-require (racket-prefix-in racket- racket/path))
;(racket-require (racket-prefix-in racket- racket/system))

;(racket-provide (racket-all-defined-out)
;                (racket-all-from-out racket/base
;                                     racket/mpair
;                                     racket/path
;                                     racket/system))


(racket-define t            (racket-quote t))
(racket-define nil          racket-null)

;; TODO: tests for the fn environment
(racket-define ac-local-env      (racket-make-parameter nil))

(racket-define arc3-namespace (racket-current-namespace))
(racket-define ac-namespace   (racket-make-parameter arc3-namespace))

(racket-define (namespace-get runtime varname (default nil))
  (racket-let ((default (racket-if (racket-procedure? default)
                                   #|(racket-or (racket-not default)
                                              )|#
                                   default
                                   (racket-lambda () default))))
    (racket-namespace-variable-value varname #f default runtime)))

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
;  Strings
;=============================================================================

;; TODO: should this throw an error...?
(racket-define (string1 x)
  (racket-cond
    ((racket-string? x)
      x)
    ((racket-symbol? x)
      (racket-symbol->string x))
    ((racket-path? x)
      (racket-path->string x))
    ((racket-number? x)
      (racket-number->string x))
    ((racket-keyword? x)
      (racket-keyword->string x))
    ((racket-bytes? x)
      (racket-bytes->string/utf-8 x))
    ((racket-mpair? x)
      (apply string x))
    ((ac-no x)
      "")
    ((racket-char? x)
      (racket-string x))))


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
      (racket-string-append (string1 name)
                            (string1 num)))))

(racket-define (ac-var x (def nil))
  (ac-apply-non-fn (ac-namespace) x def)
  )

(racket-define bound
  (racket-let ((undef (uniq)))
    (racket-lambda (name)
      (ac-tnil
        (racket-not (racket-eq? (ac-var name undef) undef))))))


;=============================================================================
;  Types
;=============================================================================

(racket-struct ac-tagged (type rep)
  ;#:prefab
  ;#:transparent
  #:constructor-name annotate
  #:mutable
;  #:guard (racket-lambda (type rep name)
;            (racket-let ((n (ac-assign-name)))
;              (racket-if (racket-and n
;                                     (racket-procedure? rep)
;                                     (racket-not (racket-parameter? rep)))
;                           (racket-values type (racket-procedure-rename rep n))
;                         (racket-values type rep))))
  ;#:property racket-prop:set!-transformer
  ;           (racket-lambda (x) (racket-display x))
  )

(racket-define (ac-exint? x)
  (racket-and (racket-integer? x) (racket-exact? x)))

(racket-define (type x)
  (racket-cond
    ((ac-tagged? x)           (ac-tagged-type x))
    ((racket-mpair? x)        (racket-quote cons))
    ((racket-null? x)         (racket-quote sym))
    ((racket-symbol? x)       (racket-quote sym))
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
  (racket-or (racket-hash-ref ac-names x #f)
             (racket-object-name x)
             nil)
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

(racket-hash-set! ac-names cons (racket-quote cons))
(racket-hash-set! ac-names list (racket-quote list))

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

(racket-define (len x)
  (racket-cond
    ((racket-string? x) (racket-string-length x))
    ((racket-hash? x)   (racket-hash-count x))
    (racket-else        (racket-mlength x)
                        )))

(racket-define (ac-mappend f x)
  (apply racket-mappend (dottedmap1 f x))
  )


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
  (racket-mlist->list (racket-apply list* args))
  )

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
  (racket-make-keyword-procedure
    (racket-lambda (kw kw-val f . racket-arg-list)
      (racket-cond
        ((racket-procedure? f)
          (racket-keyword-apply f kw kw-val racket-arg-list))
        (racket-else
          (racket-keyword-apply ac-apply-non-fn kw kw-val f racket-arg-list)))))
    )

(racket-hash-set! ac-names ac-apply (racket-quote ac-apply))


;; TODO: why is apply very slow compared to ar and Arc 3.1? fix it
(racket-define apply
  (racket-make-keyword-procedure
    (racket-lambda (kw kw-val f . args)
      (racket-keyword-apply ac-apply kw kw-val f (ac-arg-list* args))
      )))

(racket-hash-set! ac-names apply (racket-quote apply))


;=============================================================================
;  Function/Macro calls
;=============================================================================

(racket-define ac-functional-position? (racket-make-parameter #f))

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
  (racket-if (racket-procedure? f)
               (f arg1)
             (ac-apply-non-fn f arg1)))

(racket-define (ac-funcall2 f arg1 arg2)
  (racket-if (racket-procedure? f)
               (f arg1 arg2)
             (ac-apply-non-fn f arg1 arg2)))

(racket-define (ac-funcall3 f arg1 arg2 arg3)
  (racket-if (racket-procedure? f)
               (f arg1 arg2 arg3)
             (ac-apply-non-fn f arg1 arg2 arg3)))

(racket-define (ac-funcall4 f arg1 arg2 arg3 arg4)
  (racket-if (racket-procedure? f)
               (f arg1 arg2 arg3 arg4)
             (ac-apply-non-fn f arg1 arg2 arg3 arg4)))

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

(racket-define (ac-call f args)
  (racket-let* ((g  (racket-not (ac-lex? f)))
                (m  (racket-and g (ac-macro? f))))
    (racket-if m
      (ac-mac-call m args)
      (racket-let ((f  (racket-parameterize ((ac-functional-position? #t))
                         (ac-compile f))))
        (racket-cond
          ;; optimization for compose and complement in functional position
          ;; (and:or 3) => ((compose and or) 3) => (and (or 3))
          ((ac-caris f compose)
            (ac-compile (ac-decompose (cdr f) args)))
          ;; (~and 3 nil) => ((complement and) 3 nil) => (no (and 3 nil))
          ((ac-caris f complement)
            (ac-compile (list (racket-quote no)
                              (cons (cadr f) args))))
          (racket-else
            (racket-let ((args  (ac-args args)))
              (racket-cond
                ;; if we're about to call a literal fn such as ((fn (a b) ...) 1 2)
                ;; then we know we can just call it in Racket and we don't
                ;; have to use ac-apply
                ((racket-or (racket-procedure? f)
                            (ac-caris f (racket-quote racket-lambda)))
                  (cons f args))
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
             ;; This implements aliases
  (racket-if (racket-eq? (type x) (racket-quote alias))
               ((car (rep x)))
             x))

(racket-define (ac-lookup-global-arg x)
  (racket-let ((x (ac-lookup-global x)))
               ;; This implements parameters
    (racket-if (racket-eq? (type x) (racket-quote parameter))
                 ((rep x))
               x)))

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
    ;; This implements aliases
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
  (racket-string->symbol (string1 x)))

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
  (racket-cond
    ((racket-symbol? x)
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


(racket-define (ac-fn parms body)
  (cons (racket-quote racket-lambda)
        (racket-parameterize ((ac-fn-body (racket-if (ac-no body)
                                                       (list (racket-quote nil))
                                                     body)))
          (cons (racket-parameterize ((ac-local-env (ac-local-env)))
                  (ac-fn-args parms))
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

(racket-define quote
  (annotate (racket-quote mac)
    (racket-lambda (x)
     ;; TODO: not sure about the %nocompile part: is it
     ;;       fast enough?
     ;(list %nocompile (list (racket-lambda () x)))
     (racket-if (racket-eq? x (racket-quote nil))
                  nil ;x
                (list %nocompile
                  (list (racket-procedure-rename
                          (racket-lambda () x)
                          (racket-quote quoted))))))))

(racket-hash-set! ac-names quote (racket-quote quote))


;=============================================================================
;  quasiquote
;=============================================================================
; qq-expand takes an Arc list containing a quasiquotation expression
; (the x in `x), and returns an Arc list containing Arc code.  The Arc
; code, when evaled by Arc, will construct an Arc list, the
; expansion of the quasiquotation expression.
;
; This implementation is a modification of Alan Bawden's quasiquotation
; expansion algorithm from "Quasiquotation in Lisp"
; http://repository.readscheme.org/ftp/papers/pepm99/bawden.pdf
;
; You can redefine qq-expand in Arc if you want to implement a
; different expansion algorithm.

(racket-define (qq-expand-pair x)
  (racket-if (racket-mpair? x)
      (racket-let ((c (car x)))
        (racket-cond
          ;; TODO: don't hardcode the symbol unquote
          ((ac-caris c (racket-quote unquote))
            (list cons
                  (cadr c)
                  (qq-expand-pair (cdr x))))
          ;; TODO: don't hardcode the symbol unquote-splicing
          ((ac-caris c (racket-quote unquote-splicing))
            (list racket-mappend
                  (cadr c)
                  (qq-expand-pair (cdr x))))
          ;; TODO: don't hardcode the symbol quasiquote
          ((ac-caris c (racket-quote quasiquote))
            (list cons
                  (qq-expand-pair (qq-expand (cadr c)))
                  (qq-expand-pair (cdr x))))
          ((racket-mpair? c)
            (list cons
                  (qq-expand-pair c)
                  (qq-expand-pair (cdr x))))
          (racket-else
            (list cons
                  (list quote c)
                  (qq-expand-pair (cdr x))))))
    (racket-if (ac-no x)
                 x
               (list quote x))))

(racket-define (qq-expand x)
  (racket-cond
    ;; TODO: don't hardcode the symbol unquote
    ((ac-caris x (racket-quote unquote))
      (cadr x))
    ;; TODO: don't hardcode the symbol unquote-splicing
    ((ac-caris x (racket-quote unquote-splicing))
      (err ",@ cannot be used immediately after `"))
    ;; TODO: don't hardcode the symbol quasiquote
    ((ac-caris x (racket-quote quasiquote))
      (qq-expand (qq-expand (cadr x))))
    ((racket-mpair? x)
      (qq-expand-pair x))
    (racket-else
      (list quote x))))

;; TODO: make this prettier
(racket-define quasiquote (annotate (racket-quote mac)
                            (racket-lambda args
                              (racket-apply qq-expand args))))

(racket-hash-set! ac-names quasiquote (racket-quote quasiquote))


;=============================================================================
;  quasisyntax
;=============================================================================
; qs-expand takes an Arc list containing a quasisyntax expression
; (the x in #`x), and returns an Arc list containing Arc code.  The Arc
; code, when evaled by Arc, will construct an Arc list, the
; expansion of the quasisyntax expression.
;
; This implementation is a modification of the quasiquote algorithm above
;
; You can redefine qs-expand in Arc if you want to implement a
; different expansion algorithm.

(racket-define (qs-expand-pair x)
  (racket-if (racket-mpair? x)
      (racket-let ((c (car x)))
        (racket-cond
          ;; TODO: don't hardcode the symbol quote
          ((ac-caris c (racket-quote quote))
            (list cons
                  (cons quote (cdr c))
                  (qs-expand-pair (cdr x))))
          ;; TODO: don't hardcode the symbol unquote
          ((ac-caris c (racket-quote unquote))
            (list cons
                  (cadr c)
                  (qs-expand-pair (cdr x))))
          ;; TODO: don't hardcode the symbol unquote-splicing
          ((ac-caris c (racket-quote unquote-splicing))
            (list racket-mappend
                  (cadr c)
                  (qs-expand-pair (cdr x))))
          ;; TODO: don't hardcode the symbol quasisyntax
          ((ac-caris c (racket-quote quasisyntax))
            (list cons
                  (qs-expand-pair (qs-expand (cadr c)))
                  (qs-expand-pair (cdr x))))
          ((racket-mpair? c)
            (list cons
                  (qs-expand-pair c)
                  (qs-expand-pair (cdr x))))
          (racket-else
            (list cons
                  c
                  (qs-expand-pair (cdr x))))))
    (racket-if (ac-no x)
                 x
               x)))

(racket-define (qs-expand x)
  (racket-cond
    ;; TODO: don't hardcode the symbol quote
    ((ac-caris x (racket-quote quote))
      (cons quote (cdr x)))
    ;; TODO: don't hardcode the symbol unquote
    ((ac-caris x (racket-quote unquote))
      (cadr x))
    ;; TODO: don't hardcode the symbol unquote-splicing
    ((ac-caris x (racket-quote unquote-splicing))
      (err ",@ cannot be used immediately after #`"))
    ;; TODO: don't hardcode the symbol quasisyntax
    ((ac-caris x (racket-quote quasisyntax))
      (qs-expand (qs-expand (cadr x))))
    ((racket-mpair? x)
      (qs-expand-pair x))
    (racket-else
      x)))

;; TODO: make this prettier
(racket-define quasisyntax (annotate (racket-quote mac)
                             (racket-lambda args
                               (racket-apply qs-expand args))))

(racket-hash-set! ac-names quasisyntax (racket-quote quasisyntax))


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
;  %nocompile / %compile
;=============================================================================

(racket-define (ac-nocompile x)
  (racket-let ((x (racket-let self ((x x))
                    (ac-mappend (racket-lambda (x)
                                  (racket-cond
                                    ;; TODO: don't hardcode the symbol %compile
                                    ((ac-caris x (racket-quote %compile))
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


;=============================================================================
;  ssyntax
;=============================================================================

(racket-define (ssexpand x) x)
(racket-define (ssyntax  x) nil)


;=============================================================================
;  compiler / eval
;=============================================================================

(racket-define (ac-compile x)
  (racket-cond
    ((ac-no x)
      (racket-quote nil))
    ((ac-caris x ac-fn)
      (ac-fn (cadr x) (cddr x)))
    ((ac-caris x ac-if)
      (ac-if (cdr x)))
    ((ac-caris x ac-assign)
      (ac-assign (cdr x)))
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
  (racket-eval (ac-deep-fromarc (ac-compile x))
               (racket-if (ac-no runtime)
                            (racket-current-namespace)
                          runtime)))


;=============================================================================
;  load
;=============================================================================

(racket-define ac-load-paths*
  (racket-make-parameter
    (list (string1 (racket-current-directory))
          exec-dir*
          (string1 (racket-build-path exec-dir* "lib"))
          (string1 (racket-build-path exec-dir* "lang")))))

(racket-define ac-load-suffix* (racket-make-parameter ".arc"))


(racket-define (load-file-dir x)
  ;; this is just (find [file-exists:joinpath _ x] load-paths*)
  (racket-let loop ((xs (ac-load-paths*)))
    (racket-cond
      ((racket-null? xs)
        ;; TODO: should this be nil?
        nil
        )
      ((racket-file-exists? (racket-build-path (car xs) x))
        (car xs))
      (racket-else
        (loop (cdr xs))))))

(racket-define (load-normalize-path x)
  (racket-if (racket-filename-extension x)
               x
             (racket-string-append x (ac-load-suffix*))))

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
  (ac-with-find-file x
    (racket-lambda (x)
      (racket-call-with-input-file x
        (racket-lambda (in)
          (ac-eval-all in))))))
