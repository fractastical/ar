;#lang s-exp '#%kernel

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
;(racket-namespace-require/copy (racket-quote (prefix racket- racket/mpair)))
(racket-namespace-require/copy (racket-quote (prefix racket- racket/unsafe/ops)))
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


(racket-define t              (racket-quote t))
(racket-define nil            racket-null)

;; TODO: tests for the fn environment
(racket-define ac-local-env   (racket-make-parameter nil))

(racket-define arc3-namespace (racket-current-namespace))
(racket-define ac-namespace   (racket-make-parameter arc3-namespace))


;=============================================================================
;  Hash Tables
;=============================================================================

(racket-define sig       (racket-make-hash))
(racket-define ac-names  (racket-make-hash))


;=============================================================================
;  Convenience utilities
;=============================================================================

#|
;; TODO: kinda icky that this is defined up here
(racket-define (ac-deep-toarc x)
  (racket-cond
    ((racket-pair? x)
      ;; TODO: cons
      (racket-mcons (ac-deep-toarc (racket-car x))
                    (ac-deep-toarc (racket-cdr x))))
    ((racket-string? x)
      (racket-string-copy x))
    ((racket-mpair? x)
      (err "Racket mpair passed to ac-deep-toarc" x))
    (racket-else x)))

;; TODO: ew
(racket-hash-set! sig (racket-quote ac-deep-toarc)
                      ;; TODO: ew
                      (ac-deep-toarc (racket-quote (x))))|#



(racket-define-syntax-rule (ac-sref-parms hash name parms)
  (racket-hash-set! hash
                    (racket-quote name)
                    #|;; TODO: eww
                    (ac-deep-toarc )|#
                    (racket-quote parms)
                    #|(racket-let ((x (racket-quote parms)))
                      (racket-if (racket-pair? x)
                                   x ;(racket-list->mlist x)
                                 x))|#
                    ))

(racket-define-syntax-rule (ac-assign-sig name parms x)
  (racket-begin (ac-sref-parms sig name parms)
                (racket-define name x)
                (racket-hash-set! ac-names name (racket-quote name))))

#|(racket-define-syntax-rule (ac-def name parms #:sig [sig2 #f] . body)
  (racket-begin (ac-sref-parms sig name (racket-or sig2 parms))
                (racket-define name (racket-lambda parms . body))))|#

;; TODO: ew code duplication
(racket-define-syntax ac-def
  (racket-syntax-rules ()
    [(_ name parms #:sig sig2 . body)
      (racket-begin (ac-sref-parms sig name sig2)
                    (racket-define name (racket-lambda parms . body)))]
    [(_ name parms . body)
      (racket-begin (ac-sref-parms sig name parms)
                    (racket-define name (racket-lambda parms . body)))]))

(racket-define-syntax-rule (ac-mac name parms . body)
  (ac-assign-sig name parms (annotate (racket-quote mac)
                                      (racket-lambda parms . body))))



(ac-def ac-prn args
  (racket-for-each (racket-lambda (x)
                     (racket-display x)
                     (racket-display " "))
                   args)
  (racket-newline)
  (racket-car args))


;=============================================================================
;  Namespaces
;=============================================================================

(ac-def namespace-get (runtime varname (default nil))
                #:sig (runtime varname (o default))
  (racket-let ((default (racket-if (racket-procedure? default)
                                   #|(racket-or (racket-not default)
                                              )|#
                                   default
                                   (racket-lambda () default))))
    (racket-namespace-variable-value varname #f default runtime)))

(ac-def namespace-set (runtime varname value)
  (racket-namespace-set-variable-value! varname value #f runtime))


;=============================================================================
;  Predicates
;=============================================================================

(ac-def ac-tnil (x)
  (racket-if x t nil))

(ac-def ac-no (x)
  (racket-eq? x nil))

(ac-def ac-nil (x)
  (racket-if (ac-no x) #f x))

(ac-def ac-true (x)
  (racket-not (ac-no x)))

(ac-def ac-bool (x)
  (ac-tnil (ac-no x)))

(ac-def ac-isa (x y)
  (racket-eq? (type x) y))

(ac-def ac-caris (x y)
  (racket-and (racket-pair? x)
              (racket-eq? (car x) y)))


;=============================================================================
;  Mapping/Iteration
;=============================================================================

(ac-def map1 (f xs)
  (racket-if (racket-pair? xs)
               (cons (f (car xs)) (map1 f (cdr xs)))
             xs))

(ac-def dottedmap1 (f xs)
  (racket-cond ((racket-pair? xs)
                 (cons (f (car xs)) (dottedmap1 f (cdr xs))))
               ((ac-no xs)
                 xs)
               (racket-else
                 (f xs))))


;=============================================================================
;  Strings
;=============================================================================

;; TODO: should this throw an error...?
(ac-def string1 (x)
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
    ((racket-pair? x)
      (apply string x))
    ((ac-no x)
      "")
    ((racket-char? x)
      (racket-string x))))


;=============================================================================
;  Symbols
;=============================================================================

(racket-define ac-uniq-counter (racket-make-parameter 1))

(ac-def uniq ((name (racket-quote g)) (num nil))
       #:sig ((o name 'g) (o num))
  (racket-let ((num (racket-if (ac-true num)
                                 num
                               (racket-let ((num (ac-uniq-counter)))
                                 (ac-uniq-counter (racket-+ (ac-uniq-counter) 1))
                                 num))))
    (racket-string->uninterned-symbol
      (racket-string-append (string1 name)
                            (string1 num)))))

(ac-def ac-var (x (def nil))
         #:sig (x (o def))
  (ac-apply-non-fn (ac-namespace) x def))

(ac-assign-sig bound (name)
  (racket-let ((undef (uniq)))
    (racket-lambda (name)
      (ac-tnil
        (racket-not (racket-eq? (ac-var name undef) undef))))))


;=============================================================================
;  Types
;=============================================================================

;; TODO: should allow for rebinding ac-tagged
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

(ac-def ac-exint? (x)
  (racket-and (racket-integer? x) (racket-exact? x)))

(ac-def type (x)
  (racket-cond
    ((ac-tagged? x)           (ac-tagged-type x))
    ;; TODO: better ordering of these, for speed
    ((racket-pair? x)         (racket-quote cons))
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

(ac-def rep (x)
  (racket-if (ac-tagged? x)
               (ac-tagged-rep x)
             x))

#|(ac-def ac-deep-fromarc (x)
  (racket-cond
    ((racket-mpair? x)
     (racket-cons (ac-deep-fromarc (car x))
                  (ac-deep-fromarc (cdr x))))
    ((racket-pair? x)
      (err "Racket pair passed to ac-deep-fromarc" x))
    (racket-else x)))|#


;=============================================================================
;  Exceptions/Errors
;=============================================================================

(ac-assign-sig err (x . rest) racket-error)

(ac-def on-err (errf f)
  (racket-with-handlers ((racket-exn:fail? errf))
    (f)))

(ac-def details (c)
  (racket-exn-message c))


;=============================================================================
;  print
;=============================================================================

(ac-def print-w/list (primitive x port)
  (racket-cond
    ((ac-no (cdr x))
      (print primitive (car x) port)
      (racket-display ")" port))
    ((racket-pair? (cdr x))
      (print primitive (car x) port)
      (racket-display " " port)
      (print-w/list primitive (cdr x) port))
    (racket-else
      (print primitive (car x) port)
      (racket-display " . " port)
      (print primitive (cdr x) port)
      (racket-display ")" port))))

(ac-def name (x)
  (racket-or (racket-hash-ref ac-names x #f)
             (racket-object-name x)
             nil)
             )

(ac-def print-w/name (x l m r port)
  (racket-let ((x (name x)))
    (racket-display l port)
    (racket-when (ac-true x)
      (racket-display m port)
      (racket-display x port))
    (racket-display r port)))

(ac-def print (primitive x port)
  (racket-cond
    ((ac-no x)
      (racket-display "nil" port))
    ((ac-caris x ac-quote)
      (racket-display "#<quoted>" port)
      ;(print primitive (cadr x) port)
      ;(print-w/list primitive (cdr x) port)
      )
    ((racket-pair? x)
      (racket-display "(" port)
      (print-w/list primitive x port))
    ((ac-isa x (racket-quote fn))
      (print-w/name x "#<fn" ":" ">" port))
    ((ac-isa x (racket-quote mac))
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

(ac-def sread (input (eof nil))
  (racket-let ((v (racket-read input)))
    (racket-if (racket-eof-object? v)
                 eof
               ;(ac-deep-toarc )
               v)))


;=============================================================================
;  Lists
;=============================================================================

(ac-assign-sig cons (x y) racket-cons)
(ac-assign-sig list args  racket-list)


(ac-def car (x)
  (racket-if (ac-no x)
               nil
             (racket-car x)))

(ac-def cdr (x)
  (racket-if (ac-no x)
               nil
             (racket-cdr x)))


(ac-def scar (x val)
  ;(racket-set-mcar! x val)
  (racket-if (racket-pair? x)
               (racket-unsafe-set-mcar! x val)
             (racket-raise-type-error (racket-quote scar) "pair" x))
  val)

(ac-def scdr (x val)
  ;(racket-set-mcdr! x val)
  (racket-if (racket-pair? x)
               (racket-unsafe-set-mcdr! x val)
             (racket-raise-type-error (racket-quote scdr) "pair" x))
  val)


(ac-def caar (xs)
  (car (car xs)))

(ac-def cadr (x)
  (car (cdr x)))

(ac-def cddr (x)
  (cdr (cdr x)))


(ac-def list* args
  (racket-cond
    ((ac-no args)
      nil)
    ((ac-no (racket-cdr args))
     (racket-car args))
    ((ac-no (racket-cddr args))
     (cons (racket-car args) (racket-cadr args)))
    (racket-else
     (cons (racket-car args) (racket-apply list* (racket-cdr args))))))

(ac-def len (x)
  (racket-cond
    ((racket-string? x) (racket-string-length x))
    ((racket-hash? x)   (racket-hash-count x))
    (racket-else        (racket-length x)
                        )))

(ac-def join args
  (racket-apply racket-append args))

(ac-def ac-mappend (f x)
  (apply join (dottedmap1 f x))
  )


(ac-def assoc-ref (al key)
  (racket-cond
    ;; TODO: change this to be cleaner/more efficient
    ((racket-not (racket-pair? al))
      nil)
    ((racket-and (racket-pair? (car al))
                 (racket-eq? (caar al) key))
      al)
    (racket-else
      (assoc-ref (cdr al) key))))

(ac-def assoc (al key)
  (car (assoc-ref al key)))

(ac-def alref (al key)
  (cadr (assoc al key)))


;=============================================================================
;  apply
;=============================================================================

(ac-def ac-arg-list* (args)
  ;(ac-prn args)
  ;(ac-prn (racket-apply list* args))
  ;(ac-prn (racket-apply racket-list* args))
  ;(ac-prn
  #|((racket-null? args)
        args)|#
  ;(racket-let next ((args args)) )
  (racket-if (racket-null? (racket-cdr args))
               ;(ac-prn (racket-car args))
               (racket-car args) ;(racket-mlist->list )
             (racket-cons (racket-car args)
                          (ac-arg-list* (racket-cdr args))))

  ;; TODO: figure out how to avoid the mlist->list call
  ;;       maybe use racket-list*?
  ;(racket-mlist->list (racket-apply list* args))
  ;(racket-mlist->list (racket-apply racket-list* args))

  ;)
  ;(ac-prn "----------")

  ;(a b c d)
  ;(a b c d)

  ;({a b c d})
  ;(a b c d)

  ;({a b c d} {e f g})
  ;({a b c d} e f g)
  )

(ac-assign-sig ac-apply-non-fn (x (o k) (o d))
  (racket-case-lambda
    [(x)     (racket-if (ac-isa x (racket-quote parameter))
                          ((rep x))
                        (err "function call on inappropriate object" x))]
    [(x k)   (racket-cond
               ((racket-namespace? x)
                 (namespace-get x k))
               ((racket-string? x)
                 (racket-string-ref x k))
               ((racket-hash? x)
                 (racket-hash-ref x k nil))
               ((racket-pair? x)
                 (racket-if (racket-number? k)
                              (racket-list-ref x k)
                            (alref x k)))
               ((ac-isa x (racket-quote parameter))
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

(ac-assign-sig ac-apply (f . racket-arg-list)
  ;; TODO: ew
  (racket-make-keyword-procedure
    (racket-lambda (kw kw-val f . racket-arg-list)
      (racket-cond
        ((racket-procedure? f)
          (racket-keyword-apply f kw kw-val racket-arg-list))
        (racket-else
          (racket-keyword-apply ac-apply-non-fn kw kw-val f racket-arg-list)))))
  )

#|(ac-def ac-apply (f . racket-arg-list)
  (racket-cond
    ((racket-procedure? f)
      (racket-apply f racket-arg-list))
    (racket-else
      (racket-apply ac-apply-non-fn f racket-arg-list))))|#

;; TODO: why is apply very slow compared to ar and Arc 3.1? fix it
(ac-assign-sig apply (f . args)
  (racket-make-keyword-procedure
    (racket-lambda (kw kw-val f . args)
      (racket-keyword-apply ac-apply kw kw-val f (ac-arg-list* args)))))


#|(ac-def apply (f . args)
  (racket-apply ac-apply f (ac-arg-list* args)))|#


;=============================================================================
;  Function/Macro calls
;=============================================================================

(racket-define ac-functional-position? (racket-make-parameter #f))

(ac-def ac-lex? (x)
  (racket-let self ((env (ac-local-env)))
    (racket-and (racket-pair? env)
                (racket-or (racket-eq? x (car env))
                           (self (cdr env))))))

;; TODO: should ac-funcall* accept keyword args...?
(ac-def ac-funcall0 (f)
  (racket-if (racket-procedure? f)
               (f)
             (ac-apply-non-fn f)))

(ac-def ac-funcall1 (f arg1)
  (racket-if (racket-procedure? f)
               (f arg1)
             (ac-apply-non-fn f arg1)))

(ac-def ac-funcall2 (f arg1 arg2)
  (racket-if (racket-procedure? f)
               (f arg1 arg2)
             (ac-apply-non-fn f arg1 arg2)))

(ac-def ac-funcall3 (f arg1 arg2 arg3)
  (racket-if (racket-procedure? f)
               (f arg1 arg2 arg3)
             (ac-apply-non-fn f arg1 arg2 arg3)))

(ac-def ac-funcall4 (f arg1 arg2 arg3 arg4)
  (racket-if (racket-procedure? f)
               (f arg1 arg2 arg3 arg4)
             (ac-apply-non-fn f arg1 arg2 arg3 arg4)))

(ac-def ac-macro? (f)
  (racket-cond
    ((racket-symbol? f)
      (ac-macro? (ac-var f)))
      #|(racket-let ((v (ac-var f)))
        (racket-if (ac-isa v (racket-quote mac))
                     v
                   #f))|#
    ((ac-isa f (racket-quote mac))
      f)
    (racket-else #f)))

(ac-def ac-mac-call (m args)
  (racket-if
    (racket-and (ac-functional-position?)
                (racket-or (racket-eq? m compose)
                           (racket-eq? m complement)))
      (cons m args)
    (ac-compile (apply (rep m) args))))

(ac-def ac-args (args)
  (racket-parameterize ((ac-functional-position? #f))
    (racket-let loop ((x args))
      #|((ac-caris x (racket-quote :))
          (list (ac-compile (cdr x))))|#
      (racket-if (racket-pair? x)
                   (racket-let ((c (ac-compile (car x))))
                     (racket-if (ac-caris c ac-splice)
                                  ;; TODO: test this
                                  (join (cdr c) (loop (cdr x)))
                                (cons c (loop (cdr x)))))
                 x))
    #|(ac-mappend (racket-lambda (x)
                  (racket-let ((c (ac-compile x)))
                    (racket-if (ac-caris c ac-splice)
                                 (cdr c)
                               (list c))))
                args)|#
    ))

(ac-def ac-pipe-compose (args)
  (racket-let loop ((x args))
    (racket-cond
      ((ac-caris x (racket-quote :))
        (list (loop (cdr x))))
      ((racket-pair? x)
        (cons (car x) (loop (cdr x))))
      (racket-else x))))

(ac-def ac-return-apply (x)
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

(ac-def ac-decompose (fns args)
  (racket-cond
    ((ac-no fns)
      ;; TODO
      nil);`((fn vals (car vals)) ,@args)
    ((ac-no (cdr fns))
      (cons (car fns) args))
    (racket-else
      (list (car fns) (ac-decompose (cdr fns) args)))))

(ac-def ac-call (args)
  (racket-let* ((args (ac-pipe-compose args))
                (f    (car args))
                (args (cdr args))
                (g    (racket-not (ac-lex? f)))
                (m    (racket-and g (ac-macro? f))))
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
                              ;; TODO: can this be unquoted?
            (ac-compile (list (racket-quote no)
                              (cons (cadr f) args))))
          (racket-else
            (racket-let ((args (ac-args args)))
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
                    (racket-set! args (join (cddr f) args))
                    (racket-set! f    (cadr f)))

                  (cons (ac-return-apply args)
                        (cons f args)))
                ))))))))


;=============================================================================
;  %nocompile
;=============================================================================

(racket-define ac-nocompile (uniq))

;; TODO: maybe define this in core.arc?
(ac-mac %nocompile args
  (cons ac-nocompile
        (racket-if (ac-no (racket-cdr args))
                     (racket-car args)
                   (cons (racket-quote racket-begin)
                         args ;(racket-list->mlist )
                         ))))

#|(ac-mac %nocompile args
  (racket-let ((args (racket-list->mlist args)))
    (racket-if (ac-no (cdr args))
                 (car args)
               (cons (racket-quote racket-begin) args))))|#
#|
;; TODO: custom sig
(ac-mac %nocompile (args)
  (racket-if (ac-no (cdr args))
               (car args)
             (cons (racket-quote racket-begin) args)))|#


;=============================================================================
;  Variables
;=============================================================================

(ac-def ac-local-var (x)
  x)

(ac-def ac-undefined-var (x)
  (racket-lambda ()
    (err "undefined variable:" x)))

(ac-def ac-lookup-global-raw (space x)
  (ac-apply-non-fn space x (ac-undefined-var x)))

(ac-def ac-lookup-global (x)
             ;; This implements aliases
  (racket-if (ac-isa x (racket-quote alias))
               ((car (rep x)))
             x))

(ac-def ac-lookup-global-arg (x)
  (racket-let ((x (ac-lookup-global x)))
               ;; This implements parameters
    (racket-if (ac-isa x (racket-quote parameter))
                 ((rep x))
               x)))

;; TODO: pretty ew that it works based on the symbol name, but...
(ac-def ac-nolookup? (x)
  (racket-let ((x (racket-symbol->string x)))
    (racket-and (racket-> (racket-string-length x) 6)
                (racket-equal? (racket-substring x 0 7) "racket-"))))

(ac-def ac-global-var (x)
  (racket-if
    (ac-nolookup? x)
      x
    (racket-let* ((name (ac-namespace))
                                   ;; woot optimizations
                  (x    (racket-if (racket-namespace? name)
                                   ;(racket-eq? name (racket-current-namespace))
                                     x
                                   (list ac-lookup-global-raw
                                         name
                                         (list (racket-quote racket-quote) x)))))
      (racket-if (ac-functional-position?)
                   (list ac-lookup-global x)
                 (list ac-lookup-global-arg x)))))


;=============================================================================
;  assign
;=============================================================================

(ac-def ac-local-assign (a b)
  (list (racket-quote racket-set!)
        a
        (ac-compile b)))

(ac-def ac-assign-global (x a b)
  (racket-when (racket-or (ac-tagged? b)
                          (racket-procedure? b))
    (racket-hash-set! ac-names b a))

  (racket-cond
    ;; This implements parameters
    ((ac-isa x (racket-quote parameter))
      ((rep x) b))
    ;; This implements aliases
    ((ac-isa x (racket-quote alias))
      ;; TODO: not sure if `b` or `a` should go first
      ((cadr (rep x)) x b a))
    (racket-else
      (sref (ac-namespace) b a)))
  b)

(ac-def ac-assign-global-undefined (x a b)
  (ac-assign-global x a b))

(ac-def ac-assign-global-defined (x a b)
  (ac-assign-global x a b))

(ac-assign-sig ac-assign-global-raw (space a b)
  (racket-let ((u (uniq)))
    (racket-lambda (space a b)
                      ;; TODO: should this be ac-var or ac-lookup-global?
      (racket-let ((x (ac-var a u)))
        (racket-if (racket-eq? x u)
                     (ac-assign-global-undefined x a b)
                   (ac-assign-global-defined x a b))))))

(ac-def ac-global-assign (a b)
  (list ac-assign-global-raw
        (ac-namespace)
        (list (racket-quote racket-quote) a)
        (ac-compile b)))

(ac-def ac-assign1 (a b)
  (racket-unless (racket-symbol? a)
    (err "first arg to assign must be a symbol" a))

  (racket-if (ac-lex? a)
               (ac-local-assign a b)
             (ac-global-assign a b)))

(ac-def ac-assignn (x)
  (racket-if (ac-no x)
               nil
             ;; TODO: why does Arc 3.1 call ac-macex here?
             (cons (ac-assign1 (car x) (cadr x))
                              ;; TODO: ew
                   (racket-if (racket-and (ac-no (cddr x))
                                          (ac-lex? (car x)))
                                (list (ac-compile (car x)))
                              (ac-assignn (cddr x))))))

(ac-def ac-assign (x)
  (racket-let ((x (ac-assignn x)))
    (racket-if (ac-no (cdr x))
                 (car x)
               (cons (racket-quote racket-begin) x))))

(ac-mac assign args
  (cons ac-nocompile (ac-assign args))) ;(racket-list->mlist )


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

(ac-def ac-add-to (x y)
  (x (join (x) (list y))))

(ac-def ac-keyword->symbol (x)
  (racket-string->symbol (string1 x)))

(ac-def ac-fn-keyword-args (x default)
  (racket-let ((c (ac-keyword->symbol x)))
    (ac-add-to ac-local-env c)
    (list x (list c default))))

(ac-def ac-fn-optional-args (n default)
  ;; TODO: hacky
  (racket-parameterize ((ac-functional-position? #f))
    (racket-let ((default (ac-compile default)))
      (racket-if (racket-keyword? n)
                   (ac-fn-keyword-args n default)
                 (racket-begin (ac-add-to ac-local-env n)
                               (list (list n default)))))))

;; TODO: unit tests verifying the minimalness of the destructuring code output
;; TODO: huge hacky function
(ac-def ac-fn-destructuring-args (u x)
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
            ;; TODO: ew
            (ac-add-to ac-local-env u)
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
                                                                  (list cdr u)
                                                                  (list car u)
                                                                  d))))
                             ;; TODO: ew, code duplication
                             (racket-if (ac-no (cdr x))
                                          nil
                                        (cons (list u (list cdr u))
                                              (self (cdr x))))))))
        ((racket-pair? (car x))               ;; destructuring args
          (racket-let ((v (uniq)))
            (join (list (list v (list car u)))
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

#|(ac-def ac-fn-rest-args (x)
  (ac-add-to ac-local-env x)
  (list (list x (list racket-list->mlist x))))|#

(ac-def ac-fn-required-args (x)
  (racket-if (ac-fn-required-args?)
               x
             (list x (racket-quote nil))))

(ac-def ac-fn-end-of-args (x)
  (racket-if (ac-fn-excess-args?)
               (uniq)
             nil))

(ac-def ac-fn-normal-args (x)
  ;; TODO: hacky
  (racket-when (racket-pair? x)
    (scar x (ssexpand (car x))))

  (racket-cond
    ((ac-no x)                            ;; end of the argument list
      (ac-fn-end-of-args x))
    ((racket-symbol? x)                   ;; dotted rest args
      ;(ac-fn-let* (join (ac-fn-rest-args x) (ac-fn-let*)))
      (ac-add-to ac-local-env x)
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
                       (join (list (list n (racket-quote nil)))
                             (ac-fn-normal-args (cdr x))))
                     (join (ac-fn-optional-args n d)
                           (ac-fn-normal-args (cdr x))))
              ))
    ((racket-keyword? (car x))            ;; keyword args
      (join (ac-fn-keyword-args (car x) (racket-quote nil))
            (ac-fn-normal-args (cdr x))))
    ((racket-pair? (car x))               ;; destructuring args
      (racket-let ((u (uniq)))
        (ac-fn-let* (join (ac-fn-destructuring-args u (car x))
                          (ac-fn-let*)))
        (cons (ac-fn-required-args u)
              (ac-fn-normal-args (cdr x)))))
    (racket-else                          ;; normal args
      (ac-add-to ac-local-env (car x))
      (cons (ac-fn-required-args (car x))
            (ac-fn-normal-args (cdr x))))))


(ac-def ac-fn-args (x)
  (racket-cond
    ((racket-symbol? x)
      (ac-add-to ac-local-env x)
      #|(ac-fn-body (list (list* (racket-quote racket-let)
                               (ac-fn-rest-args x)
                               (ac-args (ac-fn-body)))))|#
      (ac-fn-body (ac-args (ac-fn-body)))
      x)
    (racket-else
      (racket-parameterize ((ac-fn-let* nil))
        (racket-let ((x (ac-fn-normal-args x)))
          ;(ac-add-to ac-fn-let* (make-current-env (ac-local-env)))
          #|(racket-parameterize ((ac-fn-let* (join (ac-fn-let*)
                                                  (list ))))
              )|#
          ;(ac-prn (ac-fn-let*))
          (ac-fn-body (racket-if (ac-true (ac-fn-let*))
                                   (list (list* (racket-quote racket-let*)
                                                (ac-fn-let*)
                                                (ac-args (ac-fn-body))))
                                 (ac-args (ac-fn-body))))
          x)))))


(ac-def ac-fn (parms body)
  (cons (racket-quote racket-lambda)
        (racket-parameterize ((ac-fn-body (racket-if (ac-no body)
                                                       (list (racket-quote nil))
                                                     body)))
          (cons (racket-parameterize ((ac-local-env (ac-local-env)))
                  (ac-fn-args parms))
                (ac-fn-body)))))

(ac-mac fn (parms . body)
  (cons ac-nocompile (ac-fn parms body))) ;(racket-list->mlist )


;=============================================================================
;  if
;=============================================================================

(ac-def ac-if (args)
  (racket-cond
    ((ac-no (cdr args))
      (ac-compile (car args)))
    (racket-else
      (list (racket-quote racket-if)
            (list ac-true (ac-compile (car args)))
            (ac-compile (cadr args))
            (ac-if (cddr args))))))

(ac-mac if args
  (cons ac-nocompile (ac-if args))) ;(racket-list->mlist )


;=============================================================================
;  quote
;=============================================================================

(ac-def ac-quote (x)
  x)

(ac-mac quote (x)
  ;; TODO: not sure about the %nocompile part: is it
  ;;       fast enough?
  ;(list %nocompile (list (racket-lambda () x)))

  ;; TODO: can I make due without ac-quote...?
  (list ac-quote
        (racket-if (racket-eq? x (racket-quote nil))
                     nil ;x
                   (list (racket-lambda () x)
                         #|(racket-procedure-rename

                           (racket-quote quoted))|#
                         ))))


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

(ac-def qq-expand-pair (x)
  (racket-if (racket-pair? x)
    (racket-let ((c (car x)))
      (racket-cond
        ;; TODO: don't hardcode the symbol unquote
        ((racket-and (racket-eq? c (racket-quote unquote))
                     (ac-no (cddr x)))
          (cadr x))
        ;; TODO: don't hardcode the symbol unquote-splicing
        ((racket-and (racket-eq? c (racket-quote unquote-splicing))
                     (ac-no (cddr x)))
          (err "cannot use ,@ after ."))
        ;; TODO: don't hardcode the symbol unquote
        ((ac-caris c (racket-quote unquote))
          (list cons
                (cadr c)
                (qq-expand-pair (cdr x))))
        ;; TODO: don't hardcode the symbol unquote-splicing
        ((ac-caris c (racket-quote unquote-splicing))
          (racket-if (ac-no (cdr x))
            (cadr c)
            (list join
                  (cadr c)
                  (qq-expand-pair (cdr x)))))
        ;; TODO: don't hardcode the symbol quasiquote
        ((ac-caris c (racket-quote quasiquote))
          (list cons
                (qq-expand-pair (qq-expand (cadr c)))
                (qq-expand-pair (cdr x))))
        ((racket-pair? c)
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

(ac-def qq-expand (x)
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
    ((racket-pair? x)
      (qq-expand-pair x))
    (racket-else
      (list quote x))))

(ac-mac quasiquote (x)
  (qq-expand x))


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

(ac-def qs-expand-quote (x)
  (racket-let ((c (car x)))
    (racket-cond
      ;; TODO: don't hardcode the symbol quote
      ((ac-caris c (racket-quote quote))
        (list list
              (list quote quote)
              (qs-expand-quote (cdr c))))
      ;; TODO: don't hardcode the symbol unquote
      ((ac-caris c (racket-quote unquote))
        ;(ac-compile )
        (list cons
              (list quote quote)
              (cons list (cdr c))))
      ;; TODO: don't hardcode the symbol unquote-splicing
      ((ac-caris c (racket-quote unquote-splicing))
        ;(ac-compile )
        (list* cons
               (list quote quote)
               (cdr c))
              ;(cons join (cdr c))
              )
      (racket-else
        ;(ac-compile (cons quote x))
        (cons quote x)
        ))))

(ac-def qs-expand-pair (x)
  (racket-if (racket-pair? x)
    (racket-let ((c (car x)))
      (racket-cond
        ;; TODO: don't hardcode the symbol unquote
        ((racket-and (racket-eq? c (racket-quote unquote))
                     (ac-no (cddr x)))
          (cadr x))
        ;; TODO: don't hardcode the symbol unquote-splicing
        ((racket-and (racket-eq? c (racket-quote unquote-splicing))
                     (ac-no (cddr x)))
          (err "cannot use ,@ after ."))
        ;; TODO: don't hardcode the symbol quote
        ((ac-caris c (racket-quote quote))
          (list cons
                (qs-expand-quote (cdr c))
                (qs-expand-pair (cdr x))))
        ;; TODO: don't hardcode the symbol unquote
        ((ac-caris c (racket-quote unquote))
          (list cons
                ;(ac-compile (cadr c))
                (cadr c)
                (qs-expand-pair (cdr x))))
        ;; TODO: don't hardcode the symbol unquote-splicing
        ((ac-caris c (racket-quote unquote-splicing))
          (racket-if (ac-no (cdr x))
            (cadr c)
            (list join
                  ;(ac-compile (cadr c))
                  (cadr c)
                  (qs-expand-pair (cdr x)))))
        ;; TODO: don't hardcode the symbol quasisyntax
        ((ac-caris c (racket-quote quasisyntax))
          (list cons
                (qs-expand-pair (qs-expand (cadr c)))
                (qs-expand-pair (cdr x))))
        #|
        ;; TODO: don't hardcode the symbol quasiquote
        ((ac-caris c (racket-quote quasiquote))
          (list cons
                (qq-expand-pair (qq-expand (cadr c)))
                (qq-expand-pair (cdr x))))|#
        ((racket-pair? c)
          (list cons
                (qs-expand-pair c)
                (qs-expand-pair (cdr x))))
        (racket-else
          (list cons
                ;(ac-compile c)
                c
                (qs-expand-pair (cdr x))))))
    ;(ac-compile x)
    x
    ))

(ac-def qs-expand (x)
  (racket-cond
    ;; TODO: don't hardcode the symbol quote
    ((ac-caris x (racket-quote quote))
      (qs-expand-quote (cdr x)))
    ;; TODO: don't hardcode the symbol unquote
    ((ac-caris x (racket-quote unquote))
      ;(ac-compile (cadr x))
      (cadr x)
      )
    ;; TODO: don't hardcode the symbol unquote-splicing
    ((ac-caris x (racket-quote unquote-splicing))
      (err ",@ cannot be used immediately after #`"))
    ;; TODO: don't hardcode the symbol quasisyntax
    ((ac-caris x (racket-quote quasisyntax))
      (qs-expand (qs-expand (cadr x))))
    #|
    ;; TODO: don't hardcode the symbol quasiquote
    ((ac-caris x (racket-quote quasiquote))
      (qq-expand (qq-expand (cadr x))))|#
    ((racket-pair? x)
      (qs-expand-pair x))
    (racket-else
      ;(ac-compile x)
      x
      )))

(ac-mac quasisyntax (x)
  (qs-expand x))


;=============================================================================
;  assignment
;=============================================================================

(ac-assign-sig sref (f val key (o ind))
  (racket-case-lambda
    [(f val key)
      (racket-cond
        ((racket-namespace? f)
          (namespace-set f key val))
        ((racket-hash? f)
          (racket-if (ac-no val)
                       (racket-hash-remove! f key)
                     (racket-hash-set! f key val)))
        ((racket-string? f)
          (racket-string-set! f key val))
        ((racket-pair? f)
          (racket-if (racket-number? key)
                       (scar (racket-list-tail f key) val)
                     (racket-if (ac-no val)
                                  ;; TODO: should assoc-ref be defined in compiler.arc?
                                  (racket-let ((x (assoc-ref f key)))
                                    (racket-when (ac-true x)
                                      (scar x (cadr x))
                                      (scdr x (cddr x))))
                                ;; TODO: should assoc be defined in compiler.arc?
                                (racket-let ((x (assoc f key)))
                                  (racket-cond
                                    ((ac-true x)
                                      (scar (cdr x) val))
                                    (racket-else
                                      ;; This is needed to prevent cyclic lists
                                      ;; TODO: should idfn be defined in compiler.arc?
                                      (racket-let ((x (racket-map (racket-lambda (x) x) f)))
                                        (scar f (list key val))
                                        (scdr f x))))))))
        ((racket-eq? f car)
          (scar key val))
        ((racket-eq? f cdr)
          (scdr key val))
        ((racket-eq? f caar)
          (scar (car key) val))
        ((racket-eq? f cadr)
          (scar (cdr key) val))
        ((racket-eq? f cddr)
          (scdr (cdr key) val))
        (racket-else
          (err "can't set reference" f key val)))
      val]
    [(f val key ind)
      (racket-cond
        ((racket-eq? f assoc)
          (scar (assoc-ref key ind) val))
        ((racket-eq? f alref)
          (scar (cdr (assoc key ind)) val))
        (racket-else
          (err "can't set reference" f key val ind)))
      val]))

#|
(ac-def ac-sref-if (u args)
  (racket-if (ac-no args)
               nil
             (racket-let ((x (car args))
                          (y (cadr args))
                          (z (cadr (cdr args))))
               (racket-cond
                 ((ac-no y)
                   (ac-sref-if u (cddr (cdr args))))
                 ((ac-no z)
                   (list* x
                          (list = y u)
                          (ac-sref-if u (cddr (cdr args)))))
                 (racket-else
                   (list* x
                          (list = y u)
                          (list = z u)
                          (ac-sref-if u (cddr (cdr args)))))))))

;; TODO: make this prettier
(racket-define sref-mac
  (annotate (racket-quote mac)
    (racket-lambda (f val . rest)
      (racket-let ((rest (racket-list->mlist rest)))
        (racket-cond
          ((racket-eq? f ac-if)
            (racket-let ((u (uniq)))
              (list let u val (cons f (ac-sref-if u rest)))
              #|#`(let u (+ 1 2)
                  (f ,@(ac-mappend (racket-lambda ((x y z))
                                     (if z #`(x (= y u) (= z u))
                                         y #`(x (= y u))))
                                   (tuples args 3))))
              (ac-prn f val rest)|#
              ))
          (racket-else
            (list* sref f val rest)))))))

(racket-hash-set! ac-names sref-mac (racket-quote sref-mac))|#

#|(racket-define sref-mac
  (racket-lambda
    #|
    (with (u    (uniq)
           args #`('a 'b 'c 'd 'e))
      #`(let u (+ 1 2)
          (if ,@(mappend (fn ((x y z))
                           (if z #`(x (= y u) (= z u))
                               y #`(x (= y u))))
                         (tuples args 3)))))
    |#
    [(f val . rest)
      (racket-cond
        ((racket-eq? f ac-if)
          (racket-let ((u (uniq)))
            (ac-prn #`(let u val (f ))))
        (racket-else
          (err "can't set reference" f val rest)))
      val]))|#


;=============================================================================
;  is
;=============================================================================

(ac-def ac-pairwise (pred lst)
  (racket-cond
    ((ac-no lst)       t)
    ((ac-no (cdr lst)) t)
    ((ac-true (pred (car lst) (cadr lst)))
     (ac-pairwise pred (cdr lst)))
    (racket-else nil)))

(ac-def ac-binary (bin reduce)
  (racket-case-lambda
    ((x y) (bin x y))
    (args  (reduce bin args)))) ;(racket-list->mlist )

(ac-def is2 (a b)
  (ac-tnil (racket-or (racket-eqv? a b) ;; TODO: should this use racket-eq?
                      ;; TODO: can I get rid of this...?
                      (racket-and (racket-string? a)
                                  (racket-string? b)
                                  (racket-string=? a b)))))

;; TODO: make a macro for generating these
(ac-assign-sig is args (ac-binary is2 ac-pairwise))


;=============================================================================
;  %splice
;=============================================================================

(racket-define ac-splice (uniq))


;=============================================================================
;  ssyntax
;=============================================================================

(ac-def ssexpand (x) x)
(ac-def ssyntax  (x) nil)


;=============================================================================
;  compile / eval
;=============================================================================

(ac-def ac-compile (x)
  ;(ac-prn x)
  (racket-cond
    ((ac-no x)
      (racket-quote nil))
    #|((ac-caris x ac-fn)
      (ac-fn (cadr x) (cddr x)))
    ((ac-caris x ac-if)
      (ac-if (cdr x)))
    ((ac-caris x ac-assign)
      (ac-assign (cdr x)))|#
    #|((ac-caris x ac-nocompile)
      (ac-nocompile (cdr x)))|#
    #|((ac-caris x ac-splice)
      x)|#
    ((racket-symbol? x)
      (racket-if (ac-true (ssyntax x))
                   (ac-compile (ssexpand x))
                 (racket-if (ac-lex? x)
                              (ac-local-var x)
                            (ac-global-var x))))
    ((racket-string? x)
      (racket-string-copy x))
    ((ac-caris x ac-nocompile)
      (cdr x))
    ((racket-pair? x)
      (ac-call x))
    (racket-else x)))

(ac-def eval (x (runtime nil))
       #:sig (x (o runtime))
  ;(ac-prn (racket-eq? (ac-namespace) arc3-namespace))
  (racket-eval (ac-compile x) ;(ac-deep-fromarc )
               (racket-if (ac-no runtime)
                            (racket-if (racket-namespace? (ac-namespace))
                                         (ac-namespace)
                                       (racket-current-namespace))
                          runtime)))


;=============================================================================
;  load
;=============================================================================

(ac-def ac-eval-all (in)
  (racket-let ((x (sread in)))
    (racket-if (ac-no x)
                 nil
               (racket-begin (eval x)
                             (ac-eval-all in)))))

(ac-def ac-load (x)
  (racket-call-with-input-file x ac-eval-all))
