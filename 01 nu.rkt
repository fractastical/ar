#lang racket/base
;; Nu Arc Compiler -- Corridors of Time
;; http://www.youtube.com/watch?v=9R-Isx2b-GE
;; http://www.youtube.com/watch?v=wK1eJSCmv1A

;; Nu Arc Compiler -- Manifest Destiny
;; http://www.youtube.com/watch?v=qXp3qjeM0e4


#|
Steps to change "01 ac.rkt" to "01 nu.rkt":

replace #t in cond with else
remove ac-denil
remove ar-nil-terminate
remove ac-niltree
make nil a global variable that holds the Racket null value
change instances of 'nil to nil
change instances of '() to null
allow for rebinding nil and t
remove literal?
change ac to use a parameter local-env rather than an argument env

change eqv to eq
|#

(provide (all-defined-out)
         (all-from-out racket/base))

(require ffi/unsafe)
(require racket/path)
(require racket/port)
(require racket/system)
(require racket/tcp)
(require racket/unsafe/ops)

;=============================================================================
;  Convenient extras; can remove
;=============================================================================
(require racket/pretty)

(define (acompile1 ip op)
  (let ((x (read ip)))
    (if (eof-object? x)
        #t
        (let ((scm (ac x)))
          (eval scm)
          (pretty-print scm op)
          (newline op)
          (newline op)
          (acompile1 ip op)))))

; compile xx.arc to xx.arc.scm
; useful to examine the Arc compiler output
(define (acompile inname)
  (let ((outname (string-append inname ".scm")))
    (when (file-exists? outname)
      ;; TODO
      (display "deleting file: ")
      (displayln outname)
      (delete-file outname))
    (call-with-input-file inname
      (lambda (ip)
        (call-with-output-file outname
          (lambda (op)
            (acompile1 ip op)))))))

(define (prn . args)
  (for-each (lambda (x)
              (display x)
              (display " "))
            args)
  (newline)
  (car args))


;=============================================================================
;  Arc variables
;=============================================================================
(define namespace  (make-parameter (make-empty-namespace)))
(define names      (make-hash))

;; TODO: see if redef and reset are needed
#|(define-syntax-rule (redef name parms . body)
  (reset name (lambda parms . body)))

(define-syntax-rule (reset name val)
  ((var-raw 'name) val))|#


;; creates a function that is exposed to Arc
;; use #:sig to define a custom Arc sig
(define-syntax sdef
  (syntax-rules ()
    ((_ name parms #:sig parms2 . body)
      (sset name parms2 (lambda parms . body)))
    ((_ name parms . body)
      (sset name parms (lambda parms . body)))))

;; like sdef but makes the function mutable from within Arc
;; use #:name to define a different Arc name than the Racket name
(define-syntax mdef
  (syntax-rules ()
    ((_ name parms #:name name2 . body)
      (mset name parms #:name name2 (lambda parms . body)))
    ((_ name parms #:sig parms2 . body)
      (mset name parms2 (lambda parms . body)))
    ((_ name parms . body)
      (mset name parms (lambda parms . body)))))


;; wraps the Racket value in a global variable function before making it
;; accessible to Arc while also optionally setting the sig of the name
(define-syntax sset
  (syntax-rules ()
    ((_ a b)        (let ((v b))
                      (nameit 'a v)
                      (set 'a (make-global-var v))))
    ((_ a parms b)  (begin (hash-set! sig 'a 'parms)
                           (sset a b)))))

;; like sset but makes the Racket value mutable from within Arc
;; use #:name to define a different Arc name than the Racket name
(define-syntax mset
  (syntax-rules ()
    ((_ a #:name name b)        (begin (define a b)
                                       (nameit 'name a)
                                       (set 'name (case-lambda
                                                    (()  a)
                                                    ((x) (set! a x))))))
    ((_ a b)                    (mset a #:name a b))
    ((_ a parms #:name name b)  (begin (hash-set! sig 'name 'parms)
                                       (mset a #:name name b)))
    ((_ a parms b)              (begin (hash-set! sig 'a 'parms)
                                       (mset a b)))))


;; creates a parameter in the compiler's namespace, then makes it implicit in
;; Arc's namespace
(define-syntax-rule (pset a b)
  (begin (define a (make-parameter b))
         (set 'a a)))


;; this makes the variable accessible to Arc but doesn't wrap it or do
;; anything else
(define (set a b)
  ;(sref (namespace) b a)
  (namespace-set-variable-value! a b #f (namespace))) ;(coerce (namespace) 'namespace)

(define (nameit name val)
  (when (or (procedure? val)
            (tagged? val))
    (hash-set! names val name)))

(define (make-global-var x)
  (case-lambda
    (()  x)
    ((v) (set! x v))))


;=============================================================================
;  Types
;=============================================================================
(struct tagged (type rep)
  #:constructor-name make-tagged
  ;; TODO: make mutable later, maybe
  ;#:mutable
  #|#:property prop:custom-write
             (lambda (x port mode)
               (display "#(tagged " port)
               (display (type x) port)
               (display " " port)
               (display (rep x) port)
               (display ")" port))|#
  )

(define (iround x)   (inexact->exact (round x)))
(define (wrapnil f)  (lambda args (apply f args) nil))
(define (wraptnil f) (lambda (x)  (tnil (f x))))


;=============================================================================
;  Arc stuff used by the compiler
;=============================================================================
(mset nil  null)
(mset sig  (make-hash))
(mset t    't)

;; TODO: a better argument name than typ
;; TODO: annotate doesn't need to be mutable, but does need to be exposed to
;;       both the compiler and Arc
(mdef annotate (typ rep)
      ;; TODO: does this need to eqv? rather than eq?
  (if (eqv? (type rep) typ)
      rep
      (make-tagged typ rep)))

;; car and cdr probably will be used later, but not right now
(mdef ac-car (x) #:name car
  (if (null? x)
      x
      (car x)))

(mdef ac-cdr (x) #:name cdr
  (if (null? x)
      x
      (cdr x)))

(mdef close args
  (map close1 args)
  (map (lambda (p) (try-custodian p)) args) ;; free any custodian
  nil)

(mdef close1 (p)
  (cond ((input-port? p)    (close-input-port p))
        ((output-port? p)   (close-output-port p))
        ((tcp-listener? p)  (tcp-close p))
        (else               (err "can't close " p))))

;; TODO: list + table of types for coerce
(mdef coerce (x to (base 10))
       #:sig (x to (o base 10))
  (cond ((tagged? x)
          (err "can't coerce annotated object"))
         ;; TODO: does this need to be eqv? rather than eq?
        ((eqv? to (type x)) x)
        ((symbol? x)    (case to
                          ((string)  (symbol->string x))
                          (else      (err "can't coerce" x to))))
        ((pair? x)      (case to
                          ((string)  (apply string-append
                                            (map (lambda (y) (coerce y 'string))
                                                 x)))
                          (else      (err "can't coerce" x to))))
        ;(eq? x nil)
        ((null? x)      (case to
                          ((string)  "")
                          (else      (err "can't coerce" x to))))
        ((char? x)      (case to
                          ((int)     (char->integer x))
                          ((string)  (string x))
                          ((sym)     (string->symbol (string x)))
                          (else      (err "can't coerce" x to))))
        ((exint? x)     (case to
                          ((num)     x)
                          ((char)    (integer->char x))
                          ((string)  (number->string x base))
                          (else      (err "can't coerce" x to))))
        ((number? x)    (case to
                          ((int)     (iround x))
                          ((char)    (integer->char (iround x)))
                          ((string)  (number->string x base))
                          (else      (err "can't coerce" x to))))
        ((string? x)    (case to
                          ((sym)     (string->symbol x))
                          ((cons)    (string->list x))
                          ((num)     (or (string->number x base)
                                         (err "can't coerce" x to)))
                          ((int)     (let ((n (string->number x base)))
                                       (if n  (iround n)
                                              (err "can't coerce" x to))))
                          (else      (err "can't coerce" x to))))
        (else           (err "can't coerce" x to))))

(mdef err (x . rest)
  (apply error x rest))

(mdef ac-eval (expr) #:name eval
  (eval (ac expr)))

;; macroexpand the outer call of a form as much as possible
(mdef macex (e)
  (let ((v (macex1 e)))
    (if (eq? v e)
        v
        (macex v))))

;; macroexpand the outer call of a form once
(mdef macex1 (e)
  (if (pair? e)
      (let ((m (macro? (car e))))
                   ;; TODO: not sure about this
        (if (and m (not (or (eq? m assign)
                            (eq? m fn)
                            (eq? m ac-if)
                            (eq? m ac-quote)
                            (eq? m ac-quasiquote))))
            (apply (rep m) (cdr e))
            e))
      e))

;; TODO: not sure what category this should be placed in
;; TODO: should pipe call ((caddddr x) 'wait)?
(mdef pipe (cmd)
        ;; TODO: destructuring
  (let* ((x   (process/ports #f #f (current-error-port) cmd))
         (in  (car x))
         (out (cadr x)))
    (list in out)))

(mdef protect (during after)
  (dynamic-wind (lambda () #t) during after))

(mdef rep (x)
  (if (tagged? x)
      (tagged-rep x)
      x))

(mdef scar (p x)
  (cond ((pair? p)   (unsafe-set-mcar! p x))
        ((string? x) (string-set! p 0 x))
        (else        (raise-type-error 'scar "pair" p)))
  x)

;; Later may want to have multiple indices.
(mdef sref (x val key)
  (cond ((namespace? x) (namespace-set-variable-value! key val #f x)) ;(global-name key)
                            ;(eq? val nil)
        ((hash? x)      (if (false? val)
                            (hash-remove! x key)
                            (hash-set! x key val)))
        ((string? x)    (string-set! x key val))
        ((pair? x)      (scar (list-tail x key) val))
        (else           (err "can't set reference " x key val)))
  val)

(mdef type (x)
        ;; TODO: better ordering for speed
  (cond ((tagged? x)        (tagged-type x))
        ((namespace? x)     'namespace)
        ((pair? x)          'cons)
        ((symbol? x)        'sym)
        ; (type nil) -> sym
        ((null? x)          'sym)
        ((procedure? x)     'fn)
        ((char? x)          'char)
        ((string? x)        'string)
        ((exint? x)         'int)
        ((number? x)        'num)     ; unsure about this
        ((hash? x)          'table)
        ((output-port? x)   'output)
        ((input-port? x)    'input)
        ((tcp-listener? x)  'socket)
        ((exn? x)           'exception)
        ((thread? x)        'thread)
        (else               ;(err "type: unknown type" x)
                            ;; TODO: not sure about this, but seems okay
                            nil)))

;; Racket functions
(sset -        args                    -)
(sset cons     (x y)                   cons) ;; TODO: look for some uses of cons and replace em with ac-cons
(sset instring (str (o name 'string))  open-input-string)
(sset seconds  ()                      current-seconds)

;; Racket parameters
(sset stdout ((o out))  current-output-port)  ; should be a vars
(sset stdin  ((o in))   current-input-port)
(sset stderr ((o err))  current-error-port)


;=============================================================================
;  Initialization and loading
;=============================================================================
(define exec-dir (current-directory))

(define (init (dir (current-directory)))
  (set! exec-dir dir)
  ;; TODO: why does Arc 3.1 do this?
  (putenv "TZ" ":GMT")
  ;; TODO: why is this in Arc 3.1?
  ;(print-hash-table #t)
  (current-readtable arc3-readtable)
  (%load-all dir))

(mdef %load-all (dir)
  (aload (build-path dir "02 arc.arc")))

(define (repl)
  (aload (build-path exec-dir "03 repl.arc")))

(define (aload filename)
  (call-with-input-file filename aload1))

(define (aload1 p)
  (let ((x (read p)))
    (if (eof-object? x)
        #t ;; TODO: should probably be (void)
        (begin (ac-eval x)
               (aload1 p)))))


;=============================================================================
;  The compiler
;=============================================================================
(define local-env  (make-parameter null)) ;; list of lexically bound variables
(define nocompile  (gensym)) ;; if in the car the expression won't be compiled
(define fail       (gensym))

;; compile an Arc expression into a Racket expression; both are s-expressions
(define (ac x)
  (cond ((symbol? x)
          (if (ssyntax x)
              (ac (ssexpand x))
              (ac-symbol x)))
        ((pair? x)
          (if (caris x nocompile)
              (cdr x)
              (ac-call (car x) (cdr x))))
        ((null? x)
          (list 'quote x))
        ((string? x)
          (ac-string x))
        (else x)))

(define (ac-all x)
  (dottedmap ac x))

(define (dottedmap f x)
  (if (pair? x)
        (cons (f (car x)) (dottedmap f (cdr x)))
      x))


;=============================================================================
;  Namespaces
;=============================================================================
(define (empty-namespace)
  (parameterize ((current-namespace (make-empty-namespace)))
    (namespace-init)
    (current-namespace)))

(define (namespace-init)
  (namespace-require '(rename '#%kernel  #%set  set!))
  (namespace-require '(rename '#%kernel  #%var  case-lambda))
  (namespace-require '(only   '#%kernel  #%top))
  (namespace-require '(only   '#%kernel  #%app #%datum)) ;; TODO
  ;(namespace-require '(only   racket/base  displayln))
  )

(parameterize ((current-namespace (namespace)))
  (namespace-init))


;=============================================================================
;  Variables
;=============================================================================
(define cached-global-ref  (gensym))
(define replace-var        (make-parameter null))
(define unique             (gensym))

(define (var-raw a (def nil))
  (ref (namespace) a def))

(define (var a (def nil))
  (let ((v (var-raw a fail)))
    (if (eq? v fail)
        def
        (v))))

(mdef %symbol-global (x)
  `(,(global-ref x)))

(define (ac-symbol x)
  (let ((r (assq x (replace-var))))
    (if r (cadr r)
          (if (lex? x)
              x
              (%symbol-global x)))))

                                ;; TODO
(define (global-ref name (space (namespace)))
  (let ((hash (ref space cached-global-ref
                   (lambda ()
                     (sref space (make-hash) cached-global-ref)))))
    (hash-ref! hash name
      (lambda ()
        (parameterize ((current-namespace (coerce space 'namespace))
                       ;(compile-allow-set!-undefined #t)
                       )
          (eval `(#%var (() (,name))))
          #|(eval `(#%var (()           (,name))
                        ((,unique) (#%set ,name ,unique))))|#
          )))))

(define (lex? v) ;; is v lexically bound?
  (memq v (local-env)))


;=============================================================================
;  Normal strings and atstrings
;=============================================================================
(define atstrings #f)

(define (ac-string s)
  (if atstrings
      (if (atpos s 0)
          (ac (cons 'string (map (lambda (x)
                                   (if (string? x)
                                       (unescape-ats x)
                                       x))
                                 (codestring s))))
          (unescape-ats s))
      ;; This is for normal strings
      (string-copy s))) ; avoid immutable strings

;; All of this is for atstrings, not needed for normal strings at all
(define (codestring s)
  (let ((i (atpos s 0)))
    (if i  (cons (substring s 0 i)
                 (let* ((rest (substring s (+ i 1)))
                        (in   (open-input-string rest))
                        (expr (read in))
                              ;; TODO: function for this
                        (i2   (let-values (((x y z) (port-next-location in))) z)))
                   (close-input-port in)
                   (cons expr (codestring (substring rest (- i2 1))))))
           (list s))))

; First unescaped @ in s, if any.  Escape by doubling.
(define (atpos s i)
         ;; TODO: shouldn't this use = ?
  (cond ((eqv? i (string-length s)) #f)
        ((eqv? (string-ref s i) #\@)
          (if (and (< (+ i 1) (string-length s))
                   (not (eqv? (string-ref s (+ i 1)) #\@)))
              i
              (atpos s (+ i 2))))
        (else
          (atpos s (+ i 1)))))

(define (unescape-ats s)
  (list->string (let next ((cs (string->list s)))
                  (cond ((null? cs)
                          cs)
                        ((and (eqv? (car cs) #\@)
                              (not (null? (cdr cs)))
                              (eqv? (cadr cs) #\@))
                          (next (cdr cs)))
                        (else
                          (cons (car cs) (next (cdr cs))))))))


;=============================================================================
;  Predicates
;=============================================================================
;; convert Racket booleans to Arc booleans
(define (tnil x) (if x t nil))

;; definition of falseness for Arc's if
(define (false? x)
  (or (eq? x nil)
      (eq? x #f)))

(define (true? x)
  (not (false? x)))

(define (caris x y)
  (and (pair? x)
       (eq? (car x) y)))

(define (isa x y)
  (eq? (type x) y))

(define (exint? x)
  (and (integer? x) (exact? x)))


;=============================================================================
;  Calling
;=============================================================================
(define direct-calls #f)

(define (ac-call f args)
  (let ((macfn (macro? f)))
          ; the next three clauses could be removed without changing semantics
          ; ... except that they work for macros (so prob should do this for
          ; every elt of s, not just the car)
          ;; TODO: don't hardcode the symbols
    (cond ((caris f 'compose)
            (ac (de-compose (cdr f) args)))
          ((caris f 'complement)
            (ac (list 'no (cons (cadr f) args))))
          ((caris f 'andf)
            (ac (de-andf f args)))
          (macfn
            (mac-call macfn args))
          ;; (foo bar) where foo is a global variable bound to a procedure.
          ;; this breaks if you redefine foo to be a non-fn (like a hash table)
          ;; but as long as you don't redefine anything, it's faster
          ((and direct-calls
                (symbol? f)
                (not (lex? f))
                (procedure? (var f)))
            `(,(ac f) ,@(ac-all args)))
          (else
            (let ((f (ac f)))
                  ;; optimization for (#<fn> ...) and ((lambda ...) ...)
              (if (or (procedure? f)
                      (caris f 'lambda))
                  `(     ,f ,@(ac-all args))
                  `(call ,f ,@(ac-all args))))))))

;; the next two are optimizations, except work for macros.
(define (de-compose fns args)
  (prn fns)
  (cond ((null? fns)       `((fn vals (car vals)) ,@args))
        ((null? (cdr fns)) (cons (car fns) args))
        (else              (list (car fns) (de-compose (cdr fns) args)))))

(define (de-andf f args)
  (let ((gs (map (lambda (x) (gensym)) args)))
    `((fn ,gs
        (and ,@(map (lambda (f) `(,f ,@gs))
                    (cdr f))))
      ,@args)))

;; returns #f or the macro
(define (macro? f)
  (cond ((and (symbol? f)
              (not (lex? f)))
          (macro? (var f)))
        ((isa f 'mac)
          f)
        (else #f)))

(define (mac-call m args)
  ;; TODO: (apply call ...)
  (ac (apply (rep m) args)))

(define (arg-list* args)
  (if (null? (cdr args))
      (car args)
      (cons (car args)
            (arg-list* (cdr args)))))


(define call ;(x . args)
  (case-lambda
    ((x)              (if (procedure? x)
                          (x)
                          (ref x)))
    ((x a)            (if (procedure? x)
                          (x a)
                          (ref x a)))
    ((x a b)          (if (procedure? x)
                          (x a b)
                          (ref x a b)))
    ((x a b c)        (if (procedure? x)
                          (x a b c)
                          (ref x a b c)))
    ((x a b c d)      (if (procedure? x)
                          (x a b c d)
                          (ref x a b c d)))
    ((x a b c d e)    (if (procedure? x)
                          (x a b c d e)
                          (ref x a b c d e)))
    ((x a b c d e f)  (if (procedure? x)
                          (x a b c d e f)
                          (ref x a b c d e f)))
    ((x . args)       (begin ;(prn "warning: called with 7+ arguments:" x args)
                             (if (procedure? x)
                                 (apply x args)
                                 (apply ref x args))))))

(mset ref* (make-hash))

;; call a function or perform an array ref, hash ref, etc.
;;
;; Non-fn constants in functional position are valuable real estate, so
;; should figure out the best way to exploit it.  What could (1 foo) or
;; ('a foo) mean?  Maybe it should mean currying.
;;
;; For now the way to make the default val of a hash table be other than
;; nil is to supply the val when doing the lookup.  Later may also let
;; defaults be supplied as an arg to table.  To implement this, need: an
;; eq table within scheme mapping tables to defaults, and to adapt the
;; code in arc.arc that reads and writes tables to read and write their
;; default vals with them.  To make compatible with existing written tables,
;; just use an atom or 3-elt list to keep the default.
;;
;; experiment: means e.g. [1] is a constant fn
;;       ((or (number? fn) (symbol? fn)) fn)
;; another possibility: constant in functional pos means it gets
;; passed to the first arg, i.e. ('kids item) means (item 'kids).
(mset ref (x . args)
  ;; TODO: tests for procedure? so you can say (ref car ...) (ref (fn () 5)) etc.

  ;; uses case-lambda for ridiculous speed: now using ref for *all* function
  ;; calls is just as fast as using the funcall functions, and unlike
  ;; funcall, this hardcodes up to 6 arguments rather than only 4
  ;;
  ;; I could go higher but it'd be kinda pointless and would just make the
  ;; definition of ref even bigger than it already is
  ;;
  ;; maybe I could write a macro to automatically generate the special-cases
  ;; for procedures
  (case-lambda
    ((x k)       (let ((v (hash-ref ref* (type x) nil)))
                   (cond ((true? v)       (v x k))
                         ((namespace? x)  (namespace-variable-value k #f (lambda () nil) x)) ;(global-name k)
                         ((hash? x)       (hash-ref x k nil))
                         ((string? x)     (string-ref x k))
                         ((pair? x)       (list-ref x k))
                         (else (err "function call on inappropriate object" x k)))))
    ((x k d)     (let ((v (hash-ref ref* (type x) nil)))
                   (cond ((true? v)       (v x k d))
                         ((namespace? x)  (namespace-variable-value k #f ;(global-name k)
                                            (if (procedure? d) d (lambda () d))
                                            x))
                         ((hash? x)       (hash-ref x k d))
                         (else (err "function call on inappropriate object" x k d)))))
    ((x . args)  (let ((v (hash-ref ref* (type x) nil)))
                   (if (true? v)
                       (apply v x args)
                       (apply err "function call on inappropriate object" x args))))))


;=============================================================================
;  Binaries
;=============================================================================
;; (pairwise pred '(a b c d)) =>
;;   (and (pred a b) (pred b c) (pred c d))
;; pred returns t/nil, as does pairwise
(define (pairwise pred lst)
  (cond ((null? (cdr lst)) t)
        ;; TODO: maybe the binary functions should return #t and #f rather
        ;;       than t and nil
        ((true? (pred (car lst) (cadr lst)))
          (pairwise pred (cdr lst)))
        (else nil)))

;; based on Arc's reduce. Can't use foldl because it doesn't work well with
;; multiple types (e.g. +-2)
(define (reduce f xs)
  (if (null? (cdr xs))
      (car xs) ;(f (car xs))
      (reduce f (cons (f (car xs) (cadr xs)) (cddr xs)))))

;; TODO: should pairwise take an init parameter or not...?
(define (make-pairwise f)
  (case-lambda
    ((x y) (f x y))
    (args  (pairwise f args))
    ((x)   t)
    (()    t)))

(define (make-reduce f init)
  (case-lambda
    ((x y) (f x y))
    (args  (reduce f args))
    ((x)   x)
    (()    init)))

;; generic comparison
(define (make-comparer a b c)
  (lambda (x y)
                ;; TODO: better ordering for speed
    (tnil (cond ((number? x)  (a x y))
                ((string? x)  (b x y))
                ((char? x)    (c x y))
                ((symbol? x)  (b (symbol->string x)
                                 (symbol->string y)))
                (else         (a x y))))))

;; generic +: strings, lists, numbers.
;; return val has same type as first argument.
(define (+-2 x y)
        ;; TODO: better ordering for speed
  (cond ((number? x)  (+ x y))
        ((string? x)  (string-append x (coerce y 'string)))
        ((list? x)    (append x y))
        ;; TODO: check the behavior of Arc 3.1 for (+ "foo" #\a) and (+ #\a "foo")
        ((char? x)    (string-append (string x) (coerce y 'string)))
        (else         (+ x y)
                      ;(err "can't + " x " with " y)
                      )))

(define <-2 (make-comparer < string<? char<?))
(define >-2 (make-comparer > string>? char>?))

;; not quite right, because behavior of underlying eqv unspecified
;; in many cases according to r5rs
;; do we really want is to ret t for distinct strings?
(define (is-2 a b)
  (tnil (or (eqv? a b)
            (and (string? a) (string? b) (string=? a b))
            ;; TODO: why is this here in Arc 3.1?
            ;(and (false? a) (false? b))
            )))


;=============================================================================
;  I/O
;=============================================================================
(define explicit-flush #f)

(define (print f x port)
        ;; TODO: should probably use (no x) or whatever
  (cond ((null? x)       (display "nil" port))
        ;; TODO: maybe use isa for pair? and procedure?
        ((pair? x)       (print-w/list f x port))
        ((procedure? x)  (print-w/name x "#<fn" ":" ">" port))
        ((isa x 'mac)    (print-w/name x "#<mac" ":" ">" port))
        ((tagged? x)     (display "#(tagged " port)
                         (print f (type x) port)
                         (display " " port)
                         (print f (rep x)  port)
                         (display ")" port))
        (else            (f x port)))
  nil)

(define (name x)
  (or (hash-ref names x #f)
      (and (not (tagged? x))
           (object-name x))
      nil))

(define (print-w/list f x port)
  (display "(" port)
  (let loop ((x x))
    (cond ((pair? x)
            (print f (car x) port)
            (unless (null? (cdr x))
              (display " " port)
              (loop (cdr x))))
          (else
            (display ". " port)
            (print f x port))))
  (display ")" port))

(define (print-w/name x l m r port)
  (let ((x (name x)))
    (display l port)
    (when (true? x)
      (display m port)
      (display x port))
    (display r port)))

(define (make-read f)
  (lambda ((in (current-input-port)))
    (let ((x (f in)))
      (if (eof-object? x) nil x))))

(define (make-write f)
  (lambda (c (out (current-output-port)))
    (f c out)
    c))

(define (make-print f)
  (lambda (x (out (current-output-port)))
    (print f x out)
    (unless explicit-flush (flush-output out))
    nil))

; make sure only one thread at a time executes anything
; inside an atomic-invoke. atomic-invoke is allowed to
; nest within a thread; the thread-cell keeps track of
; whether this thread already holds the lock.
(define the-sema (make-semaphore 1))

(define sema-cell (make-thread-cell #f))

; there are two ways to close a TCP output port.
; (close o) waits for output to drain, then closes UNIX descriptor.
; (force-close o) discards buffered output, then closes UNIX desc.
; web servers need the latter to get rid of connections to
; clients that are not reading data.
; mzscheme close-output-port doesn't work (just raises an error)
; if there is buffered output for a non-responsive socket.
; must use custodian-shutdown-all instead.
(define custodians (make-hash))

(define (associate-custodian c i o)
  (hash-set! custodians i c)
  (hash-set! custodians o c))

; if a port has a custodian, use it to close the port forcefully.
; also get rid of the reference to the custodian.
; sadly doing this to the input port also kills the output port.
(define (try-custodian p)
  (let ((c (hash-ref custodians p #f)))
    (if c  (begin (custodian-shutdown-all c)
                  (hash-remove! custodians p)
                  #t)
           #f)))


;=============================================================================
;  square-brackets / curly-brackets
;=============================================================================
(define (read-square-brackets ch port src line col pos)
  `(square-brackets ,@(read/recursive port #\[ #f)))

(define (read-curly-brackets ch port src line col pos)
  `(curly-brackets ,@(read/recursive port #\{ #f)))

(define arc3-readtable
  (make-readtable #f #\[ 'terminating-macro read-square-brackets
                     #\{ 'terminating-macro read-curly-brackets))

(sset square-brackets body
  (annotate 'mac (lambda body `(fn (_) ,body))))


;=============================================================================
;  assign
;=============================================================================
(define (ac-assign x)
  (let ((x (pairfn assign1 x)))
    (if (null? (cdr x))
        (car x)
        (cons 'begin x))))

(define (pairfn f x)
  (if (null? x)
      x
               ;; TODO: why does Arc 3.1 call macex here?
      (cons (f (car x) (cadr x))
                ;; this is so the assign form returns the value
            (if (and (null? (cddr x))
                     ;; TODO: why is this here?
                     (lex? (car x)))
                (list (ac (car x)))
                (pairfn f (cddr x))))))

(define (assign1 a b1)
  (if (symbol? a)
      (if (lex? a)  `(set! ,a ,(ac b1))
                    `(assign-global-raw ,(namespace) ',a ,(ac b1)))
      (err "first arg to assign must be a symbol" a)))

(define (assign-global-new space name val)
  (sref space (make-global-var val) name))

(define (assign-global-raw space name val)
  (nameit name val)
  (let ((v (ref space name fail)))
    (if (eq? v fail)
        (assign-global-new space name val)
        (v val)))
  val)

(mset assign args
  (annotate 'mac (lambda args
                   (cons nocompile (ac-assign args)))))


;=============================================================================
;  if
;=============================================================================
; (if)           -> nil
; (if x)         -> x
; (if t a ...)   -> a
; (if nil a b)   -> b
; (if nil a b c) -> (if b c)
(define (ac-ifn args)
        ;; TODO: maybe simplify this a little, like by using ac-cdr
  (cond ((null? args) 'nil)
        ((null? (cdr args))
          (ac (car args)))
        (else
               ;; TODO: inline
          `(if (true? ,(ac (car args)))
               ,(ac (cadr args))
               ,(ac-ifn (cddr args))))))

(mset ac-if args #:name if
  (annotate 'mac (lambda args
                   (cons nocompile (ac-ifn args)))))


;=============================================================================
;  fn
;=============================================================================
(define fn-gensym-args  (make-parameter #f))
(define fn-body         (make-parameter null))
(define fn-let*         (make-parameter null))

(define (cons-to x y)
  (x (cons y (x))))

(define (append-to x y)
  (x (append y (x))))

(define (ac-fn parms body)
  (cons 'lambda
        (parameterize ((fn-body (if (null? body)
                                    (list 'nil) ;; TODO: nil or 'nil ?
                                    body)))
          (cons (parameterize ((local-env  (local-env))
                               (fn-let*    null))
                  (let ((x (fn-args parms)))
                    (fn-body (if (null? (fn-let*))
                                 (ac-all (fn-body))
                                 (list (list* 'let*
                                              (fn-let*)
                                              (ac-all (fn-body))))))
                    x))
                (fn-body)))))

(define (fn-gensym x)
  (if (fn-gensym-args)
      (let ((u (gensym)))
        (cons-to replace-var (list x (list 'quote u)))
        u)
      x))

(define (fn-args x)
  (cond ((null? x) x)                  ;; end of the argument list
        ((symbol? x)                   ;; dotted rest args
          (let ((x (fn-gensym x)))
            (cons-to local-env x)
            x))
        ((and (fn-gensym-args)
              (caris (car x) 'quote))  ;; anaphoric arg
          (cons-to local-env (cadar x))
          (prn (cadar x))
          (cons (cadar x) (fn-args (cdr x)))
          )
        ((caris (car x) 'o)            ;; optional arg
          (let* ((c  (car x))
                 (n  (fn-gensym (cadr c)))
                 (d  (cddr c)))
            (cons-to local-env n)
                          ;; TODO: code duplication with ac-fn
            (cons (cons n (if (null? d)
                              (list 'nil) ;; TODO: nil or 'nil ?
                              (ac-all d)))
                  (fn-args (cdr x)))))
        ((pair? (car x))               ;; destructuring args
          (let ((u (gensym)))
            (append-to fn-let* (fn-destructuring u (car x)))
            (cons u (fn-args (cdr x)))))
        (else                          ;; normal args
          (let ((n (fn-gensym (car x))))
            (cons-to local-env n)
            (cons n (fn-args (cdr x)))))))


;; u is a local variable which refers to the current place within the object
;; that is being destructuring
;;
;; x is the destructuring argument list
;; TODO: use Arc's car and cdr so destructuring works on lists that are too
;;       small
(define (fn-destructuring u x)
  (cond ((null? x) x)                  ;; end of the argument list
        ((symbol? x)                   ;; dotted rest args
          (let ((x (fn-gensym x)))
            (cons-to local-env x)
            (list (list x u))))
        ((caris (car x) 'o)            ;; optional args
          ;; TODO: code duplication with fn-args
          (let* ((c  (car x))
                 (n  (fn-gensym (cadr c)))
                 (d  (caddr c)))
            (cons-to local-env n)
                                    ;; TODO: code duplication
            (cons (list n (ac `(if ,(cons nocompile u)
                                    (car ,(cons nocompile u))
                                   ,d)))
                  (fn-destructuring-next u x))))
        ((pair? (car x))               ;; destructuring args
          (let ((v (gensym)))
            (cons (list v (ac `(car ,(cons nocompile u))))
                  (append (fn-destructuring v (car x))
                          (fn-destructuring-next u x)))))
        (else                          ;; normal args
          (let ((n (fn-gensym (car x))))
            (cons-to local-env n)
            (cons (list n (ac `(car ,(cons nocompile u))))
                  (fn-destructuring-next u x))))))

(define (fn-destructuring-next u x)
  (if (null? (cdr x))
      null
      (cons (list u (ac `(cdr ,(cons nocompile u))))
            (fn-destructuring u (cdr x)))))

(mset fn (parms . body)
  (annotate 'mac (lambda (parms . body)
                   (cons nocompile (ac-fn parms body)))))


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
(define (qq-expand x)
        ;; TODO: don't hardcode the symbol unquote
  (cond ((caris x 'unquote)
          (cadr x))
        ;; TODO: don't hardcode the symbol unquote-splicing
        ((caris x 'unquote-splicing)
          (err ",@ cannot be used immediately after `"))
        ;; TODO: don't hardcode the symbol quasiquote
        ((caris x 'quasiquote)
          (qq-expand (qq-expand (cadr x))))
        ((pair? x)
          (qq-expand-pair x))
        (else
          (list ac-quote x))))

(define (qq-expand-pair x)
  (if (pair? x)
      (let ((c (car x)))
              ;; TODO: don't hardcode the symbol unquote
        (cond ((and (eq? c 'unquote)
                    (null? (cddr x)))
                (cadr x))
              ;; TODO: don't hardcode the symbol unquote-splicing
              ((and (eq? c 'unquote-splicing)
                    (null? (cddr x)))
                (err "cannot use ,@ after ."))
              ;; TODO: don't hardcode the symbol unquote
              ((caris c 'unquote)
                (list cons (cadr c)
                           (qq-expand-pair (cdr x))))
              ;; TODO: don't hardcode the symbol unquote-splicing
              ((caris c 'unquote-splicing)
                (if (null? (cdr x))
                      (cadr c)
                    (list append (cadr c)
                                 (qq-expand-pair (cdr x)))))
              ;; TODO: don't hardcode the symbol quasiquote
              ((caris c 'quasiquote)
                (list cons (qq-expand-pair (qq-expand (cadr c)))
                           (qq-expand-pair (cdr x))))
              (else
                (list cons (qq-expand-pair c)
                           (qq-expand-pair (cdr x))))))
          ;; TODO: maybe remove this
      (if (null? x)
          x
          (list ac-quote x))))

(mset ac-quasiquote (x) #:name quasiquote
  (annotate 'mac (lambda (x)
                   (qq-expand x))))


;=============================================================================
;  quote
;=============================================================================
(mset ac-quote (x) #:name quote
  (annotate 'mac (lambda (x)
                       ;; TODO: a little hacky
                   (if (eq? x 'nil)
                       nil
                       (list (lambda () x))))))


;=============================================================================
;  ssyntax
;=============================================================================
(define (ssyntax x)
  (and (symbol? x)
       ;(not (or (eq? x '+) (eq? x '++) (eq? x '_)))
       (let ((name (symbol->string x)))
         (has-ssyntax-char? name (- (string-length name) 1)))))

;; TODO: why does this count backwards...? efficiency, maybe?
(define (has-ssyntax-char? string i)
  (and (>= i 0)
       (or (let ((c (string-ref string i)))
             (or (eqv? c #\:) (eqv? c #\~)
                 (eqv? c #\&)
                 ;(eqv? c #\_)
                 (eqv? c #\.)  (eqv? c #\!)))
           (has-ssyntax-char? string (- i 1)))))

(define (read-from-string str)
  (let ((port (open-input-string str)))
    (let ((val (read port)))
      (close-input-port port)
      val)))

; Though graphically the right choice, can't use _ for currying
; because then _!foo becomes a function.  Maybe use <>.  For now
; leave this off and see how often it would have been useful.

; Might want to make ~ have less precedence than &, because
; ~foo&bar prob should mean (andf (complement foo) bar), not
; (complement (andf foo bar)).

;; TODO: better definition of ssexpand
(define (ssexpand sym)
  ((cond ((or (insym? #\: sym) (insym? #\~ sym)) expand-compose)
         ((or (insym? #\. sym) (insym? #\! sym)) expand-sexpr)
         ((insym? #\& sym)                       expand-and)
         (else (err "Unknown ssyntax" sym)))
   sym))

(define (expand-compose sym)
  (let ((elts (map (lambda (tok)
                     (if (eqv? (car tok) #\~)
                         (if (null? (cdr tok))
                             ;; TODO: don't hardcode the symbol 'no ?
                             'no
                             ;; TODO: don't hardcode the symbol 'complement ?
                             `(complement ,(chars->value (cdr tok))))
                         (chars->value tok)))
                   (tokens (lambda (c) (eqv? c #\:))
                           (symbol->chars sym)
                           null
                           null
                           #f))))
    (if (null? (cdr elts))
        (car elts)
        ;; TODO: don't hardcode the symbol 'compose ?
        (cons 'compose elts))))

(define (expand-sexpr sym)
  (build-sexpr (reverse (tokens (lambda (c) (or (eqv? c #\.) (eqv? c #\!)))
                                (symbol->chars sym)
                                null
                                null
                                #t))
               sym))

(define (expand-and sym)
  (let ((elts (map chars->value
                   (tokens (lambda (c) (eqv? c #\&))
                           (symbol->chars sym)
                           null
                           null
                           #f))))
    (if (null? (cdr elts))
        (car elts)
        ;; TODO: don't hardcode the symbol 'andf ?
        (cons 'andf elts))))

(define (build-sexpr toks orig)
  (cond ((null? toks)
          ;; TODO: don't hardcode the symbol 'get ?
          'get)
        ((null? (cdr toks))
          (chars->value (car toks)))
        (else
          (list (build-sexpr (cddr toks) orig)
                (if (eqv? (cadr toks) #\!)
                    ;; TODO: don't hardcode the symbol 'quote ?
                    (list 'quote (chars->value (car toks)))
                    (if (or (eqv? (car toks) #\.) (eqv? (car toks) #\!))
                        (err "bad ssyntax" orig)
                        (chars->value (car toks))))))))

(define (insym? char sym)    (member char (symbol->chars sym)))
(define (symbol->chars x)    (string->list (symbol->string x)))
(define (chars->value chars) (read-from-string (list->string chars)))

(define (tokens test source token acc keepsep?)
  (cond ((null? source)
          (reverse (if (pair? token)
                       (cons (reverse token) acc)
                       acc)))
        ((test (car source))
          (tokens test
                  (cdr source)
                  null
                  (let ((rec (if (null? token)
                                 acc
                                 (cons (reverse token) acc))))
                    (if keepsep?
                        (cons (car source) rec)
                        rec))
                  keepsep?))
        (else
          (tokens test
                  (cdr source)
                  (cons (car source) token)
                  acc
                  keepsep?))))


;=============================================================================
;  Extra stuff
;=============================================================================
(set 'namespace  namespace)

(mset dref (x (o k))
  (case-lambda
    ((n)    (let ((x (ref (namespace) n fail)))
              (if (eq? x fail)
                  nil
                  (begin (namespace-undefine-variable! n (coerce (namespace) 'namespace))
                         x))))
    ((x k)  (err "can't delete reference" x k))))

(sset % args
  (annotate 'mac (lambda args
                   (cons nocompile
                         (if (null? (cdr args))
                             (car args)
                             (cons 'begin args))))))


;=============================================================================
;  Arc functions not used by the compiler
;=============================================================================
;; Racket functions
(sset *                            args                       *)
(sset /                            args                       /)
(sset acos                         (n)                        acos)
(sset asin                         (n)                        asin)
(sset atan                         (n (o m))                  atan)
(sset break-thread                 (x)                        break-thread)
(sset ccc                          (f (o prompt))             call-with-current-continuation)
(sset cos                          (n)                        cos)
(sset current-gc-milliseconds      ()                         current-gc-milliseconds)
(sset current-process-milliseconds (x)                        current-process-milliseconds)
(sset current-thread               ()                         current-thread)
(sset expt                         (n to)                     expt)
(sset infile                       (path (o #:mode 'binary))  open-input-file)
;; use as a general fn for looking inside things
(sset inside                       (out)                      get-output-string)
(sset kill-thread                  (x)                        kill-thread)
(sset log                          (n)                        log) ;; logarithm
(sset memory                       ((o custodian))            current-memory-use)
(sset mod                          (n m)                      modulo)
(sset msec                         ()                         current-milliseconds)
(sset new-thread                   (thunk)                    thread)
(sset newstring                    (n (o c #\nul))            make-string)
(sset outstring                    ((o name 'string))         open-output-string)
(sset quit                         ((o n 0))                  exit)
(sset rand                         ((o n) (o gen))            random) ;: TODO: need to use a better seed (Arc 3.1???)
(sset sin                          (n)                        sin)
(sset sqrt                         (n)                        sqrt)
(sset tan                          (n)                        tan)

;; allow Arc to give up root privileges after it calls open-socket.
;; thanks, Eli!
(sset setuid (i) (get-ffi-obj 'setuid #f (_fun _int -> _int)))

;; binaries
(sset +  args (make-reduce    +-2 0))
(sset >  args (make-pairwise  >-2))
(sset <  args (make-pairwise  <-2))
(sset is args (make-pairwise is-2))

;; wrapnil
(sset rmfile (path)      (wrapnil delete-file))
(sset sleep  ((o sec 0)) (wrapnil sleep))
;; Will system "execute" a half-finished string if thread killed in the
;; middle of generating it?
(sset system (command)   (wrapnil system))

;; wraptnil
(sset dead        (x) (wraptnil thread-dead?))
(sset dir-exists  (x) (wraptnil directory-exists?))
(sset exact       (x) (wraptnil exint?)) ;; TODO: bad name
(sset file-exists (x) (wraptnil file-exists?))
(sset ssyntax     (x) (wraptnil ssyntax))

;; make-read
(sset readc ((o in (stdin))) (make-read read-char))
(sset readb ((o in (stdin))) (make-read read-byte))
(sset peekc ((o in (stdin))) (make-read peek-char))

;; make-write
(sset writec (c (o out (stdout))) (make-write write-char))
(sset writeb (c (o out (stdout))) (make-write write-byte))

;; make-print
(sset write  (x (o out (stdout))) (make-print write))
(sset disp   (x (o out (stdout))) (make-print display))

;; functions
(sdef apply (f . args)
  (apply call f (arg-list* args)))

;; TODO: make this better
(sdef atomic-invoke (f)
  (if (thread-cell-ref sema-cell)
      ;; TODO: why are these call...?
      (call f)
      (begin (thread-cell-set! sema-cell #t)
             (protect (lambda ()
                        (call-with-semaphore
                          the-sema
                          (lambda () (call f))))
                      (lambda ()
                        (thread-cell-set! sema-cell #f))))))

(sdef bound (x)
  (tnil (not (eq? (var-raw x fail) fail))))

(sdef call-w/stdin (port thunk)
  (parameterize ((current-input-port port)) (thunk)))

(sdef call-w/stdout (port thunk)
  (parameterize ((current-output-port port)) (thunk)))

(sdef client-ip (port)
  (let-values (((x y) (tcp-addresses port)))
              y))

(sdef declare (key val)
  (let ((flag (true? val)))
    (case key
      ((atstrings)      (set! atstrings      flag))
      ((direct-calls)   (set! direct-calls   flag))
      ((explicit-flush) (set! explicit-flush flag)))
    val))

(sset details (e) exn-message)
               ;; TODO: why does this use disp-to-string...?
  ;(lambda (e) (disp-to-string (exn-message e)))

;; TODO: better dir
(sdef dir (name)
  (map path->string (directory-list name)))

; Added because Mzscheme buffers output.  Not a permanent part of Arc.
; Only need to use when declare explicit-flush optimization.
(sdef flushout ()
  (flush-output)
  t)

(sdef force-close args
       ;; TODO: force-close1
  (map (lambda (p)
         (when (not (try-custodian p))
           (close p)))
       args)
  nil)

(sdef len (x)
  (cond ((string? x) (string-length x))
        ((hash? x)   (hash-count x))
        (else        (length x))))

(sdef maptable (fn table)
  (hash-for-each table fn) ; arg is (fn (key value) ...)
  table)

;; TODO: mkdir with make-directory*
; Would def mkdir in terms of make-directory and call that instead
; of system in ensure-dir, but make-directory is too weak: it doesn't
; create intermediate directories like mkdir -p.

(sdef mvfile (old new (flag t))
       #:sig (old new (o flag t))
  (rename-file-or-directory old new (true? flag))
  nil)

; If an err occurs in an on-err expr, no val is returned and code
; after it doesn't get executed.  Not quite what I had in mind.
(sdef on-err (errfn f)
  (with-handlers ((exn:fail? errfn)) (f))
  ;; TODO: why does Arc 3.1 implement it like this?
  #|((call-with-current-continuation
     (lambda (k)
       (lambda ()
         (with-handlers ((exn:fail? (lambda (e)
                                      (k (lambda () (errfn e))))))
                        (f))))))|#
  )

(sdef open-socket (num)
  (tcp-listen num 50 #t))

                 ;; TODO check this
(sdef outfile (f (mode 'truncate))
        #:sig (f (o mode 'truncate))
  (open-output-file f 'text mode))

(sdef pipe-from (cmd)
        ;; TODO: destructuring
  (let* ((x   (pipe cmd))
         (in  (car x))
         (out (cadr x)))
    ;; Racket docs say I need to close all 3 ports explicitly,
    ;; but the err port doesn't need to be closed, because it's
    ;; redirected to stderr
    (close out)
    in))

(sdef scdr (p x)
  (cond ((pair? p)   (unsafe-set-mcdr! p x))
        ((string? x) (err "can't set cdr of a string" x))
        (else        (raise-type-error 'scdr "pair" p)))
  x)

; the 2050 means http requests currently capped at 2 meg
; http://list.cs.brown.edu/pipermail/plt-scheme/2005-August/009414.html
(sdef socket-accept (s)
  (let ((oc (current-custodian))
        (nc (make-custodian)))
    (current-custodian nc)
    (call-with-values
      (lambda () (tcp-accept s))
      (lambda (in out)
        (let ((in1 (make-limited-input-port in 100000 #t)))
          (current-custodian oc)
          (associate-custodian nc in1 out)
          (list in1
                out
                (let-values (((us them) (tcp-addresses out)))
                            them)))))))

; sread = scheme read. eventually replace by writing read
(sdef sread (p eof)
  (let ((expr (read p)))
    (if (eof-object? expr) eof expr)))

(sdef ssexpand (x)
  (if (symbol? x) (ssexpand x) x))

; Racket provides eq? eqv? and equal? hash tables
; we need equal? for strings
(sdef table ((init nil))
      #:sig ((o init))
  (let ((h (make-hash)))
    (when (true? init)
      (init h))
    h))

(sdef timedate ((sec (current-seconds)))
         #:sig ((o sec (seconds)))
  (let ((d (seconds->date sec)))
    (list (date-second d)
          (date-minute d)
          (date-hour d)
          (date-day d)
          (date-month d)
          (date-year d))))

(sdef trunc (x)
  (inexact->exact (truncate x)))


(pset uniq-counter  1) ;; TODO: not really a part of Arc

(sdef uniq ((name 'g) (num nil))
     #:sig ((o name 'g) (o num))
  (let ((num (if (false? num)
                 (let ((num (uniq-counter)))
                   (uniq-counter (+ (uniq-counter) 1))
                   num)
                 num)))
    (string->uninterned-symbol
      (string-append (coerce name 'string)
                     (coerce num  'string)))))
