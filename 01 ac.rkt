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

;(#%require 'racket/base)

(provide (all-defined-out)
         (all-from-out racket/base)
         ;(all-from-out racket/path)
         )

;(require (file "02 nu.rkt"))
(require ffi/unsafe)
(require racket/path)
(require racket/port)
(require racket/system)
(require racket/tcp)
(require racket/unsafe/ops)

#|(define-syntax (#%top stx)
  ;(sprn stx)
  (syntax-case stx ()
    ((_ . var) (begin (prn var)
                      (#%top . var)))))|#

#|(define-syntax-rule (#%top . var)
  (begin ((#%variable-reference displayln) var)
         var))|#

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
;  The compiler!
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
          ;(list ac-quote x)
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
  ;(namespace-require '(rename '#%kernel  #%top  #%top))
  (namespace-require '(only   '#%kernel  #%top))
  (namespace-require '(only   '#%kernel  #%app #%datum)) ;; TODO
  ;(namespace-require '(only   racket/base  displayln))
  )

(define arc3-namespace  (empty-namespace))
(define namespace       (make-parameter arc3-namespace))
(define ac-unique       (gensym))
;(define ac-unique (gensym))

#|(define (global-ref name)
  (global-var-ref name arc3-namespace)
  #|(let ((v (var-raw name fail)))

    (prn v)
    )|#
  )|#

(define cached-global-ref (gensym))

                                ;; TODO
(define (global-ref name (space arc3-namespace))
  (let ((hash (ref space cached-global-ref
                   (lambda ()
                     (sref space (make-hash) cached-global-ref)))))
    ;(prn name)
    (hash-ref! hash name
      (lambda ()
        (parameterize ((current-namespace space)
                       (compile-allow-set!-undefined #t))
          (eval `(#%var (()           (,name))
                        ((,ac-unique) (#%set ,name ,ac-unique)))))))))

;(namespace-init)


;=============================================================================
;  Variables
;=============================================================================
(define replace-var  (make-parameter null))
(define sig          (make-hash))
(define names        (make-hash))

(define-syntax-rule (redef name parms . body)
  (reset name (lambda parms . body)))

(define-syntax-rule (reset name val)
  ((var-raw 'name) val))

#|(define-syntax redef
  (syntax-rules ()
    ((_ name val)           ((var-raw 'name) val))
    ((_ name parms . body)  (redef name (lambda parms . body)))))|#

(define-syntax sdef
  (syntax-rules ()
    ((_ name parms #:sig parms2 . body)
      (sset name parms2 (lambda parms . body)))
    ((_ name parms . body)
      (sset name parms (lambda parms . body)))))

(define-syntax-rule (mdef name parms . body)
  (begin (define name (lambda parms . body))
         (mset name parms name)))

#|(define-syntax mdef
  (syntax-rules ()
    ((_ name parms . body)
      (begin (define name (lambda parms . body))
             (mset name parms name)))
    ((_ name parms #:sig parms2 . body)
      (begin (define name (lambda parms . body))
             (mset name parms2 name)))

    ((_ name #:name name2 parms . body)
      (begin (define name2 (lambda parms . body))
             (mset name parms name2)))
    ((_ name #:name name2 parms #:sig parms2 . body)
      (begin (define name2 (lambda parms . body))
             (mset name parms2 name2)))))|#


;; wraps the Racket value in a global variable function before making it
;; accessible to Arc while also optionally setting the sig of the name
(define-syntax sset
  (syntax-rules ()
    ((_ a b)        (set 'a (make-global-var b)))
    ((_ a parms b)  (begin (hash-set! sig 'a 'parms)
                           (sset a b)))))

#|
;; like sset but mutable
;; should probably be the default, but I need to run speed tests first
(define-syntax-rule (mset a parms b)
  (begin (hash-set! sig 'a 'parms)
         (set 'a (case-lambda
                   (()  b)
                   ((x) (set! b x))))))|#

;; like sset but makes the Racket name mutable from within Arc.
;; can only bind Racket names, not Racket values
(define-syntax mset
  (syntax-rules ()
    ((_ a b)        (set 'a (case-lambda
                              (()  b)
                              ((x) (set! b x)))))
    ((_ a parms b)  (begin (hash-set! sig 'a 'parms)
                           (mset a b)))))


(define (lex? v) ;; is v lexically bound?
  (memq v (local-env)))

(define (make-global-var x)
  (case-lambda
    (()  x)
    ((v) #;(prn "assigning" v) (set! x v))))
#|
;; TODO: version that uses two tables mapping symbols to gensyms
(define (global-name x)
  (string->symbol (string-append "_" (symbol->string x))))|#

(define (nameit name val)
  ;(prn name val)
  (when ;(and (not (hash-has-key? names val))
             (or (procedure? val)
                 (tagged? val));)
    ;(prn name val)
    (hash-set! names val name)))


;; this makes the variable accessible to Arc (potentially giving it a name as
;; well) but doesn't wrap it or do anything else
(define (set a b)
  (nameit a b) ;(hash-set! names b 'a)
  ;(sref (namespace) b (global-name a))
                                        ;; TODO
  (namespace-set-variable-value! a b #f arc3-namespace) ;(global-name a)
  ;(void)
  )

(define (var-raw a (def nil))
  (ref (namespace) a def)) ;(global-name )

(define (var a (def nil))
  (let ((v (var-raw a fail)))
    (if (eq? v fail)
        def
        (v))))

(mdef ac-symbol-global (x)
  `(,(global-ref x)))

(define (ac-symbol x)
  (let ((r (assq x (replace-var))))
    (if r (cadr r)
          (if (lex? x)
              x
              (ac-symbol-global x)))))


;=============================================================================
;  Types
;=============================================================================
(struct tagged (type rep)
  #:constructor-name make-tagged
  ;#:constructor-name annotate
  ;; TODO: make mutable later, maybe
  ;#:mutable
  #|#:property prop:custom-write
             (lambda (x port mode)
               (display "#(tagged " port)
               (display (type x) port)
               (display " " port)
               (display (rep x) port)
               (display ")" port))|#
  #|#:guard (lambda (type rep name)
            (values type rep)
            (values type rep))|#
  )

(define (iround x) (inexact->exact (round x)))

(define (wrapnil f)  (lambda args (apply f args) nil))
(define (wraptnil f) (lambda (x)  (tnil (f x))))


;=============================================================================
;  Initialization and loading
;=============================================================================
(define (init (dir (current-directory)))
  ;(xset exec-dir* dir) ;(path->string )
  ;(reset exec-dir* dir)
  (set! exec-dir* dir)
  ;; TODO: why does Arc 3.1 do this?
  (putenv "TZ" ":GMT")
  ;; TODO: why is this in Arc 3.1?
  ;(print-hash-table #t)
  (current-readtable arc3-readtable)
  (ac-load-all dir)
  ;(prn (namespace-mapped-symbols arc3-namespace))
  )

(define (repl)
  (aload (build-path exec-dir* "repl.arc")))

(define (aload filename)
  (call-with-input-file filename aload1))

(define (aload1 p)
  (let ((x (read p)))
    (if (eof-object? x)
        #t ;; TODO: should probably be (void)
        (begin (ac-eval x)
               (aload1 p)))))


(mdef ac-load-all (dir)
  ;(aload (build-path dir "02 core.arc"))
  (aload (build-path dir "arc.arc"))
  ;(aload (build-path dir "03 import.arc"))
  ;(aload (build-path dir "04 extra.arc"))
  ;(aload (build-path dir "libs.arc"))
  ;(aload (build-path dir "lib/repl.arc"))
  ;(ac-eval '(importfn1 "repl"))
  )


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

;; convert #f from Racket to nil
(define (ac-nil x)
  (if (eq? x #f) ;(or (eq? x '()) )
      nil
      x))

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
; compile a function call
; ref uses case-lambda so it's *very fast* in the common cases of 0-6
;   arguments
;
; and for (foo bar), if foo is a reference to a global variable,
;   and it's bound to a function, generate (foo bar) instead of
;   (ref foo bar)
(define direct-calls #f)

(define (ac-call f args)
  (let ((macfn (macro? f)))
          ; the next three clauses could be removed without changing semantics
          ; ... except that they work for macros (so prob should do this for
          ; every elt of s, not just the car)
          ;; TODO: don't hardcode the symbols
    (cond ((caris f 'compose)
            ;(displayln (de-compose (cdr f) args))
            (ac (de-compose (cdr f) args)))
          ((caris f 'complement)
            (ac (list 'no (cons (cadr f) args))))
          ((caris f 'andf)
            (de-andf f args))
          (macfn
            (mac-call macfn args))
          ; (foo bar) where foo is a global variable bound to a procedure.
          ; this breaks if you redefine foo to be a non-fn (like a hash table)
          ; but as long as you don't redefine anything, it's faster
          ((and direct-calls
                (symbol? f)
                (not (lex? f))
                ;(bound? f)
                (procedure? (var f)))
            ;; TODO
            ;`(,(global-name f) ,@(ac-all args))
            `(,(ac f) ,@(ac-all args)))
          (else
            (let ((f (ac f)))
              (if (or (procedure? f)
                      (caris f 'lambda))
                  `(,f ,@(ac-all args))
                  `(ref ,f ,@(ac-all args)))))))) ;,(return-apply args)

; The next two are optimizations, except work for macros.
(define (de-compose fns args)
  (displayln fns)
  (cond ((null? fns) `((fn vals (car vals)) ,@args))
        ((null? (cdr fns)) (cons (car fns) args))
        (else (list (car fns) (de-compose (cdr fns) args)))))

(define (de-andf f args)
  (ac (let ((gs (map (lambda (x) (gensym)) args)))
         `((fn ,gs
             (and ,@(map (lambda (f) `(,f ,@gs))
                         (cdr f))))
           ,@args))))

; returns #f or the macro function
(define (macro? f)
  (cond ((and (symbol? f)
              (not (lex? f)))
          (macro? (var f)))
        ((isa f 'mac)
          (rep f))
        (else #f)))

(define (mac-call m args)
  ;(prn m args)
  ;(prn (apply m args))
  ;; TODO: (apply ac-apply ...)
  (ac (apply m args)))

#|(define (return-apply x)
  (let loop ((x x) (n 0))
    (cond ((> n 4)
            ac-apply)
          ((null? x)
            (case n
              ((0) funcall0)
              ((1) funcall1)
              ((2) funcall2)
              ((3) funcall3)
              ((4) funcall4)))
          (else
            (loop (cdr x) (+ n 1))))))|#

;; TODO: define ac-apply as a case-lambda ...?
#|(define (ac-apply f . args)
  (if (procedure? f)
      (apply f args)
      (apply ref f args)))|#

#|(define ac-apply
  (case-lambda
    ((f)              (if (procedure? f)
                          (f)
                          (ref f)))
    ((f a1)           (if (procedure? f)
                          (f a1)
                          (ref f a1)))
    ((f a1 a2)        (if (procedure? f)
                          (f a1 a2)
                          (ref f a1 a2)))
    ((f a1 a2 a3)     (if (procedure? f)
                          (f a1 a2 a3)
                          (ref f a1 a2 a3)))
    ((f a1 a2 a3 a4)  (if (procedure? f)
                          (f a1 a2 a3 a4)
                          (ref f a1 a2 a3 a4)))
    ((f . args)       (if (procedure? f)
                          (apply f args)
                          (apply ref f args)))))

; special cases of ac-apply for speed and to avoid consing arg lists
(define (funcall0 f)
  (if (procedure? f)
      (f)
      (ref f)))

(define (funcall1 f arg1)
  (if (procedure? f)
      (f arg1)
      (ref f arg1)))

(define (funcall2 f arg1 arg2)
  (if (procedure? f)
      (f arg1 arg2)
      (ref f arg1 arg2)))

(define (funcall3 f arg1 arg2 arg3)
  (if (procedure? f)
      (f arg1 arg2 arg3)
      (ref f arg1 arg2 arg3)))

(define (funcall4 f arg1 arg2 arg3 arg4)
  (if (procedure? f)
      (f arg1 arg2 arg3 arg4)
      (ref f arg1 arg2 arg3 arg4)))|#

#|(define call (case-lambda
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
               ((x . args)       (begin (prn "warning: called with 7+ arguments:" x args)
                                        (if (procedure? x)
                                            (apply x args)
                                            (apply ref x args))))))|#

(define (arg-list* args)
  (if (null? (cdr args))
      (car args)
      (cons (car args)
            (arg-list* (cdr args)))))


;=============================================================================
;  Binaries
;=============================================================================
; (pairwise pred '(a b c d)) =>
;   (and (pred a b) (pred b c) (pred c d))
; pred returns t/nil, as does pairwise
; reduce?
(define (pairwise pred lst)
  (cond ((null? lst)       t)
        ((null? (cdr lst)) t)
        ((true? (pred (car lst) (cadr lst)))
          (pairwise pred (cdr lst)))
        (else nil)))

;; Arc's reduce. Can't use foldl because it doesn't work well with multiple
;; types (e.g. +-2)
(define (reduce f xs)
  (cond ((null? xs)
          xs)
        ((null? (cdr xs))
          (f (car xs)))
        ((null? (cddr xs))
          (f (car xs) (cadr xs)))
        (else
          (reduce f (cons (f (car xs) (cadr xs)) (cddr xs))))))

;; creates a function that when given 2 arguments will be very fast, but still
;; works when given any number of arguments
;;
;; this is so you can say (binary is-2 pairwise) to create a fast multi-arg
;; version of is-2, etc.
(define (binary f reduce)
  (case-lambda
    ((x y) (f x y))
    (args  (reduce f args))))

; not quite right, because behavior of underlying eqv unspecified
; in many cases according to r5rs
; do we really want is to ret t for distinct strings?
(define (is-2 a b)
  (tnil (or (eqv? a b)
            (and (string? a) (string? b) (string=? a b))
            ;; TODO: why is this here in Arc 3.1?
            ;(and (false? a) (false? b))
            )))

#|(define (char-or-string? x) (or (string? x) (char? x)))|#

; generic +: strings, lists, numbers.
; return val has same type as first argument.
(define (+-2 x y)
  ;(prn (append x y))
  (cond ((number? x)  (+ x y))
        ((string? x)  (string-append x (coerce y 'string)))
        ((list? x)    (append x y))
        ;; TODO: check the behavior of Arc 3.1 for (+ "foo" #\a) and (+ #\a "foo")
        ((char? x)    (string-append (string x) (coerce y 'string)))
        (else         (+ x y)
                             #;(err "can't + " x " with " y))))

; generic comparison
(define (<-2 x y)
  (tnil (cond ((number? x)  (< x y))
              ((string? x)  (string<? x y))
              ((symbol? x)  (string<? (symbol->string x)
                                      (symbol->string y)))
              ((char? x)    (char<? x y))
              (else         (< x y)))))

;; TODO: ew code duplication
(define (>-2 x y)
  (tnil (cond ((number? x)  (> x y))
              ((string? x)  (string>? x y))
              ((symbol? x)  (string>? (symbol->string x)
                                      (symbol->string y)))
              ((char? x)    (char>? x y))
              (else         (> x y)))))


;=============================================================================
;  I/O
;=============================================================================
(define explicit-flush #f)

(define (print f x port)
        ;; TODO: should probably use (no x) or whatever
  (cond ((null? x)    (display "nil" port))
        ((isa x 'fn)  (print-w/name x "#<fn" ":" ">" port))
        ((isa x 'mac) (print-w/name x "#<mac" ":" ">" port))
        ((tagged? x)  (display "#(tagged " port)
                      (print f (type x) port)
                      (display " " port)
                      (print f (rep x)  port)
                      (display ")" port))
        (else         (f x port)))
  nil)

(define (name x)
  (or (hash-ref names x #f)
      (object-name x)
      nil))

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
;  readtable
;=============================================================================
(define (read-square-brackets ch port src line col pos)
  ;; TODO: square-bracket macro
  `(fn (_) ,(read/recursive port #\[ #f)))

(define arc3-readtable
  (make-readtable #f #\[ 'terminating-macro read-square-brackets))


;=============================================================================
;  assign
;=============================================================================
; (assign v1 expr1 v2 expr2 ...)
(define (ac-assign x)
  (let ((x (pairfn ac-assign1 x)))
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

(define (ac-assign1 a b1)
  (if (symbol? a)
      (if (lex? a)  `(set! ,a ,(ac b1))
                    #|(if (true? (bound a))
                        (list (global-name a) (ac b1))
                        )|#
                    `(assign-global-raw ,(namespace) ',a ,(ac b1))
                    #|(ac `(,(cons nocompile 'assign-global-raw)
                          ,(namespace)
                          ',a
                          ,b1))|#
                    )
      (err "first arg to assign must be a symbol" a)))

(define (assign-global-new space name val)
  ;(nameit name val)
  (sref space (make-global-var val) name))

(define (assign-global-raw space name val)
  (nameit name val)
  (let ((v (ref space name fail)))
    ;(prn v)
    ;(prn space v (var-raw name fail))
    (if (eq? v fail)
        (assign-global-new space name val)
        (v val)))
  ;(prn name val)
  #|(let ((v (ref space name fail)))
    (if (eq? v fail)

        ;; TODO: this should call the function later
        ;(sref space val name)
        (prn (v val))
        ))|#
  val)


#|(define (assign-all name space)
  (alet x (rep space)
    (when x
      (w/uniq (c u)
        (sref (car x) (eval #`(let c u ;; ',u
                                (alias
                                  ()  (if (is c u)
                                          name
                                          c)
                                  (u) (= c u)))
                            (cadr x)) name))
      (self (cdr x)))))

(withs (u  (uniq)
        v  u)
  (make-alias
    ()  (if (is v u) foo v)
    (x) (= v x)))


(extend %.ac-symbol (x) (isa namespace 'namespace)
  (if (lex? x)
      (orig x)
      (bound x)
      #`(,(global-name x))
      #`(do (assign-all x ,namespace)
            (,(global-name x)))))|#


;=============================================================================
;  if
;=============================================================================
; (if) -> nil
; (if x) -> x
; (if t a ...) -> a
; (if nil a b) -> b
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
  (cond ((null? x) x)        ;; end of the argument list
        ((symbol? x)         ;; dotted rest args
          (let ((x (fn-gensym x)))
            (cons-to local-env x)
            x))
        ((and (fn-gensym-args)
              (caris (car x) 'quote)) ;; anaphoric arg
          (cons-to local-env (cadar x))
          (prn (cadar x))
          (cons (cadar x) (fn-args (cdr x)))
          )
        ((caris (car x) 'o)  ;; optional arg
          (let* ((c  (car x))
                 (n  (fn-gensym (cadr c)))
                 (d  (cddr c)))
            (cons-to local-env n)
                          ;; TODO: code duplication with ac-fn
            (cons (cons n (if (null? d)
                              (list 'nil) ;; TODO: nil or 'nil ?
                              (ac-all d)))
                  (fn-args (cdr x)))))
        ((pair? (car x))     ;; destructuring args
          (let ((u (gensym)))
            (append-to fn-let* (fn-destructuring u (car x)))
            (cons u (fn-args (cdr x)))))
        (else                ;; normal args
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
  (cond ((null? x) x)        ;; end of the argument list
        ((symbol? x)         ;; dotted rest args
          (let ((x (fn-gensym x)))
            (cons-to local-env x)
            (list (list x u))))
        ((caris (car x) 'o)  ;; optional args
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
        ((pair? (car x))     ;; destructuring args
          (let ((v (gensym)))
            ;(displayln (fn-destructuring v (car x)))
            (cons (list v (ac `(car ,(cons nocompile u))))
                  (append (fn-destructuring v (car x))
                          (fn-destructuring-next u x))
                   #|(if (null? (cdr x))
                       null
                       (list (list u (list cdr u))))
                   (fn-destructuring u (cdr x))|#
                   )))
        (else                ;; normal args
          (let ((n (fn-gensym (car x))))
            (cons-to local-env n)
            (cons (list n (ac `(car ,(cons nocompile u))))
                  (fn-destructuring-next u x))))))

(define (fn-destructuring-next u x)
  (if (null? (cdr x))
      null
      (cons (list u (ac `(cdr ,(cons nocompile u))))
            (fn-destructuring u (cdr x)))))


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
;  Arc functions used by the compiler
;=============================================================================
;; TODO: a better argument name than typ
;; TODO: annotate doesn't need to be mutable, but does need to be exposed to
;;       both the compiler and Arc
(mdef annotate (typ rep)
      ;; TODO: does this need to eqv? rather than eq?
  (if (eqv? (type rep) typ)
      rep
      (make-tagged typ rep)))

(mdef close args
  (map close1 args)
  (map (lambda (p) (try-custodian p)) args) ; free any custodian
  nil)

(mdef close1 (p)
  (cond ((input-port? p)    (close-input-port p))
        ((output-port? p)   (close-output-port p))
        ((tcp-listener? p)  (tcp-close p))
        (else               (err "can't close " p))))

(mdef err (x . rest)
  (apply error x rest))

; macroexpand the outer call of a form as much as possible
(mdef macex (e)
  (let ((v (macex1 e)))
    (if (eq? v e)
        v
        (macex v))))

; macroexpand the outer call of a form once
(mdef macex1 (e)
  (if (pair? e)
      (let ((m (macro? (car e))))
        (if m  (apply m (cdr e))
               e))
      e))

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

; Later may want to have multiple indices.
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
  (cond ((tagged? x)        (tagged-type x))
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
        ;((namespace? x)     'namespace)
        ((output-port? x)   'output)
        ((input-port? x)    'input)
        ((tcp-listener? x)  'socket)
        ((exn? x)           'exception)
        ((thread? x)        'thread)
        (else               ;(err "type: unknown type" x)
                            ;(prn "warning: unknown type" x)
                            ;'unknown
                            nil)))


;; TODO: should pipe call ((caddddr x) 'wait)?
(mdef pipe (cmd)
        ;; TODO: destructuring
  (let* ((x   (process/ports #f #f (current-error-port) cmd))
         (in  (car x))
         (out (cadr x)))
    (list in out)))


;=============================================================================
;  Compiler stuff exposed to Arc
;=============================================================================
(define defref-types*   (make-hash))
(define exec-dir*       (current-directory))
(define nil             null)
(define t               't)
(define uniq-counter    (make-parameter 1))

(define (ac-car x)
  (if (null? x)
      x
      (car x)))

(define (ac-cdr x)
  (if (null? x)
      x
      (cdr x)))

;; TODO: list + table of types for coerce
(define (coerce x to (base 10))
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

(define (ac-eval expr)
  (eval (ac expr))
  #|(eval (ac expr) (if (namespace? (namespace))
                      (namespace)
                      (current-namespace)))|#
  )

;; (if ...)
(define ac-if (annotate 'mac (lambda args
                               (cons nocompile (ac-ifn args)))))

; call a function or perform an array ref, hash ref, &c
;
; Non-fn constants in functional position are valuable real estate, so
; should figure out the best way to exploit it.  What could (1 foo) or
; ('a foo) mean?  Maybe it should mean currying.
;
; For now the way to make the default val of a hash table be other than
; nil is to supply the val when doing the lookup.  Later may also let
; defaults be supplied as an arg to table.  To implement this, need: an
; eq table within scheme mapping tables to defaults, and to adapt the
; code in arc.arc that reads and writes tables to read and write their
; default vals with them.  To make compatible with existing written tables,
; just use an atom or 3-elt list to keep the default.
;
; experiment: means e.g. [1] is a constant fn
;       ((or (number? fn) (symbol? fn)) fn)
; another possibility: constant in functional pos means it gets
; passed to the first arg, i.e. ('kids item) means (item 'kids).

;; TODO: wild idea: get rid of funcall* and ac-apply and just have *all*
;;       function calls use ref. If I defined special cases in the case-lambda
;;       for 3 and 4 args, it *might* be just as fast as funcall*, but I
;;       should test the speed before actually doing it. don't forget to move
;;       the tests for procedure? so they're first in the cond, that way it'll
;;       be as fast as possible
(define ref
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
    ((x)              (if (procedure? x)
                          (x)
                          (let ((v (hash-ref defref-types* (type x) nil)))
                            (if (true? v)
                                (v x)
                                (err "function call on inappropriate object" x)))))
    ((x k)            (if (procedure? x)
                          (x k)
                          (let ((v (hash-ref defref-types* (type x) nil)))
                            (cond ((true? v)       (v x k))
                                  ((namespace? x)  (namespace-variable-value k #f (lambda () nil) x)) ;(global-name k)
                                  ((hash? x)       (hash-ref x k nil))
                                  ((string? x)     (string-ref x k))
                                  ((pair? x)       (list-ref x k))
                                  (else (err "function call on inappropriate object" x k))))))
    ((x k d)          (if (procedure? x)
                          (x k d)
                          (let ((v (hash-ref defref-types* (type x) nil)))
                            (cond ((true? v)       (v x k d))
                                  ((namespace? x)  (namespace-variable-value k #f ;(global-name k)
                                                     (if (procedure? d) d (lambda () d))
                                                     x))
                                  ((hash? x)       (hash-ref x k d))
                                  (else (err "function call on inappropriate object" x k d))))))
    ((x a b c)        (if (procedure? x)
                          (x a b c)
                          (let ((v (hash-ref defref-types* (type x) nil)))
                            (if (true? v)
                                (v x a b c)
                                (err "function call on inappropriate object" x a b c)))))
    ((x a b c d)      (if (procedure? x)
                          (x a b c d)
                          (let ((v (hash-ref defref-types* (type x) nil)))
                            (if (true? v)
                                (v x a b c d)
                                (err "function call on inappropriate object" x a b c d)))))
    ((x a b c d e)    (if (procedure? x)
                          (x a b c d e)
                          (let ((v (hash-ref defref-types* (type x) nil)))
                            (if (true? v)
                                (v x a b c d e)
                                (err "function call on inappropriate object" x a b c d e)))))
    ((x a b c d e f)  (if (procedure? x)
                          (x a b c d e f)
                          (let ((v (hash-ref defref-types* (type x) nil)))
                            (if (true? v)
                                (v x a b c d e f)
                                (err "function call on inappropriate object" x a b c d e f)))))
    ((x . args)       (begin ;(prn "warning: called with 7+ arguments:" x args)
                             (if (procedure? x)
                                 (apply x args)
                                 (let ((v (hash-ref defref-types* (type x) nil)))
                                   (if (true? v)
                                       (apply v x args)
                                       (apply err "function call on inappropriate object" x args))))))))


(define dref
  (case-lambda
    ((n)    (if (namespace? (namespace))
                (let ((x (ref (namespace) n fail)))
                  (if (eq? x fail)
                      nil
                      (begin (namespace-undefine-variable! n (namespace))
                             x)))
                (err "can't delete reference" n)))
    ((x k)  (err "can't delete reference" x k))))



#|(define ref
  (case-lambda
    ((x k)       (if (procedure? x)
                     (x k)
                     (let ((v (hash-ref defref-types* (type x) nil)))
                       (cond ((true? v)       (v x k))
                             ((namespace? x)  (namespace-variable-value (global-name k) #f (lambda () nil) x))
                             ((hash? x)       (hash-ref x k nil))
                             ((string? x)     (string-ref x k))
                             ((pair? x)       (list-ref x k))
                             (else (err "function call on inappropriate object" x k))))))
    ((x k d)     (if (procedure? x)
                     (x k d)
                     (let ((v (hash-ref defref-types* (type x) nil)))
                       (cond ((true? v)       (v x k d))
                             ((namespace? x)  (namespace-variable-value (global-name k) #f
                                                (if (procedure? d) d (lambda () d))
                                                x))
                             ((hash? x)       (hash-ref x k d))
                             (else (err "function call on inappropriate object" x k d))))))
    ((x . args)  (if (procedure? x)
                     (apply x args)
                     (let ((v (hash-ref defref-types* (type x) nil)))
                       (if (true? v)
                           (apply v x args)
                           (apply err "function call on inappropriate object" x args)))))))|#



#|(require (for-syntax racket/base))

(define-syntax (make-ref stx)
  (define (tolist x)
    (cond ((pair? x)  (cons (car x) (tolist (cdr x))))
          ((null? x)  x)
          (else       (list x))))

  #|(define (make-cond c d (trans #f))
    #`(#,c (if (procedure? #,(car c))
               #,(check trans c)
               (let ((#,u (hash-ref defref-types* (type #,(car c)) #f)))
                 (cond (#,u #,(check trans (cons u c)))
                       #,@d
                       (else (err "function call on inappropriate object" #,@(if trans (tolist c) c))))))))|#

  (define (make-cond c d (trans #f))
    #`(#,c #,(lambda-body c d (if trans (lambda (x) (trans (tolist x)))
                                        (lambda (x) x)))))

  (define u (gensym))

  (define (lambda-body x rest check)
    #`(if (procedure? #,(car x))
          #,(check x)
          ;; TODO: maybe should use nil and true?
          ;; TODO: how slow is it to put defref first...?
          (let ((#,u (hash-ref defref-types* (type #,(car x)) #f)))
            (cond (#,u #,(check (cons u x)))
                  #,@rest
                  (else #,(check `(err "function call on inappropriate object" ,@x)))))))

  ;(define vars (list (gensym)))

  (define (loop-until n to)
    (let loop ((n    (+ n 1))
               (acc  null))
      (if (< n to)
          (loop (+ n 1)
                (cons (make-cond #|(begin (set! vars (cons (gensym) vars))
                                        vars)|#
                                 (build-list n (lambda (x) (gensym)))
                                 null)
                      acc))
          (list (reverse acc) n))))

  (syntax-case stx ()
    ((_ limit . body) #`(case-lambda
                          #,@(let ((x
                             (let next ((x  (map syntax->list (syntax->list #'body)))
                                        (n  0)) ;(map syntax->list (syntax->list x))
                               (if (null? x)
                                   (append (car (loop-until n (+ 2 (syntax->datum #'limit))))
                                           (list (make-cond (cons (gensym) (gensym))
                                                            null
                                                            (lambda (x) (cons 'apply x)))))
                                   ;(let-values (((xs n) (loop-until n (+ 2 (syntax->datum #'limit))))) xs)
                                   (let* ((c  (syntax->list (car (car x))))
                                          (v  (loop-until n (length c))))
                                     (append (car v)
                                             (cons (make-cond c (cdr (car x)))
                                                   (next (cdr x) (cadr v)))))))
                              ))
                              ;(displayln (length (tolist x)))
                              (displayln (map syntax->datum x))
                              ;(displayln (syntax->datum ))
                              x)))))

#|(def make-cond (c d)
  (w/uniq u
    `(,c (if (fn? ,car.c)
             ,c
             (let ,u (defref-types* (type ,car.c))
               (if ,u
                   (,u ,@c)
                   ,@d
                   (err "function call on inappropriate object" ,@c)))))))

(mac make-ref body
  `(case-lambda ,@(awith (x  body
                          n  0)
                    (if (no x)
                        x
                        (let c (caar x)
                          (join (collect:while (< n len.c)
                                  (yield:make-cond (n-of (1+ n) (uniq)) nil)
                                  (++ n))
                                (cons (make-cond c cdar.x)
                                      (self cdr.x (1+ n)))))))))|#

(define ref
  (make-ref 10
    ((x k)    ((namespace? x)  (namespace-variable-value k #f (lambda () nil) x))
              ((hash? x)       (hash-ref x k nil))
              ((string? x)     (string-ref x k))
              ((pair? x)       (list-ref x k)))
    ((x k d)  ((namespace? x)  (namespace-variable-value k #f
                                 (if (procedure? d) d (lambda () d))
                                 x))
              ((hash? x)       (hash-ref x k d)))))|#


;; (quote ...)
(define ac-quote (annotate 'mac (lambda (x)
                                      ;; TODO: a little hacky
                                  (if (eq? x 'nil)
                                      nil
                                      (list (lambda () x))))))

;; globals
(sset arc3-namespace                   arc3-namespace)
(mset defref-types*                    defref-types*)
(mset exec-dir*                        exec-dir*)
(set  'namespace                       namespace)
(mset nil                              nil)
(mset sig                              sig)
(mset t                                t)
(set  'uniq-counter                    uniq-counter)

;; functions
(sset -        args                    -)
;; car and cdr probably will be used later, but not right now
(mset car      (x)                     ac-car)
(mset cdr      (x)                     ac-cdr)
(mset coerce   (x to (o base 10))      coerce)
(sset cons     (x y)                   cons) ;; TODO: look for some uses of cons and replace em with ac-cons
(mset dref     (x (o k))               dref)
(mset eval     (x)                     ac-eval)
(mset if       args                    ac-if)
(sset instring (str (o name 'string))  open-input-string)
(mset ref      (x . args)              ref)
(mset quote    (x)                     ac-quote)
(sset seconds  ()                      current-seconds)


;=============================================================================
;  Arc parameters
;=============================================================================
(sset stdout ((o out))  current-output-port)  ; should be a vars
(sset stdin  ((o in))   current-input-port)
(sset stderr ((o err))  current-error-port)


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
(sset +  args (binary +-2  reduce)) ;(lambda (f args) (foldl f 0 args)) ;; TODO: kinda hacky
(sset >  args (binary >-2  pairwise))
(sset <  args (binary <-2  pairwise))
(sset is args (binary is-2 pairwise))

;; wrapnil
(sset rmfile (path)      (wrapnil delete-file))
(sset sleep  ((o sec 0)) (wrapnil sleep))
;; Will system "execute" a half-finished string if thread killed in the
;; middle of generating it?
(sset system (command)   (wrapnil system))

;; TODO: sdef
(define (bound? x)
  (not (eq? (var-raw x fail) fail)))

;; wraptnil
(sset bound       (x) (wraptnil bound?))
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

;; macros
(sset assign args
  (annotate 'mac (lambda args
                   (cons nocompile (ac-assign args)))))

(sset fn (parms . body)
  (annotate 'mac (lambda (parms . body)
                   (cons nocompile (ac-fn parms body)))))

(sset quasiquote (x)
  (annotate 'mac (lambda (x)
                   (qq-expand x))))

(sset % args
  (annotate 'mac (lambda args
                   (cons nocompile
                         (if (null? (cdr args))
                             (car args)
                             (cons 'begin args))))))

;; functions
(sdef apply (f . args)
  ;(apply apply ref f args)
  (apply ref f (arg-list* args))) ;ac-apply
;(sset apply (f . args)
;  (lambda (f . args)

(sdef len (x)
  (cond ((string? x) (string-length x))
        ((hash? x)   (hash-count x))
        (else        (length x))))

                 ;; TODO check this
(sdef outfile (f (mode 'truncate))
        #:sig (f (o mode 'truncate))
  (open-output-file f 'text mode))

(sdef call-w/stdout (port thunk)
  (parameterize ((current-output-port port)) (thunk)))

(sdef call-w/stdin (port thunk)
  (parameterize ((current-input-port port)) (thunk)))

; sread = scheme read. eventually replace by writing read
(sdef sread (p eof)
  (let ((expr (read p)))
    (if (eof-object? expr) eof expr)))


(sdef open-socket (num)
  (tcp-listen num 50 #t))

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

; Racket provides eq? eqv? and equal? hash tables
; we need equal? for strings
(sdef table ((init nil))
      #:sig ((o init))
  (let ((h (make-hash)))
    (when (true? init)
      (init h))
    h))

(sdef maptable (fn table)
  (hash-for-each table fn) ; arg is (fn (key value) ...)
  table)

;; TODO: better dir
(sdef dir (name)
  (map path->string (directory-list name)))

;; TODO: mkdir with make-directory*
; Would def mkdir in terms of make-directory and call that instead
; of system in ensure-dir, but make-directory is too weak: it doesn't
; create intermediate directories like mkdir -p.

(sdef mvfile (old new (flag t))
       #:sig (old new (o flag t))
  (rename-file-or-directory old new (true? flag))
  nil)

(sset details (e) exn-message)
               ;; TODO: why does this use disp-to-string...?
  ;(lambda (e) (disp-to-string (exn-message e)))

(sdef trunc (x)
  (inexact->exact (truncate x)))

(sdef client-ip (port)
  (let-values (((x y) (tcp-addresses port)))
              y))

;; TODO: make this better
(sdef atomic-invoke (f)
  (if (thread-cell-ref sema-cell)
      ;; TODO: why are these ref...?
      (ref f)
      (begin (thread-cell-set! sema-cell #t)
             (protect (lambda ()
                        (call-with-semaphore
                          the-sema
                          (lambda () (ref f))))
                      (lambda ()
                        (thread-cell-set! sema-cell #f))))))

; Added because Mzscheme buffers output.  Not a permanent part of Arc.
; Only need to use when declare explicit-flush optimization.
(sdef flushout ()
  (flush-output)
  t)

(sdef ssexpand (x)
  (if (symbol? x) (ssexpand x) x))

;; TODO: force-close1
(sdef force-close args
  (map (lambda (p)
         (when (not (try-custodian p))
           (close p)))
       args)
  nil)

(sdef declare (key val)
  (let ((flag (true? val)))
    (case key
      ((atstrings)      (set! atstrings      flag))
      ((direct-calls)   (set! direct-calls   flag))
      ((explicit-flush) (set! explicit-flush flag)))
    val))

(sdef timedate ((sec (current-seconds)))
         #:sig ((o sec (seconds)))
  (let ((d (seconds->date sec)))
    (list (date-second d)
          (date-minute d)
          (date-hour d)
          (date-day d)
          (date-month d)
          (date-year d))))

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

(sdef scdr (p x)
  (cond ((pair? p)   (unsafe-set-mcdr! p x))
        ((string? x) (err "can't set cdr of a string" x))
        (else        (raise-type-error 'scdr "pair" p)))
  x)
