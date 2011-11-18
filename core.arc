;=============================================================================
;  Core
;=============================================================================

(assign do (annotate 'mac
             (fn args `((fn () ,@args)))))

(assign safeset (annotate 'mac
                  (fn (var val)
                    `(do (if (bound ',var)
                             (do (disp "*** redefining " (stderr))
                                 (disp ',var (stderr))
                                 (disp #\newline (stderr))))
                         (assign ,var ,val)))))

(assign def (annotate 'mac
               (fn (name parms . body)
                 `(do (sref sig ',parms ',name)
                      (safeset ,name (fn ,parms ,@body))))))

(def no (x) (is x nil))

(def pair (xs (o f list))
  (if (no xs)
       nil
      (no (cdr xs))
       (list (list (car xs)))
      (cons (f (car xs) (cadr xs))
            (pair (cddr xs) f))))

(assign mac (annotate 'mac
              (fn (name parms . body)
                `(do (sref sig ',parms ',name)
                     (safeset ,name (annotate 'mac (fn ,parms ,@body)))))))


(mac with (parms . body)
  `((fn ,(map1 car (pair parms))
     ,@body)
    ,@(map1 cadr (pair parms))))

(mac let (var val . body)
  `(with (,var ,val) ,@body))


(mac = args `(assign ,@args))

(mac remac (name parms . body)
  `(let orig (rep ,name)
     (sref sig ',parms ',name)
     (= ,name (annotate 'mac (fn ,parms ,@body)))))

(mac redef (name args . body)
  `(let orig ,name
     (sref sig ',args ',name)
     (= ,name (fn ,args ,@body))))


; Need rfn for use in macro expansions.
(mac rfn (name parms . body)
  `(let ,name nil
     (= ,name (fn ,parms ,@body))))

(mac afn (parms . body)
  `(rfn self ,parms ,@body))

; Would like to write a faster case based on table generated by a macro,
; but can't insert objects into expansions in Mzscheme.
(mac caselet (var expr . args)
  (let ex (afn (args)
            (if (no (cdr args))
                (car args)
                `(if (is ,var ',(car args))
                     ,(cadr args)
                     ,(self (cddr args)))))
    `(let ,var ,expr ,(ex args))))

(mac case (expr . args)
  `(caselet ,(uniq) ,expr ,@args))


; Ac expands x:y:z into (compose x y z), ~x into (complement x)
; Only used when the call to compose doesn't occur in functional position.
; Composes in functional position are transformed away by ac.
(mac compose args
  (let g (uniq)
    `(fn ,g
       ,((afn (fs)
           (if (cdr fs)
               (list (car fs) (self (cdr fs)))
               `(apply ,(if (car fs) (car fs) 'idfn) ,g)))
         args))))

; Ditto: complement in functional position optimized by ac.
(mac complement (f)
  (let g (uniq)
    `(fn ,g (no (apply ,f ,g)))))


;=============================================================================
;  ac
;=============================================================================

(def ac-make-read (f)
  (fn ((o port (stdin)) (o eof))
    (let c (f port)
      (if (racket-eof-object? c) eof c))))

(def ac-make-write (f)
  (fn ((o port (stdout))) (f port)))

(= ac-the-sema  (racket-make-semaphore 1)
   ac-sema-cell (racket-make-thread-cell #f))

(mac ac-notimpl (x)
  `(def ,x ,(uniq)
     (err (string ',x " is not implemented"))))


;=============================================================================
;  Multi-value functions
;=============================================================================

#|(mac let-values (x y . body)
  `(%nocompile (racket-let-values ((,x (%compile ,y)))
                 (%compile ,@body))))|#

(mac values (vars expr . body)
  `(racket-call-with-values (fn () ,expr) (fn ,vars ,@body)))


;=============================================================================
;  square-bracket/curly-bracket
;=============================================================================

(def ac-readtable-square-bracket (readtable)
  (racket-make-readtable readtable #\[ 'terminating-macro
    (fn (ch port src line col pos)
      (racket-cons 'square-bracket (racket-read/recursive port #\[ #f)))))

(def ac-readtable-curly-bracket (readtable)
  (racket-make-readtable readtable #\{ 'terminating-macro
    (fn (ch port src line col pos)
      (racket-cons 'curly-bracket (racket-read/recursive port #\{ #f)))))

;; TODO: should the old readtable be stored somewhere...?
(racket-current-readtable
  (ac-readtable-square-bracket
    (ac-readtable-curly-bracket #f)))

(mac square-bracket body
  `(fn ((o _)) (,@body)))


;=============================================================================
;  Types
;=============================================================================

(def isa      (x y) (is (type x) y))
;; TODO: macro to generate these easier
(def cons?    (x)   (isa x 'cons))
(def int?     (x)   (isa x 'int))
(def string?  (x)   (isa x 'string))
(def sym?     (x)   (isa x 'sym))
(def char?    (x)   (isa x 'char))
(def fn?      (x)   (isa x 'fn))
(def mac?     (x)   (isa x 'mac))
(def keyword? (x)   (isa x 'keyword))


(def testify (x)
  (if (fn? x) x [is _ x]))


(def string1 (x)
  (if (string? x)
        x
      (char? x)
        (racket-string x)
      (cons? x)
        (apply racket-string-append (map1 string x))
      (no x)
        ""
      (num? x)
        (racket-number->string x)
      (sym? x)
        (racket-symbol->string x)))

(def string args
  (apply racket-string-append (map1 string1 args)))


(def sym1 (x)
  (if (sym? x)
        x
      (char? x)
        (racket-string->symbol (racket-string x))
      (string? x)
        (racket-string->symbol x)))

(def sym args
  (sym1 (apply string args)))


(def ac-iround (x)
  (racket-inexact->exact (racket-round x)))

(def int (x (o base 10))
  (if (int? x)
        x
      (char? x)
        (racket-char->integer x)
      (num? x)
        (ac-iround x)
      (string? x)
        ;; TODO: can this be a simple `or` like in `num`...?
        (let n (racket-string->number x base)
          (if n  (ac-iround n)
                 (err "Can't coerce" x 'string)))))

(def char (x)
  (if (char? x)
        x
      (int? x)
        (racket-integer->char x)
      (num? x)
        (racket-integer->char (int x))))

(def num (x (o base 10))
  (if (int? x)
        (+ 0.0 x)
      (num? x)
        x
      (string? x)
        (or (racket-string->number x base)
            (err "Can't coerce" x 'string))))

;; TODO: need to figure out a better way to deal with types
(def coerce (x totype (o base 10))
  (if (ac-tnil (ac-tagged? x))
        (err "Can't coerce annotated object")

      (is totype (type x))  x
      (is totype 'int)      (int x base)
      (is totype 'num)      (num x base)
      (is totype 'char)     (char x)
      (is totype 'string)   (string x)
      (is totype 'sym)      (sym x)
      (is totype 'cons)     (if (string? x)
                                  (racket-list->mlist (racket-string->list x))
                                (no x)
                                  nil)
                            (err "Can't coerce" x type)))


;=============================================================================
;  Variable Binding
;=============================================================================

(mac w/uniq (names . body)
  (if (cons? names)
        `(with ,(ac-mappend (fn (n) (list n '(uniq))) names)
           ,@body)
      `(let ,names (uniq) ,@body)))

(mac aif (expr . body)
  `(let it ,expr
     (if it
         ,@(if (cddr body)
               `(,(car body) (aif ,@(cdr body)))
               body))))


; Destructuring means ambiguity: are pat vars bound in else? (no)
(mac iflet (var expr then . rest)
  (w/uniq gv
    `(let ,gv ,expr
       (if ,gv (let ,var ,gv ,then) ,@rest))))

(mac whenlet (var expr . body)
  `(iflet ,var ,expr (do ,@body)))


;=============================================================================
;  Conditionals
;=============================================================================

(mac when (test . body)
  `(if ,test (do ,@body)))

(mac and args
  (if args
      (if (cdr args)
            `(if ,(car args) (and ,@(cdr args)))
          (car args))
      t))

(mac or args
  (and args
       (w/uniq g
         `(let ,g ,(car args)
            (if ,g ,g (or ,@(cdr args)))))))


(mac in (x . choices)
  (w/uniq g
    `(let ,g ,x
       (or ,@(map1 (fn (c) `(is ,g ,c)) choices)))))


;; TODO: these should be under "Types"
(def num?    (x)   (or (int? x) (isa x 'num)))
(def list?   (x)   (or (no x)   (cons? x)))


;=============================================================================
;  Loops
;=============================================================================

#|(mac while (test . body)
  (w/uniq (gf gp)
    `((rfn ,gf (,gp)
        (when ,gp ,@body (,gf ,test)))
      ,test)))

(mac whilet (var test . body)
  (w/uniq (gf gp)
    `((rfn ,gf (,gp)
        (let ,var ,gp
          (when ,var ,@body (,gf ,test))))
      ,test)))|#


;=============================================================================
;  Backwards Compatibility
;=============================================================================

;(alias acons cons?)
;(alias alist list?)


;=============================================================================
;  Lists
;=============================================================================

(def reduce (f xs)
  (if (cddr xs)
        (reduce f (cons (f (car xs) (cadr xs)) (cddr xs)))
      (apply f xs)))

; Rtm prefers to overload + to do this
(def join args
  (if (no args)
        nil
      (let a (car args)
        (if (no a)
              (apply join (cdr args))
            (cons (car a) (apply join (cdr a) (cdr args)))))))


(def scar (x val)
  (sref x val 0))

(def scdr (x val)
  (racket-set-mcdr! x val)
  val)

(def caris (x val)
  (and (cons? x) (is (car x) val)))

(def caddr (x) (car (cddr x)))


(def rev (xs)
  ((afn (xs acc)
     (if (no xs)
         acc
         (self (cdr xs) (cons (car xs) acc))))
   xs nil))

(def reclist (f xs)
  (and xs (or (f xs) (reclist f (cdr xs)))))

(def mem (test seq)
  (let f (testify test)
    (reclist [if (f (car _)) _] seq)))


;=============================================================================
;  Strings
;=============================================================================

(= newstring  racket-make-string)

(def recstring (test s (o start 0))
  ((afn (i)
     (and (< i (len s))
          (or (test i)
              (self (+ i 1)))))
   start))


;=============================================================================
;  Numbers
;=============================================================================

(def trunc (x)
  (racket-inexact->exact (racket-truncate x)))

; bad name
(def exact (x)
  (ac-tnil (ac-exint? x)))


;=============================================================================
;  Binary functions
;=============================================================================

#|(mac case-fn args
  `(%nocompile
     (racket-case-lambda
       ,@(map1 (fn (x)
                 (let n (car x)
                   `(,n (%compile ,@(if (sym? n)
                                          `((= ,n (racket-list->mlist ,n))
                                            ,@(cdr x))
                                        (cdr x))))))
               args))))|#


(def +-2 ((o x 0) (o y 0))
  (if (num? x)
        (racket-+ x y)
      (or (string? x)
          (char? x))
        (string x y)
      (list? x)
        (join x y)))

(def <2 (x y)
  (ac-tnil
    (if (and (num? x) (num? y))
          (racket-< x y)
        (and (string? x) (string? y))
          (racket-string<? x y)
        (and (sym? x) (sym? y))
          (racket-string<? (sym x) (sym y))
        (and (char? x) (char? y))
          (racket-char<? x y)
        (err "Can't <" x y))))

(def >2 (x y)
  (ar-tnil
    (if (and (num? x) (num? y))
          (racket-> x y))
        (and (string? x) (string? y))
          (racket-string>? x y)
        (and (sym? x) (sym? y))
          (racket-string>? (string x) (string y))
        (and (char? x) (char? y))
          (racket-char>? x y)
        (err "Can't >" x y)))

(= +  (ac-binary +-2 reduce)
   <  (ac-binary  <2 ac-pairwise)
   >  (ac-binary  >2 ac-pairwise))


;=============================================================================
;  Hash tables
;=============================================================================

(def table ((o init))
  (let x (racket-make-hash)
    (if init (init x))
    x))

(def maptable (fn table)               ; arg is (fn (key value) ...)
  (racket-hash-table-for-each table fn)
  table)


;=============================================================================
;  Math
;=============================================================================

(= -     racket--
   *     racket-*
   /     racket-/
   mod   racket-modulo
   expt  racket-expt
   sqrt  racket-sqrt
   sin   racket-sin
   cos   racket-cos
   tan   racket-tan
   asin  racket-asin
   acos  racket-acos
   atan  racket-atan
   log   racket-log)


;=============================================================================
;  Continuations
;=============================================================================

(= ccc  racket-call-with-current-continuation)

(def protect (during after)
  (racket-dynamic-wind (fn () #t) during after))


;=============================================================================
;  I/O
;=============================================================================

(= infile     racket-open-input-file)

(def outfile (filename (o append))
  (let flag (if append 'append 'truncate)
    (racket-open-output-file filename #:mode 'text #:exists flag)))

(= instring   racket-open-input-string
   outstring  racket-open-output-string)

; use as general fn for looking inside things
(= inside     racket-get-output-string)

(def call-w/stdout (port thunk)
  (parameterize ((current-output-port port)) (thunk)))

(def call-w/stdin (port thunk)
  (parameterize ((current-input-port port)) (thunk)))

(= readc  (ac-make-read racket-read-char)
   readb  (ac-make-read racket-read-byte)
   peekc  (ac-make-read racket-peek-char))

(= writec (ac-make-write racket-write-char)
   writeb (ac-make-write racket-write-byte))

#|(def readc ((o port (stdin)) (o eof))
  (let c (racket-read-char port)
    (if (racket-eof-object? c) eof c)))

(def readb ((o port (stdin)) (o eof))
  (let c (racket-read-byte port)
    (if (racket-eof-object? c) eof c)))

(def peekc ((o port (stdin)) (o eof))
  (let c (racket-peek-char port)
    (if (racket-eof-object? c) eof c)))|#

(def open-socket (num)
  (racket-tcp-listen num 50 #t))

#|
; the 2050 means http requests currently capped at 2 meg
; http://list.cs.brown.edu/pipermail/plt-scheme/2005-August/009414.html
(def socket-accept (s)
  (with (oc (racket-current-custodian)
         nc (racket-make-custodian))
    (racket-current-custodian nc)
    (racket-call-with-values
      (fn () (racket-tcp-accept s))
      (fn (in out)
        (let in1 (racket-make-limited-input-port in 100000 #t)
          (racket-current-custodian oc)
          (racket-associate-custodian nc in1 out)
          (list in1
                out
                (let-values (us them) (racket-tcp-addresses out)
                  them)))))))|#

(def client-ip (port)
  (values (us them) (racket-tcp-addresses port) them))

(def limited-input-port (in maxbytes)
  (racket-make-limited-input-port in maxbytes #t))

(def socket-accept (s)
  (values (in out) (racket-tcp-accept s)
    (list (limited-input-port in 100000)
          out
          (client-ip out))))
#|
;; TODO: implement destructuring in fns
;; TODO: should pipe call (cont 'wait)?
(def pipe (cmd)
  (let (in out id err cont) (racket-process/ports #f #f (stderr) cmd)
    (list in out)))

(def pipe-from (cmd)
  (let (in out) (pipe cmd)
    ; Racket docs say I need to close all 3 ports explicitly,
    ; but the err port doesn't need to be closed, because it's
    ; redirected to stderr
    (close (cadr out))
    (car in)))|#

(def flushout ()
  (racket-flush-output)
  t)

(def close-port (port)
  (case (type port)
    input  (racket-close-input-port port)
    output (racket-close-output-port port)
           (err "Can't close " port)))

(def close ports
  (map1 close-port ports))

(ac-notimpl force-close)


#|
;; TODO: replace with version in arc.arc
(def prn args
  (map1 (fn (x)
          (disp x)
          (disp " "))
        args)
  (racket-newline)
  nil)|#


;=============================================================================
;  System
;=============================================================================

(= system  racket-system
   rand    racket-random
   memory  racket-current-memory-use
   quit    racket-exit)

; allow Arc to give up root privileges after it
; calls open-socket. thanks, Eli!
;; TODO: get this to work
;;(= setuid (%nocompile (get-ffi-obj 'setuid #f (_fun _int -> _int))))
(ac-notimpl setuid)

(def dir ((o name "."))
  (map1 racket-path->string (racket-directory-list name)))

; Would def mkdir in terms of make-directory and call that instead
; of system in ensure-dir, but make-directory is too weak: it doesn't
; create intermediate directories like mkdir -p.

(def file-exists (name)
  (if (ac-tnil (racket-file-exists? name)) name))

(def dir-exists (name)
  (if (ac-tnil (racket-directory-exists? name)) name))

(= rmfile  racket-delete-file)

(def mvfile (old new)
  (racket-rename-file-or-directory old new #t)
  nil)


;=============================================================================
;  Time
;=============================================================================

(= msec                          racket-current-milliseconds
   current-process-milliseconds  racket-current-process-milliseconds
   current-gc-milliseconds       racket-current-gc-milliseconds
   seconds                       racket-current-seconds)

(def timedate ((o seconds (seconds)))
  (let d (racket-seconds->date seconds)
    (map1 (fn (x) (x d)) ;; TODO: use (get d)
          (list racket-date-second
                racket-date-minute
                racket-date-hour
                racket-date-day
                racket-date-month
                racket-date-year))))


;=============================================================================
;  Threads
;=============================================================================

(= new-thread      racket-thread
   kill-thread     racket-kill-thread
   break-thread    racket-break-thread
   current-thread  racket-current-thread
   sleep           racket-sleep)


#|(def make-thread-cell (v (o preserved))
  (racket-make-thread-cell v (nil->racket-false preserved)))

(def thread-cell-ref (cell)
  (racket-thread-cell-ref cell))

(def thread-cell-set (cell v)
  (racket-thread-cell-set! cell v))

(= ar-the-sema  (make-semaphore 1)
   ar-sema-cell (make-thread-cell nil))

(def atomic-invoke (f)
  (if (thread-cell-ref ar-sema-cell)
        (f)
      (do (thread-cell-set ar-sema-cell t)
          (after (racket-call-with-semaphore ar-the-sema f)
                 (thread-cell-set ar-sema-cell nil)))))|#

(def atomic-invoke (f)
  (if (racket-thread-cell-ref ac-sema-cell)
        (f)
      (do (racket-thread-cell-set! ac-sema-cell #t)
          (racket-protect (fn () (racket-call-with-semaphore ac-the-sema f))
                          (fn () (racket-thread-cell-set! ac-sema-cell #f))))))

(def dead (x)
  (ac-tnil (racket-thread-dead? x)))


;=============================================================================
;  Macros
;=============================================================================

(def macex1 (x)
  (if (cons? x)
        (let m (ac-macro? (car x))
          (if (is m #f)
                x
              (apply m (cdr x))))
      x))

(def macex (x)
  (let y (macex1 x)
    (if (is x y)
          y
        (macex y))))


;=============================================================================
;  Compiler
;=============================================================================

(ac-notimpl declare)


;=============================================================================
;  Complex fn
;=============================================================================

#|
(def ac-fn-complex-args? (x)
  (if (no x)
        nil
      (sym? x)
        nil
      (and (cons? (car x))
           (no (caris (car x) 'o)))
        t
      (ac-fn-complex-args? (cdr x))))

;; TODO: ridiculously complicated and hacky
(def ac-fn-destructuring-args (u x)
  (w/uniq v
    `((,v (car ,u))
      ,@((afn (x)
           (if (no x)
                 nil
               (cons? (car x))
                 (join (ac-fn-destructuring-args v (car x))
                       `((,v (cdr ,v)))
                       (self (cdr x)))
               (list* `(,(car x) (car ,v))
                      `(,v (cdr ,v))
                      (self (cdr x)))))
         x))))

;; TODO: ridiculously complicated and hacky
(def ac-fn-complex-args (x body env)
  (w/uniq u
    `(,u (racket-let* ((,u (racket-list->mlist ,u))
                       ,@((afn (x)
                                ;; end of the arguments
                            (if (no x)
                                  nil
                                ;; dotted rest args
                                (sym? x)
                                  `((,x ,u))
                                (let c (car x)
                                      ;; optional args
                                  (if (caris c 'o)
                                        (list* `(,(cadr c) (racket-or (ac-nil (car ,u))
                                                                      (racket-quote ,(caddr c))))
                                               `(,u (cdr ,u))
                                               (self (cdr x)))
                                      ;; normal args
                                      (sym? c)
                                        ;; TODO: better error handling
                                        ;;       when a normal argument
                                        ;;       is missing
                                        (list* `(,c (racket-mcar ,u))
                                               `(,u (racket-mcdr ,u)) ;; TODO: should I use cdr or racket-mcdr?
                                               (self (cdr x)))
                                      ;; keyword args
                                      (keyword? c)
                                        ;; TODO: fix this
                                        (err "keyword args not supported with destructuring")
                                      ;; destructuring args
                                      (join (ac-fn-destructuring-args u c)
                                            `((,u (cdr ,u)))
                                            (self (cdr x)))))))
                          x))
           ,@body))))
|#
