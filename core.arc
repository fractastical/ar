;=============================================================================
;  Core
;=============================================================================

(sref sig 'args '=)
(assign = (annotate 'mac
            (fn args
              (ac-compile (cons assign args)))))
                          ;#`(assign ,@args)

(sref sig 'args 'do)
(= do (annotate 'mac
        (fn args
          (ac-compile (if (cdr args)
                            (list (list* fn nil args))
                            ; #`((fn () ,@args))
                          (car args))))))

(sref sig '(var) 'redefine-warning)
(= redefine-warning
   (fn (var)
     (disp "*** redefining " stderr)
     (disp var stderr)
     (disp #\newline stderr)))

(sref sig '(var val) 'safeset)
(= safeset (annotate 'mac
             (fn (var val)
               (ac-compile #`(do (if (bound ',var)
                                   (redefine-warning ',var))
                                 (= var val))))))

;; TODO: get rid of these sref's
(sref sig '(name parms . body) 'def)
(= def (annotate 'mac
         (fn (name parms . body)
           (ac-compile #`(do (sref sig ',parms ',name)
                             (safeset name (fn parms ,@body)))))))

(sref sig '(name parms . body) 'nomac)
(= nomac (annotate 'mac
           (fn (name parms . body)
             (ac-compile #`(do (sref sig ',parms ',name)
                               (safeset name (annotate ''mac (fn parms ,@body))))))))


;=============================================================================
;  Core utilities
;=============================================================================

(nomac mac (name parms . body)
  (ac-compile #`(nomac name parms (ac-compile (do ,@body)))))


(def no (x) (is x nil))

(def pair (xs (o f list))
  (if (no xs)
        nil
      (no (cdr xs))
        (list (list (car xs)))
      (cons (f (car xs) (cadr xs))
            (pair (cddr xs) f))))


(mac with (parms . body)
  #`((fn ,(map1 car (pair parms))
       ,@body)
     ,@(map1 cadr (pair parms))))

(mac let (var val . body)
  #`(with (var val) ,@body))


(mac remac (name parms . body)
  #`(let 'orig (rep name)
      (sref sig ',parms ',name)
      (= name (annotate ''mac (fn parms (ac-compile (do ,@body)))))))

(mac redef (name args . body)
  #`(let 'orig name
      (sref sig ',args ',name)
      (= name (fn args ,@body))))


; Need rfn for use in macro expansions.
(mac rfn (name parms . body)
  #`(let name nil
      (= name (fn parms ,@body))))

(mac afn (parms . body)
  #`(rfn 'self parms ,@body))

; Would like to write a faster case based on table generated by a macro,
; but can't insert objects into expansions in Mzscheme.
(mac caselet (var expr . args)
  (let ex (afn (args)
            (if (no (cdr args))
                  (car args)
                #`(if (is var ',(car args))
                      ,(cadr args)
                      ,(self (cddr args)))))
    #`(let var expr ,(ex args))))

(mac case (expr . args)
  #`(caselet ,(uniq) expr ,@args))


; Ac expands x:y:z into (compose x y z), ~x into (complement x)
; Only used when the call to compose doesn't occur in functional position.
; Composes in functional position are transformed away by ac.
(remac compose args
  (let g (uniq)
    #`(fn g
        ,((afn (fs)
            (if (cdr fs)
                (list (car fs) (self (cdr fs)))
                #`(apply ,(if (car fs) (car fs) 'idfn) g)))
          args))))

; Ditto: complement in functional position optimized by ac.
(remac complement (f)
  (let g (uniq)
    #`(fn g (no (apply f g)))))


;=============================================================================
;  ac
;=============================================================================

(def ac-make-read (f)
  (fn ((o port stdin) (o eof))
    (let c (f port)
      (if (is c racket-eof) eof c))))

(def ac-make-write (f)
  (fn ((o port stdout)) (f port)))

(= ac-the-sema  (racket-make-semaphore 1)
   ac-sema-cell (racket-make-thread-cell #f))

(mac ac-notimpl (x)
  #`(def x ,(uniq)
      (err (string ',x " is not implemented"))))


;=============================================================================
;  Multi-value functions
;=============================================================================

(mac values (vars expr . body)
  #`(racket-call-with-values (fn () expr) (fn vars ,@body)))


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
  `(,fn ((o _)) (,@body)))


;=============================================================================
;  Types
;=============================================================================

(def isa (x y)
  (is (type x) y))


(mac make-predicate (x (o y x))
         ;; TODO: this is just (sym x "?")
  #`(def ,(racket-string->symbol
            (racket-string-append (string1 x) "?"))
         ('x) (isa 'x ',y)))

(make-predicate cons)
(make-predicate int)
(make-predicate string)
(make-predicate sym)
(make-predicate char)
(make-predicate fn)
(make-predicate mac)
(make-predicate keyword)
(make-predicate table)

(def num?  (x) (ac-tnil (racket-number? x)))
(def list? (x) (if (no x) t (cons? x)))
;; TODO: should this be here...?
(def uniq? (x) (no (ac-tnil (racket-symbol-interned? x))))

(def testify (x)
  (if (fn? x) x [is _ x]))


(def string args
  (apply racket-string-append (map1 string1 args)))


;; TODO: should these throw errors...?
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
                 (err "can't coerce" x 'string)))))

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
            (err "can't coerce" x 'string))))

(def keyword (x)
  (racket-string->keyword (string x)))


;; TODO: need to figure out a better way to deal with types
(def coerce (x totype (o base 10))
  (if (ac-tnil (ac-tagged? x))
        (err "can't coerce annotated object")

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
                            (err "can't coerce" x type)))


;=============================================================================
;  Variable Binding
;=============================================================================

(mac w/uniq (names . body)
  (if (cons? names)
        #`(with ,(ac-mappend (fn (n) (list n (list uniq))) names)
            ,@body)
      #`(let names (uniq) ,@body)))

(mac aif (expr . body)
  #`(let 'it expr
      (if 'it
          ,@(if (cddr body)
                  #`(,(car body) (aif ,@(cdr body)))
                body))))


; Destructuring means ambiguity: are pat vars bound in else? (no)
(mac iflet (var expr then . rest)
  (w/uniq gv
    #`(let gv expr
        (if gv (let var gv then) ,@rest))))

(mac whenlet (var expr . body)
  #`(iflet var expr (do ,@body)))


;=============================================================================
;  Conditionals
;=============================================================================

(mac when (test . body)
  #`(if test (do ,@body)))

(mac and args
  (if args
      (if (cdr args)
            #`(if ,(car args) (and ,@(cdr args)))
          (car args))
      t))

(mac or args
  (and args
       (w/uniq g
         #`(let g ,(car args)
             (if g g (or ,@(cdr args)))))))

(mac in (x . choices)
  (if (cdr choices)
        (w/uniq g
          #`(let g x
              (or ,@(map1 (fn (c) #`(is g c)) choices))))
      #`(is x ,(car choices))))


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
  (let l (len s)
    ((afn (i)
       (and (< i l)
            (or (test i)
                (self (+ i 1)))))
     start)))


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
    (if (num? x)    (racket-< x y)
        (string? x) (racket-string<? x y)
        (sym? x)    (racket-string<? (sym x) (sym y))
        (char? x)   (racket-char<? x y)
                    (err "can't <" x y))))

(def >2 (x y)
  (ac-tnil
    (if (num? x)    (racket-> x y)
        (string? x) (racket-string>? x y)
        (sym? x)    (racket-string>? (string x) (string y))
        (char? x)   (racket-char>? x y)
                    (err "can't >" x y))))

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
  (racket-hash-for-each table fn)
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
;  Parameters
;=============================================================================

;; TODO: function version of make-alias...?
(mac make-alias (name get set)
  #`(= name (annotate ''alias (list get set))))

(mac alias (name orig)
  (w/uniq (x n v)
    #`(make-alias name
        (fn ()      orig)
        (fn (x v n) (= orig v)))))

;; TODO: make this into a function
(mac make-dynamic (name val)
  (w/uniq (u v x)
    #`(with (u  val
             x  nil)
        ;; TODO: use letr ?
        (= x (make-alias name
                                 ;; TODO: use (ref namespace ...) ?
                                 ;; TODO: how slow is it to create a new fn
                                 ;;       every time...?
               (fn ()      (let v (ac-var ',name (fn () x))
                             ;(,ac-prn ,v)
                             (if (is v x) u v)))
               #|,(fn ()  (let v (ac-var name (fn () x))
                          (ac-prn v)
                          (if (is v x) u v)))|#
                       ;; TODO: does namespace need to be quoted?
               (fn (x v u) (sref 'namespace v u))
               ;,(fn (v) (sref namespace v name))
               ))
        u)))

(mac dynamic (name (o init))
  #`(make-dynamic name init))


#|(mac parameterize (x . body)
  `(,%nocompile (racket-parameterize ,(map1 (fn ((x y))
                                              ;; TODO: should probably use %compile for x
                                              #`((rep x) ('%compile y)))
                                            (pair x))
                  (%compile ,@(or body (list nil))))))|#

(nomac parameterize (x . body)
  `(racket-parameterize ,(map1 (fn ((x y))
                                 ;; TODO: should probably use ac-compile for x
                                 #`((rep x) ,(ac-compile y)))
                               (pair x))
     ,@(or (ac-args body) nil)))

(mac make-w/ (param (o name param))
  (w/uniq (val body)
    #`(mac ,(sym "w/" name) (val . body)
        #`(parameterize (,',param val) ,@body))))

(mac make-parameter (name param)
  #`(do (= name (annotate ''parameter param))
        (make-w/ name)))

(mac parameter (name (o init))
  (w/uniq u
    #`(let u init
        (make-parameter name (racket-make-parameter u))
        u)))


(make-parameter namespace     ac-namespace)
(make-parameter uniq-counter* ac-uniq-counter*)
(make-parameter load-paths*   ac-load-paths*)
(make-parameter load-suffix*  ac-load-suffix*)

#|(make-parameter namespace
  (racket-make-derived-parameter ac-namespace
    (fn (v) (when (ac-tnil (racket-namespace? v))
              (racket-current-namespace v))
            v)
    ;; TODO: idfn
    (fn (x) x)))|#

#|(make-w/ ac-namespace namespace)

;; TODO: tests to see how fast aliases are
(make-alias namespace
  ac-namespace
  (fn (x v n) (when (ac-tnil (racket-namespace? v))
                ;(prn "alias " x " setting " n " to " v)
                (racket-current-namespace v))
              (ac-namespace v)
              ;(prn (is namespace (racket-current-namespace)))
              ))|#

(parameter debug-mode* nil)

(dynamic debug (fn args
                 (when debug-mode*
                   (apply prn (intersperse " " args)))))


;=============================================================================
;  I/O
;=============================================================================

(make-parameter stdin   racket-current-input-port)
(make-parameter stdout  racket-current-output-port)
(make-parameter stderr  racket-current-error-port)


(def ac-disp (x port)
  (racket-display x port)
  (racket-flush-output port))

(def ac-write (x port)
  (racket-write x port)
  (racket-flush-output port))

(def disp (x (o port stdout))
  (print ac-disp x port))

(def write (x (o port stdout))
  (print ac-write x port))

(mac after (x . ys)
  #`(protect (fn () x) (fn () ,@ys)))


(def close-port (port)
  (case (type port)
    input  (racket-close-input-port port)
    output (racket-close-output-port port)
           (err "can't close " port)))

(def close ports
  ;; TODO: eachfn
  (map1 close-port ports)
  nil)


;; TODO: better system than this
(let expander (fn (f var name body)
                #`(let var (f name)
                    (after (do ,@body) (close var))))

  (mac w/infile (var name . body)
    (expander 'infile var name body))

  (mac w/outfile (var name . body)
    (expander 'outfile var name body))

  (mac w/instring (var str . body)
    (expander 'instring var str body))

  (mac w/socket (var port . body)
    (expander 'open-socket var port body))

  (mac w/pipe-from (var port . body)
    (expander 'pipe-from var port body))
  )


(def readstring1 (s (o eof nil))
  (w/instring i s (read i eof)))

(def read ((o x stdin) (o eof nil))
  (if (string? x)
        (readstring1 x eof)
      (sread x eof)))


(= infile     racket-open-input-file)

(def outfile (filename (o append))
  (let flag (if append 'append 'truncate)
    (racket-open-output-file filename #:mode 'text #:exists flag)))


(= instring   racket-open-input-string
   outstring  racket-open-output-string)

; use as general fn for looking inside things
(= inside     racket-get-output-string)

(= readc  (ac-make-read racket-read-char)
   readb  (ac-make-read racket-read-byte)
   peekc  (ac-make-read racket-peek-char))

(= writec (ac-make-write racket-write-char)
   writeb (ac-make-write racket-write-byte))


(def open-socket (num)
  (racket-tcp-listen num 50 #t))

(def client-ip (port)
  (values (us them) (racket-tcp-addresses port) them))

(def limited-input-port (in maxbytes)
  (racket-make-limited-input-port in maxbytes #t))

(def socket-accept (s)
  (values (in out) (racket-tcp-accept s)
    (list (limited-input-port in 100000)
          out
          (client-ip out))))


;; TODO: should pipe call (cont 'wait)?
(def pipe (cmd)
  (let (in out id err cont)
       (racket-list->mlist (racket-process/ports #f #f (stderr) cmd))
    (list in out)))

(def pipe-from (cmd)
  (let (in out) (pipe cmd)
    ;; Racket docs say I need to close all 3 ports explicitly,
    ;; but the err port doesn't need to be closed, because it's
    ;; redirected to stderr
    (close out)
    in))


(def flushout ()
  (racket-flush-output)
  t)


(ac-notimpl force-close)


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

(def dir ((o name "") (o f))
  ;; TODO: all of this is ew
  (= name (expandpath name))
  (if (empty name) (= name "."))
  (parameterize (racket-current-directory name)
    (let x (map1 (fn (x)
                   (if (dir-exists x)
                         (string x "/")
                       (string x)))
                 (racket-list->mlist (racket-directory-list)))
      (if f (keep f x)
            x))))


; Would def mkdir in terms of make-directory and call that instead
; of system in ensure-dir, but make-directory is too weak: it doesn't
; create intermediate directories like mkdir -p.

;; TODO: racket-make-directory*

(def file-exists (name)
  (ac-tnil (racket-file-exists? name)))

(def dir-exists (name)
  (ac-tnil (racket-directory-exists? name)))

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


;; TODO: ac-sema-cell, look at ar

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
              (apply (rep m) (cdr x))))
      x))

(def macex (x)
  (let y (macex1 x)
    (if (is x y)
          y
        (macex y))))


(parameter macex-rename* t)

(def macex-all (x)
  (if (caris x ac-fn)
        (list* 'fn ;(macex-all (car x))
               (cadr x)
               (map macex-all (cddr x)))
      #|(or (caris x ac-if)
          ;(caris x quote)
          )
        (cons (macex-all (car x))
              (map macex-all (cdr x)))|#
      (caris x ac-if)
        (cons 'if (map macex-all (cdr x)))
      (caris x quote)
        x
      #|(caris x ac-quote)
        "#<quoted>"|#
        ;(cons 'quote (cdr x))
      (let y (macex1 x)
        (if (is x y)
              (if (cons? y)
                    (map macex-all y)
                  (and macex-rename*
                       (isnt y ac-quote)
                       (or (fn? y)
                           (mac? y)))
                    (or (name y) y)
                  y)
            (macex-all y)))))


;=============================================================================
;  Compiler
;=============================================================================

(ac-notimpl declare)


;=============================================================================
;  Racket
;=============================================================================

#|(mac require (x)
    ;; TODO: use dont
  #`(do (%nocompile (racket-namespace-require/copy ('racket-quote ('prefix 'racket- x))))
        nil))|#

(nomac require (x)
    ;; TODO: use dont
  #`('racket-begin (racket-namespace-require/copy ('racket-quote ('prefix 'racket- x)))
                   nil))
