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


; Need rfn for use in macro expansions.
(mac rfn (name parms . body)
  `(let ,name nil
     (assign ,name (fn ,parms ,@body))))

(mac afn (parms . body)
  `(let self nil
     (assign self (fn ,parms ,@body))))

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


;=============================================================================
;  ac
;=============================================================================

(def ac-make-read (f)
  (fn ((o port (stdin)) (o eof))
    (let c (f port)
      (if (racket-eof-object? c) eof c))))

(def ac-make-write (f)
  (fn ((o port (stdout))) (f port)))

(assign ac-the-sema  (racket-make-semaphore 1))
(assign ac-sema-cell (racket-make-thread-cell #f))

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
;  Types
;=============================================================================

(def isa      (x y) (is (type x) y))
(def cons?    (x)   (isa x 'cons))
(def int?     (x)   (isa x 'int))
(def string?  (x)   (isa x 'string))
(def sym?     (x)   (isa x 'sym))
(def char?    (x)   (isa x 'char))
(def keyword? (x)   (isa x 'keyword))


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
;  Logical Booleans
;=============================================================================

(mac w/uniq (names . body)
  (if (cons? names)
        `(with ,(ac-mappend (fn (n) (list n '(uniq))) names)
           ,@body)
      `(let ,names (uniq) ,@body)))


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



(def num?    (x)   (or (int? x) (isa x 'num)))
(def list?   (x)   (or (no x)   (cons? x)))


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

(def rreduce (f xs)
  (if (cddr xs)
        (f (car xs) (rreduce f (cdr xs)))
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


;=============================================================================
;  Strings
;=============================================================================

(assign newstring  racket-make-string)


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
                                          `((assign ,n (racket-list->mlist ,n))
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

(assign +  (ac-binary +-2 reduce))
(assign <  (ac-binary  <2 ac-pairwise))
(assign >  (ac-binary  >2 ac-pairwise))


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

(assign -    racket--)
(assign *    racket-*)
(assign /    racket-/)
(assign mod  racket-modulo)
(assign expt racket-expt)
(assign sqrt racket-sqrt)
(assign sin  racket-sin)
(assign cos  racket-cos)
(assign tan  racket-tan)
(assign asin racket-asin)
(assign acos racket-acos)
(assign atan racket-atan)
(assign log  racket-log)


;=============================================================================
;  Continuations
;=============================================================================

(assign ccc racket-call-with-current-continuation)

(def protect (during after)
  (racket-dynamic-wind (fn () #t) during after))


;=============================================================================
;  I/O
;=============================================================================

(assign infile     racket-open-input-file)

(def outfile (filename (o append))
  (let flag (if append 'append 'truncate)
    (racket-open-output-file filename #:mode 'text #:exists flag)))

(assign instring   racket-open-input-string)
(assign outstring  racket-open-output-string)

; use as general fn for looking inside things
(assign inside     racket-get-output-string)

(def call-w/stdout (port thunk)
  (parameterize ((current-output-port port)) (thunk)))

(def call-w/stdin (port thunk)
  (parameterize ((current-input-port port)) (thunk)))

(assign readc  (ac-make-read racket-read-char))
(assign readb  (ac-make-read racket-read-byte))
(assign peekc  (ac-make-read racket-peek-char))

(assign writec (ac-make-write racket-write-char))
(assign writeb (ac-make-write racket-write-byte))

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


;=============================================================================
;  System
;=============================================================================

(assign system  racket-system)
(assign rand    racket-random)
(assign memory  racket-current-memory-use)
(assign quit    racket-exit)

; allow Arc to give up root privileges after it
; calls open-socket. thanks, Eli!
;; TODO: get this to work
;;(assign setuid (%nocompile (get-ffi-obj 'setuid #f (_fun _int -> _int))))
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

(assign rmfile  racket-delete-file)

(def mvfile (old new)
  (racket-rename-file-or-directory old new #t)
  nil)


;=============================================================================
;  Time
;=============================================================================

(assign msec                         racket-current-milliseconds)
(assign current-process-milliseconds racket-current-process-milliseconds)
(assign current-gc-milliseconds      racket-current-gc-milliseconds)

(assign seconds                      racket-current-seconds)

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

(assign new-thread     racket-thread)
(assign kill-thread    racket-kill-thread)
(assign break-thread   racket-break-thread)
(assign current-thread racket-current-thread)
(assign sleep          racket-sleep)


#|(def make-thread-cell (v (o preserved))
  (racket-make-thread-cell v (nil->racket-false preserved)))

(def thread-cell-ref (cell)
  (racket-thread-cell-ref cell))

(def thread-cell-set (cell v)
  (racket-thread-cell-set! cell v))

(assign ar-the-sema (make-semaphore 1))

(assign ar-sema-cell (make-thread-cell nil))

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
;  ssyntax
;=============================================================================

(def reclist (f xs)
  (and xs (or (f xs) (reclist f (cdr xs)))))

(def recstring (test s (o start 0))
  ((afn (i)
     (and (< i (len s))
          (or (test i)
              (self (+ i 1)))))
   start))

(def ssyntax (x)
  (and (sym? x)
       ;(no (in x '+ '++ '_))
       ;; TODO: ew
       (let x (string x)
         (recstring [in (x _) #\: #\~ #\& #\. #\!] x))))
       ;(some [in _ #\: #\~ #\& #\. #\!] (string x))

#|(define (has-ssyntax-char? string i)
  (and (>= i 0)
       (or (let ((c (string-ref string i)))
             (or (eqv? c #\:) (eqv? c #\~)
                 (eqv? c #\&)
                 ;(eqv? c #\_)
                 (eqv? c #\.)  (eqv? c #\!)))
           (has-ssyntax-char? string (- i 1)))))

(define (ssyntax? x)
  (and (symbol? x)
       (not (or (eqv? x '+) (eqv? x '++) (eqv? x '_)))
       (let ((name (symbol->string x)))
         (has-ssyntax-char? name (- (string-length name) 1)))))|#

(def split-char (x y)
  (let n (recstring [if (is (y _) x) _] y)
    (if n (list (racket-substring y 0 n)
                (racket-substring y (+ n 1))))))

(def split-sym (x y)
  (map1 sym (split-char x (string y))))

;(basic-split #\. "foo.bar") -> ("foo" "bar")

(assign ssyntax-rules* (list (fn (x y) nil)
                             (fn (x y) nil)
                             (fn (x y)
                               (let x (split-sym #\. x)
                                 (if x (list x))))
                                 #| (let y (ssyntax-parse (cadr x))
                                         (racket-display y)
                                         (list x y))|#
                             (fn (x y)
                               (let x (split-sym #\! x)
                                 (if x `((,(car x) ',(cadr x))))))
                             (fn (x y)
                               (let x (split-sym #\& x)
                                 (if x `((andf ,@x)))))
                             (fn (x y)
                               (let x (split-sym #\: x)
                                 (if x `((compose ,@x)))))
                             ))

#|(insym? #\: sym)
(insym? #\~ sym)
(insym? #\. sym)
(insym? #\! sym)
(insym? #\& sym)


(fn (x)
  (whenlet (l r) (splitmatch1 ":" x)
    (list `(compose ,l ,r))))|#

#|

5 -> nil
     nil
     (1 2) -> nil nil
              nil nil
              nil nil
              nil (3 4) -> nil nil
                           nil nil
                           nil nil
                           nil nil

(1 (3 4))

|#






#|(assign ssyntax-rules* (list (make-ssyntax : (x y)
                               `(compose ,x ,y))

                             (make-ssyntax ~ (x)
                               `(complement ,x))

                             (make-ssyntax . (x y)
                               `(,x ,y))

                             (make-ssyntax ! (x y)
                               `(,x ',y))

                             (make-ssyntax & (x y)
                               `(andf ,x ,y))))|#



#|
proc parseE(prec) =
        initialise p:=parsePrimary();
        while Priority(input^) >= prec do begin
                let oper = get(input);
                let oprec = Priority(oper);
                p := mknode(p,
                                        oper,
                                        parseE(if RightAssoc(oper) then oprec else oprec+1))
        end;
        return p
endproc

proc parsePrimary() =
        case input^ of
        "(" :
                                begin
                                                get(input);
                                                let p=E(1);
                                                if input^ <> ")" then fail fi;
                                                return p
                                end;
        otherwise:
                                if input^ in ['a'..'z'] then
                                        return mkleaf(input^)
                                else
                                        fail
                                fi
        endcase
endproc


(while )
|#



#|(def ssyntax-parse (s)
  ((afn (x acc)
     (if (no x)
           acc
         (let r ((car x) s)
           (if (no r)
                 (self (cdr x) acc)
               (let r (car r)
                 (if (cons? r)
                       (do ;(racket-display r)
                           ;(racket-newline)
                           (map1 (fn (x)
                                   (racket-display x)
                                   (racket-newline)
                                   (ssyntax-parse x))
                                 r))
                     r))))))
   ssyntax-rules* nil))|#

#|(def ssyntax-parse (s)
  ((afn (x)
     (if (no x)
           s
         (let r ((car x) s)
           (if (no r)
                 (self (cdr x))
               (let r (car r)
                 (if (cons? r)
                       (do ;(racket-display r)
                           ;(racket-newline)
                           (map1 (fn (x)
                                   ;(racket-display x)
                                   ;(racket-newline)
                                   (ssyntax-parse x))
                                 r))
                     r))))))
   ssyntax-rules*))|#


; foo.bar!qux -> ((foo bar) 'qux)
; l = (foo bar!qux)
; r =      (bar 'qux)
;
; (join (cons (car l)
;             (car r))
;       (cdr r))


; foo:bar:qux -> (compose (compose foo bar) qux)
; l = (compose foo bar:qux)
; r =              (compose bar qux)
;
; (join (cons (cadr l)
;             (cadr r))
;       (cddr r))

(def ssyntax-parse (s)
  ((afn (x)
     (if (no x)
           s
         (let r ((car x) s)
           (if (no r)
                 (self (cdr x))
               (let x (car r)
                 (if (cons? x)
                       #|(do (racket-display x)
                           (racket-newline)
                           (map1 ssyntax-parse x)
                             )|#
                       (let y (ssyntax-parse (x 2))
                         ;(racket-display y)

                         #|x = (compose foo bar:qux)
                         y = (compose bar qux)

                         (compose (compose foo bar) qux)|#

                         (if (cons? y)
                               #|(do (racket-display )
                                   (racket-newline))|#
                               (list (x 0) (list (y 1) (x 1) (y 2)) (y 0))
                               ;(list (x 0) (list (y 0) (x 1) (y 1)) (y 2))
                               ;(list (y 0) (list (x 0) x (y 1)) (y 2))
                             (cons y x)
                             ;y
                             )
                         ;(list (x 0) (list (x 1) (y 1)))
                         ;(list* (car x) (car y) (ssyntax-parse (cadr x)) (ssyntax-parse (cadr (cdr x))))
                         )

                     r))))))
   ssyntax-rules*))

#|(if acons.r
                      (list r.0 (list y l r.1) r.2)
                      ;(list r.0 (list y l r.1) r.2)
                    (list y l r))|#

#|(def ssyntax-eval (x)
  (if acons.x (let (n1 x y) x
                ;(debug n1 x y)
                (zap ssyntax-eval x)
                (zap ssyntax-eval y)
                (some (fn ((n2 nil f))
                        (and (is n1 n2) (f x y)))
                      ssyntax-rules*)
                #|((alref ssyntax-rules* n) ssyntax-eval.x
                                          ssyntax-eval.y)|#
                                          )
              x))|#

(def ssexpand (x)
  (if (sym? x)
        x
      x))
