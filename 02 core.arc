;=============================================================================
;  Core
;=============================================================================

(=sigmac do args
  (if (cdr args)
        #|(list* ac-nocompile
               'racket-begin
               (ac-args args))|#
        (list (list* fn nil args))
        ;#`((fn () ,@args))
        ;#`((fn () . args))
      (car args)))

;(ac-prn (ac-compile '(do 1 2 3)))

#|(sref sig '(name parms x) '=sig)
(= =sig (annotate 'mac
          (fn (name parms x)
            #`(do (sref sig ',parms ',name)
                  (= name x)))))

(=sig =sigmac (name parms . body)
  (annotate 'mac
    (fn (name parms . body)
      #`(=sig name parms (annotate ''mac (fn parms . body))))))|#

#|(=sigmac =sig (name parms x)
  #`(do (sref sig ',parms ',name)
        (= name x)))|#

#|(= =fn (annotate 'mac
         (fn (name parms . body)
           #`(=sig name parms (fn parms . body)))))|#

;(mac fntable )

(=sigdef redefine-warning (var)
  (disp "*** redefining " stderr)
  (disp var stderr)
  (disp #\newline stderr))

(=sigmac safeset (var val)
  #`(do (if (bound ',var)
          (redefine-warning ',var))
        (= var val)))

(=sigmac =safesig (name parms x)
  #`(do (if (bound ',name)
          (redefine-warning ',name))
        (=sig name parms x)
        #|(sref sig ',parms ',name)
        (safeset name x)|#
        ))

(=sigmac def (name parms . body)
  #`(=safesig name parms (fn parms . body)))

(=sigmac mac (name parms . body)
  #`(=safesig name parms (annotate ''mac (fn parms . body))))

;; TODO: get this working
#|(mac nomac (name parms . body)
  #`(mac name parms (% ,@(ac-args body))))|#


;=============================================================================
;  Core utilities
;=============================================================================

(def no (x) (is x nil))

(def pair (xs (o f list))
  (if (no xs)
        nil
      (no (cdr xs))
        (list (list (car xs)))
      (cons (f (car xs) (cadr xs))
            (pair (cddr xs) f))))


(mac with (parms . body)
  (if parms #`((fn ,(map1 car (pair parms)) . body)
               ,@(map1 cadr (pair parms)))
            #`(do . body)))

(mac let (var val . body)
  #`(with (var val) . body))


(mac remac (name parms . body)
  #`(let 'orig (rep name)
      (=sigmac name parms . body)
      ;(=sig name parms (annotate ''mac (fn parms . body)))
      ))

(mac redef (name parms . body)
  #`(let 'orig name
      (=sigdef name parms . body)
      ;(=sig name parms (fn parms . body))
      ))


; Need rfn for use in macro expansions.
(mac rfn (name parms . body)
  #`(let name nil
      (= name (fn parms . body))))

(mac afn (parms . body)
  #`(rfn 'self parms . body))

; Would like to write a faster case based on table generated by a macro,
; but can't insert objects into expansions in Mzscheme.
(mac caselet (var expr . body)
  (let ex (afn (xs)
            (if (no (cdr xs))
                  (car xs)
                #`(if (is var ',(car xs))
                      ,(cadr xs)
                      ,(self (cddr xs)))))
    #`(let var expr ,(ex body))))

(mac case (expr . body)
  #`(caselet ,(uniq) expr . body))


; Ac expands x:y:z into (compose x y z), ~x into (complement x)
; Only used when the call to compose doesn't occur in functional position.
; Composes in functional position are transformed away by ac.
(remac compose args
  (let g (uniq)
    #`(fn g
        ,((afn (fs)
            (if (cdr fs)
                (list (car fs) (self (cdr fs)))
                #`(apply ,(if (car fs) (car fs) idfn) g)))
          args))))

; Ditto: complement in functional position optimized by ac.
(remac complement (f)
  (let g (uniq)
    #`(fn g (no (apply f g)))))


;=============================================================================
;  ac
;=============================================================================

(mac %if args
  (cons ac-nocompile
        ((afn (x)
           (if (cdr x)
                 (list 'racket-if
                       ;(ac-compile (car x))
                       (car x)
                       (cadr x)
                       (self (cddr x)))
               ;; TODO: or
               (if (car x) (car x) '(racket-quote nil))))
         args)))

(def ac-make-read (f)
  (fn ((o port stdin) (o eof))
    (let c (f port)
      (%if (racket-eof-object? c) eof c))))

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
  #`(racket-call-with-values (fn () expr) (fn vars . body)))


;=============================================================================
;  square-bracket / curly-bracket
;=============================================================================

(mac square-bracket body
  `(,fn ((o _)) (,@body)))

;; Was originally in arubic.arc New Additions
(mac curly-bracket args
  #`(obj ,@args))


;=============================================================================
;  Types
;=============================================================================

(def isa (x y)
  (is (type x) y))


#|(mac make-predicate (x (o y x))
         ;; TODO: this is just (sym x "?")
  #`(def ,(racket-string->symbol
            (racket-string-append (racket-symbol->string x) "?")) ;string1
         ('x) (isa 'x ',y)))|#

#|(mac deftype (pred (o to) (o make))
  #`(do (def ,@pred)
        ,(if to   #`(def ,@to))
        ,(if make #`(def ,@make))))|#

;(make-predicate cons)
;(make-predicate int)
;(make-predicate string)
;(make-predicate sym)
;(make-predicate char)
;(make-predicate fn)
;(make-predicate mac)
;(make-predicate keyword)
;(make-predicate table)

(def fn?  (x) (isa x 'fn))
(def mac? (x) (isa x 'mac))

(def list? (x) (if (no x) t (cons? x)))
;; TODO: should this be here...?
(def uniq? (x) (ac-tnil (racket-not (racket-symbol-interned? x))))



(def char?    (x) (isa x 'char))
(def char     (x) (coerce x 'char))

(def keyword? (x) (isa x 'keyword))
(def keyword  (x) (coerce x 'keyword))

(def table?     (x)  (isa x 'table))
(def table      (x)  (coerce x 'table))
(def make-table args (table (pair args)))

(def int? (x) (isa x 'int))
(def int  (x (o base 10))
  (coerce x 'int base))

(def num? (x) (ac-tnil (racket-number? x)))
(def num  (x (o base 10))
  (coerce x 'num base))

(def cons?  (x) (isa x 'cons))
;; TODO: kinda ew name
(def tocons (x) (coerce x 'cons))

#|(redef string1 (x)
  (coerce x 'string))|#

(def string? (x) (isa x 'string))
(def string  args
  (apply racket-string-append (map1 (fn (x) (coerce x 'string)) #|string1|# args)))

(def sym? (x) (isa x 'sym))
(def sym  args
  (coerce (apply string args) 'sym))


;=============================================================================
;  Variable Binding
;=============================================================================

(mac w/uniq (names . body)
  (if (cons? names)
        #`(with ,(ac-mappend (fn (n) (list n (list uniq))) names) . body)
      #`(let names (uniq) . body)))

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
        (if gv (let var gv then) . rest))))

(mac whenlet (var expr . body)
  #`(iflet var expr (do . body)))


;=============================================================================
;  Conditionals
;=============================================================================

(mac when (test . body)
  #`(if test (do . body)))

(mac and args
  (if args
      (if (cdr args)
            #`(if ,(car args) (and ,@(cdr args)))
          (car args))
      t))

(mac or args
  (if (cdr args)
        (w/uniq g
          #`(let g ,(car args)
              (if g g (or ,@(cdr args)))))
      (car args)))

(mac in (x . choices)
  (if (cdr choices)
        (w/uniq g
          #`(let g x
              (or ,@(map1 (fn (c) #`(is g c)) choices))))
      #`(is x ,(car choices))))


;=============================================================================
;  Type coercion
;=============================================================================

(def testify (x)
  (if (fn? x) x [is _ x]))

(def ac-iround (x)
  (racket-inexact->exact (racket-round x)))


#|(def make-table ()
  (racket-make-hash))|#

#|
(def int     (x (o base 10)) (coerce x 'int base))
(def num     (x (o base 10)) (coerce x 'num base))
(def char    (x)             (coerce x 'char))
(def keyword (x)             (coerce x 'keyword))
(def table   (x)             (coerce x 'table))
(def tocons  (x)             (coerce x 'cons))|#


(= coerce-list* nil)

(def defcoercefn (type to f)
  (aif (alref coerce-list* type)
         (sref it f to)
       (let u (racket-make-hash) ;(make-table)
         ;; TODO: push
         (= coerce-list* (cons (list type u) coerce-list*))
         (sref u f to))))

(mac defcoerce (type to parms . body)
  #`(defcoercefn type to (fn parms . body)))
#|  (w/uniq (u v)
    #`(let v type
        (aif (alref coerce-list* v)
               (sref 'it (fn parms . body) to)
             (let u (make-table)
               ;; TODO: push
               (ac-prn coerce-list*)
               (= 'coerce-list* (cons (list v u) 'coerce-list*))
               (sref u (fn parms . body) to)))))|#
#|  (push coerce-list* (cons (list to (fn parms . body))
                           (coerce-table* type)) type)|#

(mac defcoerces (type parms . body)
  #`(do ,@(map1 (fn ((x y))
                  #`(defcoerce type x parms y))
                (pair body))))


(defcoerces (compose ac-tnil racket-bytes?) (x)
  'string (racket-bytes->string/utf-8 x))

(defcoerces (compose ac-tnil racket-path?) (x)
  'string (racket-path->string x))

(defcoerces keyword? (x)
  'string (racket-keyword->string x)
  'sym    (racket-string->symbol (racket-keyword->string x)))

(defcoerces int? (x)
  'string (racket-number->string x)
  'char   (racket-integer->char x))

(defcoerces int? (x (o base 10))
  'num    (+ 0.0 x))

(defcoerces num? (x)
  'string (racket-number->string x)
  'char   (racket-integer->char (int x)))

(defcoerces num? (x (o base 10))
  'int    (ac-iround x))

(defcoerces table? (x)
  ;'cons (accum a (maptable (fn args (a args)) x))
  'cons (let a nil (maptable (fn args (= a (cons args a))) x) (nrev a))
  )

(defcoerces cons? (x)
  'string (apply string x)
  'table  (let h (make-table)
            (map (fn ((k v)) (sref h v k)) x)
            h))

(defcoerces string? (x)
  'keyword (racket-string->keyword x)
  'cons    (racket-string->list x)
  'sym     (racket-string->symbol x))

(defcoerces string? (x (o base 10))
  'int (let n (racket-string->number x base)
         (if (is n #f)
               (err "can't coerce" x 'int)
             (ac-iround n)))
  'num (let n (racket-string->number x base)
         (if (is n #f)
               (err "can't coerce" x 'num)
             n)))

(defcoerces char? (x)
  'string (racket-string x)
  'sym    (racket-string->symbol (racket-string x)))

(defcoerces char? (x (o base 10))
  'int    (racket-char->integer x))

(defcoerces sym? (x)
  'keyword (racket-string->keyword (racket-symbol->string x))
  'string  (racket-symbol->string x))

;; must be under sym?
(defcoerces no (x)
  'keyword (racket-string->keyword "")
  'string  ""
  ;; TODO: not sure about this
  'table   (racket-make-hash) ;(make-table)
  'cons    nil)


#|(defcoerces 'string (x)
  (string (racket-string->list x)) ;(racket-list->mlist )
  (no     nil))

(defcoerces cons? (x)
  ('string (string x)) ;(racket-list->mlist )
  ('table  (let h (make-table)
             (map (fn ((k v)) (sref h v k)) x)
             h)))

(defcoerces 'keyword (x)
  (string? (racket-string->keyword x)))

(defcoerces 'table (x)
  (cons? (accum a (maptable (fn args (a args)) x))))|#

#|(deftype table (fn () (racket-make-hash))
                 (fn (x)
                   (if (cons? x)
                         (let h (racket-make-hash)
                           (map (fn ((k v)) (= (h k) v)) x)
                           h))))|#

#|
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
      (is totype 'cons)     (if (string? x) (racket-string->list x)
                                (table? x)  (let a nil (maptable (fn args (push args a)) x) (nrev a))
                                (no x)      nil)
      (is totype 'table)    (if (cons? x) (let h (make-table)
                                            (map (fn ((k v)) (sref h v k)) x)
                                            h))
                            (err "can't coerce" x type)))|#

;(map1 ac-prn coerce-list*)

#|(= coerced-types* (make-table))

#hash((sym     . 4563)
      (char    . 556)
      (string  . 184)
      (cons    . 134)
      (unknown . 38))|#

(def coerce (x totype . args)
  ;(ac-prn x totype args)
  (if (ac-tnil (ac-tagged? x))
        (err "can't coerce annotated object")
      (is totype (type x))
        x
      ;; TODO: aloop
      ((afn (xs)
         (if (no xs)
               (err "can't coerce" x totype)
             (let (isa? f) (car xs)
               (aif (and (isa? x)
                         ;(sref coerced-types* (+ (or (coerced-types* (type x)) 0) 1) (type x))
                         (f totype))
                      (apply it x args)
                     #|(do ;(ac-prn f x totype)
                         ;(ac-prn (f totype (fn () (self (cdr xs)) nil)))
                       (w/uniq u
                         (let f (f totype u)
                           (if (is f u)
                                 (self (cdr xs))
                               )))
                      )|#
                    (self (cdr xs))))))
       coerce-list*)))


;=============================================================================
;  Loops
;=============================================================================

;; TODO: can be defined in terms of whilet, but it's way less efficient
(mac while (test . body)
  (w/uniq (gf gp)
    #`((rfn gf (gp)
         (when gp ,@body (gf test)))
       test)))

(mac whilet (var test . body)
  (w/uniq (gf gp)
    #`((rfn gf (gp)
         ;; TODO: this can use whenlet, but it's less efficient
         (when gp (let var gp ,@body) (gf test)))
       test)))

#|(mac while (test . body)
  #`(whilet ,(uniq) test . body))|#


;=============================================================================
;  Lists
;=============================================================================

(def reduce (f xs)
  (if (cddr xs)
        (reduce f (cons (f (car xs) (cadr xs)) (cddr xs)))
      (apply f xs)))

#|
; Rtm prefers to overload + to do this
(def join args
  (if (no args)
        nil
      (let a (car args)
        (if (no a)
              (apply join (cdr args))
            (cons (car a) (apply join (cdr a) (cdr args)))))))|#


(def caris (x val)
  (and (cons? x) (is (car x) val)))

(def caddr (x) (car (cddr x)))


(def rev (xs)
  ((afn (xs acc)
     (if (no xs)
           acc
         (self (cdr xs) (cons (car xs) acc))))
   xs nil))

(def nrev (xs)
  (let u nil
    (whilet head xs
      (= xs (cdr xs))
      (scdr head u) ; faster than (= cdr.head u) for various reasons
                    ; actually, it basically shouldn't be faster anymore
                    ; except for an additional call to sref, and conds in
                    ; sref
      (= u head))
    u))


(def reclist (f xs)
  (and xs (or (f xs) (reclist f (cdr xs)))))

(def mem (test seq)
  (let f (testify test)
    (reclist [if (f (car _)) _] seq)))


;=============================================================================
;  Strings
;=============================================================================

;; TODO: sig for newstring
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

;; TODO: compile string and join (so they can be dynamic)
(def +-2 ((o x 0) (o y 0))
  (%if (racket-number? x)
         (racket-+ x y)
       (racket-or (racket-string? x)
                  (racket-char? x))
         (string x y)
       (racket-list? x)
         (join x y)))

#|(def <2 (x y)
  (ac-tnil
    (if (num? x)    (racket-< x y)
        (string? x) (racket-string<? x y)
        (sym? x)    (racket-string<? (string x) (string y))
        (char? x)   (racket-char<? x y)
                    (err "can't <" x y))))|#

;; TODO: compile err
(def <2 (x y)
  (ac-tnil
    (%if (racket-number? x) (racket-< x y)
         (racket-string? x) (racket-string<? x y)
         (racket-symbol? x) (racket-string<? (racket-symbol->string x)
                                             (racket-symbol->string y))
         (racket-char? x)   (racket-char<? x y)
                            (err "can't <" x y))))

;; TODO: compile err
(def >2 (x y)
  (ac-tnil
    (%if (racket-number? x) (racket-> x y)
         (racket-string? x) (racket-string>? x y)
         (racket-symbol? x) (racket-string>? (racket-symbol->string x)
                                             (racket-symbol->string y))
         (racket-char? x)   (racket-char>? x y)
                            (err "can't >" x y))))

;; TODO: tests for sig for these (should be 'args)
(= +  (ac-binary +-2 reduce)
   <  (ac-binary  <2 ac-pairwise)
   >  (ac-binary  >2 ac-pairwise))


;=============================================================================
;  Hash tables
;=============================================================================

(def maptable (fn table)               ; arg is (fn (key value) ...)
  (racket-hash-for-each table fn)
  table)


;=============================================================================
;  Math
;=============================================================================

;; TODO: sig for these
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

;; TODO: sig for ccc
(= ccc  racket-call-with-current-continuation)

(def protect (during after)
  (racket-dynamic-wind (fn () #t) during after))
#|

;=============================================================================
;  Inline fns
;=============================================================================

(mac make-inline-fn (name mac fn)
  #`(safeset name (annotate ''inline-fn (list mac fn))))|#


;=============================================================================
;  Parameters
;=============================================================================

;; TODO: move this elsewhere
(mac =raw (name val)
  #`(sref ,(ac-namespace) val ',name))

;; TODO: function version of make-alias...?
(mac make-alias (name get set)
  ;; TODO: should this be safeset or = or =raw?
  #`(safeset name (annotate alias (list get set))))

(remac alias (name orig)
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


(mac parameterize (x . body)
  `(,% (racket-parameterize ,(map1 (fn ((x y))
                                     ;; TODO: should probably use ac-compile for x
                                     #`((rep x) ,(ac-compile y)))
                                   (pair x))
         ,@(or (ac-args body) nil))))

#|(nomac parameterize (x . body)
  `(racket-parameterize ,(map1 (fn ((x y))
                                 ;; TODO: should probably use ac-compile for x
                                 #`((rep x) ,(ac-compile y)))
                               (pair x))
     ,@(or (ac-args body) nil)))|#

(mac make-w/ (param (o name param))
  (w/uniq (val body)
    #`(mac ,(sym "w/" name) (val . body)
        #`(parameterize (,',param val) . body))))

(mac make-parameter (name param)
  ;; TODO: should call safeset or something (like redefine-warning) somewhere
  #`(do (=raw name (annotate parameter param))
        (make-w/ name)))

(remac parameter (name (o init))
  (w/uniq u
    #`(let u init
        (make-parameter name (racket-make-parameter u))
        u)))


(make-parameter namespace     ac-namespace)
(make-parameter uniq-counter  ac-uniq-counter)
;(make-parameter load-paths*   ac-load-paths*)
;(make-parameter load-suffix*  ac-load-suffix*)

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

#|(def debug args
  (when debug-mode*
    (apply prn (intersperse " " args))))|#


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
  #`(protect (fn () x) (fn () . ys)))


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
                    (after (do . body) (close var))))

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


;; TODO: sig for infile
(= infile     racket-open-input-file)

(def outfile (filename (o append))
  (let flag (if append 'append 'truncate)
    (racket-open-output-file filename #:mode 'text #:exists flag)))


;; TODO: sig for these
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
       (racket-process/ports #f #f (stderr) cmd) ;(racket-list->mlist )
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

;; TODO: sig for these
(= system  racket-system
   rand    racket-random
   memory  racket-current-memory-use
   quit    racket-exit)

; allow Arc to give up root privileges after it
; calls open-socket. thanks, Eli!
;; TODO: get this to work
;;(= setuid (% (get-ffi-obj 'setuid #f (_fun _int -> _int))))
(ac-notimpl setuid)

(def dir ((o name "") (o f))
  ;; TODO: all of this is ew
  (= name (expandpath name))
  ;; TODO: use (or= name "." empty) maybe...?
  (if (empty name) (= name "."))
  ;; TODO: w/cwd...?
  (parameterize (racket-current-directory name)
    (let x (map1 (fn (x)
                   (if (dir-exists x)
                         (string x "/")
                       (string x)))
                 (racket-directory-list) ;(racket-list->mlist )
                 )
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

;; TODO: sig for rmfile
(= rmfile  racket-delete-file)

(def mvfile (old new)
  (racket-rename-file-or-directory old new #t)
  nil)


;=============================================================================
;  Time
;=============================================================================

;; TODO: sig for these
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

;; TODO: sig for these
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
  (if (caris x fn)
        (list* 'fn ;(macex-all (car x))
               (cadr x)
               (map macex-all (cddr x)))
      #|(or (caris x ac-if)
          ;(caris x quote)
          )
        (cons (macex-all (car x))
              (map macex-all (cdr x)))|#
      (caris x if)
        (cons 'if (map macex-all (cdr x)))
      #|(or (caris x quote)
          ;; TODO: ew hardcoding symbols
          ;(caris x 'quote)
          )
        (cons 'quote (cdr x))|#
      (caris x ac-assign-global-raw)
        (list* 'assign
               (cadr (caddr x))
               (map macex-all (cdr (cddr x))))
      (caris x ac-lookup-global-arg)
        (cadr x)
      (caris x ac-quote)
        (list 'quote (macex-all ((car (cadr x)))))
      (caris x ac-nocompile)
        (macex-all (cdr x))
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
  #`(do (% (racket-namespace-require/copy ('racket-quote ('prefix 'racket- x))))
        nil))|#

(mac require (x)
    ;; TODO: use dont
    ;; TODO: figure out a way to use quote rather than racket-quote
  #`(do (racket-namespace-require/copy (% ('racket-quote ('prefix 'racket- x))))
        nil))
