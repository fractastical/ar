;=============================================================================
;  Type coercion
;=============================================================================

(def sym (x) (coerce x 'sym))


;=============================================================================
;  Errors/exceptions
;=============================================================================

(mac errsafe (expr)
  `(on-err (fn (c) nil)
           (fn () ,expr)))


;=============================================================================
;  Variable binding
;=============================================================================

(mac letr (var val . body)
  `(let ,var nil
     (assign ,var ,val)
     ,@body))

(mac rfn (name parms . body)
  `(letr ,name (fn ,parms ,@body)))

(mac afn (parms . body)
  `(rfn self ,parms ,@body))

(mac w/uniq (names . body)
  (if (cons? names)
      `(with ,(apply + nil (map1 (fn (n) (list n '(uniq)))
                             names))
         ,@body)
      `(let ,names (uniq) ,@body)))

(mac withs (parms . body)
  (if (not parms)
      `(do ,@body)
      `(let ,(car parms) ,(cadr parms)
         (withs ,(cddr parms) ,@body))))


;=============================================================================
;  Conditionals
;=============================================================================

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

(mac when (test . body)
  `(if ,test (do ,@body)))

(mac unless (test . body)
  `(if (not ,test) (do ,@body)))

; Destructuring means ambiguity: are pat vars bound in else? (no)

(mac iflet (var expr then . rest)
  (w/uniq gv
    `(let ,gv ,expr
       (if ,gv (let ,var ,gv ,then) ,@rest))))

(mac whenlet (var expr . body)
  `(iflet ,var ,expr (do ,@body)))

(mac aif (expr . body)
  `(let it ,expr
     (if it
         ,@(if (cddr body)
               `(,(car body) (aif ,@(cdr body)))
               body))))

(mac in (x . choices)
  (w/uniq g
    `(let ,g ,x
       (or ,@(map1 (fn (c) `(is ,g ,c)) choices)))))


;=============================================================================
;  Extending
;=============================================================================

(mac defrule (name test . body)
  (let arglist (sig name)
    (w/uniq args
      `(let orig ,name
         (assign ,name
           (fn ,args
             (aif (apply (fn ,arglist ,test) ,args)
                   (apply (fn ,arglist ,@body) ,args)
                   (apply orig ,args))))))))


;; makes apply work on macros

(defrule coerce (and (is type 'fn)
                     (isa x 'mac))
  (fn args
    (eval (apply (rep x) args))))


(defrule ac (caris s 'racket)
  (let x (cadr s)
    (if (isa x 'string)
         (racket-read-from-string x)
         x)))


;=============================================================================
;  Functions
;=============================================================================

(def ac-complex-args? (args)
  (if (not args)
       nil
      (isa args 'sym)
       nil
      (and (cons? args) (isa (car args) 'sym))
       (ac-complex-args? (cdr args))
       t))

(def ac-complex-opt (var expr ra)
  (list (list var `(if (cons? ,ra) (car ,ra) ,expr))))

(def ac-complex-args (args ra)
  (if (not args)
       nil
      (isa args 'sym)
       (list (list args ra))
      (cons? args)
       (withs (a (car args)
               r (cdr args)
               x (if (caris a 'o)
                      (ac-complex-opt (cadr a) (car (cddr a)) ra)
                      (ac-complex-args a (list 'car ra))))
         (join x (ac-complex-args (cdr args) (list 'cdr ra))))
       (err "Can't understand fn arg list" args)))

(def ac-complex-fn (args body)
  (w/uniq ra
    `(fn ,ra
       (withs ,(apply join (ac-complex-args args ra))
         ,@body))))

(defrule ac-fn (ac-complex-args? args)
  (ac (ac-complex-fn args body) env))


;=============================================================================
;  Predicates
;=============================================================================

(def list? (x) (or (not x) (cons? x)))


;=============================================================================
;  Numbers
;=============================================================================

(def 1- (x) (- x 1))
(def 1+ (x) (+ 1 x)) ; to prevent concatenation with strings


;=============================================================================
;  Iteration
;=============================================================================

(mac loop (start test update . body)
  (w/uniq (gfn gparm)
    `(do ,start
         ((rfn ,gfn (,gparm)
            (if ,gparm
                (do ,@body ,update (,gfn ,test))))
          ,test))))

(mac for (v init max . body)
  (w/uniq (gi gm)
    `(with (,v nil ,gi ,init ,gm (1+ ,max))
       (loop (assign ,v ,gi) (< ,v ,gm) (assign ,v (1+ ,v))
         ,@body))))

(def maptable (f table)
  (racket-hash-for-each table f)
  table)

(mac each (var expr . body)
  (w/uniq (gseq gf gv)
    `(let ,gseq ,expr
       (if (list? ,gseq)
             ((rfn ,gf (,gv)
                (when (cons? ,gv)
                  (let ,var (car ,gv) ,@body)
                  (,gf (cdr ,gv))))
              ,gseq)
           (isa ,gseq 'table)
             (maptable (fn ,var ,@body)
                       ,gseq)
             (for ,gv 0 (1- (len ,gseq))
               (let ,var (,gseq ,gv) ,@body))))))


;=============================================================================
;  Map
;=============================================================================

(mac square-bracket body
  `(fn (_) (,@body)))

(def testify (x)
  (if (isa x 'fn) x [is _ x]))

(def reclist (f xs)
  (and xs (or (f xs) (reclist f (cdr xs)))))

(def caddr (x) (car (cddr x)))

(def recstring (test s (o start 0))
  ((afn (i)
     (and (< i (len s))
          (or (test i)
              (self (1+ i)))))
   start))

(mac compose args
  (let g (uniq)
    `(fn ,g
       ,((afn (fs)
           (if (cdr fs)
               (list (car fs) (self (cdr fs)))
               `(apply ,(if (car fs) (car fs) 'idfn) ,g)))
         args))))

(def some (test seq)
  (let f (testify test)
    (if (list? seq)
         (reclist   (compose f car) seq)
         (recstring (compose f seq) seq))))

(def best (f seq)
  (if (not seq)
      nil
      (let wins (car seq)
        (each elt (cdr seq)
          (if (f elt wins) (assign wins elt)))
        wins)))

(def max args (best > args))
(def min args (best < args))

(assign-fn newstring (k (o char)) racket-make-string)

(def map (f . seqs)
  (if (some [isa _ 'string] seqs)
       (withs (n   (apply min (map len seqs))
               new (newstring n))
         ((afn (i)
            (if (is i n)
                new
                (do (sref new (apply f (map [_ i] seqs)) i)
                    (self (1+ i)))))
          0))
      (not (cdr seqs))
       (map1 f (car seqs))
      ((afn (seqs)
        (if (some not seqs)
            nil
            (cons (apply f (map1 car seqs))
                  (self (map1 cdr seqs)))))
       seqs)))

(def mappend (f . args)
  (apply + nil (apply map f args)))


;=============================================================================
;  Strings
;=============================================================================

(def string args
  (apply + "" (map [coerce _ 'string] args)))


;=============================================================================
;  Lists
;=============================================================================

(def rev (xs)
  ((afn (xs acc)
     (if (not xs)
         acc
         (self (cdr xs) (cons (car xs) acc))))
   xs nil))


;=============================================================================
;  Special variable binding
;=============================================================================

(assign dynamic-parameter* (table))
(assign ac-defined-vars* (table))

(def ac-defvar (v x)
  (sref ac-defined-vars* x v)
  nil)

(def ac-defined-var (v)
  (ac-defined-vars* v))

(defrule ac-global (ac-defined-var v)
  `(,(car it)))

(def ac-not-assignable (v)
  (fn (x)
    (err (string v " is not assignable"))))

(defrule ac-global-assign (ac-defined-var a)
  `(,(or (cadr it) (ac-not-assignable a)) ,b))

(mac defvar (name get (o set))
  `(ac-defvar ',name (list ,get ,set)))

(mac make-dynamic (name param)
  (w/uniq paramval
    `(let ,paramval ,param
       (sref dynamic-parameter* ,paramval ',name)
       (defvar ,name (fn () (,paramval)) (fn (val) (,paramval val))))))

(mac paramfor (name)
  `(dynamic-parameter* ',name))

(mac dlet (name val . body)
  `(racket-parameterize (paramfor ,name) ,val (fn () ,@body)))

(mac make-w/ (name)
  (let w/name (sym (+ "w/" name))
    `(mac ,w/name (val . body)
       `(dlet ,',name ,val ,@body))))

(mac make-implicit (name param)
  `(do (make-dynamic ,name ,param)
       (make-w/ ,name)))

(mac parameterize (param val . body)
  `(racket-parameterize ,param ,val (fn () ,@body)))

(mac dynamic (name (o init))
  `(make-dynamic ,name (parameter ,init)))

(mac implicit (name (o init))
  `(make-implicit ,name (parameter ,init)))


;=============================================================================
;  Printing
;=============================================================================

(make-implicit stdin  racket-stdin)
(make-implicit stdout racket-stdout)
(make-implicit stderr racket-stderr)

(def print (primitive x port)
  (primitive x port))

(def disp (x (o port stdout))
  (print racket-disp x port))

(def write (x (o port stdout))
  (print racket-write x port))

(def pr args
  (map1 disp args)
  (car args))

(mac do1 args
  (w/uniq g
    `(let ,g ,(car args)
       ,@(cdr args)
       ,g)))

(def prn args
  (do1 (apply pr args)
       (writec #\newline)))

(def printwith-list (primitive x port)
  (if (not (cdr x))
       (do (print primitive (car x) port)
           (disp ")" port))
      (cons? (cdr x))
       (do (print primitive (car x) port)
           (disp " " port)
           (printwith-list primitive (cdr x) port))
       (do (print primitive (car x) port)
           (disp " . " port)
           (print primitive (cdr x) port)
           (disp ")" port))))

(defrule print (isa x 'cons)
  (disp "(" port)
  (printwith-list primitive x port))


(def printwith-table (primitive x keys port)
  (whenlet n (car keys)
    (disp "(" port)
    (print primitive n port)
    (disp " . " port)
    (print primitive (x n) port)
    (disp ")" port)
    (when (cdr keys)
      (disp "\n      " port)
      (printwith-table primitive x (cdr keys) port))))

;; should move `keys` and `sort` into base.arc

(defrule print (isa x 'table)
  (disp "#hash(" port)
  (printwith-table primitive x (sort < (keys x)) port)
  (disp ")" port))


;=============================================================================
;  Macros
;=============================================================================

(def macex (e (o once))
  (if (cons? e)
       (let m (ac-macro? (car e))
         (if m
              (let expansion (apply m (cdr e))
                (if (not once) (macex expansion) expansion))
              e))
       e))

(def macex1 (e)
  (macex e t))


;=============================================================================
;  Ssyntax
;=============================================================================

(def ac-ssyntax (x)
  (and (isa x 'sym)
       (not (in x '+ '++ '_))
       (some [in _ #\: #\~ #\& #\. #\!] (string x))))


;=============================================================================
;  Setforms
;=============================================================================

; Note: if expr0 macroexpands into any expression whose car doesn't
; have a setter, setforms assumes it's a data structure in functional
; position.  Such bugs will be seen only when the code is executed, when
; sref complains it can't set a reference to a function.

(def setforms (expr0)
  (let expr (macex expr0)
    (if (isa expr 'sym)
         (if (ac-ssyntax expr)
             (setforms (ssexpand expr))
             (w/uniq (g h)
               (list (list g expr)
                     g
                     `(fn (,h) (assign ,expr ,h)))))
        ; make it also work for uncompressed calls to compose
        (and (cons? expr) (metafn (car expr)))
         (setforms (expand-metafn-call (ssexpand (car expr)) (cdr expr)))
        (and (cons? expr) (cons? (car expr)) (is (caar expr) 'get))
         (setforms (list (cadr expr) (cadr (car expr))))
         (let f (setter (car expr))
           (if f
               (f expr)
               ; assumed to be data structure in fn position
               (do (when (caris (car expr) 'fn)
                     (warn "Inverting what looks like a function call"
                           expr0 expr))
                   (w/uniq (g h)
                     (let argsyms (map [uniq] (cdr expr))
                        (list (+ (list g (car expr))
                                 (mappend list argsyms (cdr expr)))
                              `(,g ,@argsyms)
                              `(fn (,h) (sref ,g ,h ,(car argsyms))))))))))))


;=============================================================================
;  Continuations
;=============================================================================

(def protect (during after)
  (racket (racket-dynamic-wind (racket-lambda () #t) during after)))

(mac after (x . ys)
  `(protect (fn () ,x) (fn () ,@ys)))


;=============================================================================
;  Threads
;=============================================================================

(def make-semaphore ((o init 0))
  (racket-make-semaphore init))

(def call-with-semaphore (sema func)
  ((racket call-with-semaphore) sema (fn () (func))))

(def nil->racket-false (x)
  (if (not x) (racket "#f") x))

(def make-thread-cell (v (o preserved))
  (racket-make-thread-cell v (nil->racket-false preserved)))

(def thread-cell-ref (cell)
  (racket-thread-cell-ref cell))

(def thread-cell-set (cell v)
  ((racket racket-thread-cell-set!) cell v))

(assign ar-the-sema (make-semaphore 1))

(assign ar-sema-cell (make-thread-cell nil))

(def atomic-invoke (f)
  (if (thread-cell-ref ar-sema-cell)
       (f)
       (do (thread-cell-set ar-sema-cell t)
           (after
             (racket-call-with-semaphore ar-the-sema f)
             (thread-cell-set ar-sema-cell nil)))))

(mac atomic body
  `(atomic-invoke (fn () ,@body)))

(mac atlet args
  `(atomic (let ,@args)))

(mac atwith args
  `(atomic (with ,@args)))

(mac atwiths args
  `(atomic (withs ,@args)))


;=============================================================================
;  Input
;=============================================================================

(def racket-true (x)
  (racket (racket-if x (racket-quote t) (racket-quote nil))))

(def sread (p eof)
  (let v (racket-read p)
    (if (racket-true (racket-eof-object? v))
         eof
         (ar-toarc v))))

(def read ((o x stdin) (o eof nil))
  (if (isa x 'string) (readstring1 x eof) (sread x eof)))

(mac push (x place)
  (w/uniq gx
    (let (binds val setter) (setforms place)
      `(let ,gx ,x
         (atwiths ,binds
           (,setter (cons ,gx ,val)))))))

(mac accum (accfn . body)
  (w/uniq gacc
    `(withs (,gacc nil ,accfn [push _ ,gacc])
       ,@body
       (rev ,gacc))))

(mac collect args
  `(accum yield ,@args))

(mac xloop (withses . body)
  (let w (pair withses)
    `((rfn next ,(map1 car w) ,@body) ,@(map1 cadr w))))

(def readline ((o s stdin))
  (aif (readc s)
    (coerce
      (collect (xloop (c it)
        (if (is c #\return)
             (if (is (peekc s) #\newline)
                  (readc s))
            (is c #\newline)
             nil
             (do (yield c)
                 (aif (readc s)
                       (next it))))))
     'string)))

#|(def readline ((o x stdin))
  (string (collect
    ((afn ()
       (whenlet c (readc x)
         (yield c)
         (unless (is c #\newline)
           (self))))))))|#

#|(unless (or (is c #\newline)
                     (and (is c #\return)
                          (is (peekc x) #\newline)))|#


;=============================================================================
;  Assignment
;=============================================================================

(def expand= (place val)
  (if (and (isa place 'sym) (not (ac-ssyntax place)))
      `(assign ,place ,val)
      (let (vars prev setter) (setforms place)
        (w/uniq g
          `(atwith ,(+ vars (list g val))
             (,setter ,g))))))

(def expand=list (terms)
  `(do ,@(map (fn ((p v)) (expand= p v))  ; [apply expand= _]
                  (pair terms))))

(mac = args
  (expand=list args))
