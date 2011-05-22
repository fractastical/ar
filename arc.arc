;=============================================================================
;  Racket
;=============================================================================

; wish I could use racket-require
(mac rckt-require (x)
  `(racket (racket-require (racket-prefix-in racket- ,x))))


(def atom (x) (not (cons? x)))

(def idfn (x) x)

(def assoc (key al)
  (if (atom al)
       nil
      (and (cons? (car al)) (is (caar al) key))
       (car al)
      (assoc key (cdr al))))

(def alref (al key) (cadr (assoc key al)))

(mac complement (f)
  (let g (uniq)
    `(fn ,g (not (apply ,f ,g)))))

(def isnt (x y) (not (is x y)))

(def iso (x y)
  (or (is x y)
      (and (cons? x)
           (cons? y)
           (iso (car x) (car y))
           (iso (cdr x) (cdr y)))))

(mac while (test . body)
  (w/uniq (gf gp)
    `((rfn ,gf (,gp)
        (when ,gp ,@body (,gf ,test)))
      ,test)))

; Repeatedly evaluates its body till it returns nil, then returns vals.

(mac drain (expr (o eof nil))
  (w/uniq (gacc gdone gres)
    `(with (,gacc nil ,gdone nil)
       (while (not ,gdone)
         (let ,gres ,expr
           (if (is ,gres ,eof)
               (= ,gdone t)
               (push ,gres ,gacc))))
       (rev ,gacc))))

; For the common C idiom while x = snarfdata != stopval.
; Rename this if use it often.

(mac whiler (var expr endval . body)
  (w/uniq gf
    `(withs (,var nil ,gf (testify ,endval))
       (while (not (,gf (= ,var ,expr)))
         ,@body))))

(def empty (seq)
  (or (not seq)
      (is seq '||)
      (and (in (type seq) 'string 'table)
           (is (len seq) 0))))

(def none (test seq)
  (not (some test seq)))

(def all (test seq)
  ((complement some) (complement (testify test)) seq))

(def mem (test seq)
  (let f (testify test)
    (reclist [if (f (car _)) _] seq)))


(def int (x (o b 10))
  (coerce x 'int b))

(mac w/outstring (var . body)
  `(let ,var (outstring) ,@body))

(mac tostring body
  (w/uniq gv
   `(w/outstring ,gv
      (w/stdout ,gv ,@body)
      (inside ,gv))))

(mac erp (x)
  (w/uniq (gx)
    `(let ,gx ,x
       (w/stdout stderr
         (write ',x)
         (disp ": ")
         (write ,gx)
         (disp #\newline))
       ,gx)))

(def ac-symbol->chars (x)
  (coerce (coerce x 'string) 'cons))

(def ac-tokens (test source token acc keepsep?)
  (if (not source)
       (rev (if (cons? token)
                 (cons (rev token) acc)
                 acc))
      (test (car source))
       (ac-tokens test
                  (cdr source)
                  '()
                  (let rec (if (not token)
                                acc
                                (cons (rev token) acc))
                    (if keepsep?
                         (cons (car source) rec)
                         rec))
                  keepsep?)
       (ac-tokens test
                  (cdr source)
                  (cons (car source) token)
                  acc
                  keepsep?)))

(assign-fn ccc (k) racket-call-with-current-continuation)

(mac point (name . body)
  (w/uniq (g p)
    `(ccc (fn (,g)
            (let ,name (fn ((o ,p)) (,g ,p))
              ,@body)))))

(mac catch body
  `(point throw ,@body))

(mac inline (x)
  `',(eval x))

(rckt-require scheme/system)

(def system (cmd)
  (racket-system cmd)
  nil)

(mac caselet (var expr . args)
  (let ex (afn (args)
            (if (not (cdr args))
                (car args)
                `(if (is ,var ',(car args))
                     ,(cadr args)
                     ,(self (cddr args)))))
    `(let ,var ,expr ,(ex args))))

(mac case (expr . args)
  `(caselet ,(uniq) ,expr ,@args))

;; todo try-custodian redefined in io.arc

(def try-custodian (port))

(rckt-require scheme/tcp)

(def close ports
  (each port ports
    (case (type port)
      input  (racket-close-input-port port)
      output (racket-close-output-port port)
      socket (racket-tcp-close port)
             (err "Can't close " port))
    (try-custodian port)))

(dynamic infile racket-open-input-file)
(sref sig '(name) 'infile)

(def outfile (filename (o append))
  (let flag (if append 'append 'truncate)
    (racket (racket-open-output-file filename #:mode (racket-quote text) #:exists flag))))

(def open-socket (port)
  ((inline ((racket-module-ref 'scheme/tcp) 'tcp-listen)) port 50 (racket "#t")))


;=============================================================================
;  Input
;=============================================================================

(let expander
     (fn (f var name body)
       `(let ,var (,f ,name)
          (after (do ,@body) (close ,var))))

  (mac w/infile (var name . body)
    (expander 'infile var name body))

  (mac w/outfile (var name . body)
    (expander 'outfile var name body))

  (mac w/instring (var str . body)
    (expander 'instring var str body))

  (mac w/socket (var port . body)
    (expander 'open-socket var port body))
  )

(mac w/appendfile (var name . body)
  `(let ,var (outfile ,name 'append)
     (after (do ,@body) (close ,var))))

(def readstring1 (s (o eof nil)) (w/instring i s (read i eof)))

(def readlines (x)
  (collect (whiler v (readline x) nil
    (yield v))))


;=============================================================================
;  Ssyntax
;=============================================================================

(def ac-chars->value (x)
  (read (coerce x 'string)))

(def ac-expand-compose (sym)
  (let elts (map1 (fn (tok)
                    (if (is (car tok) #\~)
                         (if (not (cdr tok))
                             'not
                             `(complement ,(ac-chars->value (cdr tok))))
                         (ac-chars->value tok)))
                  (ac-tokens [is _ #\:] (ac-symbol->chars sym) nil nil nil))
    (if (not (cdr elts))
         (car elts)
         (cons 'compose elts))))

(def ac-expand-ssyntax (sym)
  (err "Unknown ssyntax" sym))

(defrule ac (ac-ssyntax s)
  (ac (ac-expand-ssyntax s) env))

(def ac-insym? (char sym)
  (mem char (ac-symbol->chars sym)))

(defrule ac-expand-ssyntax (or (ac-insym? #\: sym) (ac-insym? #\~ sym))
  (ac-expand-compose sym))

(def ac-build-sexpr (toks orig)
  (if (not toks)
       'get
      (not (cdr toks))
       (ac-chars->value (car toks))
       (list (ac-build-sexpr (cddr toks) orig)
             (if (is (cadr toks) #\!)
                  (list 'quote (ac-chars->value (car toks)))
                  (if (in (car toks) #\. #\!)
                       (err "Bad ssyntax" orig)
                       (ac-chars->value (car toks)))))))

(def ac-expand-sexpr (sym)
  (ac-build-sexpr (rev (ac-tokens [in _ #\. #\!] (ac-symbol->chars sym) nil nil t))
                  sym))

(defrule ac-expand-ssyntax (or (ac-insym? #\. sym) (ac-insym? #\! sym))
  (ac-expand-sexpr sym))

(assign cdar cdr:car)

(def ac-andf (s env)
  (ac (let gs (map1 [uniq] (cdr s))
        `((fn ,gs
            (and ,@(map1 (fn (f) `(,f ,@gs))
                         (cdar s))))
          ,@(cdr s)))
      env))

(def xcar (x) (and (cons? x) (car x)))

(defrule ac (is (xcar:xcar s) 'andf) (ac-andf s env))

(def ac-expand-and (sym)
  (let elts (map1 ac-chars->value
                  (ac-tokens [is _ #\&] (ac-symbol->chars sym) nil nil nil))
    (if (not (cdr elts))
         (car elts)
         (cons 'andf elts))))

(defrule ac (ac-ssyntax (xcar s))
  (ac (cons (ac-expand-ssyntax (car s)) (cdr s)) env))

(defrule ac-expand-ssyntax (ac-insym? #\& sym) (ac-expand-and sym))

; (and:or 3) => ((compose and or) 3) => (and (or 3))

(def ac-decompose (fns args)
  (if (not fns)
       `((fn vals (car vals)) ,@args)
      (not (cdr fns))
       (cons (car fns) args)
       (list (car fns) (ac-decompose (cdr fns) args))))

(defrule ac (is (xcar:xcar s) 'compose)
  (ac (ac-decompose (cdar s) (cdr s)) env))

(def cadar (x) (car (cdar x)))

; (~and 3 nil) => ((complement and) 3 nil) => (not (and 3 nil))

(defrule ac (is (xcar:xcar s) 'complement)
  (ac (list 'not (cons (cadar s) (cdr s))) env))

(def scar (x val)
  (sref x val 0))

(def scdr (x val)
  ((racket racket-set-mcdr!) x val))

(def warn (msg . args)
  (w/stdout stderr
    (disp (+ "Warning: " msg " "))
    (map [do (write _) (disp " ")] args)
    (writec #\newline)
    nil))

(def firstn (n xs)
  (if (not n)           xs
      (and (> n 0) xs)  (cons (car xs) (firstn (1- n) (cdr xs)))
                        nil))

(def nthcdr (n xs)
  (if (not n) xs
      (> n 0) (nthcdr (1- n) (cdr xs))
              xs))

; Generalization of pair: (tuples x) = (pair x)

(def tuples (xs (o n 2))
  (if (not xs)
      nil
      (cons (firstn n xs)
            (tuples (nthcdr n xs) n))))


;=============================================================================
;  Setforms
;=============================================================================

(assign setter (table))

(mac defset (name parms . body)
  (w/uniq gexpr
    `(sref setter
           (fn (,gexpr)
             (let ,parms (cdr ,gexpr)
               ,@body))
           ',name)))

(defset car (x)
  (w/uniq g
    (list (list g x)
          `(car ,g)
          `(fn (val) (scar ,g val)))))

(defset cdr (x)
  (w/uniq g
    (list (list g x)
          `(cdr ,g)
          `(fn (val) (scdr ,g val)))))

(defset caar (x)
  (w/uniq g
    (list (list g x)
          `(caar ,g)
          `(fn (val) (scar (car ,g) val)))))

(defset cadr (x)
  (w/uniq g
    (list (list g x)
          `(cadr ,g)
          `(fn (val) (scar (cdr ,g) val)))))

(defset cddr (x)
  (w/uniq g
    (list (list g x)
          `(cddr ,g)
          `(fn (val) (scdr (cdr ,g) val)))))

(def ssexpand (x)
  (if (isa x 'sym) (ac-expand-ssyntax x) x))

(def metafn (x)
  (or (ac-ssyntax x)
      (and (cons? x) (in (car x) 'compose 'complement))))

(def expand-metafn-call (f args)
  (if (is (car f) 'compose)
       ((afn (fs)
          (if (caris (car fs) 'compose)            ; nested compose
               (self (join (cdr (car fs)) (cdr fs)))
              (cdr fs)
               (list (car fs) (self (cdr fs)))
              (cons (car fs) args)))
        (cdr f))
      (is (car f) 'not)
       (err "Can't invert " (cons f args))
       (cons f args)))

(mac down (v init min . body)
  (w/uniq (gi gm)
    `(with (,v nil ,gi ,init ,gm (1- ,min))
       (loop (assign ,v ,gi) (> ,v ,gm) (assign ,v (1- ,v))
         ,@body))))

(mac repeat (n . body)
  `(for ,(uniq) 1 ,n ,@body))

; (nthcdr x y) = (cut y x).

(def cut (seq start (o end))
  (let end (if (not end)  (len seq)
               (< end 0)  (+ (len seq) end)
                          end)
    (if (isa seq 'string)
        (let s2 (newstring (- end start))
          (for i 0 (- end start 1)
            (= (s2 i) (seq (+ start i))))
          s2)
        (firstn (- end start) (nthcdr start seq)))))

(mac whilet (var test . body)
  (w/uniq (gf gp)
    `((rfn ,gf (,gp)
        (let ,var ,gp
          (when ,var ,@body (,gf ,test))))
      ,test)))

(def last (xs)
  (if (cons? xs)
        (if (cdr xs)
              (last (cdr xs))
            (car xs))
        (xs (1- (len xs)))))

(def rem (test seq)
  (let f (testify test)
    (if (list? seq)
        ((afn (s)
           (if (not s)      nil
               (f (car s))  (self (cdr s))
                            (cons (car s) (self (cdr s)))))
          seq)
        (coerce (rem test (coerce seq 'cons)) 'string))))

; Seems like keep doesn't need to testify-- would be better to
; be able to use tables as fns.  But rem does need to, because
; often want to rem a table from a list.  So maybe the right answer
; is to make keep the more primitive, not rem.

(def keep (test seq)
  (rem (complement (testify test)) seq))

;(def trues (f seq)
;  (rem nil (map f seq)))

(def trues (f xs)
  (and xs
      (let fx (f (car xs))
        (if fx
            (cons fx (trues f (cdr xs)))
            (trues f (cdr xs))))))

(mac swap (place1 place2)
  (w/uniq (g1 g2)
    (with ((binds1 val1 setter1) (setforms place1)
           (binds2 val2 setter2) (setforms place2))
      `(atwiths ,(+ binds1 (list g1 val1) binds2 (list g2 val2))
         (,setter1 ,g2)
         (,setter2 ,g1)))))

(mac rotate places
  (with (vars (map [uniq] places)
         forms (map setforms places))
    `(atwiths ,(mappend (fn (g (binds val setter))
                          (+ binds (list g val)))
                        vars
                        forms)
       ,@(map (fn (g (binds val setter))
                (list setter g))
              (+ (cdr vars) (list (car vars)))
              forms))))

(mac pop (place)
  (w/uniq g
    (let (binds val setter) (setforms place)
      `(atwiths ,(+ binds (list g val))
         (do1 (car ,g)
              (,setter (cdr ,g)))))))

(def adjoin (x xs (o test iso))
  (if (some [test x _] xs)
      xs
      (cons x xs)))

(mac pushnew (x place . args)
  (w/uniq gx
    (let (binds val setter) (setforms place)
      `(atwiths ,(+ (list gx x) binds)
         (,setter (adjoin ,gx ,val ,@args))))))

(mac pull (test place)
  (w/uniq g
    (let (binds val setter) (setforms place)
      `(atwiths ,(+ (list g test) binds)
         (,setter (rem ,g ,val))))))

(mac togglemem (x place . args)
  (w/uniq gx
    (let (binds val setter) (setforms place)
      `(atwiths ,(+ (list gx x) binds)
         (,setter (if (mem ,gx ,val)
                      (rem ,gx ,val)
                      (adjoin ,gx ,val ,@args)))))))

(mac ++ (place (o i 1))
  (if (isa place 'sym)
      `(= ,place (+ ,place ,i))
      (w/uniq gi
        (let (binds val setter) (setforms place)
          `(atwiths ,(+ binds (list gi i))
             (,setter (+ ,val ,gi)))))))

(mac -- (place (o i 1))
  (if (isa place 'sym)
      `(= ,place (- ,place ,i))
      (w/uniq gi
        (let (binds val setter) (setforms place)
          `(atwiths ,(+ binds (list gi i))
             (,setter (- ,val ,gi)))))))

; E.g. (++ x) equiv to (zap + x 1)

(mac zap (op place . args)
  (with (gop    (uniq)
         gargs  (map [uniq] args)
         mix    (afn seqs
                  (if (some not seqs)
                      nil
                      (+ (map car seqs)
                         (apply self (map cdr seqs))))))
    (let (binds val setter) (setforms place)
      `(atwiths ,(+ binds (list gop op) (mix gargs args))
         (,setter (,gop ,val ,@gargs))))))

(def prt args
  (map1 [if _ (disp _)] args)
  (car args))

(mac wipe args
  `(do ,@(map (fn (a) `(= ,a nil)) args)))

(mac set args
  `(do ,@(map (fn (a) `(= ,a t)) args)))

(mac awhen (expr . body)
  `(let it ,expr (if it (do ,@body))))

(mac aand args
  (if (not args)
      't
      (not (cdr args))
       (car args)
      `(let it ,(car args) (and it (aand ,@(cdr args))))))

(def consif (x y) (if x (cons x y) y))

(def flat x
  ((afn (x acc)
     (if (not x)  acc
         (atom x) (cons x acc)
                  (self (car x) (self (cdr x) acc))))
   x nil))

(mac check (x test (o alt))
  (w/uniq gx
    `(let ,gx ,x
       (if (,test ,gx) ,gx ,alt))))

(def pos (test seq (o start 0))
  (let f (testify test)
    (if (list? seq)
        ((afn (seq n)
           (if (not seq)
                nil
               (f (car seq))
                n
               (self (cdr seq) (1+ n))))
         (nthcdr start seq)
         start)
        (recstring [if (f (seq _)) _] seq start))))

(def mod (n m)
  (racket-modulo n m))

(def even (n) (is (mod n 2) 0))

(def odd (n) (not (even n)))

(mac fromstring (str . body)
  (w/uniq gv
   `(w/instring ,gv ,str
      (w/stdin ,gv ,@body))))

(def readfile (name) (w/infile s name (drain (read s))))

(def readfile1 (name) (w/infile s name (read s)))

(def readall (src (o eof nil))
  ((afn (i)
    (let x (read i eof)
      (if (is x eof)
          nil
          (cons x (self i)))))
   (if (isa src 'string) (instring src) src)))

(def allchars (str)
  (tostring (whiler c (readc str nil) not
              (writec c))))

(def filechars (name)
  (w/infile s name (allchars s)))

(def mvfile (old new)
  (racket-rename-file-or-directory old new (racket "#t"))
  nil)

(def writefile (val file)
  (let tmpfile (+ file ".tmp")
    (w/outfile o tmpfile (write val o))
    (mvfile tmpfile file))
  val)

(dynamic rand racket-random)
(= (sig 'rand) '((o n)))

(mac rand-choice exprs
  `(case (rand ,(len exprs))
     ,@(let key -1
         (mappend [list (++ key) _]
                  exprs))))

(mac n-of (n expr)
  (w/uniq ga
    `(let ,ga nil
       (repeat ,n (push ,expr ,ga))
       (rev ,ga))))

(def aracket-false (x)
  (is x (racket "#f")))

(def aracket-true (x)
  (not (aracket-false x)))

(def aracket-eof (x)
  (aracket-true (racket-eof-object? x)))

(def readb ((o str stdin))
  (let c (racket-read-byte str)
    (if (aracket-eof c) nil c)))

(def writeb (b (o str stdout))
  (racket-write-byte b str))

; rejects bytes >= 248 lest digits be overrepresented

(def rand-string (n)
  (let c "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
    (with (nc 62 s (newstring n) i 0)
      (w/infile str "/dev/urandom"
        (while (< i n)
          (let x (readb str)
             (unless (> x 247)
               (= (s i) (c (mod x nc)))
               (++ i)))))
      s)))

(mac forlen (var s . body)
  `(for ,var 0 (1- (len ,s)) ,@body))

(mac on (var s . body)
  (if (is var 'index)
      (err "Can't use index as first arg to on.")
      (w/uniq gs
        `(let ,gs ,s
           (forlen index ,gs
             (let ,var (,gs index)
               ,@body))))))

(def most (f seq)
  (unless (not seq)
    (withs (wins (car seq) topscore (f wins))
      (each elt (cdr seq)
        (let score (f elt)
          (if (> score topscore) (= wins elt topscore score))))
      wins)))

(def insert-sorted (test elt seq)
  (if (not seq)
       (list elt)
      (test elt (car seq))
       (cons elt seq)
      (cons (car seq) (insert-sorted test elt (cdr seq)))))

(mac insort (test elt seq)
  `(zap [insert-sorted ,test ,elt _] ,seq))

(def reinsert-sorted (test elt seq)
  (if (not seq)
       (list elt)
      (is elt (car seq))
       (reinsert-sorted test elt (cdr seq))
      (test elt (car seq))
       (cons elt (rem elt seq))
      (cons (car seq) (reinsert-sorted test elt (cdr seq)))))

(mac insortnew (test elt seq)
  `(zap [reinsert-sorted ,test ,elt _] ,seq))

(def memo (f)
  (with (cache (table) nilcache (table))
    (fn args
      (or (cache args)
          (and (not (nilcache args))
               (aif (apply f args)
                    (= (cache args) it)
                    (do (set (nilcache args))
                        nil)))))))

(mac defmemo (name parms . body)
  `(safeset ,name (memo (fn ,parms ,@body))))

(def <= args
  (or (not args)
      (not (cdr args))
      (and (not (> (car args) (cadr args)))
           (apply <= (cdr args)))))

(def >= args
  (or (not args)
      (not (cdr args))
      (and (not (< (car args) (cadr args)))
           (apply >= (cdr args)))))

(def whitec (c)
  (in c #\space #\newline #\tab #\return))

(def nonwhite (c) (not (whitec c)))

(def letter (c) (or (<= #\a c #\z) (<= #\A c #\Z)))

(def digit (c) (<= #\0 c #\9))

(def alphadig (c) (or (letter c) (digit c)))

(def punc (c)
  (in c #\. #\, #\; #\: #\! #\?))

(mac summing (sumfn . body)
  (w/uniq (gc gt)
    `(let ,gc 0
       (let ,sumfn (fn (,gt) (if ,gt (++ ,gc)))
         ,@body)
       ,gc)))

(def sum (f xs)
  (let n 0
    (each x xs (++ n (f x)))
    n))

(def treewise (f base tree)
  (if (atom tree)
      (base tree)
      (f (treewise f base (car tree))
         (treewise f base (cdr tree)))))

(def prall (elts (o init "") (o sep ", "))
  (when elts
    (pr init (car elts))
    (map [pr sep _] (cdr elts))
    elts))

(def prs args
  (prall args "" #\space))

(def tree-subst (old new tree)
  (if (is tree old)
       new
      (atom tree)
       tree
      (cons (tree-subst old new (car tree))
            (tree-subst old new (cdr tree)))))

(def ontree (f tree)
  (f tree)
  (unless (atom tree)
    (ontree f (car tree))
    (ontree f (cdr tree))))

(def dotted (x)
  (if (atom x)
      nil
      (and (cdr x) (or (atom (cdr x))
                       (dotted (cdr x))))))

(def fill-table (table data)
  (each (k v) (pair data) (= (table k) v))
  table)

(def keys (h)
  (collect:each (k v) h (yield k)))

(def vals (h)
  (collect:each (k v) h (yield v)))

(def tablist (h)
  (collect:maptable (fn args (yield args)) h))

(def listtab (al)
  (let h (table)
    (map (fn ((k v)) (= (h k) v))
         al)
    h))

(mac obj args
  `(listtab (list ,@(map (fn ((k v))
                           `(list ',k ,v))
                         (pair args)))))

(def read-table ((o i stdin) (o eof))
  (let e (read i eof)
    (if (list? e) (listtab e) e)))

(def load-table (file (o eof))
  (w/infile i file (read-table i eof)))

(def save-table (h file)
  (writefile (tablist h) file))

(def write-table (h (o o stdout))
  (write (tablist h) o))

(def copy (x . args)
  (let x2 (case (type x)
            sym    x
            cons   (apply (fn args args) x)
            string (let new (newstring (len x))
                     (forlen i x
                       (= (new i) (x i)))
                     new)
            table  (let new (table)
                     (each (k v) x
                       (= (new k) v))
                     new)
                   (err "Can't copy " x))
    (map (fn ((k v)) (= (x2 k) v))
         (pair args))
    x2))

(def abs (n)
  (if (< n 0) (- n) n))

(def trunc (x)
  (racket-inexact->exact (racket-truncate x)))

(def round (n)
  (withs (base (trunc n) rem (abs (- n base)))
    (if (> rem 1/2) ((if (> n 0) + -) base 1)
        (< rem 1/2) base
        (odd base)  ((if (> n 0) + -) base 1)
                    base)))

(def roundup (n)
  (withs (base (trunc n) rem (abs (- n base)))
    (if (>= rem 1/2)
        ((if (> n 0) + -) base 1)
        base)))

(def nearest (n quantum)
  (* (roundup (/ n quantum)) quantum))

(def avg (ns) (/ (apply + ns) (len ns)))

(def med (ns (o test >))
  ((sort test ns) (round (/ (len ns) 2))))

; Use mergesort on assumption that mostly sorting mostly sorted lists
; benchmark: (let td (n-of 10000 (rand 100)) (time (sort < td)) 1)

(def sort (test seq)
  (if (list? seq)
      (mergesort test (copy seq))
      (coerce (mergesort test (coerce seq 'cons)) (type seq))))

; Destructive stable merge-sort, adapted from slib and improved
; by Eli Barzilay for MzLib; re-written in Arc.

(def mergesort (less? lst)
  (with (n (len lst))
    (if (<= n 1) lst
        ; ; check if the list is already sorted
        ; ; (which can be a common case, eg, directory lists).
        ; (let loop ([last (car lst)] [next (cdr lst)])
        ;   (or (null? next)
        ;       (and (not (less? (car next) last))
        ;            (loop (car next) (cdr next)))))
        ; lst
        ((afn (n)
           (if (> n 2)
                ; needs to evaluate L->R
                (withs (j (/ (if (even n) n (1- n)) 2) ; faster than round
                        a (self j)
                        b (self (- n j)))
                  (merge less? a b))
               ; the following case just inlines the length 2 case,
               ; it can be removed (and use the above case for n>1)
               ; and the code still works, except a little slower
               (is n 2)
                (with (x (car lst) y (cadr lst) p lst)
                  (= lst (cddr lst))
                  (when (less? y x) (scar p y) (scar (cdr p) x))
                  (scdr (cdr p) nil)
                  p)
               (is n 1)
                (with (p lst)
                  (= lst (cdr lst))
                  (scdr p nil)
                  p)
               nil))
         n))))

; Also by Eli.

(def merge (less? x y)
  (if (not x) y
      (not y) x
      (let lup nil
        (assign lup
                (fn (r x y r-x?) ; r-x? for optimization -- is r connected to x?
                  (if (less? (car y) (car x))
                    (do (if r-x? (scdr r y))
                        (if (cdr y) (lup y x (cdr y) nil) (scdr y x)))
                    ; (car x) <= (car y)
                    (do (if (not r-x?) (scdr r x))
                        (if (cdr x) (lup x (cdr x) y t) (scdr x y))))))
        (if (less? (car y) (car x))
          (do (if (cdr y) (lup y x (cdr y) nil) (scdr y x))
              y)
          ; (car x) <= (car y)
          (do (if (cdr x) (lup x (cdr x) y t) (scdr x y))
              x)))))

(def bestn (n f seq)
  (firstn n (sort f seq)))

(def split (seq pos)
  (list (cut seq 0 pos) (cut seq pos)))

(implicit msec racket-current-milliseconds)
(= (sig 'msec nil))

(mac time (expr)
  (w/uniq (t1 t2)
    `(let ,t1 (msec)
       (do1 ,expr
            (let ,t2 (msec)
              (prn "time: " (- ,t2 ,t1) " msec."))))))

(mac jtime (expr)
  `(do1 'ok (time ,expr)))

(mac time10 (expr)
  `(time (repeat 10 ,expr)))

(def union (f xs ys)
  (+ xs (rem (fn (y) (some [f _ y] xs))
             ys)))

(def carif (x) (if (atom x) x (car x)))

(= templates* (table))

(mac deftem (tem . fields)
  (withs (name (carif tem) includes (if (cons? tem) (cdr tem)))
    `(= (templates* ',name)
        (+ (mappend templates* ',(rev includes))
           (list ,@(map (fn ((k v)) `(list ',k (fn () ,v)))
                        (pair fields)))))))

(mac addtem (name . fields)
  `(= (templates* ',name)
      (union (fn (x y) (is (car x) (car y)))
             (list ,@(map (fn ((k v)) `(list ',k (fn () ,v)))
                          (pair fields)))
             (templates* ',name))))

(def inst (tem . args)
  (let x (table)
    (each (k v) (if (cons? tem) tem (templates* tem))
      (unless (not v) (= (x k) (v))))
    (each (k v) (pair args)
      (= (x k) v))
    x))

; To write something to be read by temread, (write (tablist x))

(def temread (tem (o str (stdin)))
  (templatize tem (read str)))

; Converts list? to inst; ugly; maybe should make this part of coerce.
; Note: discards fields not defined by the template.

(def templatize (tem raw)
  (with (x (inst tem) fields (if (cons? tem) tem (templates* tem)))
    (each (k v) raw
      (when (assoc k fields)
        (= (x k) v)))
    x))

(def temload (tem file)
  (w/infile i file (temread tem i)))

(def temloadall (tem file)
  (map (fn (pairs) (templatize tem pairs))
       (w/infile in file (readall in))))

(def number (n) (in (type n) 'int 'num))

(implicit seconds racket-current-seconds)
(= (sig 'seconds nil))

(def since (t1) (- (seconds) t1))

(def minutes-since (t1) (/ (since t1) 60))
(def hours-since (t1)   (/ (since t1) 3600))
(def days-since (t1)    (/ (since t1) 86400))

; could use a version for fns of 1 arg at least

(def cache (timef valf)
  (with (cached nil gentime nil)
    (fn ()
      (unless (and cached (< (since gentime) (timef)))
        (= cached  (valf)
           gentime (seconds)))
      cached)))

(mac defcache (name lasts . body)
  `(safeset ,name (cache (fn () ,lasts)
                         (fn () ,@body))))

(def saferead (arg) (errsafe:read arg))

(def safe-load-table (filename)
  (or (errsafe:load-table filename)
      (table)))

(def file-exists (path)
  (if (racket-true (racket-file-exists? path)) path))

(def dir-exists (path)
  (if (racket-true (racket-directory-exists? path)) path))

(def ensure-dir (path)
  (unless (dir-exists path)
    (system (string "mkdir -p " path))))

(def timedate ((o seconds (seconds)))
  (let d (racket-seconds->date seconds)
    (map [_ d] (list racket-date-second
                     racket-date-minute
                     racket-date-hour
                     racket-date-day
                     racket-date-month
                     racket-date-year))))

(def date ((o s (seconds)))
  (rev (nthcdr 3 (timedate s))))

(def datestring ((o s (seconds)))
  (let (y m d) (date s)
    (string y "-" (if (< m 10) "0") m "-" (if (< d 10) "0") d)))

(def count (test x)
  (with (n 0 testf (testify test))
    (each elt x
      (if (testf elt) (++ n)))
    n))

(def ellipsize (str (o limit 80))
  (if (<= (len str) limit)
      str
      (+ (cut str 0 limit) "...")))

(def rand-elt (seq)
  (seq (rand (len seq))))

(mac until (test . body)
  `(while (not ,test) ,@body))

(def before (x y seq (o i 0))
  (with (xp (pos x seq i) yp (pos y seq i))
    (and xp (or (not yp) (< xp yp)))))

(def orf fns
  (fn args
    ((afn (fs)
       (and fs (or (apply (car fs) args) (self (cdr fs)))))
     fns)))

(def andf fns
  (fn args
    ((afn (fs)
       (if (not fs)       t
           (not (cdr fs)) (apply (car fs) args)
                          (and (apply (car fs) args) (self (cdr fs)))))
     fns)))

(def atend (i s)
  (> i (- (len s) 2)))

(def multiple (x y)
  (is 0 (mod x y)))

(mac nor args `(not (or ,@args)))

(def compare (comparer scorer)
  (fn (x y) (comparer (scorer x) (scorer y))))

(def only (f)
  (fn args (if (car args) (apply f args))))

(def retrieve (n f xs)
  (if (not n)                (keep f xs)
      (or (<= n 0) (not xs)) nil
      (f (car xs))           (cons (car xs) (retrieve (1- n) f (cdr xs)))
                             (retrieve n f (cdr xs))))

(def dedup (xs)
  (with (h (table) acc nil)
    (each x xs
      (unless (h x)
        (push x acc)
        (set (h x))))
    (rev acc)))

(def single (x) (and (cons? x) (not (cdr x))))

(def intersperse (x ys)
  (and ys (cons (car ys)
                (mappend [list x _] (cdr ys)))))

(def counts (seq (o c (table)))
  (if (not seq)
      c
      (do (++ (c (car seq) 0))
          (counts (cdr seq) c))))

(def commonest (seq)
  (with (winner nil n 0)
    (each (k v) (counts seq)
      (when (> v n) (= winner k n v)))
    (list winner n)))

(def reduce (f xs)
  (if (cddr xs)
      (reduce f (cons (f (car xs) (cadr xs)) (cddr xs)))
      (apply f xs)))

(def rreduce (f xs)
  (if (cddr xs)
      (f (car xs) (rreduce f (cdr xs)))
      (apply f xs)))

(def len< (x n) (< (len x) n))

(def len> (x n) (> (len x) n))


;=============================================================================
;  Filesystem paths
;=============================================================================

(implicit srcdir srcdir)

(make-implicit curdir
  (racket-make-derived-parameter racket-current-directory
    (fn (v) (zap string v)
            (if (empty v)
                  (racket-current-directory)
                  (racket-expand-user-path v)))
    (fn (v) (racket-path->string v))))

#|
this is now handled by ifdlet, so I should probably remove this...

(mac w/curdir (val . body)
  (w/uniq x
    `(iflet ,x ,val
       (dlet curdir ,x ,@body)
       (let curdir curdir ,@body))))
|#

(def expandpath (x)
  (zap string x)
  (if (empty x)
        x
      (racket-path->string:racket-expand-user-path x)))

(rckt-require scheme/path)

(def make-path->string (converter)
  (fn (x)
    (zap string x)

    (unless (empty x)
      (zap converter x)
      (unless (aracket-false x)
        (racket-path->string x)))))

(= dirname  (make-path->string racket-path-only:expandpath))
(= basename (make-path->string racket-file-name-from-path:expandpath))

; should probably be moved somewhere else, maybe next to len?
(def len- (x y)
  (- (len x) (len y)))

; (stripext "foo.bar" ".bar") returns "foo"
(def stripext (x y)
  (zap string x)
  (zap string y)
  (let (l r) (split x (max (len- x y) 0)) ; should split accept negatives?
    (if (is r y)
          l
          x)))


; Algorithm taken from Python's os.path.join (http://bugs.python.org/issue9921)
; Ignore the previous parts if a part is absolute.
; Insert a '/' unless the first part is empty or already ends in '/'.

(def todir (x)
  (if (is (last x) #\/)
        x
      (+ x "/")))

(def joinpath args
  (string (ccc (fn (cc)
    ((afn (args)
       (let (x y) args
         (if (not args)
               nil
             (not x)
               (self (cdr args))
             (and y (is ((expandpath y) 0) #\/)) ; wish I could use caris
                                                 ; (caris (expandpath y) #\/)
               (cc (apply joinpath (cdr args)))
             (cons (let x (expandpath x)
                     (if (cdr args)
                           (todir x)
                         x))
                   (self (cdr args))))))
     args)))))


(def abspath (x)
  (joinpath curdir x))

#|(def abspath (x)
  (w/curdir (dirname x)
    (+ curdir (basename x))))|#

(def absdir (x)
  (dirname (abspath x)))


;=============================================================================
;  Scripting
;=============================================================================

(def env (x)
  (let x (racket-getenv x)
    (unless (aracket-false x)
      x)))

(defset env (x)
  (w/uniq g
    (list (list g x)
          `(env ,g)
          `(fn (val) (do (racket-putenv ,g val)
                         val)))))

;(rckt-require scheme/mpair)

(def racket-vector->mlist (x)
  (ar-toarc (racket-vector->list x)))

(def racket-mlist->vector (x)
  (racket-list->vector (ar-toracket x))) ;(racket-mlist->list x)))

(make-implicit script-args
  (racket-make-derived-parameter racket-current-command-line-arguments
    (fn (v) (racket-mlist->vector v))
    (fn (v) (racket-vector->mlist v))))

(let x (car script-args)
  (implicit script-src (if x (abspath x))))

(mac w/script-dir body
  `(w/curdir (dirname ,script-src) ,@body))

(zap cdr script-args)


;=============================================================================
;  Input
;=============================================================================

(def load (file)
;  (prn file)
;  (ifdlet curdir (dirname (joinpath curdir file)))
;  (w/curdir file ;(absdir file)
  (w/curdir (dirname file)
;    (prn curdir)
    (w/infile f (basename file)
      (w/uniq eof
        (whiler e (read f eof) eof
          (eval e))))))

; should pipe-from call (cont 'wait)?
(def pipe-from (cmd)
  (let (in out id err cont) (ar-toarc:racket-process cmd)
    ; Racket docs say I need to close all 3 ports explicitly. Obviously I
    ; can't close the input port, since that's what we're returning, but
    ; I wonder if it's really necessary to close the output and error ports...
    (close out)
    (close err)
    in))

#|
doesn't work:

(def pipe-to (cmd)
  (let (in out id err cont) (ar-toarc:racket-process cmd)
    (close in)
    (close err)
    out))
|#

(def positive (x)
  (and (number x) (> x 0)))

(mac w/table (var . body)
  `(let ,var (table) ,@body ,var))

(def ero args
  (w/stdout stderr
    (each a args
      (write a)
      (writec #\space))
    (writec #\newline))
  (car args))

(def queue () (list nil nil 0))

; Despite call to atomic, once had some sign this wasn't thread-safe.
; Keep an eye on it.

(def enq (obj q)
  (atomic
    (++ (q 2))
    (if (not (car q))
        (= (cadr q) (= (car q) (list obj)))
        (= (cdr (cadr q)) (list obj)
           (cadr q)       (cdr (cadr q))))
    (car q)))

(def deq (q)
  (atomic (unless (is (q 2) 0) (-- (q 2)))
          (pop (car q))))

; Should redef len to do this, and make queues lists annotated queue.

(def qlen (q) (q 2))

(def qlist (q) (car q))

(def enq-limit (val q (o limit 1000))
  (atomic
     (unless (< (qlen q) limit)
       (deq q))
     (enq val q)))

(def median (ns)
  ((sort > ns) (trunc (/ (len ns) 2))))

(def flushout ()
  (racket-flush-output)
  t)

(mac noisy-each (n var val . body)
  (w/uniq (gn gc)
    `(with (,gn ,n ,gc 0)
       (each ,var ,val
         (when (multiple (++ ,gc) ,gn)
           (pr ".")
           (flushout)
           )
         ,@body)
       (prn)
       (flushout))))

(def downcase (x)
  (let downc (fn (c)
               (let n (coerce c 'int)
                 (if (or (< 64 n 91) (< 191 n 215) (< 215 n 223))
                     (coerce (+ n 32) 'char)
                     c)))
    (case (type x)
      string (map downc x)
      char   (downc x)
      sym    (sym (map downc (coerce x 'string)))
             (err "Can't downcase" x))))

(def upcase (x)
  (let upc (fn (c)
             (let n (coerce c 'int)
               (if (or (< 96 n 123) (< 223 n 247) (< 247 n 255))
                   (coerce (- n 32) 'char)
                   c)))
    (case (type x)
      string (map upc x)
      char   (upc x)
      sym    (sym (map upc (coerce x 'string)))
             (err "Can't upcase" x))))

(def inc (x (o n 1))
  (coerce (+ (coerce x 'int) n) (type x)))

(def range (start end)
  (if (> start end)
      nil
      (cons start (range (inc start) end))))

(def mismatch (s1 s2)
  (catch
    (on c s1
      (when (isnt c (s2 index))
        (throw index)))))

(def memtable (ks)
  (let h (table)
    (each k ks (set (h k)))
    h))

(= bar* " | ")

(mac w/bars body
  (w/uniq (out needbars)
    `(let ,needbars nil
       (do ,@(map (fn (e)
                    `(let ,out (tostring ,e)
                       (unless (is ,out "")
                         (if ,needbars
                             (pr bar* ,out)
                             (do (set ,needbars)
                                 (pr ,out))))))
                  body)))))

(def new-thread (f)
  (racket-thread (fn () (f))))

(def kill-thread (thd)
  (racket-kill-thread thd))

(def break-thread (thd)
  (racket-break-thread thd))

(def current-thread ()
  (racket-current-thread))

(def sleep ((o secs 0))
  (racket-sleep secs)
  nil)

(mac thread body
  `(new-thread (fn () ,@body)))

(mac trav (x . fs)
  (w/uniq g
    `((afn (,g)
        (when ,g
          ,@(map [list _ g] fs)))
      ,x)))

(mac or= (place expr)
  (let (binds val setter) (setforms place)
    `(atwiths ,binds
       (or ,val (,setter ,expr)))))

(= hooks* (table))

(def hook (name . args)
  (aif (hooks* name) (apply it args)))

(mac defhook (name . rest)
  `(= (hooks* ',name) (fn ,@rest)))

(mac out (expr) `(pr ,(tostring (eval expr))))

(def get (index) [_ index])

(= savers* (table))

(mac fromdisk (var file init load save)
  (w/uniq (gf gv)
    `(unless (bound ',var)
       (do1 (= ,var (iflet ,gf (file-exists ,file)
                               (,load ,gf)
                               ,init))
            (= (savers* ',var) (fn (,gv) (,save ,gv ,file)))))))

(mac diskvar (var file)
  `(fromdisk ,var ,file nil readfile1 writefile))

(mac disktable (var file)
  `(fromdisk ,var ,file (table) load-table save-table))

(mac todisk (var (o expr var))
  `((savers* ',var)
    ,(if (is var expr) var `(= ,var ,expr))))

(mac evtil (expr test)
  (w/uniq gv
    `(let ,gv ,expr
       (while (not (,test ,gv))
         (= ,gv ,expr))
       ,gv)))

(def rand-key (h)
  (if (empty h)
      nil
      (let n (rand (len h))
        (catch
          (each (k v) h
            (when (is (-- n) -1)
              (throw k)))))))

(def ratio (test xs)
  (if (empty xs)
      0
      (/ (count test xs) (len xs))))

(def quit ()
  (racket-exit))

(def find (test seq)
  (let f (testify test)
    (if (list? seq)
        (reclist   [if (f:car _) (car _)] seq)
        (recstring [if (f:seq _) (seq _)] seq))))

; note that (exact 1/3) is false, which is confusing

(def exact (n)
  (isa n 'int))

(def expt (x y)
  (racket-expt x y))

(def sqrt (x)
  (racket-sqrt x))
