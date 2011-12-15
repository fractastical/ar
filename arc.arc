; Main Arc lib.  Ported to Scheme version Jul 06.

; don't like names of conswhen and consif

; need better way of generating strings; too many calls to string
;  maybe strings with escape char for evaluation
; make foo~bar equiv of foo:~bar (in expand-ssyntax)
; add sigs of ops defined in ac.scm
; get hold of error types within arc
; does macex have to be defined in scheme instead of using def below?
; write disp, read, write in arc
; could I get all of macros up into arc.arc?
; warn when shadow a global name
; some simple regexp/parsing plan

; compromises in this implementation:
; no objs in code
;  (mac testlit args (listtab args)) breaks when called
; separate string type
;  (= (cdr (cdr str)) "foo") couldn't work because no way to get str tail
;  not sure this is a mistake; strings may be subtly different from
;  lists of chars

(def atom (x) (no (acons x)))

(def idfn (x) x)

(mac withs (parms . body)
  (if (no parms)
        #`(do ,@body)
      #`(let ,(car parms) ,(cadr parms)
          (withs ,(cddr parms) ,@body))))

(def isnt (x y) (no (is x y)))

; Could take n args, but have never once needed that.

(def iso (x y)
  (or (is x y)
      (and (acons x)
           (acons y)
           (iso (car x) (car y))
           (iso (cdr x) (cdr y)))))

(mac unless (test . body)
  #`(if (no test) (do ,@body)))

(mac while (test . body)
  (w/uniq (gf gp)
    #`((rfn gf (gp)
         (when gp ,@body (gf test)))
       test)))

(def empty (seq)
  (or (no seq)
      (is seq '||)
      (is (len seq) 0)))

; Like keep, seems like some shouldn't testify.  But find should,
; and all probably should.

(def some (test seq)
  (let f (testify test)
    (if (alist seq)
        (reclist f:car seq)
        (recstring f:seq seq))))

(def all (test seq)
  (~some (complement (testify test)) seq))

(def find (test seq)
  (let f (testify test)
    (if (alist seq)
        (reclist   [if (f:car _) (car _)] seq)
        (recstring [if (f:seq _) (seq _)] seq))))

; Possible to write map without map1, but makes News 3x slower.

;(def map (f . seqs)
;  (if (some1 no seqs)
;       nil
;      (no (cdr seqs))
;       (let s1 (car seqs)
;         (cons (f (car s1))
;               (map f (cdr s1))))
;      (cons (apply f (map car seqs))
;            (apply map f (map cdr seqs)))))


(def map (f . seqs)
  (if (some [isa _ 'string] seqs)
       (withs (n   (apply min (map len seqs))
               new (newstring n))
         ((afn (i)
            (if (is i n)
                new
                (do (sref new (apply f (map [_ i] seqs)) i)
                    (self (+ i 1)))))
          0))
      (no (cdr seqs))
       (map1 f (car seqs))
      ((afn (seqs)
        (if (some no seqs)
            nil
            (cons (apply f (map1 car seqs))
                  (self (map1 cdr seqs)))))
       seqs)))

(def mappend (f . args)
  (apply join (apply map f args)))

(def firstn (n xs)
  (if (no n)            xs
      (and (> n 0) xs)  (cons (car xs) (firstn (- n 1) (cdr xs)))
                        nil))

(def nthcdr (n xs)
  (if (no n)  xs
      (> n 0) (nthcdr (- n 1) (cdr xs))
              xs))

; Generalization of pair: (tuples x) = (pair x)

(def tuples (xs (o n 2))
  (if (no xs)
      nil
      (cons (firstn n xs)
            (tuples (nthcdr n xs) n))))

; If ok to do with =, why not with def?  But see if use it.

(mac defs args
  #`(do ,@(map [cons def _] (tuples args 3))))

(def warn (msg . args)
  (disp (+ "Warning: " msg ". "))
  (map [do (write _) (disp " ")] args)
  (disp #\newline))

(mac atomic body
  #`(atomic-invoke (fn () ,@body)))

(mac atlet args
  #`(atomic (let ,@args)))

(mac atwith args
  #`(atomic (with ,@args)))

(mac atwiths args
  #`(atomic (withs ,@args)))


;=============================================================================
;  = Assignment
;=============================================================================
; seems meaningful to e.g. (push 1 (pop x)) if (car x) is a cons.
; can't in cl though.  could I define a setter for push or pop?

#|(= setter (table))

(mac defset (name parms . body)
  #`(sref setter (fn parms ,@body) ',name))

(defset car (name val)
  #`(scar name val))

(defset cdr (name val)
  #`(scdr name val))

(defset caar (name val)
  #`(scar (car name) val))

(defset cadr (name val)
  #`(scar (cdr name) val))

(defset cddr (name val)
  #`(scdr (cdr name) val))

(defset assoc (name val index)
  #`(scar (assoc-ref name index) val))

(defset alref (name val index)
  #`(scar (cdr (assoc name index)) val))|#

#|
(sref (fn (g252)
        ((fn (g253)
           (ac-assign foo (cdr g252)) g253) (car g252)))
      (cons 1 ((fn (g252)
                 ((fn (g253)
                    (assign foo (cdr g252))
                    g253)
                  (car g252)))
               foo))
      foo)
|#


(def expand-full (x)
  (if (cons? x)
        (let x (macex x)
          (if (cons? x)
                (let c (expand-full car.x)
                  (if (caris c compose)
                        (expand-full (ac-decompose (cdr c) cdr.x))
                      (caris c complement)
                        (list no (expand-full (cons cadr.c cdr.x)))
                      (cons c (map expand-full cdr.x))))
              (expand-full x)))
      (ssyntax x)
        (ssexpand x)
      x))

#|
(if (some cons? cdr.place)
                (let u (map [uniq] cdr.place)
                  #`(with ,(mappend list u cdr.place)
                      ,(let place (cons car.place u)
                         (iflet f (setter car.place)
                           (apply f cadr.place val cddr.place)
                           ; assumed to be data structure in fn position
                           #`(sref ,car.place val ,@cdr.place)))))
|#

#|(def place (place)
  (let place expand-full.place
    (if (cons? place)
          (if (some cons? cdr.place)
                (withs (vars  (map [uniq] cdr.place)
                        u     (mappend list vars cdr.place)
                        place (cons car.place vars))
                  (list place u vars))
              (list place nil nil))
        (list place nil nil))))|#

(def ac-sref-if (u args)
  (if (no args)
        nil
      (with (x (car args)
             y (cadr args)
             z (cadr cdr.args))
        (if (no y)
              (ac-sref-if u (cddr cdr.args))
            (no z)
              #`(x (= y u) ,@(ac-sref-if u (cddr cdr.args)))
            #`(x (= y u) (= z u) ,@(ac-sref-if u (cddr cdr.args)))))))

(mac sref-mac (f val . rest)
  (if (is f ac-if)
        (w/uniq u
          #`(let u val (f ,@(ac-sref-if u rest)))
          #|#`(let u (+ 1 2)
              (f ,@(ac-mappend (racket-lambda ((x y z))
                                 (if z #`(x (= y u) (= z u))
                                     y #`(x (= y u))))
                               (tuples args 3))))
          (ac-prn f val rest)|#
          )
      (list* sref f val rest)))

; setforms returns (vars get set) for a place based on car of an expr
;  vars is a list of gensyms alternating with expressions whose vals they
;   should be bound to, suitable for use as first arg to with(s)
;  get is an expression returning the current value in the place
;  set is a function of one argument that when called returns a list that
;   when evaluated
;   assigns the value
;
; Note: if place expands into any expression whose car doesn't
; have a setter, expand= assumes it's a data structure in functional
; position.  Such bugs will be seen only when the code is executed, when
; sref complains it can't set a reference to a function.
(def setforms (place)
  (let place expand-full.place
    (if (cons? place)
              ;; TODO: should this hardcode quote?
          (if (some [and (cons? _)
                         (no:caris _ ac-quote)]
                    cdr.place)
                (withs (vars  (map [uniq] cdr.place)
                        bind  (mappend list vars cdr.place)
                        place (cons car.place vars))
                  ;; TODO: code duplication
                  (list bind place (fn (x) #`(sref-mac ,car.place x ,@cdr.place))))
              (list nil place (fn (x) #`(sref-mac ,car.place x ,@cdr.place))))
        (list nil place (fn (x) #`(assign place x))))))

#|(iflet f (setter car.place)
            (if bind  (fn (x) (apply f car.vars x cdr.vars))
                      (fn (x) (apply f cadr.place x cddr.place)))
            ; assumed to be data structure in fn position
            (fn (x) #`(sref ,car.place x ,@cdr.place)))|#

(mac w/setforms (name . body)
  (w/uniq bind
    #`(let (bind 'get 'set) (setforms name)
        (if bind #`(with bind ,,@body)
                   (do         ,@body)))))

(def expand= (place val)
  ;(ac-prn place val)
  (w/setforms place (set val))
  #|(let (vars get set) setforms.x
    (ac-prn
    (if vars #`(with vars ,(set val))
             (set val))))|#
  )

#|  (let (place bind vars) (setforms place)
    (if (cons? place)
          (if bind  #`(with bind
                        ,(iflet f (setter car.place)
                           (apply f car.vars val cdr.vars)
                           ; assumed to be data structure in fn position
                           #`(sref ,car.place val ,@cdr.place)))
                    (iflet f (setter car.place)
                      (apply f cadr.place val cddr.place)
                      ; assumed to be data structure in fn position
                      #`(sref ,car.place val ,@cdr.place)))
      #`(assign place val))))|#

#|(if (some cons? cdr.place)
      (w/uniq u
        `(let ,u ,@cdr.place
           (sref ,car.place ,val ,u)))|#

(def expand=list (terms)
  ;(ac-prn "assign" (map1 (fn (x) (apply expand= x)) (pair terms)))
  #`(do ,@(map [apply expand= _] (pair terms))))

(remac = args
  (expand=list args))


(mac loop (start test update . body)
  (w/uniq (gfn gparm)
    #`(do start
          ((rfn gfn (gparm)
             (if gparm
               (do ,@body update (gfn test))))
           test))))

(mac for (v init max . body)
  (w/uniq (gi gm)
    #`(with (v nil gi init gm (+ max 1))
        (loop (= v gi) (< v gm) (= v (+ v 1))
          ,@body))))

(mac down (v init min . body)
  (w/uniq (gi gm)
    #`(with (v nil gi init gm (- min 1))
        (loop (= v gi) (> v gm) (= v (- v 1))
          ,@body))))

(mac repeat (n . body)
  #`(for ,(uniq) 1 n ,@body))


(def eachfn (f x)
  (if alist.x
        ((afn (x)
           (when acons.x
             (f car.x)
             (self cdr.x)))
         x)
      (isa x 'table)
        (maptable (fn args f.args) x)
      (for y 0 (- len.x 1)
        (f x.y)))
  nil)

(mac each (var expr . body)
  #`(eachfn (fn (var) ,@body) expr))


; (nthcdr x y) = (cut y x).

(def cut (seq start (o end))
  (let end (if (no end)   (len seq)
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
    #`((rfn gf (gp)
         (let var gp
           (when var ,@body (gf test))))
       test)))

(def last (xs)
  (if (cons? xs)
        ((afn (x)
           (if (cdr x)
                 (self (cdr x))
               (car x)))
         xs)
      (xs (- (len xs) 1))))

(def rem (test seq)
  (let f (testify test)
    (if (alist seq)
        ((afn (s)
           (if (no s)       nil
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

(mac do1 args
  (w/uniq g
    #`(let g ,(car args)
        ,@(cdr args)
        g)))


;=============================================================================
;  Mutation / Assignment
;=============================================================================

#|(mac w/setonce (name form)
  #|(if (cons? name)
        (w/uniq u
        (map [uniq] name)
          `(if (cons? ,name)
                 (with (,u (map [uniq] (cdr ,name))

                        )
                   `(with ,(mappend list ,u (cdr ,name))
                      ,(let ,name (cons (car ,name) ,u)
                         ,form)))
               ,form)))|#
  #|(w/uniq u
    `(,if (,cons? ,name)
            (,let ,u (,map [,uniq] (,cdr ,name))
              `(,,with ,(,mappend ,list ,u (,cdr ,name))
                 ,(,let ,name (,cons (,car ,name) ,u)
                    ,form)))
          ,form))|#
  (w/uniq u
    #`(if (cons? name)
            (let u (map [uniq] (cdr name))
              #`(with ,(mappend list u (cdr name))
                  ,(let name (cons (car name) u)
                     form)))
          form)))|#


(mac push (x place)
  (w/setforms place
    (set #`(cons x get))))

(mac swap (place1 place2)
  (w/uniq (g1 g2)
    (if (cons? place1)
          #|(let u (map [uniq] (cdr place1))
            )|#
    ;(w/setonce (place1 place2))
    ;(w/setonce place2)
      #`(with (g1 place1
               g2 place2)
          (= place1 g2)
          (= place2 g1)))))

(mac rotate places
  (w/uniq u
    (let shift (join (cdr places) (list u))
      #`(let u ,(car places)
          (= ,@(mappend list places shift))))))

(mac pop (place)
  (w/uniq u
    (w/setforms place
      #`(let u get
          (do1 (car u)
               ,(set #`(cdr u)))))))

(def adjoin (x xs (o test iso))
  (if (some [test x _] xs)
        xs
      (cons x xs)))

(mac pushnew (x place . args)
  (w/setforms place
    (set #`(adjoin x get ,@args))))

(mac pull (test place)
  (w/setforms place
    (set #`(rem test get))))

(mac togglemem (x place . args)
  (w/uniq (u v)
    (w/setforms place
      (set #`(with (u  x
                    v  get)
               (if (mem u v)
                     (rem u v)
                   (adjoin u v ,@args)))))))

#|(mac togglemem (x place . args)
  (w/uniq (u v)
    #`(with (u  x
             v  place)
        (= place (if (mem u v)
                       (rem u v)
                     (adjoin u v ,@args))))))|#


;((fn (g243) (sref foo ((fn (g242) (if (mem g242 (foo g243)) (rem g242 (foo g243)) (adjoin g242 (foo g243)))) x) g243)) (bar qux))
;((fn (g244) (sref foo ((fn (g242 g243) (if (mem g242 g243) (rem g242 g243) (adjoin g242 g243))) x (foo g244)) g244)) (bar qux))
;((fn (g242 g243) ((fn (g244) (sref foo (if (mem g242 g243) (rem g242 g243) (adjoin g242 g243)) g244)) (bar qux))) x (foo (bar qux)))
;((fn (g240 g241) ((fn (g242) (sref foo (if (mem g240 g241) (rem g240 g241) (adjoin g240 g241)) g242)) (bar qux))) x (foo (bar qux)))

(mac zap (f place . args)
  (w/setforms place
    (set #`(f get ,@args)))
  #|(w/setonce x
    #`(= x (f x ,@args)))|#
  #|(if (cons? x)
        (let u (map [uniq] (cdr x))
          `(with ,(mappend list u (cdr x))
             ,(let x (cons (car x) u)
                `(= ,x (,f ,x ,@args)))))
    `(= ,x (,f ,x ,@args)))|#
  )

#|(mac zap (f x . args)
  (setonce x `(= ,x (,f ,x ,@args))))|#
;(setonce foo `(= ,foo (,f ,foo ,@args)))

(mac ++ (place (o i 1))
  #`(zap + place i))

(mac -- (place (o i 1))
  #`(zap - place i))

;`(or ,place (= ,place ,expr))
(mac or= (place expr)
  (w/setforms place
    #`(unless get ,(set expr))))


; Can't simply mod pr to print strings represented as lists of chars,
; because empty string will get printed as nil.  Would need to rep strings
; as lists of chars annotated with 'string, and modify car and cdr to get
; the rep of these.  That would also require hacking the reader.

(def pr args
  (map1 disp args)
  (car args))

(def prt args
  (map1 [if _ (disp _)] args)
  (car args))

(def prn args
  (do1 (apply pr args)
       (writec #\newline)))

(mac wipe args
  #`(do ,@(map (fn (a) #`(= a nil)) args)))

(mac set args
  #`(do ,@(map (fn (a) #`(= a t)) args)))

(mac awhen (expr . body)
  #`(let 'it expr (if 'it (do ,@body))))

(mac aand args
  (if (no args)
        t
      (no (cdr args))
        (car args)
      #`(let 'it ,(car args) (and 'it (aand ,@(cdr args))))))

(mac accum (accfn . body)
  (w/uniq gacc
    #`(withs (gacc nil accfn [push '_ gacc])
        ,@body
        (rev gacc))))

; Repeatedly evaluates its body till it returns nil, then returns vals.

(mac drain (expr (o eof nil))
  (w/uniq (gacc gdone gres)
    #`(with (gacc nil gdone nil)
        (while (no gdone)
          (let gres expr
            (if (is gres eof)
                  (= gdone t)
                (push gres gacc))))
        (rev gacc))))

; For the common C idiom while x = snarfdata != stopval.
; Rename this if use it often.

(mac whiler (var expr endval . body)
  (w/uniq gf
    #`(withs (var nil gf (testify endval))
        (while (no (gf (= var expr)))
          ,@body))))

;(def macex (e)
;  (if (atom e)
;      e
;      (let op (and (atom (car e)) (eval (car e)))
;        (if (isa op 'mac)
;            (apply (rep op) (cdr e))
;            e))))

(def consif (x y) (if x (cons x y) y))

(def flat x
  ((afn (x acc)
     (if (no x)   acc
         (atom x) (cons x acc)
                  (self (car x) (self (cdr x) acc))))
   x nil))

(mac check (x test (o alt))
  (w/uniq gx
    #`(let gx x
        (if (test gx) gx alt))))

(def pos (test seq (o start 0))
  (let f (testify test)
    (if (alist seq)
        ((afn (seq n)
           (if (no seq)
                nil
               (f (car seq))
                n
               (self (cdr seq) (+ n 1))))
         (nthcdr start seq)
         start)
        (recstring [if (f (seq _)) _] seq start))))

(def even (n) (is (mod n 2) 0))

(def odd (n) (no (even n)))


(mac w/outstring (var . body)
  #`(let var (outstring) ,@body))

; what happens to a file opened for append if arc is killed in
; the middle of a write?

(mac w/appendfile (var name . body)
  #`(let var (outfile name ''append)
      (after (do ,@body) (close var))))

; rename this simply "to"?  - prob not; rarely use

(mac tostring body
  (w/uniq gv
    #`(w/outstring gv
        (w/stdout gv ,@body)
        (inside gv))))

(mac fromstring (str . body)
  (w/uniq gv
    #`(w/instring gv str
        (w/stdin gv ,@body))))

; inconsistency between names of readfile[1] and writefile

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
  (tostring (whiler c (readc str nil) no
              (writec c))))

(def filechars (name)
  (w/infile s name (allchars s)))

(def writefile (val file)
  (let tmpfile (+ file ".tmp")
    (w/outfile o tmpfile (write val o))
    (mvfile tmpfile file))
  val)

(mac rand-choice exprs
  #`(case (rand ,(len exprs))
      ,@(let key -1
          (mappend [list (++ key) _]
                   exprs))))

(mac n-of (n expr)
  (w/uniq ga
    ;; TODO: shouldn't this use accum/collect?
    #`(let ga nil
        (repeat n (push expr ga))
        (rev ga))))

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
  #`(for var 0 (- (len s) 1) ,@body))

(mac on (var s . body)
  (if (is var 'index)
        (err "can't use index as first arg to on.")
      (w/uniq gs
        #`(let gs s
            (forlen 'index gs
              (let var (gs 'index)
                ,@body))))))

(def best (f seq)
  (if (no seq)
        nil
      (let wins (car seq)
        (each elt (cdr seq)
          (if (f elt wins) (= wins elt)))
        wins)))

;; TODO: case-lambda for 2-arg version
(def max args (best > args))
(def min args (best < args))

; (mac max2 (x y)
;   (w/uniq (a b)
;     #`(with (a x b y) (if (> a b) a b))))

(def most (f seq)
  (unless (no seq)
    (withs (wins (car seq) topscore (f wins))
      (each elt (cdr seq)
        (let score (f elt)
          (if (> score topscore) (= wins elt topscore score))))
      wins)))

; Insert so that list remains sorted.  Don't really want to expose
; these but seem to have to because can't include a fn obj in a
; macroexpansion.

(def insert-sorted (test elt seq)
  (if (no seq)
       (list elt)
      (test elt (car seq))
       (cons elt seq)
      (cons (car seq) (insert-sorted test elt (cdr seq)))))

(mac insort (test elt seq)
  #`(zap [insert-sorted test elt '_] seq))

(def reinsert-sorted (test elt seq)
  (if (no seq)
       (list elt)
      (is elt (car seq))
       (reinsert-sorted test elt (cdr seq))
      (test elt (car seq))
       (cons elt (rem elt seq))
      (cons (car seq) (reinsert-sorted test elt (cdr seq)))))

(mac insortnew (test elt seq)
  #`(zap [reinsert-sorted test elt '_] seq))

; Could make this look at the sig of f and return a fn that took the
; right no of args and didn't have to call apply (or list if 1 arg).

(def memo (f)
  (with (cache (table) nilcache (table))
    (fn args
      (or (cache args)
          (and (no (nilcache args))
               (aif (apply f args)
                    (= (cache args) it)
                    (do (set (nilcache args))
                        nil)))))))


(mac defmemo (name parms . body)
  #`(safeset name (memo (fn parms ,@body))))

(def <= args
  (or (no args)
      (no (cdr args))
      (and (no (> (car args) (cadr args)))
           (apply <= (cdr args)))))

(def >= args
  (or (no args)
      (no (cdr args))
      (and (no (< (car args) (cadr args)))
           (apply >= (cdr args)))))

(def whitec (c)
  (in c #\space #\newline #\tab #\return))

(def nonwhite (c) (no (whitec c)))

(def letter (c) (or (<= #\a c #\z) (<= #\A c #\Z)))

(def digit (c) (<= #\0 c #\9))

(def alphadig (c) (or (letter c) (digit c)))

(def punc (c)
  (in c #\. #\, #\; #\: #\! #\?))

(def readline ((o str stdin))
  ;; TODO: a little hacky
  (let x (tostring:whiler c (readc str)
                            [or (and (is _ #\return)
                                     (is peekc.str #\newline))
                                (in _ nil #\newline)]
           (writec c))
    (and (isnt x "") x)))

; Don't currently use this but suspect some code could.

(mac summing (sumfn . body)
  (w/uniq (gc gt)
    #`(let gc 0
        (let sumfn (fn (gt) (if gt (++ gc)))
          ,@body)
        gc)))

(def sum (f xs)
  (let n 0
    (each x xs (++ n (f x)))
    n))

(def treewise (f base tree)
  (if (atom tree)
      (base tree)
      (f (treewise f base (car tree))
         (treewise f base (cdr tree)))))

(def carif (x) (if (atom x) x (car x)))

; Could prob be generalized beyond printing.

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
  (accum a (each (k v) h (a k))))

(def vals (h)
  (accum a (each (k v) h (a v))))

; These two should really be done by coerce.  Wrap coerce?

(def tablist (h)
  (accum a (maptable (fn args (a args)) h)))

(def listtab (al)
  (let h (table)
    (map (fn ((k v)) (= (h k) v))
         al)
    h))

(mac obj args
  #`(listtab (list ,@(map (fn ((k v))
                            #`(list ',k v))
                          (pair args)))))

(def load-table (file (o eof))
  (w/infile i file (read-table i eof)))

(def read-table ((o i (stdin)) (o eof))
  (let e (read i eof)
    (if (alist e) (listtab e) e)))

(def load-tables (file)
  (w/infile i file
    (w/uniq eof
      (drain (read-table i eof) eof))))

(def save-table (h file)
  (writefile (tablist h) file))

(def write-table (h (o o (stdout)))
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
                   (err "can't copy " x))
    (map (fn ((k v)) (= (x2 k) v))
         (pair args))
    x2))

(def abs (n)
  (if (< n 0) (- n) n))

; The problem with returning a list instead of multiple values is that
; you can't act as if the fn didn't return multiple vals in cases where
; you only want the first.  Not a big problem.

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
  (if (alist seq)
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
                (withs (j (/ (if (even n) n (- n 1)) 2) ; faster than round
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
  (if (no x) y
      (no y) x
      (let lup nil
        (= lup (fn (r x y r-x?) ; r-x? for optimization -- is r connected to x?
                 (if (less? (car y) (car x))
                   (do (if r-x? (scdr r y))
                       (if (cdr y) (lup y x (cdr y) nil) (scdr y x)))
                   ; (car x) <= (car y)
                   (do (if (no r-x?) (scdr r x))
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

(mac time (expr)
  (w/uniq (t1 t2)
    #`(let t1 (msec)
        (do1 expr
             (let t2 (msec)
               (prn "time: " (- t2 t1) " msec."))))))

(mac jtime (expr)
  #`(do1 ''ok (time expr)))

(mac time10 (expr)
  #`(time (repeat 10 expr)))

(def union (f xs ys)
  (+ xs (rem (fn (y) (some [f _ y] xs))
             ys)))

(= templates* (table))

(mac deftem (tem . fields)
  (withs (name (carif tem) includes (if (acons tem) (cdr tem)))
    #`(= (templates* ',name)
         (+ (mappend templates* ',(rev includes))
            (list ,@(map (fn ((k v)) #`(list ',k (fn () v)))
                         (pair fields)))))))

(mac addtem (name . fields)
  #`(= (templates* ',name)
              ;; TODO: shouldn't this fn be unquoted?
       (union (fn ('x 'y) (is (car 'x) (car 'y)))
              (list ,@(map (fn ((k v)) #`(list ',k (fn () v)))
                           (pair fields)))
              (templates* ',name))))

(def inst (tem . args)
  (let x (table)
    (each (k v) (if (acons tem) tem (templates* tem))
      (unless (no v) (= (x k) (v))))
    (each (k v) (pair args)
      (= (x k) v))
    x))

; To write something to be read by temread, (write (tablist x))

(def temread (tem (o str (stdin)))
  (templatize tem (read str)))

; Converts alist to inst; ugly; maybe should make this part of coerce.
; Note: discards fields not defined by the template.

(def templatize (tem raw)
  (with (x (inst tem) fields (if (acons tem) tem (templates* tem)))
    (each (k v) raw
      (when (assoc fields k)
        (= (x k) v)))
    x))

(def temload (tem file)
  (w/infile i file (temread tem i)))

(def temloadall (tem file)
  (map (fn (pairs) (templatize tem pairs))
       (w/infile in file (readall in))))


(def number (n) (in (type n) 'int 'num))

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
  #`(safeset name (cache (fn () lasts)
                         (fn () ,@body))))

(mac errsafe (expr)
  (w/uniq c
    #`(on-err (fn (c) nil)
              (fn ()  expr))))

(def saferead (arg) (errsafe:read arg))

(def safe-load-table (filename)
  (or (errsafe:load-table filename)
      (table)))

(def ensure-dir (path)
  (unless (dir-exists path)
    (system (string "mkdir -p " path))))

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
  #`(while (no test) ,@body))

(def before (x y seq (o i 0))
  (with (xp (pos x seq i) yp (pos y seq i))
    (and xp (or (no yp) (< xp yp)))))

(def orf fns
  (fn args
    ((afn (fs)
       (and fs (or (apply (car fs) args) (self (cdr fs)))))
     fns)))

(def andf fns
  (fn args
    ((afn (fs)
       (if (no fs)       t
           (no (cdr fs)) (apply (car fs) args)
                         (and (apply (car fs) args) (self (cdr fs)))))
     fns)))

(def atend (i s)
  (> i (- (len s) 2)))

(def multiple (x y)
  (is 0 (mod x y)))

(mac nor args
  #`(no (or ,@args)))

; Consider making the default sort fn take compare's two args (when do
; you ever have to sort mere lists of numbers?) and rename current sort
; as prim-sort or something.

; Could simply modify e.g. > so that (> len) returned the same thing
; as (compare > len).

(def compare (comparer scorer)
  (fn (x y) (comparer (scorer x) (scorer y))))

; Cleaner thus, but may only ever need in 2 arg case.

;(def compare (comparer scorer)
;  (fn args (apply comparer map scorer args)))

; (def only (f g . args) (aif (apply g args) (f it)))

(def only (f)
  (fn args (if (car args) (apply f args))))

(mac conswhen (f x y)
  (w/uniq (gf gx)
    #`(with (gf f gx x)
        (if (gf gx) (cons gx y) y))))

; Could combine with firstn if put f arg last, default to (fn (x) t).

(def retrieve (n f xs)
  (if (no n)                 (keep f xs)
      (or (<= n 0) (no xs))  nil
      (f (car xs))           (cons (car xs) (retrieve (- n 1) f (cdr xs)))
                             (retrieve n f (cdr xs))))

(def dedup (xs)
  (with (h (table) acc nil)
    (each x xs
      (unless (h x)
        (push x acc)
        (set (h x))))
    (rev acc)))

(def single (x) (and (acons x) (no (cdr x))))

(def intersperse (x ys)
  (and ys (cons (car ys)
                (mappend [list x _] (cdr ys)))))

(def counts (seq (o c (table)))
  (if (no seq)
      c
      (do (++ (c (car seq) 0))
          (counts (cdr seq) c))))

(def commonest (seq)
  (with (winner nil n 0)
    (each (k v) (counts seq)
      (when (> v n) (= winner k n v)))
    (list winner n)))

(def rreduce (f xs)
  (if (cddr xs)
      (f (car xs) (rreduce f (cdr xs)))
      (apply f xs)))

(let argsym (uniq)

  (def parse-format (str)
    (accum a
      (with (chars nil  i -1)
        (w/instring s str
          (whilet c (readc s)
            (case c
              #\# (do (a (coerce (rev chars) 'string))
                      (wipe chars)
                      (a (read s)))
              #\~ (do (a (coerce (rev chars) 'string))
                      (wipe chars)
                      (readc s)
                      (a (list argsym (++ i))))
                  (push c chars))))
         (when chars
           (a (coerce (rev chars) 'string))))))

  (mac prf (str . args)
    #`(let argsym (list ,@args)
        (pr ,@(parse-format str))))
)

(def load (file)
  (w/infile f file
    (w/uniq eof
      (whiler e (read f eof) eof
        (eval e)))))

(def positive (x)
  (and (number x) (> x 0)))

(mac w/table (var . body)
  #`(let var (table) ,@body var))

(def ero args
  (w/stdout (stderr)
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
    (if (no (car q))
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

(mac noisy-each (n var val . body)
  (w/uniq (gn gc)
    #`(with (gn n gc 0)
        (each var val
          (when (multiple (++ gc) gn)
            (pr ".")
            (flushout)
            )
          ,@body)
        (prn)
        (flushout))))

(mac point (name . body)
  (w/uniq (g p)
    #`(ccc (fn (g)
             (let name (fn (('o p)) (g p))
               ,@body)))))

(mac catch body
  #`(point 'throw ,@body))

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
             (err "can't downcase" x))))

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
             (err "can't upcase" x))))

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
    #`(let needbars nil
        (do ,@(map (fn (e)
                     #`(let out (tostring e)
                         (unless (is out "")
                           (if needbars
                                 (pr bar* out)
                               (do (set needbars)
                                   (pr out))))))
                   body)))))

(def len< (x n) (< (len x) n))

(def len> (x n) (> (len x) n))

(mac thread body
  #`(new-thread (fn () ,@body)))

(mac trav (x . fs)
  (w/uniq g
    #`((afn (g)
         (when g
           ,@(map [list _ g] fs)))
       x)))

(= hooks* (table))

(def hook (name . args)
  (aif (hooks* name) (apply it args)))

(mac defhook (name . rest)
  #`(= (hooks* ',name) (fn ,@rest)))

(mac out (expr)
  #`(pr ,(tostring (eval expr))))

; if renamed this would be more natural for (map [_ user] pagefns*)

(def get (index) [_ index])

(= savers* (table))

(mac fromdisk (var file init load save)
  (w/uniq (gf gv)
    #`(unless (bound ',var)
        (do1 (= var (iflet gf (file-exists file)
                      (load gf)
                      init))
             (= (savers* ',var) (fn (gv) (save gv file)))))))

(mac diskvar (var file)
  #`(fromdisk var file nil readfile1 writefile))

(mac disktable (var file)
  #`(fromdisk var file (table) load-table save-table))

(mac todisk (var (o expr var))
  #`((savers* ',var)
     ,(if (is var expr)
            var
          #`(= var expr))))


(mac evtil (expr test)
  (w/uniq gv
    #`(let gv expr
        (while (no (test gv))
          (= gv expr))
        gv)))

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


; any logical reason I can't say (push x (if foo y z)) ?
;   eval would have to always ret 2 things, the val and where it came from
; idea: implicit tables of tables; setf empty field, becomes table
;   or should setf on a table just take n args?

; idea: use constants in functional position for currying?
;       (1 foo) would mean (fn args (apply foo 1 args))
; another solution would be to declare certain symbols curryable, and
;  if > was, >_10 would mean [> _ 10]
;  or just say what the hell and make _ ssyntax for currying
; idea: make >10 ssyntax for [> _ 10]
; solution to the "problem" of improper lists: allow any atom as a list
;  terminator, not just nil.  means list recursion should terminate on
;  atom rather than nil, (def empty (x) (or (atom x) (is x "")))
; table should be able to take an optional initial-value.  handle in sref.
; warn about code of form (if (= )) -- probably mean is
; warn when a fn has a parm that's already defined as a macro.
;   (def foo (after) (after))
; idea: a fn (nothing) that returns a special gensym which is ignored
;  by map, so can use map in cases when don't want all the vals
; idea: anaph macro so instead of (aand x y) say (anaph and x y)
; idea: foo.bar!baz as an abbrev for (foo bar 'baz)
;  or something a bit more semantic?
; could uniq be (def uniq () (annotate 'symbol (list 'u))) again?
; idea: use x- for (car x) and -x for (cdr x)  (but what about math -?)
; idea: get rid of strings and just use symbols
; could a string be (#\a #\b . "") ?
; better err msg when , outside of a bq
; idea: parameter (p foo) means in body foo is (pair arg)
; idea: make ('string x) equiv to (coerce x 'string) ?  or isa?
;   quoted atoms in car valuable unused semantic space
; idea: if (defun foo (x y) ...), make (foo 1) return (fn (y) (foo 1 y))
;   probably would lead to lots of errors when call with missing args
;   but would be really dense with . notation, (foo.1 2)
; or use special ssyntax for currying: (foo@1 2)
; remember, can also double; could use foo::bar to mean something
; wild idea: inline defs for repetitive code
;  same args as fn you're in
; variant of compose where first fn only applied to first arg?
;  (> (len x) y)  means (>+len x y)
; use ssyntax underscore for a var?
;  foo_bar means [foo _ bar]
;  what does foo:_:bar mean?
; matchcase
; idea: atable that binds it to table, assumes input is a list
; crazy that finding the top 100 nos takes so long:
;  (let bb (n-of 1000 (rand 50)) (time10 (bestn 100 > bb)))
;  time: 2237 msec.  -> now down to 850 msec
