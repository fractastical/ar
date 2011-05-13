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


;=============================================================================
;  Predicates
;=============================================================================

(def list? (x) (or (not x) (cons? x)))


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
    `(with (,v nil ,gi ,init ,gm (+ ,max 1))
       (loop (assign ,v ,gi) (< ,v ,gm) (assign ,v (+ ,v 1))
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
             (for ,gv 0 (- (len ,gseq) 1)
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

(assign-fn recstring (test s (o start 0))
  (fn args
    (with (test (car args)
           s    (cadr args)
           start (if (cddr args) (caddr args) 0))
      ((afn (i)
         (and (< i (len s))
              (or (test i)
                  (self (+ i 1)))))
       start))))

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
                    (self (+ i 1)))))
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
