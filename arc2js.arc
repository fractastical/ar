(use arc strings)

(implicit linesep "\n")
(implicit indent  "    ")
(implicit spaces  " ")
(implicit local)

(mac w/whitespace (x . body)
  (case x
    minify `(w/linesep ""
              (w/indent ""
                (w/spaces ""
                  ,@body)))
    (err "unknown whitespace mode" x)))


(= js-rules* (table))

(mac defjs (name parms . body)
  `(= (js-rules* ',name) (fn ,parms (string ,@body))))


(def addsep (x args)
  (intersperse (string x spaces) (map tojs args)))


(def pushfront (x . rest)
  (when x
    (= (cdr x) (cons (car x) (cdr x)))
    (= (car x) (sym:apply string rest))))

(def tocomplexfn (x body)
  ((afn (x i)
     (let c (car x)
       (if (no x) nil
           (caris c 'o)
             (let n (cadr c)
               (iflet v (caddr c)
                 (pushfront body n spaces
                            "=" spaces n spaces
                            "||" spaces (tojs v)))
               (cons n (self (cdr x) (+ i 1))))
           (aand (cdr x) (isa it 'sym))
             (do (pushfront body "var " (cdr x) spaces "=" spaces
                            "Array.prototype.slice.call(arguments," spaces i ")")
                 (cons c nil))
           (cons c (self (cdr x) (+ i 1))))))
   x 1))

(def tojsparms (x body)
  (if (no x)
        (string "()")
      (isa x 'cons)
        (string "(" (addsep "," (tocomplexfn x body)) ")")
      (isa x 'sym)
        (do (pushfront body "var " x spaces "=" spaces
                       "Array.prototype.slice.call(arguments)")
            (string "()"))
      (err "invalid argument list" x)))

(def line args
  (string indent args ";" linesep))

;; should be in arc.arc
(def lastcdr (x)
  (nthcdr (- (len x) 1) x))

;; should use (defset last ...)
(mac setlast (x y)
  `(whenlet it (lastcdr ,x)
     (= (car it) ,y)))

(mac reindent/if (x . body)
  `(w/indent (if ,x (string indent origindent)
                    (= origindent indent))
     ,@body))


(def optimizefn (x) x)

(extend optimizefn ((x . rest)) (caris x 'do)
  (join (cdr x) rest))

(extend optimizefn ((x . rest)) (and (caris x 'let)
                                     (no rest))
  (cons (sym:string "var " (x 1) spaces "=" spaces (x 2))
        (or (nthcdr 3 x) (list nil))))

#|(extend macex (x (o once)) (let y x
                             (prn y)
                             (and y))
  it)|#

#|(def macex (e (o once))
  (if (acons e)
       (let m (ac-macro? (car e))
         (if m
              (let expansion (apply m (cdr e))
                (if (no once) (macex expansion) expansion))
              e))
       e))|#


(defjs fn (parms . body)
  (do (zap optimizefn body) nil)

  "(function" spaces (tojsparms parms body) spaces "{"

  (reindent/if local
    (w/local t
      (= body (map tojs body))

      (if (is (last body) "undefined")
        (zap cut body 0 -1)
        (setlast body (string "return " it)))

      (when body
        (cons linesep (map line body)))))

  (and local body indent)
  "})")


(defjs list args
  "[" (addsep "," args) "]")

(defjs assign (x y)
  (unless local "var ")
  x spaces "=" spaces (tojs y)
  (unless local ";"))

(defjs if args
  ((afn (x)
     (if (no:cdr x)
           (tojs:car x)
         (string "(" (tojs:car x)
                 spaces "?" spaces
                 (tojs:cadr x)
                 spaces ":" spaces
                 (self (cddr x)) ")")))
   args))


(defjs sref (x v k)
  x "[" (tojs k) "]" spaces "=" spaces (tojs v))

(defjs quote (x) (tojs:coerce x 'string))

(defjs prn args
  "console.log(" (addsep "," args) ")")

(defjs pr args
  "console.dir(" (addsep "," args) ")")

(defjs err args
  "console.error(" (addsep "," args) ")")

(defjs warn args
  "console.warn(" (addsep "," args) ")")


(redef expand= (place val)
  (if (isa place 'sym)
        `(assign ,place ,val)
      `(sref ,(car place) ,val ,(cadr place))))

;; should be in arc.arc
(redef expand=list (terms)
  (if (cddr terms)
        `(do ,@(map (fn ((p v)) (expand= p v))
                    (pair terms)))
      (apply expand= terms)))


(= uniq-counter* 0)

;; ew
(redef uniq ()
  (do1 (sym:string "__arc_gensym_" uniq-counter*)
       (++ uniq-counter*)))

;; ew
(mac def (name parms . body)
  `(assign ,name (fn ,parms ,@body)))


(def binwrap (name args)
  (string "(" (intersperse (string spaces name spaces) args) ")"))

(def bin (name args)
  (binwrap name (map tojs args)))

(def binand (name (x . args))
  (if (cdr args)
        (binwrap "&&" (map [bin name (list x _)] args))
      (bin name (cons x args))))

(mac binaries args
  `(do ,@(map (fn (x)
                `(defjs ,x args (bin ',x args)))
              args)))


(binaries + - * /)

(defjs or args
  (bin "||" args))

(defjs and args
  (bin "&&" args))

(defjs is args
  (binand "===" args))


(= name-rules* '(("-"  "_")
                 ("w/" "with_")))

(def mangle-name (x)
  (multisubst name-rules* (string x)))

(def fncall (f . args)
  (string (mangle-name f) "(" (addsep "," args) ")"))


(defjs mac (name parms . body)
  (do (eval `(assign ,name (annotate 'mac (fn ,parms ,@body))))
      nil))


(def tojs (x)
  (err "unknown expression" x))

(extend tojs (expr) (acons expr)
  (let x (macex expr)
    (if (is x expr)
          (apply fncall (tojs:car x) (cdr x))
        (tojs x))))

(extend tojs (x) (isa x 'string)
  (string "\"" x "\""))

(def js-literal? (x)
  (in (type x) 'int 'num 'sym))

(extend tojs (x) (js-literal? x) x)
(extend tojs (x) (no x) "undefined")

(extend tojs (x) (isa x 'char)
  (tostring (pr #\") (pr x) (pr #\")))

(extend tojs (x) (errsafe:js-rules*:car x)
  (apply it (cdr x)))


(def dispfile (x f)
  (w/outfile o f (disp x o)))

(def arc2js (arc js)
  (w/infile f arc
    (w/uniq eof
      (let x (string:intersperse (string linesep linesep)
               (rem empty (drain (tojs:read f eof) eof)))
        (dispfile x js))))
  t)
