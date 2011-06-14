(use arc strings)

(implicit linesep "\n")
(implicit indent  "    ")
(implicit spaces  " ")

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


(def tojsparms (x)
  (if (no x)
        (string "()")
      (isa x 'cons)
        (string "(" (addsep "," x) ")")
      (err "invalid argument list" x)))

(def line args
  (string indent args ";" linesep))

(defjs fn (parms . body)
  "(function" spaces (tojsparms parms) spaces "{"
  (if body linesep)
  (let x (map tojs body)
    (whenlet y (nthcdr (- (len x) 1) x)
      (= (car y) (string "return " (car y)))
      (map line x)))
  "})")


(defjs list args
  "[" (addsep "," args) "]")

(defjs assign (x y)
  x spaces "=" spaces (tojs y))

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


(def binary (name args)
  (addsep (string spaces name) args))

(mac binaries args
  `(do ,@(map (fn (x)
                `(defjs ,x args (binary ',x args)))
              args)))

(binaries + - * /)

(defjs is args
  (binary "===" args))


(= name-rules* '(("-" "_")))

(def mangle-name (x)
  (multisubst name-rules* (string x)))

(def fncall (f . args)
  (let x (errsafe:eval f)
    (if (isa x 'mac)
          (tojs:macex:cons f args)
        (string (mangle-name f) "(" (addsep "," args) ")"))))


(def tojs (x)
  (err "unknown expression" x))

(extend tojs (x) (acons x)
  (if (acons (car x))
        (apply fncall (tojs:car x) (cdr x))
      (apply fncall x)))

(extend tojs (x) (isa x 'string)
  (string "\"" x "\""))

(def js-literal? (x)
  (in (type x) 'int 'num 'sym))

(extend tojs (x) (js-literal? x) x)
(extend tojs (x) (no x) "undefined")

(extend tojs (x) (errsafe:js-rules*:car x)
  (apply it (cdr x)))


(def arc2js (arc js)

  )
