(use arc strings)

#|
(extend * (x . args) (isa x 'string)
  (apply string (n-of (apply + args) x)))|#

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


(def tojsparms (x)
  (if (no x)
        (string "()")
      (isa x 'cons)
        (string "(" (addsep "," x) ")")
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

(defjs fn (parms . body)
  "(function" spaces (tojsparms parms) spaces "{"
  (if body linesep)

  (reindent/if local
    (w/local t
      (let x (map tojs body)
        (setlast x (string "return " it))
        (map line x))))

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
#|(defjs quote (x)
  (string "\"" (tojs x) "\""))|#

#|(defjs bound (x)
  "(function" spaces "()" spaces "{" linesep
  indent "try" spaces "{" linesep
  indent (line "return " (tojs x) spaces "!==" spaces "undefined")
  indent "}" spaces "catch" spaces "(e)" spaces "{" linesep
  indent (line "return false")
  indent "}" linesep
  "}())")|#

#|(defjs def (name parms . body)
  name spaces "=" spaces "function" )|#

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

(redef expand=list (terms)
  (if (cddr terms)
        `(do ,@(map (fn ((p v)) (expand= p v))
                    (pair terms)))
      (apply expand= terms)))

#|(defjs ar-disp (x y)
  "console.log(" (tojs x) ")")|#


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
  (if (acons:car x)
        (apply fncall (tojs:car x) (cdr x))
      (apply fncall x)))

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


(def arc2js (arc js)

  )
