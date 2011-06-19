(use arc strings)

(implicit linesep     "\n")
(implicit indent      "    ")
(implicit spaces      " ")
(implicit gensym-name "__g")

(implicit global      t)
(implicit local       nil)
(implicit precedence  0)

(implicit optimize?   t)
(implicit readable?   t)
(implicit strict?     t)
(implicit wrapper?    t)

(dynamic  replace     nil)

(mac w/whitespace (x . body)
  (case x
    minify `(w/linesep   ""
            (w/indent    ""
            (w/spaces    ""
            (w/readable? nil
            (w/optimize? t
              ,@body)))))
    (err "unknown whitespace mode" x)))


(= js-rules*            (table)
   js-mac-rules*        (table)
   js-opt-fn-rules*     (table)
   js-opt-global-rules* (table))
;   js-replace-rules* (table))

(mac defjs (name parms . body)
  `(= (js-rules* ',name) (fn ,parms (string ,@body))))

(mac defjsmac (name parms . body)
  `(= (js-mac-rules* ',name) (fn ,parms ,@body)))

(mac defoptfn (name parms . body)
  `(= (js-opt-fn-rules* ',name) (fn ,parms ,@body)))

(mac defoptglobal (name parms . body)
  `(= (js-opt-global-rules* ',name) (fn ,parms ,@body)))

#|(def replace (x v)
  (= (js-replace-rules* x) v))|#

(mac replaceall (x . body)
  `(dlet replace (if replace (join ,x replace) ,x) ,@body))

(mac w/replace (from to . body)
  `(replaceall ((,from ,to)) ,@body))


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


(= origindent indent)

(mac reindent/if (x . body)
  `(w/indent (if ,x (string indent origindent)
                    (= origindent indent))
     ,@body))


(def lines (x)
  (sym:string:intersperse (string ";" linesep linesep) x))


#|(extend optfn ((x . rest)) (and (caris x 'with)
                                     (no rest))
  (join (map (fn ((x y))
               (sym:string "var " (tojs x) spaces "=" spaces (tojs y)))
             (pair:cadr x))
        (cddr x)))|#


(defoptfn do args args)

#|(defoptfn let (n v . body)
  (w/uniq u
    (cons (sym:string "var " u spaces "=" spaces (tojs v))
          (or (w/replace n u (map sym:tojs body))
              (list nil)))))|#

;; should be in arc.arc
(mac mapeach (var expr . body)
  `(map (fn (,var) ,@body) ,expr))

;; should be in arc.arc
(extend * (x . args) (isa x 'string)
  (apply string (n-of (apply + args) x)))

(defoptfn with (parms . body)
  (let acc nil
    (cons (sym:string "var "
            (intersperse (string "," linesep indent (* spaces 4))
              (mapeach (n v) (pair parms)
                ;(prn " " n " " v " ")
                (w/uniq u
                  (push (list n u) acc)
                  (list u spaces "=" spaces (tojs v))))))
          (or (replaceall acc (map sym:tojs
                                   (mappend [optimize optfn _]
                                            body)))
              (list nil)))))
#|
  (w/uniq u
    (cons (sym:string "var " u spaces "=" spaces (tojs v))
          (or (w/replace n u (map sym:tojs body))
              (list nil)))))|#


(defoptglobal do args
  (list:lines:map tojs args))


#|(extend optglobal (x) (caris x 'do)
  (list:lines:map tojs (cdr x)))|#


(extend macex (x (o once)) (errsafe:js-mac-rules*:car x)
  (iflet x (apply it (cdr x))
    x
    (orig x once)))

(extend macex (x (o once)) (and local (errsafe:js-rules*:car x))
  (sym:apply it (cdr x)))

(mac letis (var test then else)
  (w/uniq u
    `(let ,u ,test
       (if (is ,u ,var)
             (let ,var ,u ,then)
           (let ,var ,u ,else)))))

#|(if local (optfn expr)
          (optglobal expr))|#

(def optimize (f expr)
  ;(if (js-opt-global-rules*:car expr) (prn expr))
  ;(prn " " f " " expr)

  ;(prn " " expr)

  (letis expr (if optimize? (f expr) expr)
    (letis expr (macex expr t)
      (list expr)
      (optimize f expr))
    expr))
        #|(do (if local (zap optfn expr)
                      (zap optglobal expr))
            (let x (macex expr t)
              ;(prn " " x)
              ;(prn " " expr)
              (if (is x expr)
                    x
                  (optimize x))))|#
;      (list expr)))

(def optglobal (x) x)
  ;(prn x)
;  (optimize idfn x))

(def optfn (x) x)
  ;(prn x)
;  (optimize idfn x))

(extend optglobal (x) (errsafe:js-opt-global-rules*:car x)
  (apply it (cdr x)))
;  (optimize [apply it (cdr _)] x))

(extend optfn (x) (errsafe:js-opt-fn-rules*:car x)
  (apply it (cdr x)))
;  (optimize [apply it (cdr _)] x))

#|(extend optglobal (x) (errsafe:js-rules*:car x)
  (sym:apply it (cdr x)))

(extend optfn (x) (errsafe:js-rules*:car x)
  (sym:apply it (cdr x)))|#

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
  (w/local t
    (= body (mappend [optimize optfn _]
                     #|(fn (x)
                       (zap optimize x)
                       (prn x)
                       (if (acons:car x)
                             x
                           (list x)))|#
                     body))
    ;(zap optimize body)
    ;(prn body)
    nil)

  "function" spaces (tojsparms parms body) spaces "{"

  (reindent/if local
    (w/local t
      (= body (map tojs body))

      ;(prn (intersperse "\n" body))

      ;(zap remlast body "void 0")
      ;(prn (last body) " " (type (last body)))

      (unless (is (last body) '|void 0|)
        ;(= body (rem "void 0" body))
        ;(zap remlast body "void 0")
        ;(zap cut body 0 -1)
        (setlast body (string "return " it)))

      (= body (rem '|void 0| body))

      (when body
        (cons linesep (map line body)))))

  (and local body indent)
  "}")


(defjs list args
  "[" (addsep "," args) "]")

(defjs assign (x y)
  (unless (or (ac-ssyntax x) local) "var ")
  (tojs x) spaces "=" spaces (tojs y))

(defjs if args
  ((afn (x)
     (if (no:cdr x)
           (tojs:car x)
         (string (tojs:car x)
                 spaces "?" spaces
                 (tojs:cadr x)
                 spaces ":" spaces
                 (self (cddr x)))))
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
  (do1 (sym:string gensym-name uniq-counter*)
       (++ uniq-counter*)))


#|(defjsmac let (n v . body)
  (if readable?
    `(do (let ,n ,v ,@body))))|#

(defjsmac def (name parms . body)
  `(assign ,name (fn ,parms ,@body)))


(def math (name args)
  ;13
  ;(prn " " name " " args " " precedence)
  (prn " ")
  (string (unless (cdr args) name)
  (binwrap name (map (fn (x)
                       (if (acons x)
                             (let c (car x)
                               (if (is c name)
                                     (w/precedence
                                       (if (errsafe:cddr x)
                                             precedence
                                           13)
                                       (tojs x))
                                   (w/precedence (+ precedence 1)
                                     ;(prn " " c " " name " " precedence " " (op-precedence c))
                                     ;(prn x)
                                     (w/precedence
                                       (if (cddr x)
                                             precedence
                                           0)
                                       (tojs x)))))
                           (tojs x)))
                     args))))

(def binwrap (name args)
  ;(let op (op-precedence name)
    ;(prn " " name " " op " " precedence)
    (string ;(when (< op precedence) "(")
            (intersperse (string spaces name spaces) args)
                ;(when (< op precedence) ")")
                ))
                ;)

(def bin (name args)
  (binwrap name (map tojs args)))
#|                                      (mapeach x args
                                        ;(prn x " " (errsafe:op-precedence:car x) " " precedence)
                                        x)))))|#

(def binand (name (x . args))
  (if (cdr args)
        (binwrap "&&" (map [bin name (list x _)] args))
      (bin name (cons x args))))

(def prefix (name (x))
  (string name (tojs x)))

#|(mac binaries args
  `(do ,@(map (fn (x)
                `(defjs ,x args (bin ',x args)))
              args)))|#


(= op-precedence (obj if      2
                      assign  1))

#|(= op-precedence (obj no    '("!"   4)
                      *     '("*"   5)
                      /     '("/"   5)
                      mod   '("%"   5)
                      +     '("+"   6)
                      -     '("-"   6)
                      <     '("<"   8)
                      <=    '("<="  8)
                      >     '(">"   8)
                      >=    '(">="  8)
                      is    '("===" 9)
                      isnt  '("!==" 9)
                      and   '("&&"  13)
                      or    '("||"  14)))|#

(mac defop (from to precedence type)
  `(do (= (op-precedence ',from) ,precedence
          (op-precedence ',to)   ,precedence)
       (defjs ,from args
         (,type ',to args))))


#|(def makebin (args type)
  ;(prn args)
  `(do ,@(mapeach (x y z) (tuples args 3)
           `(defop ,x ,y ,z ,type))))|#

#|(mac binaries args
  (makebin args 'bin))

(mac binand args
  (makebin args 'binandl))

(mac binor args
  (makebin args 'binor))

(mac prefix args
  (makebin args 'prefix))|#

(mac defops args
  `(do ,@(mappend (fn ((type . args))
                    (mapeach x (tuples args 3)
                      `(defop ,@x ,type)))
                  args)))


(defops
  (math     /     /        12
            *     *        12
            +     +        11
            -     -        11)

  (bin      and   &&       4
            or    \|\|     3)

  (binand   mod   %        12
            <     <        9
            <=    <=       9
            >     >        9
            >=    >=       9
            is    ===      8
            isnt  !==      8)

  (prefix   del   delete\  14
            no    !        13))

#|(defjs or args
  (bin "||" args))

(defjs and args
  (bin "&&" args))

(defjs is args
  (binand "===" args))

(defjs isnt args
  (binand "!==" args))|#


(= name-rules* '(("-"  "_")
                 ("w/" "with_")
                 ("/"  "_")))

(def remlast (x f)
  (with (last 0
         i    0)
    (each x x
      (++ i)
      (when (isnt f x)
        (= last i)))
    (cut x 0 last)))

(def fncall (f . args)
  (zap remlast args nil)

  (string (if (isa f 'string) "(")
          f
          (if (isa f 'string) ")")
          "(" (addsep "," args) ")"))


(defjs mac (name parms . body)
  (do (eval `(mac ,name ,parms ,@body))
      nil))

(defjs use args
  (do (eval `(use ,@args))
      nil))


(def tojs (x)
  (err "unknown expression" x))

(extend tojs (expr) (acons expr)
  (let x (car:optimize optglobal expr)
    (if #|(js-literal? x)
          x|#
        (is x expr)
          (apply fncall (tojs:car x) (cdr x))
        (tojs x))))

(extend tojs (x) (isa x 'string)
  (string "\"" x "\""))

(extend tojs (x) (isa x 'char)
  (tostring (pr #\") (pr x) (pr #\")))


(def mangle-name (x)
  (sym:multisubst name-rules* (string x)))

(extend tojs (x) (isa x 'sym)
  (mangle-name x))


(extend tojs (x) (errsafe:js-rules*:car x)
  (apply it (cdr x)))

(extend tojs (x) (errsafe:op-precedence:car x)
  ;(prn " " (car x) " " precedence " " it)
  (= x (w/precedence it (orig x)))
  (if (> precedence it)
        (string "(" x ")")
      x))


(extend tojs (x) (no x) '|void 0|)

(extend tojs (x) global
  (w/global nil
    ;(= x (orig (optfn (list x))))
    (zap orig x)
    (if (empty x)
          nil
        (string x ";"))))

(def js-literal? (x)
  (and x (in (type x) 'int 'num)))

(extend tojs (x) (js-literal? x) x)

;(extend tojs (x) (js-replace-rules* x) it)
(extend tojs (x) (assoc x replace) (cadr it))


(def dispfile (x f)
  (w/outfile o f (disp x o)))

(def arc2js (arc js)
  (w/infile f arc
    (w/uniq eof
      (let x (drain (read f eof) eof)
        (if strict?
          (push "use strict" x))

        (= x (string:intersperse (string linesep linesep)
                                 (rem no (map tojs x))))

        (if wrapper?
          (= x (string "(function" spaces "()" spaces "{" linesep
                       indent (subst "\n"
                                (string indent "\n")
                                (subst (string "\n" indent)
                                  "\n"
                                  x))
                       linesep
                       "})();")))

        (dispfile x js))))
  t)
