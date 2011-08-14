(use arubic)

;(def debug args (apply prn args))

(implicit linesep         "\n")
(implicit indent          "    ")
(implicit spaces          " ")
(implicit terminator      ";")
(implicit gensym-name     "__g")
(implicit uniq-counter    1)
(implicit undefined       '|void 0|) ;; ew
(implicit gensym-replace  "abcdefghijklmnopqrstuvwxyz")

(implicit optimize?       t)
(implicit strict?         t)
(implicit wrapper?        t)
(implicit shorten?        t)
;(implicit unsafe?         t)

(implicit wrap            nil)
(implicit first           t)
(implicit local           nil)
(implicit local-vars      nil)
(implicit global          t)
(implicit global-vars     nil)
(implicit global-assign   nil)
(implicit replace         nil)
(implicit varlist         nil)
(implicit precedence      0)
(implicit islast          nil) ;;


(mac w/mode (x . body)
  (case x
    minify `(w/linesep   ""
            (w/indent    ""
            (w/spaces    ""
            (w/optimize? t
            (w/shorten?  t
              ,@body)))))
    (err "unknown mode" x)))


(= output-rules*  (table)
   mac-rules*     (table)
   operators*     (table)
   unary-ops      (table)
   op-precedence  (table))


(mac defop (name parms . body)
  `(= (operators* ',name) (fn ,parms ,@body)))

(mac compiler-mac (name parms . body)
  `(= (mac-rules* ',name) (fn ,parms ,@body)))

(mac defoutput (name parms . body)
  `(= (output-rules* ',name) (fn ,parms ,@body)))


(defoutput compose (x y)
  ;; TODO: clunky
  (compile-expr:macex1 `(compose ,x ,y)))

(defoutput complement (x)
  ;; TODO: clunky
  (compile-expr:macex1 `(complement ,x)))

#|(compiler-mac arc (x)
  (debug "arc" x)
  ;(lit:compile:eval x)
  nil)|#

(compiler-mac arc (x)
  ;(debug "arc" x)
  (lit:compile:eval x))

#|(defjsmac arc (x)
  (jslit:tojsexpr:eval x))|#


(mac compiler-alias (x (o y x))
  (w/uniq u
    `(compiler-mac ,x ,u
       ;(debug (compile (eval `(,',y ,@,u))))
       ;(compile-expr `(arc (,',y ,@,u)))
       ;nil
       `(arc (,',y ,@,u)))))

(compiler-alias mac compiler-mac)
(compiler-alias use)


(mac def-operators args
  `(do ,@(mappend (fn ((type . args))
                    (mapeach (from to pri) (tuples args 3)
                      (if (is type 'unary)
                            (= (unary-ops from) pri)
                          (is type 'noop)
                            (= (op-precedence from) pri)
                          `(do (= (op-precedence ',from) ,pri)
                               (defoutput ,from args
                                 ;(debug ',from ',to args ',type (operators* ',type))
                                 (lit:apply (operators* ',type) ',to args))))))
                  args)))

(defop prefix (name x)
  (str name compile.x))

(defop suffix (name x)
  (str compile.x name))

;; TODO: could use a better name
(def binwrapper (wrap name args)
  (if (cdr args)
        (let last car.args
          (binwrap wrap (mapeach x cdr.args
                          (do1 (bin name last x)
                               (= last x)))))
      (apply bin name args)))

(defop binor (name . args)
  (binwrapper "||" name args))

(def binwrap (name args (o pre spaces))
  ;(debug "binwrap" name args)
  (str:intersperse (str pre name spaces) args))

(def bin (name . args)
  ;(debug "bin" name args)
  (w/global nil
    (binwrap name (map compile args))))

(def math-wrap (name x)
  (if (cons? x)
        (withs (c     (car x)
                wrap  (if cddr.x
                            (is op-precedence.c precedence)
                          (unary-ops c)
                            (is c name)))
          (str (if wrap "(")
               (compile x)
               (if wrap ")")))
      (compile x)))

(defop math (name . args)
  ;(debug name (unary-ops name) unary-ops args)
  (if (cdr args)
        (binwrap name (mapeach x args (math-wrap name x)))
      (unary-ops name)
        (str name (math-wrap name car.args))
      (car args)))

(= operators*!bin bin)

(defop premulti (name . args)
  (str name (compile `(+ ,@(intersperse " " args)))))

(defop binand (name . args)
  ;(debug "binand" name args)
  (binwrapper "&&" name args))

(defop trinary ((l r) . args)
  (w/global nil
    ((afn (x)
       (if (no:cdr x)
             (compile:car x)
           (no:cddr x)
             (compile `(and ,@x))
             #|(w/precedence 5
               (str (tojsexpr:car x)
                       spaces "&&" spaces
                       (tojsexpr:cadr x)))|#
           (str (compile:car x)
                spaces l spaces
                (compile:cadr x)
                spaces r spaces
                (self cddr.x))))
     args)))

(defop bin= (name . args)
  ;; TODO: is this needed?
  ;(zap remlast args nil)

  ;(zap uniq-table var var)

  (let wrap nil
    (= args (mapeach x args
              ;(debug "bin=" x)
              (uniq-table x x)))

    (let args (if cdr.args
                    (cut args 0 -1)
                  args)

      ;; TODO: hacky
      (when (or no.args
                (all sym? args))
        (= wrap t))

      ;(debug "assign" args local-vars)

      (each x args
        (unless (or global
                    (no:sym? x)
                    ;(ac-ssyntax x)
                    (some x local-vars))
          (push x global-assign))))

    ;(debug "assign" name args global)

    #|(and global
             ;(no nested)
             )|#

    (if (and global wrap)
          (if (or (no:cdr args)
                  (and (no:cddr args)
                       (caris cdr.args nil)))
                nil
              (lit "var " (apply bin name args)))
        (apply bin name args))))

(defop binsep (name . args)
  (str:intersperse (if local (str name linesep indent (* spaces 2))
                             (str name spaces))
                   (map compile args)))

#|(mac unary args
  (each (from to pri) (tuples args 3)
    (= (unary-ops from) pri)))|#
#|
(defop unary (name x)

  )|#

(defop incr (name x (o y 1))
  (let (name l r) name
    ;(debug "incr" name l r "--" x y "--" precedence)
    (if (is y 1)
          (lit name compile.x)
        (w/precedence r
          (lit compile.x spaces l spaces compile.y)))))



(def lit args
  ;(debug "lit" args)
  (annotate 'literal (apply sym args)))

(extend coerce (x type . r) (isa x 'literal)
  ;(debug "coerce" x type)
  (case type
    string (str:rep x)
    sym    (sym:rep x)
           (apply orig x type r)))



(def compiler-ssexpand (x)
  (if (ac-ssyntax x)
        (ac-expand-ssyntax x)
      x))

(def nested-pair (name x)
  ;(debug x)
  ((afn ((x . rest))
     ;(debug x rest)
     (if (cdr rest)
           `(,name ,(self rest) ,x)
         `(,name ,(read:str:car rest) ,x)))
   (rev x)))

(extend compiler-ssexpand (x) (ac-insym? #\! x)
  (nested-pair 'ref (tokens str.x #\!)))

(mac letis (var test then else)
  (w/uniq u
    `(let ,u ,test
       (if (is ,u ,var)
             (let ,var ,u ,then)
           (let ,var ,u ,else)))))

(def compiler-macex1 (x)
  ;(debug "compiler-macex1" x)
  (aif (errsafe:mac-rules*:car x)
         (apply it cdr.x)
         ;(debug "compiler-macex1" x y)
       (macex1 x)))

#|(def compiler-macex (x)
  ;(prn x)
  ;(errsafe:debug "compiler-macex" x (atom x) (output-rules*:car x))
  (if (or (atom x);(isa x 'js-literal)
          (output-rules*:car x)
          ;(js-opt-global-rules*:car x)
          ;(js-opt-fn-rules*:car x)
          )

        x
      (caris x 'fn)
        `(,car.x ,cadr.x ,@(map compiler-macex cddr.x))
      (letis x compiler-macex1.x
        x
        (compiler-macex x))))|#

;; TODO: move somewhere else... is this needed?
(def dottedmap (f x)
  (afneach (x . rest) x
    ;(debug "dottedmap" x rest (type rest))
    (cons (f x)
          (if (or no.rest cons?.rest) ;(and rest (no:cons? rest))
                (self rest)
              (f rest)))))


(def info args
  (annotate 'compiler-info (listtab:pair args)))
;  `(annotate 'compiler-info (obj ,@args)))

(extend keys (x) (isa x 'compiler-info)
  (orig:rep x))

(defcall compiler-info (x . args)
  (apply rep.x args))

(extend sref (x . args) (isa x 'compiler-info)
  (apply orig rep.x args))

;(extend ac (x env) (isa x 'compiler-info) nil)

(defrule print (isa x 'compiler-info)
  (w/indent-level (+ indent-level 1)
    (disp "#info(" port)
    (printwith-table primitive x (sort < (keys x)) port) ; (sort < (keys x))
    (disp ")" port)))

;(new-table type  'compiler-info
;           print (printwith-table "#info"))



#|
;; TODO: some code duplication between find-global and find-inner
(def find-global (x)
  (if (no x)
        nil
      (caris x 'quote)
        nil
      (caris x 'fn)
        (w/local-vars (join cadr.x local-vars)
          (mappend find-global cddr.x))
      (cons? x)
        (mappend find-global x)
      (and (sym? x)
           (none x local-vars))
        [x]))|#

(def find-local (x f)
  (if (no x)
        nil
      (caris x 'quote)
        nil
      (caris x 'fn)
        (w/local-vars (join cadr.x local-vars)
          (mappend (^:find-local _ f) cddr.x))
      (cons? x)
        (mappend (^:find-local _ f) x)
      (and (sym? x)
           (f x local-vars))
        [x]))


(def tocomplexfn (x body)
  (cons ((afn (x i)
           ;(debug "tocomplexfn" (no x) (sym? x) x i)
           (if (no x) nil
               (sym? x)
                 (when body
                   (= body `(((fn (,x)
                                ,@body)
                              ((ref (ref (ref Array 'prototype) 'slice) 'call) arguments ,i)
                              #|,(lit "Array.prototype.slice.call(arguments,"
                                    spaces i
                                    ")")|#
                              )))
                   nil)
               (let c car.x
                 ;(debug "tocomplexfn" (caris c 'o) (or c (uniq)) (cdr x))
                 (if (caris c 'o)
                       (let n cadr.c
                         (awhen (and body caddr.c)
                           (= body `((or ,n (assign ,n ,it)) ,@body)))
                         (cons n (self cdr.x (1+ i))))
                     (cons (or c (uniq))
                           (self cdr.x (1+ i)))))))
         x 0)
        body))

(def norm-parms (x body)
  ;(debug "norm-parms" x body)
  (if (no x)
        (cons nil body)
      (cons? x)
        (tocomplexfn x body)
      (sym? x)
        [nil (when body
               `((fn (,x)
                   ,@body)
                 ((ref (ref (ref Array 'prototype) 'slice) 'call) arguments)))]
                    ;,(lit  "Array.prototype.slice.call(arguments)")
      (err "invalid argument list" x)))


(def genrepl (x)
  ;; TODO: figure out a better way to handle duplicate args
  (let tab (table)
    (map (fn (x) ;; TODO: dottedmap
      (let y (aif (uniq? x)
                    x
                  (tab x)
                    it
                  (= (tab x) (uniq)))
        [x y]))
      x)))

#|(def var-replace (x)
  ;(debug x)
  (aif (cons? x)
         (map var-replace x)
       (and (sym? x)
            (assoc x replace))
         (cadr it)
       x))|#


(def expand (x)
  ;(prn x)

  (when sym?.x
    (zap compiler-ssexpand x))

  ;(debug "expand" x)

  (if (or (sym? x)
          (caris x 'quote))
        x
      (caris x 'fn)
        `(,car.x ,cadr.x ,@(map expand cddr.x))
      (cons? x)
        (if (output-rules*:car x)
              ;; TODO: clunky
              `(,car.x ,@(map expand cdr.x))
            (letis x compiler-macex1.x
              (map expand x) ;; TODO: dottedmap
              (expand x)))
        #|(do (zap compiler-macex x)
            (zap2 dottedmap expand x)
            x)|#
        ;(do (debug "expand" x compiler-macex.x)
        ;)
        #|(do (zap compiler-macex x)
            ;(debug x)
            (if (output-rules*:car x)
                  (cons car.x (dottedmap expand cdr.x))
                (dottedmap expand x)))|#
        ;(map temptojs (compiler-macex x))
      x))

(def normalize (x)
  ;(debug "normalize" x)
  (if (caris x 'fn)
        (let (parms . body) (norm-parms cadr.x cddr.x)
          ;(debug "normalize" parms body)
          `(fn ,parms ,@(map normalize body)))
      (cons? x)
        (map normalize x) ;; TODO: dottedmap
      x))


(def shorten (x) x)

(extend shorten (x) shorten?
  ;(debug "shorten" x)
  (aif (caris x 'fn)
         (let parms (genrepl cadr.x)
           `(fn ,(map cadr parms)
                ,@(w/replace (join parms replace)
                  (w/local t
                    (map shorten cddr.x)))))

       #|(isa x 'compiler-info)
         (do ;(zap shorten-parms x!parms)
             ;(debug)
             (let parms (genrepl x!parms)
               ;(debug x)
               ;(debug)
               (= x!parms (map cadr parms))
               (w/replace (join parms replace)
                 ;(debug replace x!inner)
                 (zap var-replace x!inner)
                 ;(zap var-replace x!body)
                 (zap2 map shorten x!body)
                 ;(= x!inner (var-replace x!inner))
                 ;(= x!inner (var-replace x!inner))
                 ;(debug (var-replace x!body))
                 ;(= x!body (var-replace x!body))
                 ))
             ;(map shorten x!body)
             x)|#
       (and local (caris x 'var))
         ;; TODO: need a function to return every n elements, like (cut cdr.x nil nil 2)
         ;; or maybe (every-n cdr.x 2)
         (do (zap2 join (genrepl:map car (pair cdr.x)) replace)
             ;(debug "shorten" replace)
             (map shorten x))
       (cons? x)
         (map shorten x) ;; TODO: dottedmap
       (and (sym? x)
            (assoc x replace))
         (cadr it)
       x))


(def analyze (x)
  ;(debug "analyze" x)

  (if (caris x 'fn)
        (with (inner  (dedup:find-local cddr.x some)
               old    local-vars)
          (w/local-vars (join cadr.x old)
            (info 'name    car.x
                  'parms   cadr.x
                  'inner   inner
                  'global  (dedup:find-local cddr.x none)
                  'body    (map analyze cddr.x))))
      (cons? x)
        (map analyze x)
      x))


#|(def optimizefn (x)
  (iflet c (and (cons? x)
                (caris car.x 'fn)
                (car x))
    ;(debug c)
    (let parms (genrepl:cadr c)
      ;(= varlist parms)
      ;(debug parms)
      ;(debug cadr.c)


      ;(debug "optimizefn" parms cdr.x)
      (w/replace parms
         #|(zap2 join (zip (map var-replace cadr.c)
                         cdr.x)
                    varlist)|#
         (or (mappend optimizefn cddr.c)
             [nil])))
      #|(do1 ;(zap join varlist (zip (map cadr parms) cdr.x))
             ) ; (map var-replace cadr.c)|#
    (list:var-replace x)))|#
#|
(def optimizedo1 (x)
  (debug "optimizedo1" x)
  (if (caris x 'do)
        (mappend optimizedo1 cdr.x)
      [x]))

(def optimizedo (x)
  ;(debug "optimizedo" x)
  (if #|(caris x 'do)
        (mappend optimizedo cdr.x)|#
      (caris x 'fn)
        [`(fn ,cadr.x ,@(mappend optimizedo1 cddr.x))]
      (cons? x)
        (let c car.x
          (if (and (caris c 'fn)
                   (no cadr.c)
                   (no cdr.x))
                (optimizedo `(do ,@cddr.c))
              [(mappend optimizedo x)]))
      [x]))|#

(def optimizedo (x)
  ;(debug "optimizedo" x)
  (if (caris x 'do)
        (mappend optimizedo cdr.x)
      (cons? x)
        (let c car.x
          (if (and (caris c 'fn)
                   (no cadr.c)
                   (no cdr.x))
                (optimizedo `(do ,@cddr.c))
              [(mappend optimizedo x)])) ;; TODO: dottedmap
      [x]))

#|(def optimizelet1 (x y)
  (let c car.y
    ;; TODO: incredibly clunky
    (let (l r) (splitlast cddr.c)
      `((,car.c ,cadr.c ,@l (,car.x ,@r ,@cddr.x)) ,@cdr.y))))|#
#|
(def optimizelet1 (x)
  ;; TODO: clunky
  (var wrap nil)
  (let x (cons car.x
               (afneach (x . rest) cdr.x
                 (if (and (cons? x)
                          (caris car.x 'fn))
                       (let c car.x
                         (if first
                               (let (l r) (splitlast cddr.c)
                                 ;(debug "optimizelet1 - l" l)
                                 ;(debug "optimizelet1 - r" r)
                                 ;(debug "optimizelet1 - x" x)
                                 (zap join (car  varlist) cadr.c)
                                 (zap join (cadr varlist) cdr.x) ;[`(do ,@cdr.x ,@l)]
                                 ;; TODO: clunky
                                 (def wrap (x)
                                   ;(debug "optimizelet1" x)
                                   `(do ,@l ,x))
                                 (= x car.r))
                             (do (zap join (car  varlist) cadr.c)
                                 ;(push cadr.c (car  varlist))
                                 ;(push nil (cadr varlist))
                                 (zap join (cadr varlist) '(nil))
                                 (= x `(do ,@(mapeach x (zip cadr.c cdr.x)
                                               (cons 'assign x))
                                           ,@cddr.c))))
                             ;(zap join varlist (mapeach x cadr.c (list x nil)))
                             ;(zap join varlist (zip cadr.c cdr.x))
                         (w/first nil
                           (cons x self.rest)))
                     #|(cons? x)
                       (map self x)|#
                     (w/first nil
                       (when cons?.x
                         (zap optimizelet1 x))
                       ;(debug "optimizelet1" x rest)
                       (cons x self.rest)))))
    ;; TODO: clunky
    (when wrap
      (zap wrap x))
    x))

(def optimizelet (x)
  ;(debug "optimizelet" x)
  (if (and optimize?
           (cons? x)
           ;(debug "optimizelet" x (cons? x) (no:cons? car.x) (no:caris car.x 'fn))
           ;; TODO: is this needed?
           (no:caris x 'fn)
           (no:caris car.x 'fn)
           ;(no:cons? car.x)
           )
        (w/varlist [nil nil]
          (zap optimizelet1 x)
          ;(zap2 map optimizelet x)
          (debug "optimizelet" x)
          (if car.varlist
                (let x `((fn ,car.varlist ,x) ,@cadr.varlist)
                  ;(debug x)
                  x)
              x))
          #|(let c cadr.x
            (if (and (cons? c)
                     (caris car.c 'fn))
                  (w/varlist nil

                    (ret [x]))
                (ret [x])))|#
      x))|#

(def optimizelet1 (x)
  ;(debug "optimizelet1" x)
  (aif (and (cons? x)
            (caris car.x 'fn)
            car.x)
         (if first
               (let (l r) (splitlast cddr.it)
                 (zap join   car.varlist  cadr.it)
                 (zap join  cadr.varlist  cdr.x)
                 ;; TODO: clunky
                 (redef wrap (x)
                   `(do ,@l ,x))
                 car.r)
             (do (zap join   car.varlist  cadr.it)
                 (zap join  cadr.varlist  '(nil))
                 `(do ,@(mapeach (x y) (zip cadr.it cdr.x)
                          `(assign ,x ,y))
                      ,@cddr.it)))
       (caris x 'var)
         (let (l r) (apply zip (pair cdr.x))
           (if first (do (zap join   car.varlist  l)
                         (zap join  cadr.varlist  r)
                         (x (- len.x 2)))
                     (do (zap join   car.varlist  l)
                         (zap join  cadr.varlist  '(nil))
                         `(do ,@(mapeach (x y) (pair cdr.x)
                                  `(assign ,x ,y))))))
       x))

(def optimizelet (x)
  ;(debug "optimizelet" x)
  (w/varlist [nil nil]
    ;(debug "optimizelet" x)
    (w/wrap nil
      (let x (list* car.x
                    (if (cons? cdr.x)
                          (cons (optimizelet1 cadr.x)
                                (w/first nil
                                  (map optimizelet1 cddr.x)))
                        cdr.x))
        (debug "optimizelet" x)
        (when wrap (zap wrap x))
        (if car.varlist
              `((fn ,car.varlist ,(optimize x)) ,@cadr.varlist)
            x)))))

#|(def optimizevar1 (x y z)
  `(do ,y (,x ,cadr.y ,z)))

(def optimizevar (x)
  (let last nil
    (each x x

      )))|#

(def optimize (x)
  ;(debug "optimize" x)
  (if (caris x 'fn)
        ;; TODO: clunky
        (let y (map optimize cddr.x) ;; TODO: dottedmap
          ;(debug "optimize" x) ;(mappend optimizedo cddr.x))
          ;(debug cddr.x)
          ;(debug "optimize")
          ;(debug y)
          ;(debug (mappend optimizedo y))
          `(fn ,cadr.x ,@(if optimize?
                               (mappend optimizedo y)
                             y)))
      (and (caris x 'do)
           (no:cddr x))
        cadr.x
      (cons? x)
        (map optimize (if optimize?
                            optimizelet.x ; (optimizevar )
                          x)) ;; TODO: dottedmap
        ;(map optimizelet x)
      x))
#|(if (isa x 'compiler-info)
        (w/varlist nil
          (let body (optimizefn x!body)
          ;`(fn ,x!parms ,@(optimizefn x!body))
            `(fn ,x!parms
                 ,@(if varlist (varblock varlist))
                 ,@(map optimizefn body))))
      (cons? x)
        (map optimize x)
      x))|#


(def remlast (x f)
  (with (last 0
         i    0)
    (each x x
      (++ i)
      (when (isnt f x)
        (= last i)))
    (cut x 0 last)))

(def addsep (x args)
  ;(debug "addsep" x args)
  (intersperse (str x spaces) (map compile args)))

(def fncall (f x args)
  (catch
    ;; TODO: this is clunky
    #|(awhen (mac-rules* f)
      (throw:compile:apply it args))|#

    (zap remlast args nil)

    ;(debug "fncall" f args)
    ;(debug "foo!" (addsep "," args))

    ;(let wrap (isa f 'compiler-info) ;(caris f 'fn))
    #|(when (ac-ssyntax f)
      (zap compiler-ssexpand f))|#

    (awhen car.args
      (when (and (caris it 'ref)
                 (no:cadr it))
        (throw:fncall `(ref ,f ,@cddr.it) x cdr.args)))

    (if (caris f 'compose)
          (compile-expr `(,(cadr f) (,(caddr f) ,@args)))
        (caris f 'complement)
          (compile-expr `(no (,(cadr f) ,@args)))

        (lit (w/precedence 16 compile.f)
             (or (and local #|wrap |#no.x
                      (some 'this global-vars)
                      (zap2 cons 'this args)
                      ".call")
                 x)
             "(" (w/global nil (w/precedence 2 (addsep "," args))) ")"))))


(def mangle-name (x)
  ;(debug "mangle-name" x)
  (re-multi-replace x ("^[^$_a-zA-Z]"    "_&"  )
                      ("^[^$_a-zA-Z]"    "_"   )
                      ("[^$_a-zA-Z0-9]"  "_"  g)))

(def lines (x)
  ;(debug "lines" x)
  (lit:intersperse (str terminator linesep linesep) x))


(def false? (x)
  (in x nil 'false 'null 'NaN 0 ""))


(def compile (x)
  ;(zap expand x)

  ;(debug "compile" x (str x))

  (aif (isa x 'int 'num 'literal)
         x
       (uniq? x)
         (touniq x)
       (and (isa x 'compiler-info)
            (output-rules* x!name))
         (it x)
       (cons? x)
         (aif (caris x 'do)
                (if optimize?
                      (if global
                            (lines:map compile cdr.x)
                          ;; TODO: clunky
                          (apply (output-rules* car.x) cdr.x))
                      (compile-expr `((fn () ,@cdr.x)))) ;; TODO: use compile rather than compile-expr
                                                         ;; it needs to use compile-expr because of the
                                                         ;; analyze phase
                #|(and global
                     optimize?
                     )|#

              (output-rules* car.x)
                (apply it cdr.x)
              (fncall car.x nil cdr.x))
       (or (str? x)
           (char? x))
         (lit "\"" x "\"")
       (no x)
         (lit undefined)
       (is x 't)
         'true
       (sym? x)
         (lit:mangle-name:str x)
       (fn? x)
         nil
       (err "bad expression" x)))

(def fn-precedence (it x . args)
  ;(debug args)
  (let n args
    (= x (w/precedence it (apply x args)))
    (if (and n (> precedence it))
          (str "(" x ")")
        x)))

(extend compile (x) (errsafe:or (and (isa x 'compiler-info)
                                     (op-precedence x!name))
                                (and (no:cddr x)
                                     (unary-ops:car x))
                                (op-precedence:car x))
  ;(debug "precedence" x precedence it (> precedence it))
  (fn-precedence it orig x)
  ;(prn " " (car x) " " precedence " " it)
  #|(let n (cddr x)
    (= x (w/precedence it (orig x)))
    (if (and n (> precedence it))
          (str "(" x ")")
        x))|#
        )

(def compile-expr (x)
  (compile:analyze:optimize:shorten:normalize:expand x))

(def compile-global (x)
  )


;; TODO: ew
(redef uniq ((o n))
  (racket-string->uninterned-symbol:str (or n gensym-name)))

#|(extend print (primitive x port) (uniq? x)
  (primitive (sym x uniq-counter) port))|#

(= uniq-table (table))

(mac is-global args
  (w/uniq u
    `(let ,u (sym ,@args)
       ;(debug ,u x global-vars)
       ;(prn "foo!! " ,u " " global-vars " " (some ,u global-vars))
       (if (some ,u global-vars)
             (self:+ y 1)
           (do (= uniq-counter (+ y 1))
               ;(debug "touniq" ,u global-vars)
               ;(push ,u global-vars)
               (= (uniq-table x) ,u))))))

(def touniq (x)
  ;(debug "touniq" x (uniq-table x) global-vars)
  (if (uniq? x)
        ;(debug "touniq" x (uniq-table x))
        (iflet y (uniq-table x)
          (if shorten?
            (if (sym? y)
                  y
                (let len (len gensym-replace)
                  (zap dedup global-vars)
                  ;(prn global-vars)
                  ;(debug "uniq" y)
                  ((afn (y)
                     (withs (y-  (- y 1)
                             l   (- y- len))
                       ;(debug "touniq" y l len )
                       (if ;(>= l len)
                           (>= l 0)
                             (is-global (gensym-replace (mod l len)) ;(ceil:/ l len)
                                        (floor:/ y- len))
                             #|(is-global (gensym-replace l)
                                        (floor:/ y- len))|#
                           (is-global:gensym-replace y-))))
                   y)))
            (lit x y))
          (do ;(debug "touniq" x uniq-counter)
              (= (uniq-table x) uniq-counter)
              (++ uniq-counter)
              (touniq x)))
      x))


#|(= uniq-table (table))

(def touniq (x)
  (iflet y (uniq-table x)
    (sym "__g" y)
    (do (= (uniq-table x) uniq-counter)
        (++ uniq-counter)
        (touniq x))))

(extend print (primitive x port) (uniq? x)
  (primitive (touniq x) port))|#


;; TODO: look for an alternative to dottedmap
