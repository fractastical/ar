(use compiler)

(def-operators
  (prefix    new         "new "      17
             del         "delete "   15
             no          !           14)

  (suffix    len         .length     18)

  (binor     instanceof  instanceof  15)

  ;; ew
  (incr      ++          (++  +=  2)     15
             --          (--  -=  2)     15)

  (math      /           /           13
             *           *           13
             +           +           12
             -           -           12)

  (bin       mod         %           13
             and         &&          5
             or          "||"        3)

  (premulti  err         "throw "    12)

  (binand    <           <           10
             <=          <=          10
             >           >           10
             >=          >=          10
             is          ===         9
             isnt        !==         9)

  (trinary   if          (? :)       3)

  (bin=      assign      =           2)

  (binsep    do          ","         1)

  (unary     +           +           14
             -           -           14)

  ;; ew
  (noop      apply       nil         16
             fn          nil         2
             ;list        nil         2
             )
)


(= origindent indent)

(mac reindent body
  `(w/indent (str indent origindent)
     (w/local t
       ,@body)))

(mac reindent/local body
  `(w/indent (if local (str indent origindent)
                       (= origindent indent))
     (w/local t
       ,@body)))


(def optimizefn (x)
  ;(debug "optimizefn" x)
  (if (cons? x)
        (iflet f (and (isa car.x 'compiler-info) car.x)
          (let body f!body
            ;(debug "optimizefn" x)
            (zap join varlist (zip f!parms cdr.x))
            ;(debug (optimizefn car.body) cdr.body)
            ;(zap2 map optimizefn body)
            ;(debug (optimizefn car.body))
            (let x (join (optimizefn car.body) cdr.body)
            #|(if cdr.body
                   ;(if unsafe?
                         ;(mappend optimizefn body)
                   (join (optimizefn car.body) cdr.body) ;`(do ,@)
                       ;)
                 (optimizefn car.body)
                  ;`(do ,@)
                 )|#
              ;(debug "optimizefn" x)
              x)
            ;(= car.body (optimizefn car.body))
            ;`((fn ,f!parms ,@body) ,@cdr.x)
            ;body
            )
          ;[(mappend optimizefn x)]
          [x]
          )
      [x]))

(def varblock (vars)
  ;(debug "varblock" vars)
  (lit "var "
       (intersperse (sym "," linesep indent (* spaces 4))
                    (mapeach (x y) vars
                      ;(debug "varblock" x y (temptojs2 x))
                      [(compile x)
                       (when y
                         [spaces "=" spaces
                          (w/indent (if (cdr vars)
                                          (sym indent (* spaces 4))
                                        indent)
                            (compile y))])]))))


(def js-identifier? (x)
  (when (str? x)
    (re-match "^[$_a-zA-Z][$_a-zA-Z0-9]*$" x)))

(def js-dot (x y)
  (lit x "." y))

(def js-brackets (x y)
  (lit x "[" (compile y) "]"))

(def js-attr (x y)
  ;; ew
  (when (and (caris y 'quote)
             (sym? cadr.y))
    (zap str:cadr y))

  (when (and (int? x)
             (js-identifier? y)) ;; ew
    (zap lit x "."))

  (zap compile x)

  (if (js-identifier? y)
        (js-dot x y)
      (js-brackets x y)))


(def js-literal? (x)
  (and x (isa x 'int 'num 'literal)))


(mac defjscall (x y)
  `(defoutput ,x args
     (lit ',y "(" (addsep "," args) ")")))

(defjscall prn   console.log)
(defjscall pr    console.dir)
;(defjscall err   console.error)
(defjscall warn  console.warn)


(compiler-mac list* args
  (let (args last) (splitlast args)
    `(join (list ,@args) ,@last)))


(compiler-mac or= args
  `(do ,@(mapeach (x y) (pair args)
           (if (cons? x)
             (zap (^:apply js-attr _) x))

           `(or ,x (= ,x ,y)))))


(compiler-mac zap (x y . args)
  `(= ,y (,x ,y ,@args)))
#|
  `(if (is ,x nil)
         (= ,x ,y)
       ,x))|#

;; TODO: should I provide both str and string?
(compiler-mac str args
  `(+ "" ,@args))


#|(defoutput uniq (x)
  ;; TODO: clunky
  (debug "uniq" args (uniq x))
  ;(compile:str:compile `(arc (uniq ,@args)))
  (compile:str:compile:apply uniq args))|#

(compiler-mac uniq ((o x))
  `(arc:str:compile:uniq ,x))


(compiler-mac each (var x . body)
  (w/uniq (u l)
    (if (sym? x)
          `(let ,l (len ,x) ;,var  nil
             (for ,u 0 ,l
               (let ,var (ref ,x ,u) ;(= ,var (ref ,x ,u))
                 ,@body)))
        (w/uniq v
          `(withs (,v  ,x
                   ,l  (len ,v)) ; ,var  nil
             (for ,u 0 ,l
               (let ,var (ref ,v ,u) ;(= ,var (ref ,v ,u))
                 ,@body)))))))


(= map-types (obj 'cons    "[object Array]"
                  'fn      "[object Function]"
                  'sym     "[object String]"
                  'string  "[object String]"
                  'table   "[object Object]"
                  ;'int     "[object Number]"
                  'num     "[object Number]"))

(compiler-mac type (x)
  `(Object!prototype!toString!call ,x))

(compiler-mac isa (x . args)
  `(in (type ,x) ,@(mapeach x args (map-types x x))))


(compiler-mac = args
  `(do ,@(mapeach (place val) (pair args)
           (if (isa place 'sym 'literal)
                 `(assign ,place ,val)
               (caris place 'ref)
                 `(sref ,(cadr place) ,val ,@(cddr place))
               `(sref ,(car place) ,val ,@(cdr place))))))

(compiler-mac def (name parms . body)
  `(= ,name (fn ,parms ,@body)))

(compiler-mac quasiquote args
  (let x (apply qq-expand args)
    ;(debug x)
    x))


(compiler-mac extend (name parms test . body)
  (w/uniq (args u b)
    `(withs (orig  ,name
             ,u    (fn ,parms ,test)
             it    nil
             ,b    (fn ,parms ,@body))
       (= ,name (fn ,args
                  (if (= it (apply ,u ,args))
                        (apply ,b ,args)
                      (apply orig ,args)))))))



(defoutput join args
  (let (x . rest)
    (afneach (x . rest) args
      (if (caris x 'join)
            (join (self:cdr x) (self rest))
          (and (caris x 'list)
               (all (^:or (no:cons? _)
                          (and (caris _ 'quote)
                               (no:cons?:cadr _)))
                    (cdr x)))
            (join (cdr x) (self rest))
          (caris x 'quote)
            (if (cons?:cadr x)
                  (catch
                    (if (cddr x)
                          x
                        (join (mapeach _ cadr.x
                                (if (cons? _)
                                      (throw:cons x (self rest))
                                    ['quote _]))
                              (self rest))))
                (no:cadr x)
                  (self rest)
                (err "cannot join" x))
          ;; TODO: is this needed...?
          (do (zap compile x)
              (if (is rep.x undefined)
                    (self rest)
                  (cons x (self rest))))))

    (debug "join" x (type x))

    (if (no x)
          (= x '(list))
        (no:isa x 'literal)
        ;(no:sym? x)
          (= x `(list ,x)))

    (compile `((ref ,x 'concat) ,@rest))))


(defoutput list* args
  (let (args last) (splitlast args)
    (compile `(join (list ,@args) ,@last))))


(defoutput apply (x y . args)
  (when args
    (= y `(list* ,y ,@args)))
    ;(= y `(join ,y ,@args))
    ;(debug y)

  (fncall x ".apply" ['this y]))


(defoutput listtab (x)
  (debug "listtab" x)
  (let c cdr.x
    (lit "{"
         (when c linesep)
         (reindent/local
           ;(zap (^:map cdr c) x)
           (intersperse (str "," linesep)
             (mapeach (k v) (map cdr c)
               [indent compile.k ":" spaces compile.v])))
         (when c linesep)
         (when (and c local) indent)
         "}")))


(defoutput ref (x y (o z))
  (let r (js-attr x y)
    (if z (compile `(or ,r ,z)) r)))

(defoutput sref (x v k)
  (when (cons? x)
    (= x `(ref ,@x)))

  (lit (js-attr x k) spaces "=" spaces (compile v)))


(defoutput list args
  (lit "[" (w/precedence 2 (w/global nil (addsep "," args))) "]"))


(defoutput quote (x)
  ;(debug x (type x))
  (if (cons? x)
        (compile `(list ,@(mapeach x x ['quote x])))
      (no x)
        (lit undefined)
      ;(isa x 'int 'num)
      (js-literal? x)
        x
      ;(lit:compile-expr:str x)
      (compile:str x)))


(mac make-block (name parms body)
  ;(debug parms)
  (w/uniq u
    `(let ,u ,body
       (list (list ,name spaces ,@parms
                   ,(when parms spaces) "{"
                   (when ,u linesep))

             (reindent/local
               ;(debug "make-block" ',body)
               ;; should this be a macro...?
               (let x (map compile-expr ,u)
                 (when islast
                   (setlast x (str "return " it)))

                 (cons (when x indent)
                       (intersperse (str ";" linesep indent) x))))

             (list (when ,u
                     (str linesep (when local indent)))
                   "}")))))

#|(mac block-statement args
  `(arc (lit (w/islast nil (make-block ,@args)))))|#
#|
(mac block-statement args
  ;`(arc (lit (w/islast nil (make-block ,@args))))
  `(lit (w/islast nil (make-block ,@args))))


(= for-token (uniq))

(compiler-mac for (var init max . body)
  )

(= (output-rules* for-token)
   (fn (var test incr . body)
     ))|#

(mac compiler-mac-output (name parm1 body1 parm2 body2)
  `(let ,name (uniq)
     (compiler-mac ,name ,parm1 ,body1)
     (= (output-rules* ,name) (fn ,parm2 ,body2))))

(mac block-statement (name parm1 body1 parm2 cond body2)
  `(compiler-mac-output ,name
                        ,parm1 ,body1
                        ,parm2 (lit:w/islast nil
                                 (make-block ,str.name
                                             ,cond
                                             ,body2))))


#|(block-statement for
  (var init max . body)
    (do (zap expand body)
        `(let ,var ,init
           (,for ,var (< ,var ,max) (++ ,var)
             ,@(mappend optimizedo body))
           nil))
  (var test incr . body)
    ("("
      (compile var) ";" spaces
      (compile test) ";" spaces
      (compile incr)
     ")")
    (do (debug "for" body) body))|#

#|(defoutput while (test . body)
  ;(debug "while" test body)
  (lit "while" spaces "(" (compile test) ")" spaces "{" linesep
       (reindent
         (intersperse (str ";" linesep indent)
                      (map compile body)))
       linesep indent
       "}")
  )|#

(block-statement while
  (test . body)
    `(do (,while ,test ,@body) nil)
  (test . body)
    ("(" (compile test) ")")
    body)

(compiler-mac for (var init max . body)
  `(let ,var ,init
     (while (< ,var ,max)
       ,@body
       (++ ,var))))


(compiler-mac on-err (x y)
  ;(debug "on-err" x y)
  (let c `(arc (lit (with (x (make-block "try" nil ',cddr.y)
                           y (make-block "catch"
                                         ("(" (addsep "," ',cadr.x) ")")
                                         ',cddr.x))
                      [x spaces y])))
    (debug "on-err" c global local)
    (if local  `(do ,c nil)
        global c
               `((fn () ,c nil)))))


#|(block-statement for (var init max . body)
  )|#


#|(defoutput for (var init max . body)
  #|(debug:expand `(block-statement "for" ("("
                                         ',var ";" ,spaces
                                         '(< ,var ,max) ";" ,spaces
                                         '(++ ,var)
                                         ")")
                                  ,body))|#
  (compile-expr `(let ,var ,init
                   (block-statement "for" ("("
                                           (compile ',var) ";" spaces
                                           (compile '(< ,var ,max)) ";" spaces
                                           (compile '(++ ,var))
                                          ")")
                                    ,body)
                   nil)))|#


(defoutput fn (x)
  ;(debug "fn" x!global x!inner)
  (w/uniq-counter 1 ;uniq-counter
  (w/global-vars (mapeach x (join x!global x!inner)
                   (uniq-table x x))
  (w/local-vars (join (map touniq x!parms) local-vars)
  ;(debug "fn" dedup.global-vars dedup.local-vars)
  (w/global nil
  ;(debug global-vars)
  (w/precedence 0
    (let body x!body
      (lit "function" spaces "(" (addsep "," x!parms) ")" spaces "{"
           (reindent/local
             (when optimize?
               #|(= body (afneach (x . rest) body
                         (w/varlist nil
                           ;(debug "fn" x)
                           (zap optimizefn x)
                           (debug "fn" x)
                           ;(zap2 mappend optimizefn x)
                           (zap2 join (map car varlist) local-vars)
                           (let vars (if varlist varblock.varlist)
                             #|(when (and cons?.x cdr.x)
                               (zap self cdr.x)
                               (= x car.x))|#
                             (when cons?.x
                               (= x car.x))
                             ;(debug x)
                             (if vars
                                   ;; TODO: use list*
                                   (cons vars (cons x self.rest))
                                 (cons x self.rest))))))|#

              (zap2 mappend (afn (x)
                              (w/varlist nil
                                ;(debug "fn" x)
                                (zap optimizefn x)
                                (zap2 join (map touniq:car varlist) local-vars)
                                ;(debug "fn" local-vars)
                                (let vars (if varlist varblock.varlist)
                                  (when cdr.x
                                    ;(= x (cons car.x (self cdr.x)))
                                    ;(zap (^:mappend self _) cdr.x)
                                    (zap2 mappend self cdr.x)
                                    ;(= x (cons car.x (map self cdr.x)))
                                    )
                                  ;(debug "fn" x)
                                  (if vars
                                        (cons vars x)
                                      x))))
                            body)
              #|(= body (mappend x body
                        ))|#
               #|(zap2 mappend (^:w/varlist nil
                               (debug _)
                               (zap optimizefn _)
                               (debug _)

                               (if varlist
                                     (cons varblock.varlist _)
                                   _)) body)|#
                                   )

             ;(w/global-vars (join (map touniq x!global) (map touniq x!inner))
             ;; TODO: clunky
             ;(w/global-vars (map touniq global-vars)
             ;(w/global-vars global-vars
               ;(w/local-vars local-vars ;(mapeach x local-vars
                                                     ;  (uniq-table x x))
             ;(debug "fn" local-vars global-vars)
             (zap2 map compile body)

             (unless (is (rep:last body) undefined)
               (setlast body (str "return " it)))

             (zap2 rem (^:is (rep _) undefined) body)

             (when body
               [linesep indent
                (intersperse (str ";" linesep indent) body)
                linesep]))
           (and local body indent) "}"))))))))


#|(compiler-mac-output var
  args
    `(do (,var ,@args) nil)
  args
    (varblock pair.args))|#

(defoutput var args
  (varblock pair.args))

(def tojs (x)
  (w/global-assign nil
    (zap compile-expr x)

    ;(debug "tojs" x)
    ;(debug "tojs" x (rep x) (len:rep x) (is (rep x) '||) (empty:rep x))

    (when (empty:rep x)
      (= x nil))

    (str (when global-assign
           ["var " (addsep "," (rev:dedup global-assign))
            terminator
            (when x
              [linesep linesep])])
         x (when x
             terminator))))


(def dispfile (x f)
  (w/outfile o f (disp x o)))

(def compile-file (arc js)
  (w/infile f arc
    (w/uniq eof
      (let x (drain (read f eof) eof)
        (if strict?
          (push "use strict" x))

        ;(if wrapper? (= x `(((fn () ,@x)))))

        ;(if wrapper? (= x `(((fn () ,@x)))))
        ;(prn x)

        (debug x)

        (= x (str:intersperse (str linesep linesep)
                              (rem empty (map tojs x))))

        (if wrapper?
          (= x (str "(function" spaces "()" spaces "{" linesep
                    indent (subst "\n"
                             (str indent "\n")
                             (subst (str "\n" indent)
                               "\n"
                               x))
                    linesep
                    "})()" terminator)))

        (dispfile x js))))
  t)
