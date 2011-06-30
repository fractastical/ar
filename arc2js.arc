(use arc strings)

(implicit linesep        "\n")
(implicit indent         "    ")
(implicit spaces         " ")
(implicit terminator     ";")
(implicit gensym-name    "__g")
(implicit uniq-counter   1)
(implicit undefined      "void 0")

(implicit global         t)
(implicit local          nil)
(implicit nested         nil)
(implicit precedence     0)
(implicit local-vars     nil)
(implicit global-assign  nil)

(implicit optimize?      t)
(implicit readable?      t)
(implicit strict?        t)
(implicit wrapper?       t)

(dynamic  replace        nil)


;; should be in arc.arc
(mac mapeach (var expr . body)
  `(map (fn (,var) ,@body) ,expr))

;; should be in arc.arc
(extend * (x . args) (isa x 'string)
  (apply string (n-of (apply + args) x)))

;; should be in arc.arc
(mac and= args
  `(do ,@(mapeach (x y) (pair args)
           `(and ,x (= ,x ,y)))))


(mac w/whitespace (x . body)
  (case x
    minify `(w/linesep   ""
            (w/indent    ""
            (w/spaces    ""
            ;(w/readable? nil
            (w/optimize? t
              ,@body))))
            ;)
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

(mac w/replace (x . body)
  `(dlet replace (if replace (join ,x replace) ,x) ,@body))

#|(mac w/replace (from to . body)
  `(replaceall ((,from ,to)) ,@body))|#


(def addsep (x args)
  (intersperse (string x spaces) (map tojs args)))


(def jslit args
  (annotate 'js-literal (apply string args)))


;; should be in arc.arc
(mac zap2 (x y var . args)
  `(zap [apply ,x ,y _ ,args] ,var))


#|(def pushfront (x . rest)
  (when x
    (= (cdr x) (cons (car x) (cdr x)))
    (= (car x) (apply jslit rest))))|#

(def replace-list (x y)
  (when x
    (= (car x) (car y)
       (cdr x) (cdr y))))

(def tocomplexfn (body x)
  ;(debug body)
  ((afn (x i)
     (let c (car x)
       (if (no x) nil
           (caris c 'o)
             (let n (cadr c)
               (iflet v (caddr c)
                 (replace-list body `((or= ,n ,v) ,@body))
                 #|(pushfront body n spaces
                            "=" spaces n spaces
                            "||" spaces (tojs v))|#
                  )
               (cons n (self (cdr x) (+ i 1))))
           (aand (cdr x) (isa it 'sym))
             (do (replace-list body
                   `((let ,(cdr x)
                          ,(jslit "Array.prototype.slice.call(arguments,"
                                  spaces i
                                  ")")
                          ,@body)))
                 #|(pushfront body "var " (tojs:cdr x) spaces "=" spaces
                            "Array.prototype.slice.call(arguments," spaces i ")")|#
                 (cons c nil))
           (cons c (self (cdr x) (+ i 1))))))
   x 1))

   #|`(let ,(cdr x) (jslit "Array.prototype.slice.call(arguments," spaces i ")")
      ,@body)|#

(def tojsparms (x body)
  (if (no x)
        ;(string "()")
        nil
      (isa x 'cons)
        (tocomplexfn body x)
        ;(string "(" (addsep "," (tocomplexfn body x)) ")")
      (isa x 'sym)
        (do (replace-list body
              `((let ,x ,(jslit "Array.prototype.slice.call(arguments)")
                  ,@body)))
            #|(pushfront body "var " (tojs x) spaces "=" spaces
                       "Array.prototype.slice.call(arguments)")|#
            ;(string "()")
            nil)
      (err "invalid argument list" x)))

(def line args
  (string indent args terminator linesep))

;; should be in arc.arc
(def lastcdr (x)
  (nthcdr (- (len x) 1) x))

;; should use (defset last ...)
(mac setlast (x y)
  `(whenlet it (lastcdr ,x)
     (= (car it) ,y)))


(= origindent indent)

(mac reindent/local body
  `(w/indent (if local (string indent origindent)
                       (= origindent indent))
     (w/local t
       ,@body)))

(def lines (x)
  (jslit:intersperse (string terminator linesep linesep) x))


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
(def macex1 (x)
  (macex x t))


(def optimize-helper (x) (macex1 x))

(defrule optimize-helper ;(and local ;; remove this
                         (errsafe:js-rules*:car x)
                          ;)
  ;(debug x)
  (jslit:tojs x)
  ;(jslit:apply it (cdr x))
  )

(defrule optimize-helper (errsafe:js-mac-rules*:car x)
  ;(debug x)
  (iflet x (apply it (cdr x))
    x
    (orig x)))

(mac letis (var test then else)
  (w/uniq u
    `(let ,u ,test
       (if (is ,u ,var)
             (let ,var ,u ,then)
           (let ,var ,u ,else)))))

(def optimize (expr (o f optfn))
  ;(debug f)
  ;(if (js-opt-global-rules*:car expr) (prn expr))
  ;(prn " " f " " expr)

  ;(prn " " expr)

  ;(debug expr " " (errsafe:js-rules*:car expr))
  ;(debug "---------------")
  ;(debug expr)

  (letis expr (if optimize? (f expr) expr)
    ;(do (debug expr)
    (letis expr (optimize-helper expr)
      (list expr)
      (optimize expr f))
    expr))

(def isoptfn (x y)
  (if (caris x y) x
      (letis x (optimize-helper x)
        nil
        (isoptfn x y))))
#|      (letis x (macex1 x)
        nil
        (isoptfn x y))))|#


#|
                      (if iswith
                            (list ","
                              linesep (* spaces 4)
                              (w/nested t
                                (optimize optfn (car body))))
                          nested
                            (list ";" linesep)
                              ;(if iswith list:sym:string cons)

                          ))))))|#

#|(def make= (x)
  (let (n v) x
    (w/uniq u
      (push (list n u) acc)
      (sym:string (tojs u)
                  (when v
                    (list spaces "=" spaces (tojs v)))))))

(def =rec (parms body)
  (when (isoptfn (car body) 'with)
    (let x (pair parms)
      (cons (sym:string (if nested (* spaces 4) "var ") (make=:car x))
            (map make= (cdr x))))
    (cons
      (prn " " (sym:string:intersperse (string "," linesep (* spaces 4)) (cut (optimize optfn (car body)) 0 -1))))|#


(def make= (x)
  (w/indent (if (or nested (cddr x))
                  (string indent (* spaces 4))
                indent)
    (let acc nil
      (list (mapeach x (pair x)
              (let (n v) x
                (push n local-vars)
                (let u (if (auniq n) n (uniq))
                  ;(push u local-vars)
                  #|(debug local-vars)
                  (debug local-vars)
                  (debug "==========")|#
                  (push (list n u) acc)
                  (jslit (tojs u)
                         (when v
                           (list spaces "=" spaces (tojs v)))))))
            acc))))


(def nested-with (parms body)
  ;(debug "nest" body)
  (let iswith (cdr:isoptfn (car body) 'with)
    ;(debug iswith)
    (w/nested (if iswith t nested)
      (let (parms repl) (make= parms)
        (w/replace repl
          ;(prn " " replace " " body)
          ;(prn (cdr parms))
          (if iswith
                (let (x y) (nested-with (car iswith) (cdr iswith))
                  (list (join parms x)
                        (join y (map jslit:tojs (cdr body)))))
              (list parms
                    (or (map jslit:tojs (mappend optimize body))
                        (list nil)))))
#|              (= body
                 (map sym:tojs (cddr iswith))

                 )|#
        ;(list parms body)

;        (prn (mappend [optimize optfn _] (or (cddr iswith)
;                                             body)))

                                             ))))
#|    (prn " " parms " " (make= parms) " " iswith)))
    (let l (make= parms)
              (w/replace (cadr l)
                (= body (join
                  (let r (make=:cadr iswith)
                    ;(prn (cadr r))
                    ;(= parms (car l))
                    (= parms (join (car l) (car r)))
                    ;(prn parms)
                    ;(prn (cddr iswith))
                    (w/replace (cadr r)
                      (mappend [optimize optfn _] (cddr iswith)))
                    nil)
                  (mappend [optimize optfn _] body)))))|#
    #|(let x
    (if iswith  (w/replace
                (list parms body))))|#


(defoptfn with (parms . body)
  ;(prn " " (optimize optfn (car body)))
  ;(prn " " (car body) " " (isoptfn (car body) 'with))
    ;(prn " " (cdr parms))
              #|(if (cddr parms) ;(or iswith )
                    (string indent (* spaces 4))
                  indent)|#
  (w/local-vars local-vars
    (let (parms body) (nested-with parms body)
      ;(debug body)
      (cons (jslit "var "
                   (intersperse
                     (string "," linesep indent (* spaces 4))
                   parms))
            body))

      #|(prn:nested-with parms body)
      (if iswith
            (do ;(prn parms) (prn:cadr iswith)
            ;(= parms (join parms (cadr iswith)))
            ;(prn parms)
            (let l (make= parms)
              (w/replace (cadr l)
                (= body (join
                  (let r (make=:cadr iswith)
                    ;(prn (cadr r))
                    ;(= parms (car l))
                    (= parms (join (car l) (car r)))
                    ;(prn parms)
                    ;(prn (cddr iswith))
                    (w/replace (cadr r)
                      (mappend [optimize optfn _] (cddr iswith)))
                    nil)
                  (mappend [optimize optfn _] body))))))

          (let l (make= parms)
            (= parms (car l))
            (w/replace (cadr l)
              (= body (map tojs (mappend [optimize optfn _] body))))))|#

    ;(do (prn parms) (prn body) nil)

    ))
#|    (if (no nested) "var " )
    (if iswith (do )
               (do ))

    ;(=rec parms body)

    ;(if (isoptfn (car body) 'with)
    (join (let x (pair parms)
            #|(if (or nested iswith)
                (list:sym:string:intersperse (string "," linesep indent (* spaces 4))
                  (cons (sym:string (if nested (* spaces 4) "var ") (make=:car x))
                        (map make= (cdr x)))))|#
            (cons (jslit (unless nested "var ") (make=:car x))
                  (map make= (cdr x))))

          #|(cons (sym:string (if nested (* spaces 4) "var ")
            (w/indent (if (or iswith
                              nested
                              (cddr parms))
                            (+ indent (* spaces 4))
                          indent)
              (intersperse (string "," linesep indent)
                (mapeach (n v) (pair parms)
                  ;(prn " " (auniq n) " " n " " v " ")
                  ;(prn (auniq n))
                  (w/uniq u
                    #|(if (auniq n)
                          (list n spaces "=" spaces (tojs v))
                        (do))|#
                    (push (list n u) acc)
                    (cons (tojs u)
                          (if v (list spaces "=" spaces (tojs v)
                                      (if iswith
                                            (list ","
                                              linesep (* spaces 4)
                                              (w/nested t
                                                (optimize optfn (car body))))
                                          nested
                                            (list ";" linesep)
                                              ;(if iswith list:sym:string cons)

                                          )))))))))|#
          ;linesep
          (when iswith
            ;(prn " " (map make= (pair parms)))
            (w/replace acc
              (w/nested t
                ;(prn " " (optimize optfn (car body)))))
            ;(prn " " (optimize optfn (car body)))
            ;(prn " " (sym:string:intersperse (string "," linesep (* spaces 4)) (cut (optimize optfn (car body)) 0 -1)))
            nil)
          (or (w/replace acc (map jslit:tojs
                                  (join (w/nested t
                                          (optimize optfn (car body)))
                                        (mappend [optimize optfn _]
                                                 (cdr body)))))
                                  #|(cons (optimize optfn (car body))
                                        (mappend [optimize optfn _]
                                                 (cdr body)))))|#
              (list nil)))))
|#

#|(defoptfn with (parms . body)
  ;(prn " " (optimize optfn (car body)))
  ;(prn " " (car body) " " (isoptfn (car body) 'with))
  (withs (acc     nil
          iswith  (isoptfn (car body) 'with)
          make=   (fn (x)
                    (let (n v) x
                      (w/uniq u
                        (push (list n u) acc)
                        (sym:string (tojs u)
                                    (when v
                                      (list spaces "=" spaces (tojs v))))))))

    ;(=rec parms body)

    ;(if (isoptfn (car body) 'with)
    (join (let x (pair parms)
            #|(if (or nested iswith)
                (list:sym:string:intersperse (string "," linesep indent (* spaces 4))
                  (cons (sym:string (if nested (* spaces 4) "var ") (make=:car x))
                        (map make= (cdr x)))))|#
            (cons (jslit (unless nested "var ") (make=:car x))
                  (map make= (cdr x))))

          #|(cons (sym:string (if nested (* spaces 4) "var ")
            (w/indent (if (or iswith
                              nested
                              (cddr parms))
                            (+ indent (* spaces 4))
                          indent)
              (intersperse (string "," linesep indent)
                (mapeach (n v) (pair parms)
                  ;(prn " " (auniq n) " " n " " v " ")
                  ;(prn (auniq n))
                  (w/uniq u
                    #|(if (auniq n)
                          (list n spaces "=" spaces (tojs v))
                        (do))|#
                    (push (list n u) acc)
                    (cons (tojs u)
                          (if v (list spaces "=" spaces (tojs v)
                                      (if iswith
                                            (list ","
                                              linesep (* spaces 4)
                                              (w/nested t
                                                (optimize optfn (car body))))
                                          nested
                                            (list ";" linesep)
                                              ;(if iswith list:sym:string cons)

                                          )))))))))|#
          ;linesep
          (when iswith
            ;(prn " " (map make= (pair parms)))
            (w/replace acc
              (w/nested t
                ;(prn " " (optimize optfn (car body)))))
            ;(prn " " (optimize optfn (car body)))
            ;(prn " " (sym:string:intersperse (string "," linesep (* spaces 4)) (cut (optimize optfn (car body)) 0 -1)))
            nil)
          (or (w/replace acc (map jslit:tojs
                                  (join (w/nested t
                                          (optimize optfn (car body)))
                                        (mappend [optimize optfn _]
                                                 (cdr body)))))
                                  #|(cons (optimize optfn (car body))
                                        (mappend [optimize optfn _]
                                                 (cdr body)))))|#
              (list nil)))))|#

#|(defjsmac with (parms . body)
  (when nested
    `(do ,@(mapeach (x y) (pair parms)
             `(assign ,x ,y))
         ,@body)))|#
#|
  (w/uniq u
    (cons (sym:string "var " u spaces "=" spaces (tojs v))
          (or (w/replace n u (map sym:tojs body))
              (list nil)))))|#


(defoptglobal do args
  (list:lines:map tojs args))

#|(defoptglobal assign args
  (prn args)
  (let x (bin "=" args)
    (list:if nested (sym x)
                    (sym:string "var " x))))|#
#|    (mapeach x args
      (prn x)
      (string "var " x))))|#
;  (list:lines:map [do (prn _) (string "var " (tojs _))] args))
;  (string "var " (orig x))))


#|(extend optglobal (x) (caris x 'do)
  (list:lines:map tojs (cdr x)))|#

#|(if local (optfn expr)
          (optglobal expr))|#
        #|(do (if local (zap optfn expr)
                      (zap optglobal expr))
            (let x (macex expr t)
              ;(prn " " x)
              ;(prn " " expr)
              (if (is x expr)
                    x
                  (optimize x))))|#
;      (list expr)))

;(def optglobal (x) x)
  ;(prn x)
;  (optimize idfn x))

;(def optfn (x) x)
  ;(prn x)
;  (optimize idfn x))

#|(extend optglobal (x) ;(and (no nested)
                      (errsafe:js-opt-global-rules*:car x)
  (apply it (cdr x)))|#
;  (optimize [apply it (cdr _)] x))
#|
(extend optfn (x) (errsafe:js-opt-fn-rules*:car x)
  (apply it (cdr x)))|#

(def optfn (x)
  (aif (errsafe:js-opt-fn-rules*:car x)
         (apply it (cdr x))
       x))

(def optglobal (x)
  (aif (errsafe:js-opt-global-rules*:car x)
         (apply it (cdr x))
       x))
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


(defjsmac fn (parms . body)
  (w/local-vars local-vars
    (jslit "function" spaces

           (let x (tojsparms parms body)
             ;(debug x)
             (zap join local-vars x)
             ;(debug local-vars)
             (list "(" (addsep "," x) ")"))

           spaces "{"

           ;(zap optimize body)
           ;(pr " ")
           ;(map prn body)
           ;(do (prn " " local) nil)
           (reindent/local
             (w/uniq-counter (if (or local (isnt uniq-counter 1))
                                   uniq-counter
                                 1)
             #|(fn (x)
                                  (zap optimize x)
                                  (prn x)
                                  (if (acons:car x)
                                        x
                                      (list x)))|#
             ;(prn " " (len indent) " " (len origindent))
             ;(debug body)
               (zap [map tojs (mappend optimize _)] body))
             ;(debug (last body))

             ;(prn (intersperse "\n" body))

             ;(zap remlast body "void 0")
             ;(prn (last body) " " (type (last body)))

             (unless (is (rep:last body) undefined)
               ;(= body (rem "void 0" body))
               ;(zap remlast body "void 0")
               ;(zap cut body 0 -1)
               (setlast body (string "return " it)))

             (= body (rem [is (rep _) undefined] body))

             (when body
               `(,linesep ,indent
                 ,@(intersperse (string terminator linesep indent) body)
                 ,linesep)))
                               ;(map line body)

           (and local body indent)
           "}")))


#|(defjs uniq ((o x))
  (let n gensym-name
    (when x
      (= n (if (caris x 'quote)
                 (cadr x)
               x)))
               #|(isa x 'string)
                 x
               (tojs x))))|#
    (w/gensym-name n
      (tojs:uniq))))|#

(defjsmac arc (x)
  (jslit:tojs:eval x))

#|(defjsmac compose (x y)
  (w/uniq args
    `(fn ,args
       (,x (apply ,y ,args)))))|#

#|(defjsmac compose (x y)
  (w/uniq args
    `(fn ,args
       (,x (apply ,y ,args)))))|#

(defjsmac ref (x y (o z))
  ;(debug:js-identifier? y)

  ;(debug x (type x))
  ;(debug y (type y))
    ;(zap [jslit "(" _ ".)"] x))

  (when (isa x 'int)
    (zap jslit x "."))

  #|(if (isa x 'string)
        (if (re-match "^\\d+$" x)
              (++ x "."))
        )|#

  (zap tojs x)

  ;(debug (js-identifier? y))

  (let r (if (js-identifier? y)
               (js-dot x y)
             (js-brackets x y))
    (if z `(or ,r ,z) r)))

(defjsmac list args
  (jslit "[" (addsep "," args) "]"))

(defjsmac listtab (x)
  (jslit "{"
         (when (cdr x) linesep)
         (reindent/local
           ;(zap [map cdr (cdr x)] x)
           (intersperse (string "," linesep)
             (mapeach (k v) (map cdr (cdr x))
               (list indent (tojs k) ":" spaces (tojs v)))))
         (when (cdr x) linesep)
         (when local indent)
         "}"))

#|(defjs assign (x y)
  (unless (or (ac-ssyntax x) local nested) "var ")
  (tojs x) spaces "=" spaces (tojs y))|#

(def js-dot (x y)
  (jslit x "." y))

(def js-brackets (x y)
  (jslit x "[" (tojs y) "]"))

(defjsmac sref (x v k)
  (jslit (js-brackets x k) spaces "=" spaces (tojs v)))

;; should be in arc.arc
(mac afneach (parms x . body)
  (w/uniq u
    `((afn (,u)
        (when ,u
          (let ,parms ,u
            ,@body)))
      ,x)))

(defjsmac join args
  ;(debug (car rest))
  (let rest (afneach (x . rest) args
              ;(debug x rest)
              (if (caris x 'join)
                    (join (self:cdr x) (self rest))
                  (and (caris x 'list)
                       (no:some acons (cdr x)))
                    (join (cdr x) (self rest))
                  (caris x 'quote)
                    (if (acons:cadr x)
                          (catch
                            (if (cddr x)
                                  x
                                (join (map [if (acons _)
                                                 (throw:cons x (self rest))
                                               (list 'quote _)]
                                           (cadr x))
                                      (self rest))))
                        (no:cadr x)
                          (self rest)
                        (err "cannot join" x))
                  (do (zap tojs x)
                      (if (is (rep x) undefined)
                            (self rest)
                          (cons x (self rest))))))
    `((list)!concat ,@rest)))
         #|((afn (x)
            (when x
              (let (x . rest) x
                `(,x !concat ,@(self rest)))))
          args)|#
;  (jslit "foobar"))

(defjsmac quote (x)
  ;(debug x (type x))
  (if (acons x)
        `(list ,@(map [list 'quote _] x))
      (no x)
        (jslit undefined)
      (in (type x) 'int 'num)
        x
      (jslit:tojs:coerce x 'string)))

(defjsmac quasiquote args
  (let x (apply qq-expand args)
    ;(debug x)
    x))
  ;(debug (tojs `(arc:quasiquote ,@args)))
  ;(jslit "foobar"))
  ;`(list ,@(tojs `(arc:quasiquote ,x))))
#|
  (if (acons x)
        `(list ,@(mappend (fn (x)
                            (debug x)
                            (if (caris x 'unquote)
                                  (list:cadr x)
                                (caris x 'unquote-splicing)
                                  (do (debug (tojs `(arc:quasiquote ,x)))
                                    `((!concat ,(cadr x))))
                                `((quote ,x))))
                          x))
        #|`(list ,@((afn (x)
                    (if (no x) nil
                        (let (x . rest) x
                          ;(debug x rest)
                          (cons (if (caris x 'unquote)
                                      (cadr x)
                                    (caris x 'unquote-splicing)
                                      `(!concat ,(cadr x) ,@(self rest))
                                    (list 'quote x))
                                (self rest)))))
                  x))|#
        #|`(list ,@(mapeach x x
                   (debug x)
                   (if (caris x 'unquote)
                         (cadr x)
                       (caris x 'unquote-splicing)
                         (do (debug x) nil)
                       (list 'quote x))))|#
      `(quote ,x)))
|#


#|(defjs prn args
  "console.log(" (addsep "," args) ")")

(defjs pr args
  "console.dir(" (addsep "," args) ")")

(defjs err args
  "console.error(" (addsep "," args) ")")

(defjs warn args
  "console.warn(" (addsep "," args) ")")|#

(extend coerce (x type . r) (isa x 'js-literal)
  ;(prn " " x " " type " " r " ")
  (case type
    string (string:rep x)
    sym    (sym:rep x)
           (apply orig x type r)))

(mac defjscall (x y)
  `(defjsmac ,x args
     (jslit ',y "(" (addsep "," args) ")")))

(defjscall prn   console.log)
(defjscall pr    console.dir)
(defjscall err   console.error)
(defjscall warn  console.warn)


#|(defjs atomic-invoke (x)
  (do (debug
      (tojs x)))|#

#|(defjsmac atomic-invoke (x)
  `(do ,@(cddr x)))|#


(defjsmac or= args
  `(do ,@(mapeach (x y) (pair args)
           (if (acons x)
             (zap [apply js-brackets _] x))

           `(or ,x (= ,x ,y)))))


(defjsmac zap (x y . args)
  `(= ,y (,x ,y ,@args)))
#|
  `(if (is ,x nil)
         (= ,x ,y)
       ,x))|#

(defjsmac string args
  `(+ "" ,@args))

(defjsmac for (var init max . body)
  `(let ,var ,init
     ;(block "for" )
     (arc:jslit "for" spaces "("
                (tojs ',var) ";" spaces
                (tojs '(< ,var ,max)) ";" spaces
                (tojs '(++ ,var))
                ")" spaces "{"
                ,(when body linesep)

                (w/local t
                  (reindent/local
                    ;(tojs `(do ,@body))
                    (intersperse (string ";" linesep)
                                 (map [jslit indent (tojs _)] ',body))
                                 ;(map [jslit indent (tojs _)]
                                      ;body))
                    ;(intersperse ";" (map [jslit indent (tojs _)] body))
                    ;(map [jslit indent (tojs _) ";" linesep] body)
                    ))

                ,(when body
                   (string linesep indent))
                "}")
     nil))

#|(defjs for (var init max . body)
  "for" spaces "("
  (tojs var) ";" spaces
  (tojs `(< ,var ,max)) ";" spaces
  (tojs `(++ ,var))
  ;,@(reindent/local:map tojs body)
  ")" spaces "{" linesep indent
  "}")|#

(defjsmac len (x)
  (sym:string (if (acons x) "(")
              (tojs x)
              (if (acons x) ")")
              "!length"))

(defjsmac each (var x . body)
  (w/uniq u
    (if (isa x 'sym)
          `(let ,var nil
             (for ,u 0 (len ,x)
               (= ,var (ref ,x ,u))
               ,@body))
        (w/uniq v
          `(with (,v    ,x
                  ,var  nil)
             (for ,u 0 (len ,v)
               (= ,var (ref ,v ,u))
               ,@body))))))

#|(redef setforms (expr)
  (prn expr)
  (if (isa expr 'sym)
       (w/uniq (g h)
         (list (list g expr)
               g
               `(fn (,h) (assign ,expr ,h))))
       (w/uniq (g h)
         (let argsyms (map [uniq] (cdr expr))
            (list (+ (list g (car expr))
                     (mappend list argsyms (cdr expr)))
                  `(,g ,@argsyms)
                  `(fn (,h) (sref ,g ,h ,(car argsyms))))))))|#


(redef expand= (place (o val))
  (if (in (type place) 'sym 'js-literal)
        `(assign ,place ,val)
      `(sref ,(car place) ,val ,@(cdr place))))

;; should be in arc.arc
(redef expand=list (terms)
  (if (cddr terms)
        `(do ,@(map (fn ((p v)) (expand= p v))
                    (pair terms)))
      (apply expand= terms)))

;; ew
#|(redef uniq ()
  (do1 (sym:string gensym-name uniq-counter)
       (++ uniq-counter)))|#

#|(= uniq-has-seen (table))

(extend print (primitive x port) (auniq x)
  (unless (uniq-has-seen x)
    (= (uniq-has-seen x) t)
    (++ uniq-counter))
  (disp (string gensym-name uniq-counter) port))|#


#|(defjsmac let (n v . body)
  (if readable?
    `(do (let ,n ,v ,@body))))|#

(defjsmac def (name parms . body)
  `(= ,name (fn ,parms ,@body)))

(defjsmac extend (name parms test . body)
  ;(let args (sym:tojs:uniq)
  (w/uniq (args u b)
    `(withs (orig  ,name
             ,u    (fn ,parms ,test)
             it    nil
             ,b    (fn ,parms ,@body))
       (= ,name (fn ,args
                  (if (= it (apply ,u ,args))
                        (apply ,b ,args)
                      (apply orig ,args)))))))


(def splitlast (x)
  (split x (- (len x) 1)))

(defjsmac apply (x y . args)
  (when args
    (let (args last) (splitlast args)
      (= y (jslit "["
                  (addsep "," (cons y args))
                  "].concat("
                  (tojs:car last)
                  ")")))
    nil)

  (fncall x ".apply" (list 'this y)))
#|  (tojs x)
  ".apply(this," spaces
  args
  ")")|#


(def math-wrap (name x)
  (if (acons x)
        (withs (c     (car x)
                wrap  (if (cddr x)
                            (is (op-precedence c)
                                 precedence)
                          (unary-ops c)
                            (is c name)))
          (string (if wrap "(")
                  (tojs x)
                  (if wrap ")")))
      (tojs x)))

(def math (name args)
  ;13
  ;(prn " " name " " args " " precedence)
  ;(prn " ")
  (if (cdr args)
        (binwrap name (map [math-wrap name _] args))
  #|(iflet pri (unary-ops:car x)
                                 (do
                                   (debug precedence)
                                   (debug pri)
                                   (debug x)
                                   (list:tojs x)))|#
      (unary-ops name)
        ;(w/precedence it ;(op-precedence name)
        (string name (math-wrap name (car args)))
      (car args)))


#|(def math (name args)
  ;13
  ;(prn " " name " " args " " precedence)
  ;(prn " ")
  (string (unless (cdr args) name)
          (binwrap name (mapeach x args
                          (if (acons x)
                                (let c (car x)
                                  (if (is c name)
                                        (w/precedence
                                          (if (cddr x)
                                                precedence
                                              13)
                                          (tojs x))
                                      (w/precedence (+ precedence 1)
                                        (w/precedence
                                          (if (cddr x)
                                                precedence
                                              0)
                                          (tojs x)))))
                              (tojs x))))))|#

(def binwrap (name args (o pre spaces))
  ;(let op (op-precedence name)
    ;(prn " " name " " op " " precedence)
  (string:intersperse (string pre name spaces) args
              ;(when (< op precedence) "(")
              ;(when (< op precedence) ")")
              ))
                ;)

(def bin (name args)
  (binwrap name (map tojs args)))

(def binsep (name args)
  (string:intersperse (string name spaces) (map tojs args)))
;  (binwrap name (map tojs args) nil))
  ;(binwrap name (map [math-wrap name _] args) nil))
#|                                      (mapeach x args
                                        ;(prn x " " (errsafe:op-precedence:car x) " " precedence)
                                        x)))))|#

(def binand (name args)
  (if (cdr args)
        (let last (car args)
          (binwrap "&&" (mapeach x (cdr args)
                          (do1 (bin name (list last x))
                               (= last x)))))
      (bin name args)))

(def prefix (name (x))
  (string name (tojs x)))

(def suffix (name (x))
  (string (tojs x) name))

(def trinary ((l r) args)
  ((afn (x)
     (if (no:cdr x)
           (tojs:car x)
         (no:cddr x)
           (tojs `(and ,@x))
           #|(w/precedence 5
             (string (tojs:car x)
                     spaces "&&" spaces
                     (tojs:cadr x)))|#
         (string (tojs:car x)
                 spaces l spaces
                 (tojs:cadr x)
                 spaces r spaces
                 (self (cddr x)))))
   args))

;; for assign
(def bin= (name args)
  ;(debug local-vars)
  ;(debug global-assign)
  ;(debug args)
  ;(debug (remlast args nil))
  ;(zap remlast args nil)

  ;(debug args)

  (let args (if (cdr args)
                  (cut args 0 -1)
                args)

    (each x args
      ;(debug x local-vars)
      (unless (or (some x local-vars)
                  (ac-ssyntax x)
                  (no:isa x 'sym))
        (push x global-assign)))

;    (debug "-----")
    )

  (if (and (no local)
           (no nested)
           (or (single args)
               (and (no:cddr args)
                    (caris (cdr args) nil))))
        nil
      (binwrap name (map tojs args)))

  #|(if (and (no:cddr args)
           (caris (cdr args) nil))
        (zap cut args 0 -1))|#

  ;(debug global-assign)
  )

#|(def incr ((l r) (x y))
  (if (is y 1)
        (w/precedence 15
          (list l (tojs x)))
      (w/precedence 1
        (list (tojs x) spaces ',y spaces (tojs y)))))|#

#|(mac binaries args
  `(do ,@(map (fn (x)
                `(defjs ,x args (bin ',x args)))
              args)))|#


(= op-precedence (table)
   unary-ops     (table))
                 #|(obj ;mod     12
                      ;if      2
                      assign  1))|#

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
  `(do (= (op-precedence ',from) ,precedence)
;          (op-precedence ',to)   ,precedence
       (defjs ,from args
         (w/nested t
           (,type ',to args)))))
           ;(mappend [optimize optglobal _] args))))))


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

#|(mac unary args
  `(do ,@(mapeach (from to pri) (tuples args 3)
           (let orig (js-rules* from)
             `(defjs ,from (x . args)
                (if args (apply ,orig x args)
                         (do (debug precedence " " ,pri)
                           (w/precedence ,pri
                             (jslit ',to (tojs x))))))
            ))))|#

(mac unary args
  (each (from to pri) (tuples args 3)
    (= (unary-ops from) pri)))

(mac makeincr (from to l r)
  `(defjsmac ,from (x (o y 1))
     (w/nested t
       (if (is y 1)
             (w/precedence ,l
               (jslit ',from (tojs x)))
           (w/precedence ,r
             (jslit (tojs x) spaces ',to spaces (tojs y)))))))

#|  (suffix   ++      ++        15
            --      --        15)|#

#|  (incr     ++      (++  +=)  (15  1)
            --      (--  -=)  (15  1))|#
#|
  (unary    )

|#


#|(defjs -- (x (o y 1))
  (w/nested t
    (if (is y 1)
          (w/precedence 15
            (list (tojs x) "--"))
        (w/precedence 1
          (list (tojs x) spaces "-=" spaces (tojs y))))))|#

#|(defjs if args
  (w/nested t
    ((afn (x)
       (if (no:cdr x)
             (tojs:car x)
           (string (tojs:car x)
                   spaces "?" spaces
                   (tojs:cadr x)
                   spaces ":" spaces
                   (self (cddr x)))))
     args)))|#

#|(defop mod % 12 (fn (n (x y))
  (list (tojs x) spaces "%" spaces (tojs y))))|#

#|(defjs mod (x y)
  (w/nested t
    (list (tojs x) spaces "%" spaces (tojs y))))|#

#|(defjs or args
  (bin "||" args))

(defjs and args
  (bin "&&" args))

(defjs is args
  (binand "===" args))

(defjs isnt args
  (binand "!==" args))|#


;(= name-rules* '(("w/" "with_")))

(def remlast (x f)
  (with (last 0
         i    0)
    (each x x
      (++ i)
      (when (isnt f x)
        (= last i)))
    (cut x 0 last)))

(def fncall (f x args)
  (zap remlast args nil)

  (let wrap (caris f 'fn)

    ;(debug f args)

    #|(debug (ac-ssyntax f))

    (debug (errsafe:js-expand-ssyntax f))|#

    (when (ac-ssyntax f)
      (zap js-expand-ssyntax f))

    (iflet x (car args)
      (when (headmatch "!" (string x))
        (zap cdr args)
        ;(debug f)
        ;(debug (tojs f))
        (let (x nil y) (js-expand-ssyntax x)
          (= f (list x f y)))))

    ;(debug f args)

    ;(debug "----------")

    (if (caris f 'compose)
          ;(fncall (cadr f) x `((,(caddr f) ,@args)))
          (tojs `(,(cadr f) (,(caddr f) ,@args)))
        (caris f 'complement)
          (tojs `(no (,(cadr f) ,@args)))
        (do (unless wrap
              (zap tojs f))

            (jslit (if wrap "(")
                   (tojs f)
                   (if wrap ")")
                   (or #|(iflet x (car args)
                         (when (headmatch "!" (string x))
                           (zap cdr args)
                           (let x (js-expand-ssyntax x)
                             (debug x))))|#
                       (and local (no x) wrap
                         (zap2 cons 'this args)
                         ".call")
                       x)
                   "(" (w/precedence 2 (addsep "," args)) ")")))))

;(w/precedence 1 (tojs `(do ,@(map [jslit:math-wrap "," _] args))))


#|(defjs mac (name parms . body)
  (do (eval `(mac ,name ,parms ,@body))
      nil))|#

(defjsmac mac (name parms . body)
  `(arc:defjsmac ,name ,parms ,@body))

(defjsmac use args
  `(arc:use ,@args))

#|(defjs use args
  (do (eval `(use ,@args))
      nil))|#


(def tojs (x)
  (err "unknown expression" x " "))

(extend tojs (expr) (acons expr)
  ;(debug "tojs" expr)
  (let x (car:optimize expr optglobal)
    (if #|(js-literal? x)
          x|#
        (is x expr)
          (fncall (car x) nil (cdr x))
        (tojs x))))

(extend tojs (x) (isa x 'string)
  (string "\"" x "\""))

(extend tojs (x) (isa x 'char)
  (tostring (pr #\") (pr x) (pr #\")))


#|
;; should be in re.arc
(def re-replace (pat in to)
  (racket-regexp-replace pat in to))

;; should be in re.arc
(def re-replace* (pat in to)
  (racket-regexp-replace* pat in to))|#

;; should be somewhere else, like re.arc
(mac re-multi-replace (x . args)
  ((afn (((from to (o g)) . rest))
     (list (if g 'racket-regexp-replace*
                 'racket-regexp-replace)
           (string from)
           (if rest (self rest) x)
           (string to)))
   (rev args)))

#|(def js-expand-ssyntax (x)
  ;(debug x)
  (jslit:subst "." "!" (string x)))|#

#|(defrule ac-expand-ssyntax t
  (orig sym))|#

#|

foo!bar-qux!corge!nou%
foo["bar-qux"].corge["nou%"];

(foo bar-qux)

((foo bar-qux) corge)

(((foo bar-qux) corge) nou%)

(ref (ref (ref foo "bar-qux) "corge") "nou%")

|#

(def js-expand-ssyntax (x)
  (ac-expand-ssyntax x))

(def nested-pair (name x)
  ;(debug x)
  ((afn ((x . rest))
     ;(debug x rest)
     (if (cdr rest)
           `(,name ,(self rest) ,x)
         `(,name ,(read:string:car rest) ,x)))
   (rev x)))

(defrule js-expand-ssyntax (ac-insym? #\! x)
  (nested-pair 'ref (tokens (string x) #\!)))

#|(defrule js-expand-ssyntax (ac-insym? #\. x)
  (orig x))|#
#|  (tojs:car:rev (let x ((afn (x)
                ;(debug x)
               (if (cddr x)
                 `((,(car x) ,@(self:cdr x) ref))
                 (list x)))
             (tokens (string x) #\!))
            ;(debug:rev x)
            x
             )))|#
;  (tojs:orig sym))

(def js-identifier? (x)
  (when (isa x 'string)
    (re-match "^[$_a-zA-Z][$_a-zA-Z0-9]*$" x)))

(def mangle-name (x)
  ;(debug x)
  (re-multi-replace x ("^[^$_a-zA-Z]"    "_&"  )
                      ("^[^$_a-zA-Z]"    "_"   )
                      ("[^$_a-zA-Z0-9]"  "_"  g))

  #|(re-replace* "[^$_a-zA-Z0-9]+"
               (re-replace "^[^$_a-zA-Z]"
                           (re-replace "^[0-9]" x "_&")
                           "_")
               "_")|#
  #|(debug (re-replace "^[^$_a-zA-Z]|[^$_a-zA-Z0-9]+"
              x
              "_"))
  (re-replace "^[^$_a-zA-Z]|[^$_a-zA-Z0-9]+"
              (re-replace "^[0-9]" x "_&")
              "_")|#
  ;x
  #|(re-replace (re-replace x "^[0-9]" "_&")
              "^[^$_a-zA-Z]|[^$_a-zA-Z0-9]+"
              "_")|#
)
;  (sym:multisubst name-rules* x))

(extend tojs (x) (isa x 'sym)
  (if (ac-ssyntax x)
        (tojs:js-expand-ssyntax x)
      (jslit:mangle-name:string x)))


(= uniq-table (table))

(extend tojs (x) (auniq x)
  (iflet x (uniq-table x)
    (jslit gensym-name x)
    (do (= (uniq-table x) uniq-counter)
        (++ uniq-counter)
        (tojs x))))


(extend tojs (x) (errsafe:or (and (no nested)
                                  (no local)
                                  (js-opt-global-rules*:car x))
                                  ;(errsafe:js-mac-rules*:car x)
                             ;(errsafe:js-mac-rules*:car x)
                             (js-rules*:car x))
  (apply it (cdr x)))

#|(extend tojs (x)
  (apply it (cdr x)))|#

(extend tojs (x) (errsafe:or (and (no:cddr x)
                                  (unary-ops:car x))
                             (op-precedence:car x))
  ;(prn " " (car x) " " precedence " " it)
  (= x (w/precedence it (orig x)))
  ;(debug x " " precedence " " it " " (> precedence it))
  (if (> precedence it)
        (string "(" x ")")
      x))


(extend tojs (x) (no x) undefined)

(extend tojs (x) global
  ;(prn (caar:optimize optglobal x))
  (w/global nil
    (w/global-assign nil
      ;(= x (orig (optfn (list x))))
      (zap orig x)
      ;(prn x)

      (when (empty x)
        (= x nil))

      ;(debug x)

      ;(debug local-vars)
      ;(debug global-assign)
      (string (when global-assign
                (list "var " (addsep "," (rev:dedup global-assign))
                      terminator
                      (when x
                        (list linesep linesep))))
              x (when x
                  terminator)))))

;; seems kinda icky...
#|(extend tojs (x) (and (no nested)
                      (no local)
                      (caris x 'assign)
                      (no:ac-ssyntax:cadr x))
                      #|(let x (car:optimize optglobal x)
                        (when (and (caris x 'assign)
                                   (no:ac-ssyntax:cadr x))
                          x)))|#
                      ;(caris (car:optimize optglobal x) 'assign))

  (let (n v . rest) (cdr x)
    (unless rest
      (zap remlast x nil)))

  (string "var " (orig x)))
|#

(def js-literal? (x)
  ;(debug x (type x))
  (and x (in (type x) 'int 'num 'fn 'js-literal)))

(extend tojs (x) (js-literal? x) x)

;(extend tojs (x) (js-replace-rules* x) it)
(extend tojs (x) (assoc x replace) (orig:cadr it))


(def dispfile (x f)
  (w/outfile o f (disp x o)))

(def arc2js (arc js)
  (w/infile f arc
    (w/uniq eof
      (let x (drain (read f eof) eof)
        (if strict?
          (push "use strict" x))

        ;(if wrapper? (= x `(((fn () ,@x)))))
        ;(prn x)

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
                       "})()" terminator)))

        (dispfile x js))))
  t)


(defops
  (prefix   new     new\      17
            del     delete\   15
            no      !         14)

  (math     /       /         13
            *       *         13
            +       +         12
            -       -         12)

  (bin      mod     %         13
            and     &&        5
            or      \|\|      3)

  (binand   <       <         10
            <=      <=        10
            >       >         10
            >=      >=        10
            is      ===       9
            isnt    !==       9)

  (trinary  if      (? :)     3)

  (bin=     assign  =         2)

  (binsep   do      \,        1))

(unary      +       +         14
            -       -         14)

(makeincr   ++      +=        15  2)
(makeincr   --      -=        15  2)
