(use arc)

(def debug args (apply prn (intersperse " " args)))

(redef sym args
  (coerce (apply string args) 'sym))


;; TODO: should really be in ac.arc...
(redef ac-assign1 (a b1 env)
  (unless (ar-tnil racket-symbol?.a)
    (err "First arg to assign must be a symbol" a))
  (let result (ac b1 env)
    `(racket-begin ,(if (ac-lex? a env)
                          (list 'racket-set! a result)
                        (ac-global-assign a result))
                   ,result)))

;; TODO: should really be in ac.arc...
(redef ar-apply-non-fn (x args)
  ;(debug type.x args)
  (case type.x
    list   (racket-mlist-ref x car.args)
    string (racket-string-ref x car.args)
    table  (racket-hash-ref x car.args cadr.args)
           (err "Function call on inappropriate object" x args)))

;=============================================================================
;  Should be in strings.arc
;=============================================================================

(def splitmatch1 (pat seq)
  ;; TODO: clunky
  (let x string.seq
    (iflet pivot (posmatch pat x)
            ;; TODO: clunky
      (list (coerce (cut x 0 pivot)           type.seq)
            (coerce (cut x (+ len.pat pivot)) type.seq)))))

;; TODO: should be faster
;; TODO: should handle symbols as well as strings
;; TODO: clunky
(def splitmatch (pat seq)
  (let l len.pat
    ((afn (x)
       (let pivot (posmatch pat x)
         (cons (cut x 0 pivot)
               (when pivot
                 (self:cut x (+ l pivot))))))
     seq)))

#|(def splitmatch (pat seq)
  (let l len.pat
    ((afn (x)
       (let pivot (posmatch pat x)
         (prn x pivot)
         (= pivot (split x pivot))
         (zap self cadr.pivot)
         pivot))
     seq)))|#

;; TODO: tests
;;
;; > (splitmatch1 "foo" "barfoobarfoobar")
;; ("bar" "barfoobar")
;;
;; > (splitmatch1 "foo" "barquxcorge")
;; nil
;;
;; > (splitmatch "foo" "barfoobarfoobar")
;; ("bar" "bar" "bar")
;;
;; > (splitmatch "foo" "barufooquxufoocorgeu")
;; ("baru" "quxu" "corgeu")
;;
;; > (splitmatch "foo" "barufoofoofooquxufoocorgeu")
;; ("baru" "" "" "quxu" "corgeu")
;; ?????


;=============================================================================
;  Should be in alias.arc
;=============================================================================

(def defvar (n x y)
  ;; TODO: I don't like varset, because it uses eval
  (varset n x)
  (= (ac-var-assigner* n) y)
  (ac-zeroarg n))

(mac alias (x y)
  (w/uniq u
    `(defvar ',x
       (fn ()   ,y)
       (fn (,u) (= ,y ,u)))))

(alias cons? acons)
(alias list? alist)
(alias pow   expt)
(alias str   string) ;;
(alias str?  string?) ;;
(alias uniq? auniq)


;=============================================================================
;  Should be in predicate.arc
;=============================================================================

(mac make-predicate (x (o y x))
  `(def ,(sym x "?") (x)
     (isa x ',y)))

(make-predicate char) ;;
;(make-predicate cons)
(make-predicate fn)
(make-predicate int)
(make-predicate mac)
(make-predicate num)
(make-predicate string) ;;
(make-predicate sym)
(make-predicate table)


;=============================================================================
;  Should be in ar.arc
;=============================================================================

(= racket-#f (ail-code #f))


;=============================================================================
;  curly-bracket macro
;=============================================================================

(def ar-read-curly-brackets (ch port src line col pos)
  `(curly-bracket ,@(racket-read/recursive port #\{ racket-#f)))

(def ar-curly-bracket-readtable (x)
  (racket-make-readtable x #\{ 'terminating-macro ar-read-curly-brackets))

(zap ar-curly-bracket-readtable arc-readtable*)

(mac curly-bracket args
  `(obj ,@args))


;=============================================================================
;  Should be in arc.arc
;=============================================================================

;; TODO: funny, but somewhat inaccurate name
;;       donil sounds awkward
(mac dont body
  `(do ,@body nil))

(mac retif (name x)
  `(iflet ,name ,x
     ,name
     ,name))

#|
;; TODO: haha, not needed
(def nthcar (n xs)
  (car (nthcdr n xs)))|#

(mac letr (var expr . body)
  `(let ,var nil
     (= ,var ,expr)
     ,@body))

(mac buildeach (name f)
  (w/uniq args
    `(mac ,name ,args
       ;; TODO: clunky, shouldn't use car, cadr, and cddr
       `(,,f (fn (,(car ,args)) ,@(cddr ,args)) ,(cadr ,args)))))

(buildeach mapeach  map)
(buildeach mappeach mappend)
(buildeach someach  some)
(buildeach keepeach keep)

#|(mac mapeach (var expr . body)
  `(map (fn (,var) ,@body) ,expr))

(mac mappeach (var expr . body)
  `(mappend (fn (,var) ,@body) ,expr))

(mac someach (var expr . body)
  `(some (fn (,var) ,@body) ,expr))|#

;; TODO: def, mac, and extend should use = rather than assign
(mac remac (name parms . body)
  `(let orig (rep ,name)
     (= (sig ',name) ',parms)
     (= ,name (annotate 'mac (fn ,parms ,@body)))))

(remac redef (name args . body)
  `(let orig ,name
     (= (sig ',name) ',args)
     (= ,name (fn ,args ,@body))))


(def carref (x y)
  (and cons?.y (x car.y)))


(redef empty (seq)
  (or (no seq)
      (is seq '||)
      (is len.seq 0)))


(mac collect (x)
  `(accum yield ,x))


(redef dedup (xs (o f idfn))
  (collect:let h (obj)
    (each x xs
      (let k f.x
        (unless h.k
          (yield x)
          (= h.k t))))))


(mac ^ args
  `(fn ((o _)) ,@args))


;; TODO: this macro definition doesn't work
#|(mac isa (x . args)
  `(in (type ,x) ,@args))|#

(redef isa (x y . args)
  ;(assign x (type x))
  (zap type x)
  (or (is x y)
      ;; can't use `some`, because that causes an infinite loop
      (if args (reclist (^:is x car._) args))))

#|(def isa (x . args)
  (some x args))|#


(redef empty (x)
  (errsafe:is len.x 0))


;; TODO: rewrite code to use dopair
(mac dopair (var args . body)
  (w/uniq u
    `(let ,u (mapeach ,var (pair ,args) ,@body)
       (if (cdr ,u)
             `(do ,@,u)
           (car ,u)))))


#|
;; TODO: throws an error
(mac var args
  (dopair (x y) args
    (if (sym? x)
          `(ail-code (racket-define ,x ,y))
        (err "cannot create variable" x))))|#

(mac var args
  `(ail-code
     (racket-begin
       ,@(mapeach (x y) pair.args
           (if (sym? x)
                 `(racket-define ,x ,y)
               (err "cannot create variable" x))))))


;; TODO: should this use dopair...?
(remac in (x . choices)
  (if (cdr choices)
        (w/uniq g
          `(let ,g ,x
             (or ,@(map1 (fn (c) `(is ,g ,c)) choices))))
      `(is ,x ,(car choices))))


;; TODO: inefficient
(def zip args args
  ;; TODO: this causes it to stop when all the lists are empty.
  ;;       however, in Python, it stops when the first list is
  ;;       empty. I'm not sure which is the better semantic.
  ;;       to get the Python behavior, just change `all` to `some`
  (if (all no args)
        nil
      (cons (apply list (map car args))
            (apply zip  (map cdr args)))))


(def flat1 args
  (apply + nil args))

(def flatzip args
  (apply flat1 (apply zip args)))


(def cdddr (x)
  (cdr:cddr x))

(mac require (x)
  `(ail-code (racket-require (racket-prefix-in racket- ,x))))


(def 1+ (x)
  (+ 1 x))

(def 1- (x)
  (- x 1))


;; TODO: is there a better name than imac?
(mac imac (parms . body)
  `(annotate 'mac (fn ,parms ,@body)))


(def splitlast (x)
  (split x (- (len x) 1)))


(def list* args
  (if (no args)
        nil
      (cddr args)
        (cons car.args (apply list* cdr.args))
      (cons car.args cadr.args)))


;; TODO: I think macex1 and macex can be defined better
(def macex1 (x)
  (macex x t))


(def lastcdr (x)
  (nthcdr (- (len x) 1) x))

;; TODO: should use (defset last ...)
(mac setlast (x y)
  `(whenlet it (lastcdr ,x)
     (scar it ,y)))
     ;(= (car it) ,y))


(mac zap2 (x y var . args)
  `(= ,var (,x ,y ,var ,@args)))
;  `(zap (^:apply ,x ,y _ ,args) ,var))

(redef expand=list (terms)
  (if (cddr terms)
        `(do ,@(mapeach (p v) (pair terms)
                 (expand= p v)))
      (apply expand= terms)))

(def none (x y)
  (no:some x y))

(extend * (x . args) (or (sym? x) (isa x 'string)) ;(str? x)
  (apply sym (n-of (apply + args) x)))

(mac and= args
  `(do ,@(mapeach (x y) (pair args)
           `(and ,x (= ,x ,y)))))

(mac afneach (parms x . body)
  (w/uniq u
    `((afn (,u)
        (whenlet ,parms ,u
          ,@body))
      ,x)))

(mac w/ccc (x . body)
  `(ccc (fn (,x) ,@body)))

;; TODO: should this return a function or quasiquoted code?
(def thunk (x)
  `(fn () ,x))

(redef avg ns (/ (apply + ns) (len ns)))

(def square (x) (expt x 2))

(def readlines (x)
  (drain:readline x))


(def floor (x)
  (racket-floor x))

(def ceil (x)
  (racket-ceiling x))


(extend coerce (x y . r) (and (table? x)
                              (is y 'cons))
  (tablist x))

(extend coerce (x y . r) (and (cons? x)
                              (is y 'table))
  (listtab x))


;=============================================================================
;  Should be in binary.arc
;=============================================================================

(def bitand args
  (apply racket-bitwise-and args))


;=============================================================================
;  Should be in fninfo.arc
;=============================================================================

(def fninfo (x)
  (cons (name x)
        (cons (fn-args x)
              (fn-body x))))


;=============================================================================
;  Should be in re.arc
;=============================================================================

(mac re-multi-replace (x . args)
  ;; TODO: can this use afneach?
  ((afn (((from to (o g)) . rest))
     ;(debug "re-multi-replace" from to g)
     (list (if g 'racket-regexp-replace*
                 'racket-regexp-replace)
           (str from)
           (if rest self.rest x)
           (if sym?.to str.to to)))
   (rev args)))

(def re-split (x y)
  (ar-r/list-toarc:racket-regexp-split (regexp x) y))


;=============================================================================
;  Should be somewhere else (not sure where)
;=============================================================================

(def fn-iferr (trial then else)
  (let (had-err val) (on-err (^:list t _)
                             (^:list nil (trial)))
    (.val:if had-err then else)))

(mac aiferr (x y . body)
  `(fn-iferr (fn ()   ,x)
             (fn (it) ,y)
             (fn (it) ,@body)))
