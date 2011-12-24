(def partition (f xs)
  (awith (x  xs
          l  nil
          r  nil)
        ;; TODO: should use cons? rather than no
    (if (no x)
          (list l r)
        (f car.x)
          (self cdr.x (cons car.x l) r)
        (self cdr.x l (cons car.x r)))))


(def zip args
  ;; TODO: how fast is map applied to multiple lists?
  (apply map list args))


;; (percent 100 200) -> 100
;; (percent 100 150) -> 50
;; (percent 100 50)  -> -50
;; (percent 100 25)  -> -75
(def percent (x y)
  (- (* (/ y x) 100) 100))


;; Infinite nil generator, courtesy of akkartik (rocketnia reminded me)
(= nils list.nil)
(= (cdr nils) nils)


;; TODO: name is good, but not ideal
;;       aletwhen is accurate but too long
;;       alet is misleading because afnlet does a null check
;;       what about aletif ...? no, because it uses when, not if
(mac afnlet (parms x . body)
  (w/uniq u
    #`(alet u x
        (whenlet parms u . body))))

#|(mac afnlet2 (parms x . body)
  #`(alet parms x (when parms . body)))|#


(mac catcherr (expr)
  (w/uniq c
    #`(on-err (fn (c) (details c))
              (fn ()  expr nil))))

;; TODO: should be in strings.arc
(def xml-encode (s)
  (multisubst '(("&" "&amp;")
                ("<" "&lt;")) s))


;; TODO: these should be in io.arc
(def readlines (x (o eof (uniq)) . args)
  (drain (apply readline x eof args) eof))

(def pipe-lines (y)
  (w/pipe-from x y readlines.x))

(def dispfile (val file)
  ;; TODO: should probably use a temporary file
  (w/outfile o file (disp val o))
  val)


;; TODO: should this be (+ 1 x) or (+ x 1)?
(def 1+ (x) (+ 1 x))
(def 1- (x) (- x 1))


;=============================================================================
;  defmac / defprint
;=============================================================================

;; TODO: macro for generating these
(= defmac-types* (obj))

(mac defmac (name parms . body)
  #`(= (defmac-types* ',name) (fn parms ,@body)))

(extend ac-macro? (x) (defmac-types* (type x))
  (fn args (apply it x args)))


;; TODO: macro for generating these
(= defprint-types* (obj))

(mac defprint (name parms . body)
  #`(= (defprint-types* ',name) (fn parms ,@body)))

(extend print (primitive x port) (defprint-types* (type x))
  (it primitive x port))


;=============================================================================
;  %splice
;=============================================================================

#|(def ac-nocompile (x)
  (let x (let self ((x x))
           (mappend (fn (x)
                          ;; TODO: don't hardcode the symbol %compile
                      (if (caris x '%compile)
                            (ac-args (cdr x))
                          (cons? x)
                            (list (self x))
                          (list x)))
                     x))
    (if (no (cdr x))
          (car x)
        (cons 'racket-begin x))))|#

#|(nomac % args
  (if (cdr args)
        `(racket-begin ,@args)
      (car args)))|#

(mac %splice args
  #`(% (ac-splice . args)))


;=============================================================================
;  compat.arc
;=============================================================================

(alias acons cons?)
(alias alist list?)

(def call-w/stdout (port thunk)
                ;; TODO: %get
  (parameterize (racket-current-output-port port) (thunk)))

(def call-w/stdin (port thunk)
                ;; TODO: %get
  (parameterize (racket-current-input-port port) (thunk)))


;=============================================================================
;  arubic.arc New additions
;=============================================================================

(alias pow   expt)
(alias str   string)
(alias str?  string?)

(def mv (old new)
  ;; only difference with mvfile is this one uses #f
  ;; TODO: should probably be an argument instead
  (%inline:racket-rename-file-or-directory old new #f)
  nil)


(mac buildeach (name f)
  (w/uniq (args expr body)
    #`(mac name (args expr . body)
        #`(f (fn (args) . body) expr))))

;; TODO: w/let
;; TODO: probably don't need this let
(let mappend mappend
  (mac fnify args
    #`(do ,@(mappend (fn (x)
                       (let f (sym x 'fn)
                         #`((= f x)
                            (buildeach x f))))
                     args))))


#|
;; TODO: use buildeach
(mac maplet (var x . body)
  #`(map (fn (var) ,@body) x))|#

(buildeach maplet map)


;=============================================================================
;  arc.arc
;=============================================================================

(mac atomic body
  #`(atomic-invoke (fn () ,@body)))

(mac atlet args
  #`(atomic (let ,@args)))

(mac atwith args
  #`(atomic (with ,@args)))

(mac atwiths args
  #`(atomic (withs ,@args)))


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


;; Used in news.arc and app.arc but not anywhere else
(= hooks* (obj))

(def hook (name . args)
  (aif (hooks* name) (apply it args)))

(mac defhook (name . rest)
  #`(= (hooks* ',name) (fn ,@rest)))
