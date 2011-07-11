(use arc defcall strings)

;; should be in arc.arc
(def 1+ (x)
  (+ 1 x))

;=============================================================================
;  Ssyntax
;=============================================================================

(def object-ssyntax? (x)
  (and (isa x 'sym)
       (posmatch "<-" (string x))))

(extend ac-ssyntax (x) (object-ssyntax? x) t)

#|(let orig ac-ssyntax
  (= ac-ssyntax (fn (x)
                  (or (object-ssyntax? x)
                      (orig x)))))|#

(def ac-expand-object (x)
  (let pos (object-ssyntax? x)
    (zap string x)
    (with (l (cut x 0 pos)
           r (cut x (+ pos 2)))
      `(get-attribute ,(sym l) ',(sym r)))))

(defrule ac-expand-ssyntax (object-ssyntax? sym)
  (ac-expand-object sym))


;=============================================================================
;  fail
;=============================================================================

(implicit fail)

(mac fail? (x)
  `(w/fail (uniq)
     (is ,x fail)))

(mac isnt/fail (var val then . else)
  (w/uniq (x y)
    `(let (,x ,y) (w/fail (uniq)
                    (list ,val fail))
       (if (is ,x ,y)
         (do ,@else)
         (let ,var ,x
           ,then)))))


;=============================================================================
;  Attributes
;=============================================================================

(def get-attribute (tab key (o default fail))
  (racket-hash-ref (rep tab) key default))

(def set-attribute (tab key (o value))
  (ail-code (racket-hash-set! (rep tab) key value))
  value)

(def del-attribute (tab key)
  (ail-code (racket-hash-remove! (rep tab) key))
  nil)

(defset get-attribute (x k)
  (w/uniq g
    (list (list g x)
          `(get-attribute ,x ',g)
          `(fn (val) (set-attribute ,g ,k val)))))


;=============================================================================
;  Objects
;=============================================================================

(implicit self)

(mac object args
  (w/uniq x
    `(let ,x (table)
       ,@(map (fn ((k v))
                `(set-attribute ,x ',k ,v))
              (pair args))
       (annotate 'object ,x))))

#|
(if (no y) x y)
(if y y x)
(or y x)
|#

(def call-w/self (x f . args)
  (w/self (or self x) ; (if (object? self) self x)
    (apply f args)))

(def obj-attr (x n)
  (when (object? x)
    (w/fail (uniq)
      (let x (get-attribute x n)
        (and (isnt x fail)
             (or x t))))))

;(extend not (x) (is x fail) t)


(with (type type
       is   is)

  (def object? (x)
    (is (type x) 'object)))


(def make-multi-is (x)
  (object ;type  nil
          keys  (fn ()  x)
          is    (fn (y) (some y x))
          print (fn ()  (string "#<is [" (intersperse #\space x ) "]>"))))

#|(def object? (x)
  (is (type x) 'object))
|#

(extend type (x) (object? x)
  (isnt/fail x (get-attribute x 'type)
    (if (isa x 'sym)
          x
          (make-multi-is x))
    ;'table
    (make-multi-is '(table object))
    ))



; tests for multi-argument is
; (is 'cons (make-multi-is '(cons table)) (make-multi-is '(table cons)))
; (is (make-multi-is '(cons table)) 'cons (make-multi-is '(table cons)))
; (is (make-multi-is '(cons table)) (make-multi-is '(table cons)) 'cons)
(extend is (x . rest) (obj-attr x 'is)
  (apply call-w/self x it rest))

#|(and (object? x)
                       (no:isa (type x) 'sym))|#
#|(extend isa (x y) (object? x)
  (zap type x)
  (if (isa x 'sym)
        (is x y)
      (some y x)))|#
  #|(when (isa x 'sym)
    (zap list x))|#
  #|(some y (if (isa x 'sym)
                (list x)
                x)))|#

#|
(extend type (x) (object? x)
  (isnt/fail x (get-attribute x 'type)
    x
    (orig x)))
|#

(def maxstrlen (xs)
  (apply max (map [len:string _] xs)))

(def padding (x p)
  (newstring (- p (len:string x)) #\space))

(def str< (x y)
  (< (string x) (string y)))

(def pretty-print-table (name tab (o f tab))
  (pr "(" name)
  (withs (k (sort str< (keys tab))
          s (1+ (len name))
          m (maxstrlen k))

    ((afn ((x . y))
       (when x
         (pr " " x (padding x m) " " (f x))
         (when y
           (prn)
           (pr:newstring s #\space)
           (self y))))
     k))
  (pr ")"))

#|(each x k
      (pr " ")
      (pr:string x (padding x m))
      (pr " ")
      (prn (tab x))
      (pr:newstring s #\space))|#

(def print-object (x port)
  #|(when (fail? x<-keys)
    (= x (listtab:collect:each (k v) x
           (yield:list k (x k))))
    (prn x))|#

  (w/stdout port
    (pretty-print-table "object"
                        x
                        (if (fail? x<-keys)
                              (fn (k)
                                (get-attribute x k))
                               x))))

#|  (each (k v) x
    (disp " " port)
    (disp k port)
    (disp " " port)
    (disp v port)
    (disp "\n       " port))|#

(extend print (p x port) (object? x)
  (isnt/fail print (get-attribute x 'print)
    (disp (call-w/self x print) port)
    (isnt/fail type (get-attribute x 'type)
      (orig p x port)
      (print-object x port))))

(extend len (x) (object? x)
  (isnt/fail len (get-attribute x 'len)
    (call-w/self x len)
    (isnt/fail keys (get-attribute x 'keys)
      (len (call-w/self x keys))
      (len (rep x)))))

(extend maptable (f tab) (object? tab)
  (isnt/fail keys (get-attribute tab 'keys)
    (do (each k (call-w/self tab keys)
          (f k (tab k)))
        tab)
    (orig f (rep tab))))


#|(def ar-apply (x . args)
  (racket-apply x (ar-toracket args)))|#

#|(let orig ar-apply
  (def ar-apply (x . args)
    (orig orig x args)))

(extend ar-apply (x . args) (object? x)
  (isnt/fail call (get-attribute x 'call)
    (apply call args)
    (apply (rep x) args)))|#

#|(defcall object (x . args)
  )|#

(extend ar-apply-non-fn (x args) (object? x)
  ;(prn x " " args)
  (isnt/fail call (get-attribute x 'call)
    (apply call-w/self x call args)
    (apply get-attribute x args)))

#|
;; inefficient, use defcall later
(extend coerce (x type . r) (and (is type 'fn)
                                 (object? x))
  (isnt/fail call (get-attribute x 'call)
    (fn args
      (apply call-w/self x call args))
    (fn args
      (apply get-attribute x args))))|#
    ;(orig (rep x) type)))

  #|
  (w/fail (uniq)
    (let call (get-attribute x 'call)
      (if (is call fail)
            (fn (k (o d fail))
              (get-attribute x k d))
            call))))
  |#

(extend sref (x v k) (object? x)
  (isnt/fail set (get-attribute x 'set)
    (call-w/self x set k v)
    (set-attribute x k v)))


;=============================================================================
;  Deletion
;=============================================================================

(= del-rules* (table))

(mac defdel (name parms . body)
  `(= (del-rules* ',name) (fn ,parms ,@body)))

(let fail (uniq)
  (def dref (x l (o r fail) (o s fail))
    ;(prn x)
    (case (type x)
      table (do (ail-code (racket-hash-remove! x l))
                x)
      cons  (do (if (and (is r fail)
                         (is s fail))
                    (= (x l (1+ l)) nil)
                    (= (x l r s) nil))
                #|(join (cut x 0 l)
                      (cut x (or r (1+ l))))|#
                x)
            (err "cannot delete" x))))

(mac del (x)
  (when (ac-ssyntax x)
    (zap ac-expand-ssyntax x))

  (if #|(ac-ssyntax x)
        `(del ,(ac-expand-ssyntax x))|#
      (acons x)
        `(do1 ,x
              ,(iflet d (del-rules* (car x))
                 (apply d (cdr x))
                 `(zap dref ,@x)))
      (err "unimplemented")))
#|  (case (car x)
    get-attribute `(del-attribute ,@(cdr x))
                   (err "unimplemented")))|#

(defdel car (x)
  `(zap cdr ,x))

(defdel cdr (x)
  `(scdr ,x nil))



(extend dref (x k . rest) (object? x)
  (isnt/fail del (get-attribute x 'del)
    (apply call-w/self x del k rest)
    (del-attribute x k))
  x)

(defdel get-attribute (x y)
  `(del-attribute ,x ,y))
