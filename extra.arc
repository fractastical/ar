(defset assoc (y x)
  (w/uniq g
    (list (list g x)
          `(assoc ,y ,g)
          `(fn (val) (scar (assoc-ref ,y ,g) val)))))

(defset alref (x y)
  (w/uniq g
    (list (list g x)
          `(alref ,g ,y)
          `(fn (val) (scar (cdr:assoc ,y ,g) val)))))


#|
;; TODO: inefficient
(def zip args
  ;; TODO: this causes it to stop when all the lists are empty.
  ;;       however, in Python, it stops when the first list is
  ;;       empty. I'm not sure which is the better semantic.
  ;;       to get the Python behavior, just change `all` to `some`
  (if (all no args)
        nil
      (cons (apply list (map car args))
            (apply zip  (map cdr args)))))|#

(def zip args
  ;; Faster than the above: 395ms compared to 695ms
  ;; TODO: how fast is map applied to multiple lists?
  (apply map list args))


(mac rloop (name parms . body)
  (let (l r) (apply zip pair.parms)
    `((rfn ,name ,l ,@body) ,@r)))

(mac aloop (parms . body)
  `(rloop self ,parms ,@body))


(mac afneach (parms x . body)
  (w/uniq u
    `((afn (,u)
        (whenlet ,parms ,u
          ,@body))
      ,x)))


(mac catcherr (expr)
  `(on-err (fn (c) (details c))
           (fn () ,expr nil)))
