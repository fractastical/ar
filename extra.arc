(def zip args
  ;; TODO: how fast is map applied to multiple lists?
  (apply map list args))


(mac rloop (name parms . body)
  ;; TODO: can this just use mappend rather than zip? should it?
  (let (l r) (apply zip pair.parms)
    #`((rfn name l ,@body) ,@r)))

(mac aloop (parms . body)
  #`(rloop 'self parms ,@body))


(mac afnlet (parms x . body)
  (w/uniq u
    #`((afn (u)
         (whenlet parms u
           ,@body))
       x)))


(def maplast (f xs)
  (if (no cdr.xs)
        (f car.xs)
      (do (f car.xs)
          (maplast f cdr.xs))))


(mac catcherr (expr)
  #`(on-err (fn ('c) (details 'c))
            (fn ()   expr nil)))

(def xml-encode (s)
  (multisubst '(("&" "&amp;")
                ("<" "&lt;")) s))


(def dispfile (val file)
  ;; TODO: should probably use a temporary file
  (w/outfile o file (disp val o))
  val)


(mac extend (name parms test . body)
  (w/uniq u
    #`(let 'orig name
        (= name (fn u
                  ;; TODO: (apply (fn ,parms ...) ,u) ?
                  (let parms u
                    (aif test
                           (do ,@body)
                         ;; TODO: does this need %nocompile...?
                         ;;       probably not
                         (%nocompile (apply 'orig u))))
                 )))))


(= defcall-types* (table))

(mac defcall (name parms . body)
  #`(= (defcall-types* ',name) (fn parms ,@body)))

(extend ac-apply-non-fn (x . args)
  (%nocompile (orig defcall-types* (type x)))
  (apply it x args))


(mac %eval body
  (eval #`(do ,@body)))


(def listify (x)
  (if (cons? x) x (list x)))
