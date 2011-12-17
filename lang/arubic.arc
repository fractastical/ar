(import re strings script)

;=============================================================================
;  New additions
;=============================================================================

(alias pow   expt)
(alias str   string)
(alias str?  string?)

(def mv (old new)
  ;; only difference with mvfile is this one uses #f
  (racket-rename-file-or-directory old new #f)
  nil)

(mac curly-bracket args
  #`(obj ,@args))


(mac buildeach (name f)
  (w/uniq (args expr body)
    #`(remac name (args expr . body)
        #`(f (fn (args) ,@body) expr))))

;; TODO: w/let
;; TODO: probably don't need this let
(let mappend mappend
  (mac fnify args
    #`(do ,@(mappend (fn (x)
                       (let f (sym x 'fn)
                         #`((= f x)
                            (buildeach x f))))
                     args))))


;=============================================================================
;  Changes
;=============================================================================

(= arubic-namespace (zap new-namespace namespace)) ;(namespace-partition)

;; TODO: macro to generate these
(mac w/arubic body
  #`(eval-w/ arubic-namespace ,@body))


(fnify mappend some all keep rem)

(= mapfn  map)
(= map    maplet)


(remac square-bracket args
  #`(list ,@args))

(redef isa (x y . args)
  (let x type.x
    (or (is x y)
        ;; can't use `some`, because that causes an infinite loop
        (if args (reclist (fn (y) (is x car.y)) args)))))

(extend prn args cdr.args
  (apply orig (intersperse " " args)))
