(import re strings script)

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
