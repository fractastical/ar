;;(load:string %.exec-dir "lib/utils.arc")

(require-rename parameterize #%parameterize)

#|(mac %nocall args
  (cons %.nocompile (%.ac-all args)))|#

#|(mac %nocall args
  ;(%.ac-all args)
  (prn:map (fn (x) `(%no ,x)) args)
  )|#

#|(mac parameterize (parms . body)
  `(%nocall (%no #%parameterize) (%no ,(pair:map %.ac parms))
              ,@body))|#

(mac parameterize (parms . body)
  `(%no:#%parameterize ,(pair %.ac-all.parms)
          ,@%.ac-all.body))

(mac make-w/ (x)
  `(mac ,(sym:string "w/" x) (v . body)
     `(parameterize ((%no ,',x) ,v) ;(var ',',x)
        ,@body)))

(mac make-parameter (x y)
  `(do (make-w/ ,x)
       (= (var ',x) ,y))) ;(sref (%.namespace) (%.make-parameter ,y) ',x)

(mac parameter (x (o y))
  `(make-parameter ,x (%.make-parameter ,y)))
