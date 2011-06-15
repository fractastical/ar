(mac w/element (e n . args)
  `(let ,e (document.createElement ,(string n))
     ,@((afn (l)
          (let (x y) l
            (if (no x)
                  nil
                (acons x)
                  (cons `(,(sym:string e ".appendChild") ,x)
                         (self cdr.l))
                (cons (case x
                        on    `(,(sym:string e ".addEventListener")
                                 ,(string:car y)
                                  (fn ,@(cdr y))
                                  true)
                        #|style `(,(sym:string e ".setAttribute")
                                 "style"
                                 ,(string:map (fn ((x y))
                                                (list x ":" y ";"))
                                              (pair y)))|#
                        style `(do ,@(map (fn ((x y))
                                            `(= ,(sym:string e ".style." x)
                                                ,(string y)))
                                          (pair y)))
                              `(,(sym:string e ".setAttribute") ,(string x) ,y))
                      (self cddr.l)))))
        args)
     ,e))

(mac element (n . args)
  `(w/element self ,n ,@args))

(mac w/div (e . args)
  `(w/element ,e div ,@args))

(mac div args
  `(element div ,@args))

(mac append (to . rest)
  `(do ,@(map (fn (x)
                `(,(sym:string to ".appendChild") ,x))
              rest)))
