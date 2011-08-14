;(use arc2js)

(mac w/element (e n . args)
  `(let ,e (document!createElement ,(str n))
     ,@((afn (l)
          (let (x y) l
            (if (no x)
                  nil
                (cons? x)
                  (cons `((ref ,e 'appendChild) ,x) ;,(sym e "!appendChild")
                         (self cdr.l))
                (cons (case x
                        on    `((ref ,e 'addEventListener) ;,(sym e "!addEventListener")
                                 ,(str:car y)
                                  (fn ,@(cdr y))
                                  true)
                        #|style `(,(sym e "!setAttribute")
                                 "style"
                                 ,(str:mapeach (x y) (pair y)
                                    [x ":" y ";"]))|#
                        style `(do ,@(mapeach (x y) (pair y)
                                       `(= ((,e 'style) ',x) ,(str y)))) ;,(sym e "!style!" x)
                              `((ref ,e 'setAttribute) ,(str x) ,y)) ;,(sym e "!setAttribute")
                      (self cddr.l)))))
        args)
     ,e))

(mac on (type parms . body)
  `(addEventListener ,(str type)
                     (fn ,parms ,@body)
                     false))

(mac style (e . args)
  #|(when (ac-ssyntax e)
    (zap js-expand-ssyntax e))|#
  (let n (mapeach (x y) (pair args)
           `(= ((,e 'style) ',x) ;,(sym e "!style!" x)
               ,(str y)))
    (if (and (sym? e)
             (no:ac-ssyntax e))
          `(do ,@n)
        (w/uniq u
          `(let ,u (ref ,e 'style) ;,(sym e "!style")
             ,@(mapeach (x y) (pair args)
                 `(= (,u ',x)
                     ,(str y))))))))

(mac css (x)
  `(document!querySelectorAll ,(str x)))

(mac element (n . args)
  `(w/element self ,n ,@args))

(mac w/div (e . args)
  `(w/element ,e div ,@args))

(mac div args
  `(element div ,@args))

;,(sym to "!appendChild")
(mac append (to . rest)
  (w/uniq u
    ;; ew code duplication
    #|(if (sym? to)
          `(do ,@(mapeach x rest
                   `((ref ,to 'appendChild) ,x))
               ,to)|#
        `(let ,u ,to
           ,@(mapeach x rest
               `((ref ,u 'appendChild) ,x))
           ,u)
           ;)
           ))


(mac trigger (x type (o y false) (o z false))
  (w/uniq u
    `(let ,u (document!create-event "Event")
       ((ref ,u 'init-event) ,str.type ,y ,z)
       ((ref ,x 'dispatch-event ,u)))))

(mac remove (x)
  (w/uniq u
    `(iflet ,u (ref ,x 'parent-node)
       ((ref ,u 'remove-child) x))))
