(use arc)

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
