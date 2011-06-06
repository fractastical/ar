(= print-types* (table))

(defrule print (print-types* type.x) (it x port))

(mac defprint (type args . body)
  (w/uniq xargs
    `(= (print-types* ',type)
        (fn ,xargs
          (disp (let ,args ,xargs ,@body)
                (cadr ,xargs))))))


(defprint fn  (x) "#<fn>")
(defprint mac (x) "#<mac>")
