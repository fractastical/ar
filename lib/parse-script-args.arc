; ew, so inefficient...
(def split-last (x)
  (list (cut x 0 -1) (last x)))

(= shortopts (table)
   longopts  (table))

(def is- (x)
  (is x #\-))

(mac add-option (opt var)
  (let f `(fn () ,var)
    (if (is-:opt 1) `(= (',longopts ,(cut opt 2)) ,f)
                    `(= (',shortopts ,(opt 1)) ,f))))

(def parse-all (args)
  (collect:each x args
    (if (is-:x 0)
          (if (is-:x 1)
                (iflet x (longopts (cut x 2))
                  (x)
                  (yield x))
                (each x (cut x 1)
                  (iflet x (shortopts x)
                    (x)
                    (yield (+ "-" x)))))
          (yield x))))

(mac parse-script-args args
  `(do ,@(mappend (fn (x)
                    (let (p v) (split-last x)
                      (map (fn (x)
                             `(add-option ,x ,v)) p)))
                  args)
       (zap parse-all script-args)))
