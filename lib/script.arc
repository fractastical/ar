(def racket-vector->mlist (x)
  (racket-list->mlist:racket-vector->list x))
  (def racket-mlist->vector (x)
  (racket-list->vector:racket-mlist->list x))
  (make-parameter script-args
  (racket-make-derived-parameter racket-current-command-line-arguments
    racket-mlist->vector
    racket-vector->mlist
    ))

;; TODO: ew
(= script-src (car script-args))
(zap cdr script-args)


(def extension (x)
  (let x (racket-filename-extension string.x)
    (if (is x #f)
          nil
        (string x)))
  )

(def hidden-file (x)
  (is x.0 #\.))


;; TODO: should maybe be elsewhere?
(def dirall ((o x) (o f))
  (w/cwd x
    ((afn (d)
       (mappend (fn (x)
                  (if (dirname x) ;; dirname
                        (self (string d x))
                      (list (string d x))))
                (dir d f)))
     nil)))


;; Algorithm taken from Python's os.path.join (http://bugs.python.org/issue9921)
;; Ignore the previous parts if a part is absolute.
;; Insert a '/' unless the first part is empty or already ends in '/'.

(def todir (x)
  (if (is last.x #\/)
        x
      (string x "/")))

(def absdir ((o x))
  (dirname abspath.x))
