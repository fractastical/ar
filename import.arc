(make-w/ load-suffix*)
(make-w/ load-paths*)
#|(implicit load-paths* (list cwd (abspath) (abspath "lib/")))
(implicit load-suffix* ".arc")

(def load-file-dir (x)
  (car:mem (fn (y)
             (file-exists (joinpath y x)))
           load-paths*))|#

(make-implicit cwd
  (racket-make-derived-parameter (%nocompile racket-current-directory)
    (fn (v) (zap string v)
            (if empty.v
                  (racket-current-directory)
                (racket-expand-user-path v)))
    (fn (v) (racket-path->string v))))

(def expandpath (x)
  (zap string x)
  (if empty.x
        x
      (racket-path->string:racket-expand-user-path x)))

(def joinpath args
  (racket-path->string:apply racket-build-path
    (aloop (x   args
            acc nil)
      (if (no x)
            rev.acc
          (let c (expandpath car.x)
            (if (is c.0 #\/)
                  (self cdr.x (cons c nil))
                (self cdr.x (cons c acc))))))))

;; TODO: ew
(redef load-file-dir (x)
  ;; this is just (car:mem [file-exists (joinpath _ x)] load-paths*)
  (find [file-exists:joinpath _ x] load-paths*))
#|  (aloop (xs load-paths*)
    (if (no xs)
          nil
        (file-exists:joinpath car.xs x)
          (car xs)
        (self cdr.xs))))
|#
(mac import args
  `(do ,@(map (fn (x)
                (let x (load-normalize-path string.x)
                  ;; TODO: use w/cwd ...?
                  `(w/cwd ,load-file-dir.x
                  ;`(parameterize (racket-current-directory ,load-file-dir.x)
                     (load ,x))))
              args)))
