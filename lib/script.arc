(make-implicit cwd
  (racket-make-derived-parameter racket-current-directory
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

(require scheme/path)

(def make-path->string (converter)
  (fn (x)
    (zap string x)
    (unless empty.x
      (zap converter x)
      (when ac-tnil.x
        (racket-path->string x)))))

(= dirname  (make-path->string racket-path-only:expandpath))
(= basename (make-path->string racket-file-name-from-path:expandpath))
