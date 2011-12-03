#|(make-implicit cwd
  (racket-make-derived-parameter (%nocompile racket-current-directory)
    (fn (v) (zap string v)
            (if empty.v
                  (racket-current-directory)
                (racket-expand-user-path v)))
    (fn (v) (racket-path->string v))))|#


(def racket-vector->mlist (x)
  (racket-list->mlist:racket-vector->list x))
  ;(ar-toarc:racket-vector->list x))

(def racket-mlist->vector (x)
  (racket-list->vector:racket-mlist->list x))
  ;(racket-list->vector:ar-toracket x))

(make-implicit script-args
  (racket-make-derived-parameter (%nocompile racket-current-command-line-arguments)
    (fn (v) (racket-mlist->vector v))
    (fn (v) (racket-vector->mlist v))))

;; TODO: ew
(= script-src (car script-args))
(zap cdr script-args)


#|(def expandpath (x)
  (zap string x)
  (if empty.x
        x
      (racket-path->string:racket-expand-user-path x)))|#

;(require racket/path)

(def make-path->string (converter)
  (fn (x)
    (zap string x)
    (unless empty.x
      (zap converter x)
      (when ac-tnil.x
        (racket-path->string x)))))

(= dirname  (make-path->string racket-path-only:expandpath))
(= basename (make-path->string racket-file-name-from-path:expandpath))


(def extension (x)
  (let x (racket-filename-extension string.x)
    (if (is x #f)
          nil
        ;; TODO: maybe this should be in string1...?
        (racket-bytes->string/utf-8 x)))
  ;(cadr:re-match ".+\\.(\\w+)$" string.x)
  )

(def hidden-file (x)
  (is x.0 #\.))


;; TODO: should maybe be elsewhere?
(def dirall ((o x) (o f))
  #|(let f (fn (x) (or (dirname x)
                     (if f (f x) t))))|#
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

#|(def joinpath args
  (racket-path->string:apply racket-build-path
    (aloop (x   args
            acc nil)
      (if (no x)
            rev.acc
          (let c (expandpath car.x)
            (if (is c.0 #\/)
                  (self cdr.x (cons c nil))
                (self cdr.x (cons c acc))))))))|#

#|    (catch:afneach (x . rest) args
      (if empty.x
            self.rest
          (aand car.rest (is expandpath.it.0 #\/))
            (throw:self rest)
          (cons expandpath.x self.rest)))))|#

#|(def joinpath args
  (string:catch:afneach (x . rest) args
    (if empty.x
          self.rest
        (aand car.rest (is expandpath.it.0 #\/))
          (throw:self rest)
        (cons (let x expandpath.x
                (if rest todir.x
                         x))
              self.rest))))|#

;; TODO: use a racket equivalent
(def abspath ((o x))
  (joinpath cwd x))

#|(def abspath ((o x))
  (racket-path->string (racket-normalize-path (expandpath x))))|#

(def absdir ((o x))
  (dirname abspath.x))
