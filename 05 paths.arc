;;(load:string %.exec-dir "lib/utils.arc")
;;(load:string %.exec-dir "lib/parameters.arc")

(%require racket/path)

(make-parameter cwd
  (%.make-derived-parameter %.current-directory
    (fn (v) (zap string v)
            (if empty.v
                (%.current-directory)
                (%.expand-user-path v)))
    (fn (v) (%.path->string v))))

;; TODO: inefficient
(make-parameter script-args
  (%.make-derived-parameter %.current-command-line-arguments
    %.list->vector
    %.vector->list))

(= script-src (car script-args))
(zap cdr script-args)


(def extension (x)
  (aand (%.filename-extension string.x) %.bytes->string/utf-8.it))

(def expandpath (x)
  (zap string x)
  (if empty.x
      x
      (%.path->string %.expand-user-path.x)))

(def path args
  (%.path->string:apply %.build-path
    ((afn (x acc)
       (if no.x
           rev.acc ;; nrev
           (let c (expandpath car.x)
             (if (empty c)
                   (self cdr.x acc)
                 (is c.0 #\/)
                   (self cdr.x (cons c nil))
                 (self cdr.x (cons c acc))))))
     args nil)))

(= exec-dir %.exec-dir)

(def exec-path args
  (apply path exec-dir args))


(def make-path->string (converter)
  (fn (x)
    (zap string x)
    (unless empty.x
      (aand converter.x %.path->string.it))))

(= dirname  (make-path->string:compose %.path-only           expandpath))
(= basename (make-path->string:compose %.file-name-from-path expandpath))

(def abspath ((o x))
  (%.path->string (%.path->complete-path expandpath.x)))

(def absdir ((o x))
  (dirname abspath.x))


(def dirall ((o x ".") (o f idfn))
  (w/cwd x
    ;; alet
    ((afn (d)
       (mappend (fn (x)
                  (let x (path d x)
                    (when f.x
                          ;; dirname
                      (if (dir-exists x)
                          (self x)
                          (list x)))))
                (dir (or d "."))))
     nil)))

(def todir (x)
  (zap string x)
          ;; TODO: last
  (if (is (x:- len.x 1) #\/)
      x
      (string x "/")))


(def hidden-file? (x)
  (is string.x.0 #\.))
