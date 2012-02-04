(load:string %.exec-dir "lib/01 utils.arc")
(load:string %.exec-dir "lib/02 parameters.arc")
(load:string %.exec-dir "lib/03 paths.arc")
;(load:string %.exec-dir "lib/04 namespaces.arc")


(parameter debug? t)

(redef debug args
  (when debug?
    (apply prn (intersperse " " args))))


(parameter import-dirs     (list cwd
                                 exec-dir
                                 (exec-path "lib/")))
(parameter import-cache    (hash (exec-path "02 arc.arc")            t
                                 (exec-path "lib/01 utils.arc")      t
                                 (exec-path "lib/02 parameters.arc") t
                                 (exec-path "lib/03 paths.arc")      t
                                 (exec-path "lib/04 import.arc")     t))
(parameter import-suffix   ".arc")
(parameter import-loading  nil)


(def import-file-dir (x)
  (find [file-exists:path _ x] import-dirs))

(def import-normalize-path (x)
  (if extension.x
      x
      (string x import-suffix)))

(def call-w/find-file (x f)
  (parameterize (%.port-count-lines-enabled #t)
    (let y import-normalize-path.x
      (iflet it import-file-dir.y
        (f (path it y) y)
        (iflet it (and (isnt x y)
                       (import-file-dir x))
          (f (path it x) x)
          (err:string "file \"" x "\" was not found"))))))


(def import-file (x)
  (call-w/find-file string.x
    (fn (path name)
      (let path abspath.path
        (if import-cache.path
            (debug " skipping:" name)
            (do (debug " loading: " name)
                (= import-cache.path t)
                (w/import-loading t
                  (load path))))))))

(def import-dir (x)
  (push abspath.x import-dirs))

(def import1 (x)
  (if basename.x
      import-file.x
      import-dir.x))

(mac import args
  `(w/import-dirs import-dirs
     (eachfn import1 ',args)))

(mac reimport args
  `(w/import-cache (table)
     (import ,args)))
