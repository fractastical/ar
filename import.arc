(implicit load-paths* (list cwd (abspath) (abspath "lib/")))
(implicit load-suffix* ".arc")

(def find-file-path (x)
  (car:mem (fn (y)
             (file-exists (joinpath y x)))
           load-paths*))

(mac import args
  `(do ,@(map (fn (x)
                (unless (extension x)
                  (zap string x load-suffix*))
                `(w/cwd ,(find-file-path x)
                   (load ,x)))
              args)))
