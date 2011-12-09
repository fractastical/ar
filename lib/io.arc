(mac quiet body
  (w/uniq gv
    ;; TODO: figure out a way to do this without outstring
    #`(w/outstring gv
        (w/stdout gv
          (w/stderr gv
            ,@body)))))
