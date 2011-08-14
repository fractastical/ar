(mac send-request (x parms . body)
  `(chrome!extension!sendRequest ,x (fn ,parms ,@body)))
