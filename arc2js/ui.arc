;(use arc2js)
;(use dom)

;; TODO: should be somewhere else
(mac defvar (name parms . body)
  `(var ,name (fn ,parms ,@body)))

(mac set (tab . args)
  `(= ,@(mappeach (x y) pair.args
          `((,tab ',x) ,y))))

(mac call (x . args)
  `(when (typeof ,x "function")
     (,x ,@args)))


;(mac event (x type

;(on modal dragover (e)
;  (alert "foo"))

; (mac iskey ... )


(var modal (letr top (div title "\\ x00"
                          style (position "fixed   !important"
                                 left     "0px     !important"
                                 top      "0px     !important"
                                 width    "100%    !important"
                                 cursor   "default !important"))
  (var info {})

  (defvar drag (e)
    (style top display "none !important")
    (var target (document!elementFromPoint e!clientX e!clientY))
    (style top display "")

    (unless (info!parent!contains target)
      (remove)))

  (defvar remove (e)
    (del:on modal dragover    drag)
    (del:on modal contextmenu remove)
    (del:on modal click       remove)
    (del:on nil   keydown     remove)

    (when e (e!preventDefault))

    (style info!parent z-index info!z-index)
    (remove top)
    (call info!action)
    (trigger info!element "UI-modal-off"))

  (defvar keydown (e)
    (when (iskey e!which ) ;(is e!which 27)
      (remove e)))

  (fn (x action)
    (if (instanceof x Element)
          (let parent x!parentNode
            (set info
              element  x
              action   action
              parent   parent
              z-index  parent!style!z-index)

            (style parent z-index "9002 !important")
            (parent!insertBefore top x)
            (trigger x "UI-modal-on")

            (add-event-listener "keydown" keydown true)
            (modal!add-event-listener "dragover"  drag true)
            (modal!add-event-listener "contextmenu" remove true)
            (modal!add-event-listener "click" remove true))
        info!parent
          (do (trigger top "click")
              (del info!parent))))))


(mac make-context-menu (x)
  `(letr top (element ul
                      class-name "UI-contextMenu"
                      title      "\x00";
                      hidden     ""
                      on         (contextmenu (e)
                                   (e!preventDefault)))
     (= top!keys {})

     (defvar close ()
       (modal null))

     (defvar unhover )
     (defvar select )
     (defvar unselect )
     (defvar keydown )
))

(mac w/context-menu (name . args)
  `(make-context-menu (fn (,name)
                        )))

(mac context-menu args
  `(w/context-menu it ,@args))
