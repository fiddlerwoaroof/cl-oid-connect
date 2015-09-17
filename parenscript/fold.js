(in-package :ps_translator)

(macros 
  (getOffset (el) `($ (,el) (offset) top)))

(def-event document ready ()
  (def-event ".link-header, .feed-header" click ()
    ($ (".menu") (add-class "open")) t)

  (def-event ".link-header" click ()

    ($each (this (siblings ".link-content"))
      (if (= ($this (css "max-height")) "0px")

        (let ((added-height ($this (children) (outer-height)))
              (parent-height ($this (parents ".post-list") (css "max-height"))))
          ($this (css "max-height" added-height))

          ($this (parents ".post-list")
                 (css "max-height" (+ added-height parent-height))))

        ($this (css "max-height" "0px"))))
    ($this (parent) (toggle-class "closed")))

  (def-event ".feed-header" click ()
    ($each (this (siblings ".post-list"))
      (if (= ($this (css "max-height")) "0px")
        ($this (css "max-height" (@ this scroll-height)))
        ($this (css "max-height" "0px"))))
    ($this (parent) (toggle-class "closed")))

  (def-event ".flip-button" click ()
    (let* ((style-sheet ($ ("link[href^=\"/theme\"]")))
           (style-sheet-name (chain style-sheet (attr "href"))))
      (chain style-sheet
             (attr "href"
                   (if (chain style-sheet-name (match (regex |dark|)))
                     (chain style-sheet-name (replace (regex |dark|) "light"))
                     (chain style-sheet-name (replace (regex |light|) "dark")))))))
  nil)

; vim: set ft=lisp :
