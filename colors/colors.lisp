;;;; colors.lisp
(defpackage #:colors
  (:use #:cl)
  (:export #:colorscheme #:palette *palette*
           #:invert-palette
           #:let-palette #:with-palette
           #:accentize
           #:colorscheme-bg #:colorscheme-bg-highlight 
           #:colorscheme-fg-deemph #:colorscheme-fg #:colorscheme-fg-highlight 
           #:colorscheme-accent 
           #:colorscheme-hover-highlight))

(in-package #:colors)
(declaim (optimize (debug 2) (safety 2) (speed 0)))

;;; Generic utility macro TODO: separate these out!!!
(defmacro initialize-to (obj1-v obj2-v &body slot-swaps)
  (alexandria:with-gensyms (obj1 obj2)
    `(let* ((,obj1 ,obj1-v)
            (,obj2 ,obj2-v))
       ,@(loop for (to from) in slot-swaps
               collect `(setf (,to ,obj1) (,from ,obj2))))))

;;; This macro connects the "-" prefixed slots in the colorscheme class
;;; To the appropriate palette
(defmacro def-palette-accessor (scheme-slot scheme palette )
  `(progn
     (defgeneric ,scheme-slot (,scheme))
     (defmethod ,scheme-slot ((,scheme colorscheme))
       (slot-value ,palette (,(intern (concatenate 'string "-" (symbol-name scheme-slot))) ,scheme)))))


;; &group interfaces
;;; Palette class and methods &group

(defclass palette () ; solarized http://ethanschoonover.com/solarized
  ((base03     :accessor palette-base03      :initform "#002b36")
   (base02     :accessor palette-base02      :initform "#073642")
   (base01     :accessor palette-base01      :initform "#586e75")
   (base00     :accessor palette-base00      :initform "#657b83")
   (base0      :accessor palette-base0       :initform "#839496")
   (base1      :accessor palette-base1       :initform "#93a1a1")
   (base2      :accessor palette-base2       :initform "#eee8d5")
   (base3      :accessor palette-base3       :initform "#fdf6e3")
   (yellow     :accessor palette-yellow      :initform "#b58900")
   (orange     :accessor palette-orange      :initform "#cb4b16")
   (red        :accessor palette-red         :initform "#dc322f")
   (magenta    :accessor palette-magenta     :initform "#d33682")
   (violet     :accessor palette-violet      :initform "#6c71c4")
   (blue       :accessor palette-blue        :initform "#268bd2")
   (cyan       :accessor palette-cyan        :initform "#2aa198")
   (green      :accessor palette-green       :initform "#859900")))

(defgeneric invert-palette (palette))

;;; The palette var: this defaults to the solarized palette defined
;;; above, but can (and should) be temporarily rebound via the 
;;; with-palette macro below.
(defparameter *palette* (make-instance 'palette))

(defmacro let-palette (palette &body body)
  "Set custom palette in end-user code"
  `(let ((*palette* ,palette))
     ,@body))

(defmacro with-palette ((place) &body body)
  "Access the current palette"
  `(let ((,place *palette*))
     ,@body))

;;; &endgroup
;;; &group Color scheme
(defclass colorscheme ()
  ((bg           :accessor -colorscheme-bg           :initform 'base03)
   (bg-highlight :accessor -colorscheme-bg-highlight :initform 'base02)
   (fg-deemph    :accessor -colorscheme-fg-deemph    :initform 'base01)
   (fg           :accessor -colorscheme-fg           :initform 'base0 )
   (fg-highlight :accessor -colorscheme-fg-highlight :initform 'base1 )
   (hover-highlight :accessor -colorscheme-hover-highlight :initform 'base3 )
   (accent       :accessor -colorscheme-accent       :initform 'violet)))

(defgeneric accentize (colorscheme accent))

(def-palette-accessor colorscheme-bg               scheme *palette*)
(def-palette-accessor colorscheme-bg-highlight     scheme *palette*)
(def-palette-accessor colorscheme-fg-deemph        scheme *palette*)
(def-palette-accessor colorscheme-fg               scheme *palette*)
(def-palette-accessor colorscheme-fg-highlight     scheme *palette*)
(def-palette-accessor colorscheme-accent           scheme *palette*)
(def-palette-accessor colorscheme-hover-highlight  scheme *palette*)

;;; &endgroup
;; &endgroup

(defmethod invert-palette ((palette palette))
  (let ((result (make-instance 'palette)))
    (initialize-to result palette
      (palette-base03 palette-base3)
      (palette-base02 palette-base2)
      (palette-base01 palette-base1)
      (palette-base00 palette-base0)
      (palette-base0  palette-base00)
      (palette-base1  palette-base01)
      (palette-base2  palette-base02)
      (palette-base3  palette-base03))
    result))

(defmethod accentize ((colorscheme colorscheme) accent)
  (setf (colorscheme-accent colorscheme) (funcall accent colorscheme)))

; vim: foldmethod=marker foldmarker=&group,&endgroup foldlevel=0 :
