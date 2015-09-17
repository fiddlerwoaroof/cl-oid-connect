(ql:quickload :parenscript)

(defpackage :ps_translator
  (:use :parenscript :cl)
  (:export main))

(in-package :ps_translator)
(use-package :parenscript)

(defpsmacro macros (&body body)
  `(lisp (progn
           ,@(loop for form in body
                   collect `(defpsmacro ,@form))
           "")))

(defmacro+ps $ ( (selector) &rest values) `(chain (j-query ,selector) ,@values))

(defmacro+ps $this (&rest values) `($ (this) ,@values))

(defmacro+ps $each ((selector &rest actions) &body code)
  `($ (,selector) ,@actions
      (each (lambda ()
              ,@code))))

(defmacro+ps def-event (target event args &body run)
 `($ (,target)
          (,event
            (lambda ,args
              ,@run))))

(defun translate-file (infile outfile)
  (let ((*JS-TARGET-VERSION* 1.9))
    (with-open-file (o outfile :direction :output :if-exists :supersede)
      (with-open-file (i infile :direction :input)
        (write-line (ps-compile-file i) o)))))

(defun main (args)
  (in-package :ps_translator)
  (apply #'translate-file (cdr args)))


