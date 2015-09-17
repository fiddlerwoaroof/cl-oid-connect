(ql:quickload :parenscript)

(defpackage :angular
  (:use :parenscript :cl)
  (:export main))
(declaim (optimize (speed 0) (safety 2) (debug 2)))

(in-package :angular)

(defpsmacro macros (&body body)
  `(lisp (progn
           ,@(loop for form in body
                   collect `(defpsmacro ,@form))
           "")))


(defmacro+ps scope-vars (&rest bindings)
  `(progn ,@(loop for (n v) in bindings collect `(scope-var ,n ,v))))

(defmacro+ps scope-var (name value)
  `(setf ($s ,name) ,value))

(defmacro+ps resource (name url params &body body)
  `(var ,name
        ($resource ,url ,(cons 'create params)
                   ,@(loop for (name form) in body
                           collect (list 'create name (cons 'create form))))))

(defmacro+ps scope-function (name arguments &body body)
  `(scope-var ,name (lambda ,arguments
                      ,@body)))

(defmacro+ps $s (&rest values) `(chain $scope ,@values))

(defun build-lambda (dependencies body)
  `(list ,@(mapcar #'symbol-to-js-string dependencies)
         (lambda ,(loop for x in dependencies
                        collect x)
           ,@body
           nil)))


; This is just for slimv's sake
(defmacro defcontroller (name dependencies &body body)
  (declare (ignore name dependencies body)))

(defpsmacro def-module (module-name dependencies &body body)
  `(macrolet ((defcontroller (name dependencies &body body)
                (let ((dependencies (cons '$scope dependencies)))
                  (list 'chain ',module-name `(controller ,(symbol-to-js-string name)
                                                          ,(build-lambda dependencies body))))))
     (progn (var ,module-name ((@ angular module) ,(symbol-to-js-string module-name) ,dependencies))
            ,@body)))

(defmacro+ps select-> (selector)
  `(chain document (query-selector ,selector)))

(defmacro+ps select->element (selector)
  `(chain angular (element (select-> ,selector))))


(defmacro+ps ng-ajax (mthd endpoint data resultS &body callback)
  `(chain $http
          (,mthd ,endpoint ,data)
          (success (lambda (,resultS)
                     ,@callback))))

(defun translate-file (infile outfile)
  (let ((*JS-TARGET-VERSION* 1.9))
    (with-open-file (o outfile :direction :output :if-exists :supersede)
      (with-open-file (i infile :direction :input)
        (write-line (ps-compile-file i) o)))))

(defun main (args)
  (in-package :angular)
  (apply #'translate-file (cdr args)))


