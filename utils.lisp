(defpackage whitespace.utils
  (:use #:cl #:alexandria #:iterate))

(in-package whitespace.utils)

(defun ensure-mapping (list)
  "Make sure that each item of the list is a pair of symbols"
  (mapcar (lambda (x) (if (symbolp x) (list x x) x)) list))
(export 'ensure-mapping)

(defun alist-string-hash-table (alist)
  (alexandria:alist-hash-table alist :test #'string=))
(export 'alist-string-hash-table)

(defun make-pairs (symbols)
  (cons 'list (iterate (for (key value) in symbols)
                       (collect (list 'list* (symbol-name key) value)))))
(export 'make-pairs)

(defmacro copy-slots (slots from-v to-v)
  (with-gensyms (from to)
    `(let ((,from ,from-v) (,to ,to-v))
       ,@(iterate (for (fro-slot to-slot) in (ensure-mapping slots))
                  (collect `(setf (slot-value ,to ',to-slot) (slot-value ,from ',fro-slot))))
       ,to)))
(export 'copy-slots)


(defun transform-alist (pair-transform alist)
  (iterate (for (k . v) in-sequence alist)
           (collect
             (funcall pair-transform k v))))
(export 'transform-alist)

(defun %json-pair-transform (k v)
  (cons (make-keyword (string-downcase k))
        (typecase v
          (string (coerce v 'simple-string))
          (t v))))
(export '%json-pair-transform)

(defun %default-pair-transform (k v)
  (cons (make-keyword (string-upcase k)) v))
(export '%default-pair-transform)

(defmacro default-when (default test &body body)
  (once-only (default)
    `(or (when ,test
           ,@body)
         ,default)))
(export 'default-when)

(defmacro transform-result ((list-transform pair-transform) &body alist)
  `(funcall ,list-transform
            (transform-alist ,pair-transform
                             ,@alist)))
(export 'transform-result)


(defmacro slots-to-pairs (obj (&rest slots))
  (once-only (obj)
    (let* ((slots (ensure-mapping slots))
           (bindings (iterate (for (slot v &key bind-from) in slots)
                              (collect (or bind-from slot)))))
      `(with-slots ,bindings ,obj
         ,(make-pairs slots)))))
(export 'slots-to-pairs)


