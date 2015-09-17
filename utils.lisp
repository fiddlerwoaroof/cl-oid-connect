(in-package whitespace.utils)
(lquery:define-lquery-list-function tag-name (nodes &rest tags)
  "Manipulate elements on the basis of there tag-name.
   With no arguments, return their names else return
   the corresponding tags."
  (if (null tags)
    (map 'vector #'plump:tag-name nodes)
    (apply #'vector
           (loop for node across nodes
                 if (find (plump:tag-name node) tags :test #'string=)
                 collect node))))


