(defun associative-list-add (lst key value)
  (append lst (list (cons key value))))

(defun associative-list-get (lst key)
  (if (eq lst nil)
      (values nil nil)
      (if (eq (car (car lst)) key)
          (values (cdr (car lst)) T)
          (associative-list-get (cdr lst) key))))

(defun property-list-add (lst key value)
  (append lst (list key value)))

(defun property-list-get (lst key)
  (if (eq lst nil)
      (values nil nil)
      (if (eq key (car lst))
          (values (car (cdr lst)) T)
          (property-list-get (cdr (cdr lst)) key))))

(defun binary-tree-add (tree key val)
  (let ((left (cadr tree))
        (right (caddr tree))
        (root (car tree)))
    (cond
      ((null root) (list (cons key val) left right))
      ((string< (car root) key) (list root right (binary-tree-add right key val)))
      ((string> (car root) key) (list root (binary-tree-add left key val) right))
      )))

(defun binary-tree-get (tree key)
  (let ((left (cadr tree))
        (right (caddr tree))
        (root (car tree)))
    (cond
      ((null tree) (values NIL NIL))
      ((string= key (car root)) (values (cdr root) T))
      ((string< (car root) key) (binary-tree-get right key))
      ((string> (car root) key) (binary-tree-get left key)))))
