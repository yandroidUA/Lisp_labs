(defun add-one (a)
  (+ a 1))

(defun add-values (a b)
  (+ a b))

(defun fast (a b)
  (concatenate 'string a b))

(defun my-map (func lst)
  (if (eq lst nil)
      lst
      (cons (funcall func (car lst)) (my-map func (cdr lst)))))

(defun my-filter (func lst)
  (if (eq lst nil)
      NIL
      (if (eq T (funcall func (car lst)))
          (cons (car lst) (my-filter func (cdr lst)))
          (my-filter func (cdr lst)))))
 
(defun is-one-predicate (a)
  (eq a 1))

(defun my-fold (func lst &optional (start NIL start-p))
  (let
      ((acc
        (if (eq start-p NIL)
            (car lst)
            start))
       (mList
        (if (eq start-p NIL)
            (cdr lst)
            lst)))
    (labels ((mFunc (func lst acc)
               (if (eq lst NIL)
                   acc
                   (mFunc func (cdr lst) (funcall func acc (car lst))))))
      (mFunc func mList acc))))
