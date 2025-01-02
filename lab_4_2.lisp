(in-package :cl-user)

(defun remove-each-rnth-reducer (n &key key)
  (let ((predicate (or key (constantly t)))
        (count 0))
    (lambda (elem acc)
      (incf count)
      (if (and (zerop (mod count n)) (funcall predicate elem))
          acc
          (cons elem acc)))))

(defun check-reducer (test-name reducer-fn list &key (from-end t) (initial-value '()) (expected '()))
  (let ((result (reduce reducer-fn list :from-end from-end :initial-value initial-value)))
    (format t "~:[FAILED~;PASSED~] ~a~%"
            (equal result expected)
            test-name)
    result))

(defun test-remove-each-rnth-reducer ()
  (check-reducer "Remove each 2nd from (1 2 3 4 5)"
                 (remove-each-rnth-reducer 2)
                 '(1 2 3 4 5)
                 :expected '(1 3 5))
  (check-reducer "Remove each 3rd from (1 2 3 4 5 6 7)"
                 (remove-each-rnth-reducer 3)
                 '(1 2 3 4 5 6 7)
                 :expected '(1 3 4 6 7))
  (check-reducer "Remove each 2nd even from (1 2 2 2 3 4 4 4 5)"
                 (remove-each-rnth-reducer 2 :key #'evenp)
                 '(1 2 2 2 3 4 4 4 5)
                 :expected '(1 2 3 4 5)))
