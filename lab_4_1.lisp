(defun insert-into-sorted (elem sorted &key (key #'identity) (test #'<))
  "Insert ELEM into the sorted list SORTED based on KEY and TEST."
  (if (or (null sorted) (funcall test (funcall key elem) (funcall key (car sorted))))
      (cons elem sorted)
      (cons (car sorted) (insert-into-sorted elem (cdr sorted) :key key :test test))))

(defun insertion-sort-functional (lst &key (key #'identity) (test #'<))
  "Sort the list LST using insertion sort with KEY and TEST."
  (if (null lst)
      nil
      (insert-into-sorted (car lst) (insertion-sort-functional (cdr lst) :key key :test test) :key key :test test)))


(defun check-sorting (name func input expected)
  (let ((result (funcall func input)))
    (format t "~:[FAILED~;PASSED~] ~a~%~%" (equal result expected) name)
    result))

(defun test-sorting ()
  (check-sorting "test 1" #'insertion-sort-functional '(3 1 4 1 5 9) '(1 1 3 4 5 9))
  (check-sorting "test 2" #'insertion-sort-functional '(1 7 4 2 8 5 9) '(1 2 4 5 7 8 9))
  (check-sorting "test 3" #'insertion-sort-functional '(1 2 3) '(1 2 3)))
