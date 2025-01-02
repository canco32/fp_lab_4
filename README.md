<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 4</b><br/>
"Функції вищого порядку та замикання"
</p>
<p align="right"><strong>Студент:</strong> <i>Сілін Ілля Денисович КВ-12</i><p>
<p align="right"><strong>Рік:</strong> <i>2025</i><p>

  ## Загальне завдання

  Завдання складається з двох частин:

1. Переписати функціональну реалізацію алгоритму сортування з лабораторної
   роботи 3 з такими змінами:
     - використати функції вищого порядку для роботи з послідовностями (де це доречно);
     - додати до інтерфейсу функції (та використання в реалізації) два ключових параметра: ```key``` та ```test``` , що працюють аналогічно до того, як працюють
    параметри з такими назвами в функціях, що працюють з послідовностями.
    При цьому ```key``` має виконатись мінімальну кількість разів.
2. Реалізувати функцію, що створює замикання, яке працює згідно із завданням за
   варіантом (див. п 4.1.2). Використання псевдо-функцій не забороняється, але, за
   можливості, має бути мінімізоване.

  ## Варіант першої частини: 4

  Алгоритм сортування вставкою №2 (з лінійним пошуком справа) за незменшенням.

  ## Лістинг реалізації першої частини завдання

  ```
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
  ```
  ### Тестові набори та утиліти першої частини

  ```
(defun check-sorting (name func input expected)
  (let ((result (funcall func input)))
    (format t "~:[FAILED~;PASSED~] ~a~%~%" (equal result expected) name)
    result))

(defun test-sorting ()
  (check-sorting "test 1" #'insertion-sort-functional '(3 1 4 1 5 9) '(1 1 3 4 5 9))
  (check-sorting "test 2" #'insertion-sort-functional '(1 7 4 2 8 5 9) '(1 2 4 5 7 8 9))
  (check-sorting "test 3" #'insertion-sort-functional '(1 2 3) '(1 2 3)))
  ```

  ### Тестування першої частини
  ```
  CL-USER> (test-sorting)
  PASSED test 1
  
  PASSED test 2
  
  PASSED test 3
  
  (1 2 3)
  ```

## Варіант другої частини: 8
## Лістинг реалізації другої частини завдання
```
(defun remove-each-rnth-reducer (n &key key)
  (let ((predicate (or key (constantly t)))
        (count 0))
    (lambda (elem acc)
      (incf count)
      (if (and (zerop (mod count n)) (funcall predicate elem))
          acc
          (cons elem acc)))))
```
### Тестові набори та утиліти другої частини
```
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

```
### Тестування другої частини
```
CL-USER> (test-remove-each-rnth-reducer)
PASSED Remove each 2nd from (1 2 3 4 5)
PASSED Remove each 3rd from (1 2 3 4 5 6 7)
PASSED Remove each 2nd even from (1 2 2 2 3 4 4 4 5)
(1 2 3 4 5)
CL-USER> 
```
