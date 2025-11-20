<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 3</b><br/>
"Конструктивний і деструктивний підходи до роботи зі списками"<br/>
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right"><b>Студент(-ка)</b>: Прізвище Ім'я По-батькові група</p>
<p align="right"><b>Рік</b>: рік</p>
## Загальне завдання

## Варіант <22>
Сортування обміном №1 (без оптимізацій) за незменшенням
## Лістинг функції з використанням конструктивного підходу
```lisp
(defun bubble-pass-functional (lst)
  (labels
      ((bubble-step (current rest)
        
         (if (null rest)
            
             (values (list current) nil)
             (let ((next (car rest)))
               (if (> current next) 
                   
                   (multiple-value-bind (sublist changed)
                       (bubble-step current (cdr rest))
                     (declare (ignore changed)) ;
                     (values (cons next sublist) t))
                   (multiple-value-bind (sublist changed)
                       (bubble-step next (cdr rest))
                     (values (cons current sublist) changed)))))))
    (if (null lst)
        (values nil nil)
        (bubble-step (car lst) (cdr lst)))))


(defun bubble-sort-functional (lst)
  (labels
      ((iterate (xs)
         (multiple-value-bind (new-list changed)
             (bubble-pass-functional xs)
           (if changed
               (iterate new-list)  
               new-list))))         
    (iterate lst)))
```


## Лістинг функції з використанням деструктивного підходу
```lisp
(defun bubble-sort-imperative (lst)
  (let* ((result (copy-list lst))      
         (n      (length result)))
    ;; Зовнішній цикл: N-1 проходів 
    (do ((i 0 (1+ i)))
        ((>= i (1- n)) result)         
      (do ((j 0 (1+ j)))
          ((>= j (- n i 1)))
        (let ((a (nth j result))
              (b (nth (1+ j) result)))
          (when (> a b)                
            (rotatef (nth j result)
                     (nth (1+ j) result))))))))
```
### Тестові набори та утиліти
```lisp
(defvar *bubble-test-total* 0)
(defvar *bubble-test-passed* 0)

(defun report-test-result (name passed)
  (incf *bubble-test-total*)
  (if passed
      (progn
        (incf *bubble-test-passed*)
        (format t "~A ... PASSED~%" name))
      (format t "~A ... FAILED~%" name)))

(defun check-bubble-sort-case (name input expected)
  (let* ((original (copy-list input))
         (res-func  (bubble-sort-functional input))
         (res-imper (bubble-sort-imperative input))
         (passed
           (and (equal res-func expected)
                (equal res-imper expected)
                (equal input original))))
    (report-test-result name passed)))

(defun test-bubble-sort ()
  (setf *bubble-test-total* 0
        *bubble-test-passed* 0)

  ;; ----- Тест-кейси -----
  (check-bubble-sort-case "empty list" '() '())
  (check-bubble-sort-case "single element" '(1) '(1))
  (check-bubble-sort-case "two elements reversed" '(2 1) '(1 2))
  (check-bubble-sort-case "random list" '(3 1 4 1 5 2) '(1 1 2 3 4 5))
  (check-bubble-sort-case "reverse sorted" '(5 4 3 2 1) '(1 2 3 4 5))
  (check-bubble-sort-case "already sorted" '(1 2 3 4 5) '(1 2 3 4 5))
  (check-bubble-sort-case "with negatives" '(0 -1 3 -2 2) '(-2 -1 0 2 3))
  (check-bubble-sort-case "all identical" '(1 1 1 1) '(1 1 1 1))

  ;; ПІДСУМОК
  (format t "~%Summary: ~D/~D tests passed.~%"
          *bubble-test-passed* *bubble-test-total*)

  (if (= *bubble-test-passed* *bubble-test-total*)
      (format t "All tests PASSED successfully!~%")
      (format t "Some tests FAILED.~%"))

  (values))
```

### Тестування
```lisp
*
(load "C:/Users/СС/Desktop/uniwork/Lisp/lr3/lr3.lisp")
T
*

(test-bubble-sort)
empty list ... PASSED
single element ... PASSED
two elements reversed ... PASSED
random list ... PASSED
reverse sorted ... PASSED
already sorted ... PASSED
with negatives ... PASSED
all identical ... PASSED

Summary: 8/8 tests passed.
All tests PASSED successfully!
```