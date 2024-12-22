;; Функція для вставки елемента в упорядкований список (функціональний підхід)
(defun insert-descending (value sorted)
  (cond
    ((null sorted) (list value)) ;; Якщо список порожній, створюємо новий список з елементом
    ((<= value (car sorted)) (cons value sorted)) ;; Якщо елемент менший або рівний першому, вставляємо на початок
    (t
     (cons (car sorted) ;; Додаємо перший елемент до результату рекурсивного виклику
           (insert-descending value (cdr sorted))))))

;; Реалізація сортування вставками (функціональний підхід)
(defun sort-insertion-functional (unsorted)
  (if (null unsorted)
      nil ;; Якщо список порожній, повертаємо nil
      (insert-descending (car unsorted) (sort-insertion-functional (cdr unsorted)))))

;; Реалізація сортування вставками (імперативний підхід)
(defun sort-insertion-imperative (lst)
  (let ((sorted (copy-list lst))) ;; Створюємо копію початкового списку
    (loop for i from 1 below (length sorted) do
      (let ((key (nth i sorted))
            (j (1- i)))
        (loop while (and (>= j 0) (> (nth j sorted) key)) do
          (setf (nth (1+ j) sorted) (nth j sorted)) ;; Зсуваємо елементи вправо
          (decf j))
        (setf (nth (1+ j) sorted) key))) ;; Вставляємо елемент на потрібну позицію
    sorted))

;; Тестування обох реалізацій
(defun test-sorting ()
  (let ((test-cases '((3 1 4 1 5 9)
                      (9 7 5 3 1 0)
                      (1 2 3 4 5 6))))
    ;; Функціональний підхід
    (dolist (case test-cases)
      (format t "Functional Sort: ~A -> ~A~%" case
              (sort-insertion-functional case)))
    ;; Імперативний підхід
    (dolist (case test-cases)
      (format t "Imperative Sort: ~A -> ~A~%" case
              (sort-insertion-imperative case)))))

;; Перевірка функціонального підходу з очікуваннями
(defun validate-functional (name input expected)
  (format t "~:[FAILED~;PASSED~]: ~a~%" (equal (sort-insertion-functional input) expected) name))

;; Перевірка імперативного підходу з очікуваннями
(defun validate-imperative (name input expected)
  (format t "~:[FAILED~;PASSED~]: ~a~%" (equal (sort-insertion-imperative input) expected) name))

;; Тести для функціональної реалізації
(defun test-functional-sorting ()
  (validate-functional "Functional Test 1" '(3 1 4 1 5 9) '(1 1 3 4 5 9))
  (validate-functional "Functional Test 2" '(9 7 5 3 1) '(1 3 5 7 9))
  (validate-functional "Functional Test 3" '(1 2 3 4 5) '(1 2 3 4 5))
  (validate-functional "Functional Test 4" nil nil)
  (validate-functional "Functional Test 5" '(1) '(1)))

;; Тести для імперативної реалізації
(defun test-imperative-sorting ()
  (validate-imperative "Imperative Test 1" '(3 1 4 1 5 9) '(1 1 3 4 5 9))
  (validate-imperative "Imperative Test 2" '(9 7 5 3 1) '(1 3 5 7 9))
  (validate-imperative "Imperative Test 3" '(1 2 3 4 5) '(1 2 3 4 5))
  (validate-imperative "Imperative Test 4" nil nil)
  (validate-imperative "Imperative Test 5" '(1) '(1)))

;; Запуск тестів
(test-sorting)
(test-functional-sorting)
(test-imperative-sorting)