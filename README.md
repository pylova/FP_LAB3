<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 3</b><br/>
"Конструктивний та деструктивний підходи до роботи зі списками"<br/>
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right"><b>Студенка:</b> <i>Пильова Д.М КВ-11</i><p>
<p align="right"><b>Рік:</b> <i>2024</i><p>

## Загальне завдання
Реалізуйте алгоритм сортування чисел у списку двома способами: функціонально і
імперативно.
1. Функціональний варіант реалізації має базуватись на використанні рекурсії і
конструюванні нових списків щоразу, коли необхідно виконати зміну вхідного списку.
Не допускається використання: псевдо-функцій, деструктивних операцій, циклів,
функцій вищого порядку або функцій для роботи зі списками/послідовностями, що
використовуються як функції вищого порядку. Також реалізована функція не має
бути функціоналом (тобто приймати на вхід функції в якості аргументів).
2. Імперативний варіант реалізації має базуватись на використанні циклів і
деструктивних функцій (псевдофункцій). Не допускається використання функцій
вищого порядку або функцій для роботи зі списками/послідовностями, що
використовуються як функції вищого порядку. Тим не менш, оригінальний список
цей варіант реалізації також не має змінювати, тому перед виконанням
деструктивних змін варто застосувати функцію copy-list (в разі необхідності).
Також реалізована функція не має бути функціоналом (тобто приймати на вхід
функції в якості аргументів).
## Варіант 4
Алгоритм сортування вставкою №2 (з лінійним пошуком справа) за незменшенням.

## Лістинг функції з використанням конструктивного підходу
```lisp
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


```
### Тестові набори та утиліти
```lisp
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
```
### Тестування
```bash
$ sbcl --script scripts/constructive-sort.lisp 
Test 1... passed
Test 2... passed
Test 3... passed
Test 4... passed
Test 5... passed
Test 6... passed
```
## Лістинг функції з використанням деструктивного підходу
```lisp
(defun destructive-sort (lst)
"Constructs sorted list by non-descending,
 using insertion sort, linear search from the right"
  (let ((sorted-lst (copy-list lst)))
    (loop for i from 1 below (list-length sorted-lst) do
      (let ((x (nth i sorted-lst)) (j i))
        (loop while (and (> j 0) (< x (nth (- j 1) sorted-lst))) do
          (setf (nth j sorted-lst) (nth (- j 1) sorted-lst))
          (decf j)
        )
        (setf (nth j sorted-lst) x)
      )
    )
    sorted-lst
  )
)
```
### Тестові набори та утиліти
```lisp
(defun check-destructive-sort (name input expected)
"Execute `destructive-sort' on `input',
 compare result with `expected' and print comparison status"
  (format t "Test ~a... ~:[FAILED~;passed~]~%"
    name (equal (destructive-sort input) expected) 
  )
)

(defun test-destructive-sort ()
  (check-destructive-sort "1" '(1 2 3 4 5) '(1 2 3 4 5))
  (check-destructive-sort "2" '(5 4 3 2 1) '(1 2 3 4 5))
  (check-destructive-sort "3" '(4 1 3 2 5) '(1 2 3 4 5))
  (check-destructive-sort "4" '(5 5 3 3 1) '(1 3 3 5 5))
  (check-destructive-sort "5" '(1 -1 2 -2 0 0) '(-2 -1 0 0 1 2))
  (check-destructive-sort "6" '() '())
)

(test-destructive-sort)
```
### Тестування
sbcl --script lab_3.lisp
Functional Sort: (3 1 4 1 5 9) -> (1 1 3 4 5 9)
Functional Sort: (9 7 5 3 1 0) -> (0 1 3 5 7 9)
Functional Sort: (1 2 3 4 5 6) -> (1 2 3 4 5 6)
Imperative Sort: (3 1 4 1 5 9) -> (1 1 3 4 5 9)
Imperative Sort: (9 7 5 3 1 0) -> (0 1 3 5 7 9)
Imperative Sort: (1 2 3 4 5 6) -> (1 2 3 4 5 6)
PASSED: Functional Test 1
PASSED: Functional Test 2
PASSED: Functional Test 3
PASSED: Functional Test 4
PASSED: Functional Test 5
PASSED: Imperative Test 1
PASSED: Imperative Test 2
PASSED: Imperative Test 3
PASSED: Imperative Test 4
PASSED: Imperative Test 5
