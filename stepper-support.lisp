;; -*- coding: utf-8 ; Encoding: utf-8 ; system :clcon-server ; -*-
;; stepper support
(in-package :clco)

(defun restart-with-name-exists-p (name)
  (let ((result (restart-with-name-exists-p-inner name)))
    (etypecase result
      (null 0)
      (symbol
       (assert (symbol-package result) () "Имя рестарта должно быть в пакете")
       (list (package-name (symbol-package result)) (string result))))))

(defun restart-with-name-exists-p-inner (name)
  "name - символ или список символов, ищет и возвращает первое имя рестарта, к-рое существует или 0, если ни одного нет"
  (etypecase name
    (null
     nil)
    (symbol
     (if
      (find-restart name)
      ;(find name swank::*sldb-restarts* :key #'restart-name)
      name
      nil))
    (cons
     (or (restart-with-name-exists-p-inner (car name))
         (restart-with-name-exists-p-inner (cdr name))))
    ))

(defun invoke-sldb-restart-by-name (name)
   (invoke-restart (find name swank::*sldb-restarts* :key #'restart-name)))
   
