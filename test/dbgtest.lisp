(in-package :cl-user)

(proclaim '(optimize debug))


(defun f (x)
  (+ (g x) (+ x x)))


(defun g (x)
  (break)
  (+ x 4)
  )
