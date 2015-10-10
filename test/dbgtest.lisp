(in-package :cl-user)

(declaim (optimize (debug 3) (speed 0) (compilation-speed 0) (space 0)))


(defun f (x)
  (+ (g x) (+ x x)))


(defun g (x)
  (break)
  (+ x 4)
  )
