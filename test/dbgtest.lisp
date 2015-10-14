(in-package :cl-user)

; for stepper, must be 
; debug > max(speed,compilation-speed,space)
(declaim (optimize (debug 3) (speed 0) (compilation-speed 0) (space 0)))

(defun f (x)
  "Outer function"
  (+ (g x) (+ x x)))

(defun g (x)
  "Inner function"
  (break)
  (+ x 4)
  )
