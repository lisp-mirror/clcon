; -*- coding: utf-8; -*-
(in-package :cl-user)

; for stepper, must be 
; debug > max(speed,compilation-speed,space)

#| Находки:

sb-c::maybe-expand-local-inline - условие, при котором наступает inline расширение

  (if (and (policy call
                   (and (>= speed space)
                        (>= speed compilation-speed)))
           (not (eq (functional-kind (node-home-lambda call)) :external))
           (inline-expansion-ok call))

sb-c::STORE-CLOSURE-DEBUG-POINTER - что-то полезное (возможно)

А ТАКЖЕ НАДО ПОСМОТРЕТЬ НА INHIBIT-WARNINGS!!!

|#

(declaim (optimize (debug 3) (speed 0) (compilation-speed 2) (space 2) (safety 3)))

;(declaim (optimize (sb-c::let-conversion 0)))
;(declaim (optimize (sb-c::store-closure-debug-pointer 3)))

(defun bubu (x)
  (budden-tools:with-the1 y symbol x
                          (break)
                          (print y)))

(defun obmanka3 (x) x)
(declaim (notinline obmanka3))

#|(defun f (x)
  "Outer function"
  (let ((y (obmanka2 x)))
    (break)
    (+ (g y) (+ x x))))|#

(defun f (x)
  "Outer function"
  (let ((y (budden-tools::keep-var-for-debug x))
        (z (+ x 1)))
    (+ (g y) (+ y z))))



(defun g (y)
  "Inner function"
  (break)
  (+ y 4)
  )

(defmacro with-necessary-stepper-handler (&body body)
  `(handler-bind ((step-condition 'sb-impl::invoke-stepper))
     ,@body))

(defun stepper-session-for-clcon ()
  "For stepping to be possible, it is not sufficient to set compiler policy according to manual. 
 Also we need to be in scope of appropriate handler, which is ensured by sb-impl::toplevel-repl,
 but which is not true in other threads unless we use something like my 'with-stepping-possible' 
 macro. "
  (with-necessary-stepper-handler
   (f 5)))


(defun stepper-session-for-sbcl-console ()
  "This is demo of using stepper in thread to be started from bare SBCL prompt. Is is invented
 by me, Budden, and I'm not sure if it is ok. 

 There is another SBCL-console specific problem which affects stepper: user can not interact with 
 the thread unless it is 'foreground'. It is described only briefly in manual. 

 Here we see an example of dealing with foreground state of the thread. 
 As we start (stepper-session-for-sbcl-console), we are initially in a foreground repl thread. 

 It creates new thread and calls release-foreground for the sake of new thread. This gives our 'worker' thread 
 an access to user interaction streams. As work is done, it releases foreground.

"
  (let ((thread (sb-thread:make-thread
                 (lambda ()
                   (with-necessary-stepper-handler
                    (unwind-protect
                        (f 5)
                      (sb-thread::release-foreground)
                      )))
                 :name "Worker thread")))
    (sb-thread::release-foreground thread)))




