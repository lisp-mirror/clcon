;; Autotests for clcon. Hope will be organized later into with some unit-test 
;; facility. Now just compile and load the file.


(def-merge-packages::! :clcon-tests
                       (:use :cl)
  (:always t)
  )

(in-package :clcon-tests)



#+windows (def-trivial-test::! odu-file-path-namestring.1 
                     (string=
                      (DIRED::ODU-FILE-PATH-NAMESTRING "c:\\Program Files\\Steel Bank Common Lisp\\1.2.16\\sbcl.exe")
                      "c:/Program Files/Steel Bank Common Lisp/1.2.16/"
                      )
  t)

#+windows (def-trivial-test::! odu-file-path-namestring.2
                     (string=
                      (DIRED::ODU-FILE-PATH-NAMESTRING "c:/Program Files/Steel Bank Common Lisp/1.2.16/sbcl.exe")
                      "c:/Program Files/Steel Bank Common Lisp/1.2.16/"
                      )
  t)


