;; general concept of testing:
#|
1. In tcl, programmatically open file
2. Do some sequence of edits (programmatically)
3. Compare result strings. 
|#

(in-package :clco)

(defun string-between-marks (beg end)
  (setf (oduvanchik::region-start oduvanchik-internals::*internal-temp-region*) beg
        (oduvanchik::region-end oduvanchik-internals::*internal-temp-region*) end)
  (oduvanchik::region-to-string oduvanchik-internals::*internal-temp-region*))

(defun entire-buffer-string (&optional (buffer (oduvanchik::current-buffer)))
  (string-between-marks
   (oduvanchik::buffer-start-mark buffer)
   (oduvanchik::buffer-end-mark buffer)))
