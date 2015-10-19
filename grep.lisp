;; -*- coding : utf-8 ; Encoding : utf-8 ; system :clcon-server ; -*-

;; general concept of testing:
#|
1. In tcl, programmatically open file
2. Do some sequence of edits (programmatically)
3. Compare result strings. 
|#

(in-package :clco)

#| (defun make-filtered-file-list (pathname &rest keyargs &key dir-options (file-test #'identity) (dir-test #'identity) (subdirs :recurse))
  "Returns list of pathnames. Args are as in budden-tools:map-dir"
  "Subdirs can be :recurse, :skip, :msp (fn is called for subdirs too) :map-and-recurse (fn is called and recursion occurs). Output format is ((pathname . (fn pathname)) ... (:subdir pathname ((pathname . (fn pathname)) ...))). directory structure should not be modified by fn except by deletion of pathname"
  (
    (budden-tools:map-dir (constantly nil)

 (defun grep (map-dir-args string-filter-args title)
  (with-output-to-string (r)
    
  (budden-tools::map-dir 
  grep-one-file file 

|#

(defstruct filter-match
  (filename (budden-tools:mandatory-slot 'filename) :type (or string pathname))
  (line-number (budden-tools:mandatory-slot 'line-number) :type alexandria:positive-integer)
  (line (budden-tools:mandatory-slot 'line) :type string)
  start-position ; reserved
  )

(defun filter-one-file (infile expr &key (mode :nocase))
  "Mode can be :exact, :nocase, :regexp. Returns list of matches. Match is a list of three values: string, its number, "
  (assert (eq mode :nocase) () "Only :nocase mode is supported")
  (with-open-file (in infile :direction :input)
    (do* ((result nil result)
          (line (read-line in nil nil) (read-line in nil nil))
          (line-number 1 (+ line-number 1))
          (position nil nil))
         ((null line)
          (nreverse result))
      (setf position (search expr line :test 'char-equal))    
      (when position
        (push (make-filter-match
               :filename infile
               :line-number line-number
               :line line
               :start-position nil
               ; when more than one match, we will have two matches,
               ; loop complicates. Skip this for now.
               ) result)))))
          
(defun calc-code-for-one-entry (match)
  (let* ((code-to-jump-to-location
	   (with-output-to-string (ou2)
             (write-code-to-pass-to-loc ou2 (location :mode :eval)))
            (t
             "{}")))
         )
    (format nil "::erbr::AppendData ~A ~A ~A ~A ~A"
            serial
            (cl-tk:tcl-escape (string-downcase (string severity)))
            (cl-tk:tcl-escape title)
            (cl-tk:tcl-escape details-code)
            (cl-tk:tcl-escape code-to-jump-to-location))))


