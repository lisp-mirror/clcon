;; -*- coding : utf-8 ; Encoding : utf-8 ; system :clcon-server ; -*-
;; backend for grep-browser.grbr.tcl
;; and toplevel search functions, e.g. clco::find-in-clcon-sources

(in-package :clco)


(defstruct filter-match
  (filename (budden-tools:mandatory-slot 'filename) :type string)
  (line-number (budden-tools:mandatory-slot 'line-number) :type alexandria:positive-integer)
  (line (budden-tools:mandatory-slot 'line) :type string)
  (start-position 0) ; reserved
  (end-position 1) ; reserved
  )

(defun filter-one-file (infile expr &key (mode :nocase))
  "Mode can be :exact, :nocase, :regexp. Returns list of matches. Match is a list of three values: string, its number, "
  (assert (eq mode :nocase) () "Only :nocase mode is supported")
  (with-open-file (in infile :direction :input)
    (do* ((result nil result)
          (filename (namestring infile) filename)
          (line (read-line in nil nil) (read-line in nil nil))
          (line-number 1 (+ line-number 1))
          (position nil nil))
         ((null line)
          (nreverse result))
      (setf position (search expr line :test 'char-equal))    
      (when position
        (push (make-filter-match
               :filename filename
               :line-number line-number
               :line line
               :start-position nil
               ; when more than one match, we will have two matches,
               ; loop complicates. Skip this for now.
               ) result)))))


(defun clcon-sources ()
  "Returns an approximate list of clcon source files"
  (let ((filelist nil))
    (dolist (mask '("**/*.lisp" "**/*.tcl" "**/*.asd" "**/*.md"))
      (dolist (file (directory (merge-pathnames mask *clcon-source-directory*)))
        (push file filelist)))
    (nreverse filelist)))

(defun filter-many-files (files expr &rest keyargs &key (mode :nocase))
  "Filters list of files and appends all results"
  (declare (ignore mode)) ; it is used as part of keyargs
  (loop :for f :in files :for r = (apply #'filter-one-file f expr keyargs)
    :appending r)
  )
          
(defun calc-code-for-filter-match (grbr match serial)
  (let* ((code-to-jump-to-location
          (with-output-to-string (ou2)
            (write-code-to-pass-to-file-line-char
             ou2
             (filter-match-filename match)
             (filter-match-line-number match)
             (or (filter-match-start-position match) 0)
             ))))
    (format nil "::grbr::AppendData ~A ~A ~A ~A ~A ~A ~A ~A"
            grbr
            serial
            (cl-tk:tcl-escape (filter-match-line match))
            (cl-tk:tcl-escape (filter-match-filename match))
            (filter-match-line-number match)
            (filter-match-start-position match)
            (filter-match-end-position match)
            (cl-tk:tcl-escape code-to-jump-to-location))))


(defun present-text-filtering-results (results &key refresh-command)
  "Gets results from, say, filter-one-file and present them to tcl"
  (let* ((grbr (eval-in-tcl (format nil "::grbr::OpenGrepBrowser") :nowait nil))
         (serial 0)
         (print-progress-when 100))
    (when refresh-command ; it will be used as refresh-command later... maybe... FIXME
      (eval-in-tcl (format nil "::grbr::FillHeaderText ~A ~A"
                           grbr
                           (cl-tk:tcl-escape refresh-command))))
    (dolist (match results)
      (eval-in-tcl (calc-code-for-filter-match grbr match (incf serial)))
      (when (= serial print-progress-when)
        (format t "~%loaded ~D messages into ~A" serial grbr)
        (setf print-progress-when (* print-progress-when 2))
        )
      )
    (eval-in-tcl (format nil "::grbr::ShowGrepBrowser ~A" grbr))
    grbr
    ))


(defun find-in-clcon-sources (string)
  "Searches for string, ignores case"
  (clco::present-text-filtering-results
   (clco::filter-many-files (clco::clcon-sources) string)
   :refresh-command
   (prin1-to-string  `(clco::filter-many-files (clco::clcon-sources) ,string))))
