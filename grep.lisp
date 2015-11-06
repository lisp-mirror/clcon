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

(defun file-type-to-language (file-type)
  (let ((c-file-type (string-downcase file-type)))
    (alexandria:switch (c-file-type :test 'string=)
                       ("tcl" :tcl)
                       ("lisp" :lisp)
                       ("asd" :lisp)
                       (t :lisp))))


(defun lisp-definition-filter (expr line)
  "Extremely simple: toplevel is the form which starts from #\( at the beginning of line"
  (declare (ignore expr))
  (and (> (length line) 0)
       (char= (elt line 0) #\())
  )

(defun tcl-definition-filter (expr line)
  "Form that starts from the non-comment at the beginning of line, or which starts from the words proc, ::snit or namespace"
  (declare (ignore expr))
  (cond
   ((= (length line) 0)
    nil)
   (t
    (let* ((first-char (elt line 0))
           (trimmed (string-left-trim cl-ppcre::+whitespace-char-string+ line))
           (trimmed-len (length trimmed))
           (trimmed-first-char (and (> trimmed-len 0) (elt trimmed 0))))
      (cond
       ((null trimmed-first-char)
        nil)
       ((member trimmed-first-char '(#\# #\}) :test 'char=)
        nil)
       ((not (find first-char cl-ppcre::+whitespace-char-string+))
        t)
       (t
        (dolist (keyword '("proc" "namespace" "::snit::widget" "::snit::type" "::snit::widgetadaptor" "::snit::macro"))
          (multiple-value-bind (found suffix)
                               (alexandria:starts-with-subseq
                                keyword trimmed :test 'char= :return-suffix t)
            (when (and found)
              (or (string= suffix "")
                  (find (elt suffix 0) cl-ppcre::+whitespace-char-string+))
              (return-from tcl-definition-filter t))))))))))

(defun filter-one-file-or-err (infile display-filename expr mode)
  "Mode can be a string or a function of two arguments, expr and match. If mode is a function
   it must return position of the match when match is found or null when it is not found"
  (let ((filter 
         (etypecase mode
           (symbol 
            (assert (eq mode :nocase) () "Only :nocase mode is supported")
            (lambda (expr line)
              (search expr line :test 'char-equal)))
           (function
            mode
            ; believe everything is ok
            ))))
    (with-open-file (in infile :direction :input)
      (do* ((result nil result)
            (line (read-line in nil nil) (read-line in nil nil))
            (line-number 1 (+ line-number 1))
            (position nil nil))
           ((null line)
            (nreverse result))
        (setf position (funcall filter expr line))
        (when position
          (push (make-filter-match
                 :filename display-filename
                 :line-number line-number
                 :line line
                 :start-position nil
                 ;; when more than one match, we will have two matches,
                 ;; loop complicates. Skip this for now.
                 ) result))))))

(defun filter-one-file (infile expr &key (mode :nocase))
  "Mode can be :exact, :nocase, :regexp. Returns list of matches. Match is a list filter-match structures"
  (let ((display-filename (namestring infile)))
    (multiple-value-bind (matches error)
        (ignore-errors
          (filter-one-file-or-err infile display-filename expr mode))
      (cond
        (error
         (list (make-filter-match
                :filename display-filename
                :line-number 1
                :line (format nil "Error processing file: ~A" error)
                :start-position nil)))
        (t
         matches))
      )))
  

(defun clcon-sources ()
  "Returns an approximate list of clcon source files"
  (let ((filelist nil))
    (dolist (mask '("**/*.lisp" "**/*.tcl" "**/*.asd" "**/*.md" "../oduvanchik/**/*.lisp" "../oduvanchik/*.asd"))
      (dolist (file (directory (merge-pathnames mask *clcon-source-directory*)))
        (unless
            (or (member "xlam" (pathname-directory file) :test 'equalp)
                (member (pathname-name file) '("rather-big-file" "very-big-file") :test 'equalp))
          (push file filelist))))
    (nreverse filelist)))


(defun files-by-glob-list (&rest glob-list)
  "For each glob in list, return all matching files"
  (let ((filelist nil))
    (dolist (mask glob-list)
      (dolist (file (directory mask))
        (pushnew file filelist :test 'equal)))
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


(defun present-text-filtering-results (results &key refresh-command title)
  "Gets results from, say, filter-one-file and present them to tcl"
  (declare (ignore refresh-command)) ; reserved
  (let* ((grbr (eval-in-tcl (format nil "::grbr::OpenGrepBrowser") :nowait nil))
         (serial 0)
         (print-progress-when 100))
    (when title ; it will be used as refresh-command later... maybe... FIXME
      (eval-in-tcl (format nil "::grbr::FillHeaderText ~A ~A"
                           grbr
                           (cl-tk:tcl-escape title))))
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
   :title
   (format nil "Find in Tcl Sources: ~A" string)))

(defun find-string-in-files (string file-list &key regexp case-sensitive secondary-string)
  "For each string in file-list, calls directory. Then searches string in all of the files. See also clco:files-by-glob-list. Example is in the source"
; (clco:FIND-STRING-IN-FILES "f4" (clco:FILES-BY-GLOB-LIST "c:/clcon/lp/**/*.lisp"))
  (when
      (or regexp case-sensitive secondary-string)
    (error "keyargs are not implemented"))
  (clco::present-text-filtering-results
   (clco::filter-many-files file-list string)
   :title
   (format nil "Find in ~A files: ~A" (length file-list) string))
  )

(defun find-current-file-declarations (infile)
  "Very basic definition navigation facility. For lisp, returns list of toplevel forms. For tcl, returns toplevel procs and procs packed in the namespace"
  (clco::present-text-filtering-results
   (let* ((file-type (pathname-type infile))
          (language (file-type-to-language file-type))
          (filter
           (ecase language
             (:lisp
              #'lisp-definition-filter)
             (:tcl
              #'tcl-definition-filter))))
     (filter-one-file-or-err infile infile nil filter)
     )
   :title
   (format nil "Declarations of ~%~A" infile)
   ))
