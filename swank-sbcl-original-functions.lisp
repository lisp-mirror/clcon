; -*- coding: utf-8 ; Encoding: utf-8 ; system :clcon-server -*- 
(in-package :swank/sbcl)
(named-readtables:in-readtable nil)
;; there are functions we patch. Their copies with swank/sbcl-original- prefix are given here
;; we could also use define-advice


(defun swank/sbcl-original-definition-source-file-location (definition-source)
  (with-definition-source (pathname form-path character-offset plist
                           file-write-date) definition-source
    (let* ((namestring (namestring (translate-logical-pathname pathname)))
           (pos (or (and form-path
                         (or
                          #+#.(swank/backend:with-symbol 'definition-source-form-number 'sb-introspect)
                          (and (sb-introspect:definition-source-form-number definition-source)
                               (ignore-errors (file-form-number-position definition-source)))
                          (ignore-errors
                           (source-file-position namestring file-write-date
                                                 form-path))))
                    character-offset))
           (snippet (source-hint-snippet namestring file-write-date pos)))
      (make-location `(:file ,namestring)
                     ;; /file positions/ in Common Lisp start from
                     ;; 0, buffer positions in Emacs start from 1.
                     `(:position ,(1+ pos))
                     `(:snippet ,snippet)))))

(defun swank/sbcl-original-nth-frame (index)
  (do ((frame *sldb-stack-top* (sb-di:frame-down frame))
       (i index (1- i)))
      ((zerop i) frame)))
