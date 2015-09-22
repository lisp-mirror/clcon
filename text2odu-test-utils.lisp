;; -*- coding : utf-8 ; Encoding : utf-8 ; system :clcon-server ; -*-

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

(defun entire-clcon_text-string (clcon_text)
  (let ((cmd
         (format nil "~A get 0.0 end" clcon_text)))
    (eval-in-tcl cmd :nowait nil)))

(defun winmerge-strings-fn ()
  "If we have winmerge-string function, return its symbol"
  (let ((p (find-package '#:budden0)))
    (when p
      (read-from-string "budden0:winmerge-strings"))))

(defun compare-clcon_text-and-oduvanchik-buffer-contents (clcon_text)
  (let*
      ((b (oi::clcon_text-to-buffer clcon_text))
       (odu-str (entire-buffer-string b))
       (clcon_text-str (entire-clcon_text-string clcon_text)))
    (unless (string= odu-str clcon_text-str)
      (let ((winmerge-strings-fn (winmerge-strings-fn)))
        (cond
          (winmerge-strings-fn
           (funcall winmerge-strings-fn clcon_text-str odu-str
                    :lfile "~/.clcon_text-str"
                    :rfile "~/.odu-str"))
          (t
           (cerror "oduvanchik vs clcon_text buffer contents mismatch" "Continue")))))))
            
