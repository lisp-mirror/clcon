;; -*- coding : utf-8 ; Encoding : utf-8 ; system :clcon-server ; -*-
;; Our lisp mode and other application-level oduvanchik commands for clcon. 
(in-package :oduvanchik)

;; Прежде всего, надо подумать, как лучше организовать работу.
;; Вообще и для поиска определений в частнотси.

(oduvanchik::defcommand "Find Source" (p)
    ""
    ""
  (let* ((s (symbol-string-at-point))
         (code (clco::server-lookup-definition s)))
    code))

(defun encode-marks-for-line (line stream &key line-number)
  "{linenumber {charpos0 font0} {charpos1 font1} ...} 
  If we know line-number, we can pass it"
  (let* ((marks (oi::line-marks line))
         (any-mark (first marks))
         (exact-line-number line-number)
         (have-something nil)
         )
    (unless any-mark
      (return-from encode-marks-for-line nil))
    (unless exact-line-number
      (setf exact-line-number
            (+ (nth 1 (oi::mark-position any-mark)) 1))
      (assert (= exact-line-number
                 (oi::tag-line-number (oi::%line-tag line)))))
    (dolist (m (sort marks '< :key 'oi::mark-charpos))
      (typecase m
        (oi:font-mark
         (cond
           (have-something)
           (t
            (setf have-something t)
            (format stream "{~D " exact-line-number)
            ))            
         (format stream "{~D ~D} " (oi:mark-charpos m) (oi::font-mark-font m)))))
    (when have-something 
      (format stream "} "))
    ))

(defun numbered-line-of-buffer-by-clcon (clcon_text number)
  "Number starts from 1"
  (let* ((first-line 
          (slot-value
           (slot-value
            (slot-value
             (oi::clcon_text-to-buffer clcon_text)
             'oi::%region)
            'oi::start)
           'oi::line))
         (line first-line)
         (i 1))
    (assert (> number 0))
    (loop
       (when (= number i)
         (return line))
       (setf line (oi::line-next line))
       (incf i))))
       
    

(defun dump-all-marks (clcon_text stream-designator)
  (do ((s (odu::numbered-line-of-buffer-by-clcon clcon_text 1)
          (oi::line-next s)
          ))
      ((null s) t)
    (encode-marks-for-line s stream-designator)
    ))


(defmethod oi::recompute-syntax-marks :around (line tag)
  (let* ((result (call-next-method))
         (marks (oi::sy-font-marks result))
         (buffer (line-buffer line)))
    (declare (ignorable marks buffer))
    #+oduvan-enable-highlight
    (when (bufferp buffer)
      (let* ((clcon_text (oi::buffer-to-clcon_text buffer))
             (connection (oduvanchik-interface:variable-value
                          'odu::swank-connection ;"Swank Connection"
                          :current buffer))
             )
        (when (and clcon_text connection marks)
          (let* ((cmd (with-output-to-string (ou)
                        (format ou "::edt::ApplyHighlightToLine ~A " clcon_text)
                        (encode-marks-for-line line ou)
                        )))
            ;(format *trace-output* "~S" cmd)
            (swank::with-connection (connection)
              ;; we should carefully synchronize them indeed
              (clco:eval-in-tcl cmd :nowait nil))))))
    result))






; (slot-value (slot-value (slot-value (slot-value (oi::clcon_text-to-buffer ".__edit1.text") 'oi::%region) 'oi::start) 'oi::line) 'oi::marks)

; (clco:eval-in-tcl (with-output-to-string (ou) (format ou "::edt::ApplyHighlightToLine .__edit1.text ") (odu::encode-marks-for-line (odu::numbered-line-of-buffer-by-clcon ".__edit1.text" 3) ou)))
