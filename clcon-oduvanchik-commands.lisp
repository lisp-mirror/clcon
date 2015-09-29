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
                 (oi::tag-line-number (oi::line-tag line)))))
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

(defun first-line-of-buffer-by-clcon (clcon_text)
  (slot-value
   (slot-value
    (slot-value
     (oi::clcon_text-to-buffer clcon_text)
     'oi::%region)
    'oi::start)
   'oi::line))

(defun dump-all-marks (clcon_text stream-designator)
  (do ((s (odu::first-line-of-buffer-by-clcon clcon_text)
          (oi::line-next s)
          ))
      ((null s) t)
    (encode-marks-for-line s stream-designator)
    ))
    





; (slot-value (slot-value (slot-value (slot-value (oi::clcon_text-to-buffer ".__edit1.text") 'oi::%region) 'oi::start) 'oi::line) 'oi::marks)
