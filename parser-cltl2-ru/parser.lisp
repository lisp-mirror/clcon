(in-package :parser-cltl2-ru)

(defstruct entry expression types)

(defstruct entry-line name links)

(defun parse (parsed-html)
  (let ((body (third (second parsed-html)))
        current
        result)
    (dolist (line body)
      (cond
       ((not (listp line)) t)
       ((eq (second (car (third line))) :id) 
        (when current (push current result))
        (setf current 
              (make-entry :expression 
                          (string-trim '(#\Space #\Tab #\NO-BREAK_SPACE) 
                                       (make-name (second line) 
                                                  (fourth line))))))
       ((eq (second (car (third line))) :href)
        (when current
          (push (make-entry-line :name (string-trim '(#\Space #\Tab #\NO-BREAK_SPACE #\,) 
                                                    (second line))
                                 :links (get-links (cddr line)))
                (entry-types current))))))
    (nreverse result)))
      
(defun make-name (part1 part2)
  (if (stringp part2)
      (concatenate 'string part1 part2)
      part1))

(defun get-links (line)
  (mapcar 
   (lambda (x) (third (car x)))
   (remove-if-not (lambda (x) (and (listp x)
                                   (listp (car x))
                                   (eq (second (car x)) :href)))
                  line)))
           