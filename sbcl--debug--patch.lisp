; -*- coding: utf-8 ; Encoding: utf-8 ; system :clcon-server -*- 
;;;; Патч отладчика SBCL. Связан с c:/yar/ит/my-full-eval/otladchik-sbcl.lisp

(in-package :sb-debug)

; disable stepping
(declaim (optimize (debug 3) (compilation-speed 3) (safety 3)))

(sb-ext:without-package-locks

 (defparameter *ПРЕОБРАЗОВЫВАТЬ-КАДРЫ-СТЕКА-ДЛЯ--MY-EVAL* t)

 (defun ew-symbol (name)
   (and (find-package :ew)
        (find-symbol name :ew)))

 ;;; Prints a representation of the function call causing FRAME to
 ;;; exist. VERBOSITY indicates the level of information to output;
 ;;; zero indicates just printing the DEBUG-FUN's name, and one
 ;;; indicates displaying call-like, one-liner format with argument
 ;;; values.
 (defun print-frame-call (frame stream
                                &key print-frame-source
                                number
                                (method-frame-style *method-frame-style*)
                                (emergency-best-effort (> *debug-command-level* 1)))
   (when number
     (format stream "~&~S: " (if (integerp number)
                                 number
                                 (sb-di:frame-number frame))))
   (multiple-value-bind (name args info)
                        (frame-call frame :method-frame-style method-frame-style)


     (when *ПРЕОБРАЗОВЫВАТЬ-КАДРЫ-СТЕКА-ДЛЯ--MY-EVAL*
       (let ((fn (ew-symbol "CONVERT-SBCL-DEBUGGER-FRAME-FOR-EW")))
         (when fn
           (multiple-value-setq (name args)
             (funcall fn frame name args)))))
     (pprint-logical-block (stream nil :prefix "(" :suffix ")")
                           (let ((*print-pretty* nil)
                                 (*print-circle* t))
                             ;; Since we go to some trouble to make nice informative
                             ;; function names like (PRINT-OBJECT :AROUND (CLOWN T)), let's
                             ;; make sure that they aren't truncated by *PRINT-LENGTH* and
                             ;; *PRINT-LEVEL*.
                             (let ((*print-length* nil)
                                   (*print-level* nil)
                                   (name (if emergency-best-effort
                                             (ensure-printable-object name)
                                             name)))
                               (write name :stream stream :escape t :pretty (equal '(lambda ()) name)))
                             
                             ;; For the function arguments, we can just print normally.  If
                             ;; we hit a &REST arg, then print as many of the values as
                             ;; possible, punting the loop over lambda-list variables since
                             ;; any other arguments will be in the &REST arg's list of
                             ;; values.
                             (let ((args (if emergency-best-effort
                                             (ensure-printable-object args)
                                             args)))
                               (if (listp args)
                                   (format stream "~{ ~_~S~}" args)
                                   (format stream " ~S" args)))))
     (when info
       (format stream " [~{~(~A~)~^,~}]" info)))
   (when print-frame-source
     (let ((loc (sb-di:frame-code-location frame)))
       (handler-case
           (let ((source (handler-case
                             (code-location-source-form loc 0)
                           (error (c)
                                  (format stream "~&   error finding frame source: ~A" c)))))
             (format stream "~%   source: ~S" source))
         (sb-di:debug-condition ()
                                ;; This is mostly noise.
                                (when (eq :always print-frame-source)
                                  (format stream "~&   no source available for frame")))
         (error (c)
                (format stream "~&   error printing frame source: ~A" c))))))
 


 (defun my-step-form (original-fn form args)
   (declare (ignore original-fn))
  (restart-case
      (signal 'step-form-condition
              :form form
              :args args)
    (step-continue ()
      :report "Беги"
      (sb-impl::disable-stepping)
      (setf sb-impl::*step-out* nil))
    (step-out ()
      :report "Выбегай к месту, где мы сказали шагнуть внутрь и шагай дальше"
      (ecase sb-impl::*step-out*
        ((nil)
         (cerror "Перешагни" "Нельзя сделать STEP-OUT, т.к. не было сделано STEP-IN")
         nil)
        ((t :maybe)
         (sb-impl::disable-stepping)
         (setf sb-impl::*step-out* t)))
      nil)
    (step-next ()
      :report "Перешагни"
      nil)
    (step-into ()
      :report "Шагни внутрь"
      t)))

  (cl-advice:define-advice sb-impl::step-form #'my-step-form)
 
 ) ; sb-ext:without-package-locks

