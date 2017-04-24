;; -*- coding : utf-8 ; Encoding : utf-8 ; system :clcon-server ; -*-
;; Infrastructe for oduvanchik-clcon interaction. Patches to oduvanchik
;; which are more convenient to keep here to avoid rebuilding of oduvanchik
;; См. также ../../src/yar/clcon--режим-яр.lisp - там ссылки на остальные связанные файлы. 

(in-package :oduvanchik)
(named-readtables::in-readtable :oduvanchik-ext-readtable)

(defun oduvan-invisible-kill-open-paren-font-marks ()
  "Rework of kill-open-paren-font-marks"
  (when *open-paren-font-marks*
    (let* ((start (region-start *open-paren-font-marks*))
           (end (region-end *open-paren-font-marks*))
           (lines (mapcar #'mark-line (list start end)))
           (economy-lines (remove nil (remove-duplicates lines))))
      (delete-font-mark start)
      (delete-font-mark end)
      (setf *open-paren-font-marks* nil)
      (when (every 'oi::line-tag-valid-p economy-lines)
        (dolist (line economy-lines)
          (oi::maybe-send-line-highlight-to-clcon line))))))


(defun maybe-send-package-to-tcl (clcon_text)
  "Send package at current-point if it is known. We assume and check that clcon_text corresponds to current buffer. Returns t if package was sent. See also maybe-send-readtable-to-tcl"
  (let* ((p (current-point))
         (line (mark-line p))
         (buffer (line-buffer line))
         (package (odu::package-at-point-if-known))
         (last-package (oi::buffer-last-package-name-sent-to-tcl buffer))
         )
    (assert (eq buffer (oi::clcon_text-to-buffer clcon_text)))
    (cond
     ((equalp package last-package)
      ; do nothing
      nil
      )
     (t
      (send-package-to-clcon clcon_text package buffer)
      (setf (oi::buffer-last-package-name-sent-to-tcl buffer) package)
      t
      ))))

(defun maybe-send-readtable-to-tcl (clcon_text)
  "Similar to maybe-send-package-to-tcl"
  (let* ((p (current-point))
         (line (mark-line p))
         (buffer (line-buffer line))
         (rt (odu::readtable-at-point-if-known))
         (last-rt (oi::buffer-last-readtable-name-sent-to-tcl buffer))
         )
    (assert (eq buffer (oi::clcon_text-to-buffer clcon_text)))
    (cond
     ((equalp rt last-rt)
      ; do nothing
      nil
      )
     (t
      (send-readtable-to-clcon clcon_text rt buffer)
      (setf (oi::buffer-last-readtable-name-sent-to-tcl buffer) rt)
      t
      ))))

(defun maybe-send-mode-to-tcl (clcon_text)
  "Send mode at current-point if it is known. We assume and do not check that clcon_text corresponds to current buffer. Returns t if package was sent. See also maybe-send-readtable-to-tcl"
  (let* ((p (current-point))
         (line (mark-line p))
         (buffer (line-buffer line))
         (mode (first (oi::buffer-modes buffer)))
         (last-mode (oi::buffer-last-mode-name-sent-to-tcl buffer))
         )
    (cond
     ((equalp mode last-mode)
      ; do nothing
      nil
      )
     (t
      (send-mode-to-clcon clcon_text mode buffer)
      (setf (oi::buffer-last-mode-name-sent-to-tcl buffer) mode)
      t
      ))))


(defun oduvan-invisible-maybe-highlight-open-parens ()
  "Rework of odu::maybe-highlight-open-parens. Works in the current buffer. 
 It looks like working with parens is independent of buffer highlighting. So we don't
 bother ourselves with redrawing"
  (check-something-ok)
  (when (value highlight-open-parens)
    (multiple-value-bind (start end)
        (funcall (value open-paren-finder-function)
                 (current-point))
      (cond
        ((and start end)
         (oduvan-invisible-kill-open-paren-font-marks)
         (set-open-paren-font-marks start end)
         (let* ((lines (mapcar #'mark-line (list start end)))
                (economy-lines (remove nil (remove-duplicates lines))))
           (when (every 'oi::line-tag-valid-p economy-lines)
             (dolist (line economy-lines)
;               (format t "~a~%" (oi::line-number line))
               (oi::maybe-send-line-highlight-to-clcon line)))))
        (t
         (oduvan-invisible-kill-open-paren-font-marks)
         )))))

(defun augment-with-parens (marks line)
  (if (and odu::*open-paren-font-marks* (eq (mark-line (region-start odu::*open-paren-font-marks*)) line))
      (oi::smart-insert-font-mark 
       (region-end odu::*open-paren-font-marks*) 
       (oi::smart-insert-font-mark (region-start odu::*open-paren-font-marks*)
                                   marks))
      (copy-list marks)))

(defun НАЙТИ-ПОСЛЕДНИЙ-ШРИФТ-В-СТРОКЕ (line default)
  "Если не найдено, ищет в предыдущих строках. Если и там не найдено, возвращает умолчание. См. примечание к НАЙТИ-ШРИФТ-ДЛЯ-НАЧАЛА-СТРОКИ"
  (loop 
    (let* ((tag (line-tag-no-recalc line))
           (syntax-info (oi::tag-syntax-info tag))
           (copy-marks (copy-list (oi::sy-font-marks syntax-info)))
           (sorted-marks (sort copy-marks '< :key 'oi::mark-charpos))
           (ВОЗМОЖНО-ПОСЛЕДНЯЯ-МАРКА (car (last sorted-marks)))
           (ПРЕД-СТРОКА (line-previous line)))
      (cond
       (ВОЗМОЖНО-ПОСЛЕДНЯЯ-МАРКА
        (return (oi::font-mark-font ВОЗМОЖНО-ПОСЛЕДНЯЯ-МАРКА)))
       (ПРЕД-СТРОКА (setf line ПРЕД-СТРОКА))
       (t (return default))))))

(defun НАЙТИ-ШРИФТ-ДЛЯ-НАЧАЛА-СТРОКИ (line)
  "Если шрифт не указан в начале строки, ищем его в предыдущих строках. 
Это может понадобиться только при раскраске лексером Яра многострочных структур.
После завершения раскраски отсутствие марки в начале строки означает, что начало строки имеет шрифт 0"
  (let* ((tag (line-tag-no-recalc line))
         (default 0)
         (syntax-info (oi::tag-syntax-info tag))
         (copy-marks (copy-list (oi::sy-font-marks syntax-info)))
         (sorted-marks (sort copy-marks '< :key 'oi::mark-charpos))
         (ВОЗМОЖНО-ПЕРВАЯ-МАРКА (first sorted-marks))
         (ПРЕД-СТРОКА (line-previous line))
         (ЕСТЬ-МАРКА-В-НАЧАЛЕ-СТРОКИ
          (and ВОЗМОЖНО-ПЕРВАЯ-МАРКА (= 0 (oi:mark-charpos ВОЗМОЖНО-ПЕРВАЯ-МАРКА)))))
    (cond
     (ЕСТЬ-МАРКА-В-НАЧАЛЕ-СТРОКИ
      (oi::font-mark-font ВОЗМОЖНО-ПЕРВАЯ-МАРКА))
     (ПРЕД-СТРОКА
      (НАЙТИ-ПОСЛЕДНИЙ-ШРИФТ-В-СТРОКЕ ПРЕД-СТРОКА default))
     (t
      default))))

(defun line-sy-font-marks (line)
  (oi::sy-font-marks (oi::tag-syntax-info (line-tag-no-recalc line))))  

(defun ПЕРЕНЕСТИ-ШРИФТ-С-ПРОШЛЫХ-СТРОК (line new-mark-pos)
  "new-mark-pos - это позиция в строке марки, к-рую мы собираемся вставить в эту строку после возврата из данной ф-ии. 
В Одуванчике принято, что строка начинается с нулевого шрифта. Если она должна начинаться с другого шрифта, то должна быть вставлена марка. А лексер Яра ничего об этом не знает и просто вызывает smart-insert-font-mark! для каждого начала лексемы. Данная функция смотрит на line и все предыдущие строки, у которых пока нет марок, ищет последнюю лексему (где-то в прошлом) и, если её шрифт - не 0, вставляет во всех предшествующих строках соответствующий шрифт. Если new-mark-pos <> 0, то вставляет и в эту строчку. "
  (when (line-sy-font-marks line)
    ;; если в текущей строке уже есть марки, значит мы считаем, что всё нужное уже перенесено с прошлых строк
    ;; логика здесь состоит в том, что мы красим файл сверху вниз
    (return-from ПЕРЕНЕСТИ-ШРИФТ-С-ПРОШЛЫХ-СТРОК nil))
  (let ((ШРИФТ-ПО-УМОЛЧАНИЮ 0)
        ;; для начала найдём самую раннюю строчку, к-рую надлежит менять
        (САМАЯ-РАННЯЯ-СТРОКА-БЕЗ-МАРОК 
         (let* ((cur line)
                ; следующая по файлу, но предыдущая по итерациям
                (following nil))
           (loop
             (unless cur
               ;; дошли до начала и нигде ничего нет. Однако поставим здесь 0
               (return line))
             (when (line-sy-font-marks cur)
               (return following))
             (setf following cur)
             (setf cur (line-previous cur))))))
    ;; если ничего не надо менять, то выходим
    (unless САМАЯ-РАННЯЯ-СТРОКА-БЕЗ-МАРОК
      (return-from ПЕРЕНЕСТИ-ШРИФТ-С-ПРОШЛЫХ-СТРОК nil))
    ;; определили шрифт
    (let* ((ШРИФТ (НАЙТИ-ШРИФТ-ДЛЯ-НАЧАЛА-СТРОКИ САМАЯ-РАННЯЯ-СТРОКА-БЕЗ-МАРОК)))
      (let ((cur САМАЯ-РАННЯЯ-СТРОКА-БЕЗ-МАРОК))
        ;; теперь идём по строчкам вперёд и вписываем этот шрифт.
        ;; Если у нас будет большой кусок
        ;; без марок, то мы будем всё время на нём тормозить, 
        ;; поэтому вписываем его безусловно (даже если шрифт по умолчанию).
        (loop
          (cond
           ((null cur)
            (error "Куда-то пропала строчка..."))
           ((line-sy-font-marks cur)
            (error "Ошибка в алгоритме раскраски"))
           ((or
             (and (eq line cur)
                  (not (= new-mark-pos 0)))
             (not (eq line cur)))              
            (oi::smart-insert-font-mark!
             (oi::font-mark cur 0 ШРИФТ) cur
             :send-highlight nil)))
          ;; Проверяем условие возврата и переходим к следующей итерации
          (when (eq cur line)
            (return))
          (setf cur (line-next cur)))))))

(defun line-effective-marks (line)
  "Список, состоящий из шрифта в начале строки (он может быть унаследован с прошлой строки), Marks of the line plus marks of odu::*open-paren-font-marks*.Первый элемент списка - всегда число (шрифт), а остальные - марки!"
  (oi::check-something-ok)
  ;; Закомментаренный вариант интересен тем, что он показывает способ сложения слоёв раскраски. Правда, способ кривой :) 
  #|(let* ((tag (line-tag-no-recalc line))
         (syntax-info (oi::tag-syntax-info tag))
         (marks (oi::sy-font-marks syntax-info))
         (sorted-marks (sort (augment-with-parens marks line) '< :key 'oi::mark-charpos)))
    (oi::check-something-ok)
    sorted-marks
    )|#

  ;; Новый алгоритм не лезет в синтаксис, потому что эти марки доступны и через простой oi::line-marks.
  ;; Однако оба алгоритма некорректны: в строке ("В") , если встать за скобкой, оказывается две марки в одном месте, 
  ;; поэтому строка "В" обезцвечивается (хотя это и трудно заметить из-за её цвета). Более видимо в случае (;ff\n)
  ;; Т.е. в данном алгоритме корректное сложение слоёв отсутствует. Теперь надо поставить CMU CL и посмотреть, как там дела обстоят
  (let* ((marks (oi::line-marks line))
         (filtered-marks
          (let ((res nil))
            (dolist (m marks)
              (when (and (oi::font-mark-p m) (not (oi::mark-deleted-p m)))
                (push m res)))
            res))
         (sorted-marks (sort filtered-marks '< :key 'oi::mark-charpos)))
    sorted-marks))

(defun encode-marks-for-line (line stream)
  "{linenumber {charpos0 font0} {charpos1 font1} ...} 
  If we know line-number, we can pass it. line-tag must be ok."
  (oi::check-something-ok)
  (let* ((tag (line-tag-no-recalc line))
       ;(syntax-info (oi::tag-syntax-info tag))
       (exact-line-number (oi::tag-line-number tag))
       (sorted-marks (line-effective-marks line)))
    (format stream "{~D " exact-line-number)
    (dolist (m sorted-marks)
      (typecase m
        (oi:font-mark
         (format stream "{~D ~D} " (oi:mark-charpos m) (oi::font-mark-font m)))))
    (format stream "} ")
    (oi::check-something-ok)
    ))

(defun numbered-line-of-buffer (buffer number)
  "Number starts from 1. See also oi::mark-row-and-col"
  (let* ((first-line 
          (slot-value
           (slot-value
            (slot-value
             buffer
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

(defun numbered-line-of-buffer-by-clcon (clcon_text number)
  "Number starts from 1. See also oi::mark-row-and-col"
  (numbered-line-of-buffer (oi::clcon_text-to-buffer clcon_text) number))
       
    

;;(defun dump-all-marks (clcon_text stream-designator)
;;  (do ((s (odu::numbered-line-of-buffer-by-clcon clcon_text 1)
;;          (oi::line-next s)
;;          ))
;;      ((null s) t)
;;    (encode-marks-for-line s stream-designator)
;;    ))


(defun buffer-change-id (buffer)
    "We believe that: 
    oi::now-tick always grows
    oi::buffer-modified-tick contains some value of (oi::tick) 
    "
    (oi::buffer-modified-tick buffer))


(defmethod oi::maybe-send-line-highlight-to-clcon :around (line)
  "If we have highlight enabled in features, send it to clcon"
  #-oduvan-enable-highlight
  (declare (ignore line))
  #+oduvan-enable-highlight
  (cond
    ((null line))
    (t 
     (let* ((buffer (line-buffer line))
            (tag (oi:line-tag-no-recalc line))
            (line-number (oi::tag-line-number tag))
            (sy (oi::tag-syntax-info tag)))
       (assert sy)
       (when (bufferp buffer)
         (let* ((clcon_text (oi::buffer-to-clcon_text buffer))
                (connection (and clcon_text
                                 ;; if not, this is not clcon_text backend buffer
                                 (oduvanchik-interface:variable-value
                                  'odu::swank-connection ;"Swank Connection"
                                  :buffer buffer))
                  ))
           (when (and clcon_text connection)
             (let* ((encoded-marks
                     (with-output-to-string (ou)
                       (encode-marks-for-line line ou)))
                    (change-id (buffer-change-id buffer)))
               (clco::notify-highlight-single-line 
                clcon_text encoded-marks line-number change-id buffer)
               )
             )))))))

(defun send-package-to-clcon (clcon_text package buffer)
  "Posts event of package change to a highlight events queue. See also send-readtable-to-clcon"
  #-oduvan-enable-highlight
  (declare (ignore clcon_text package buffer))
  #+oduvan-enable-highlight
  (clco::notify-package-change clcon_text package buffer)
  )


(defun send-readtable-to-clcon (clcon_text rt buffer)
  "Similar to send-package-to-clcon"
  #-oduvan-enable-highlight
  (declare (ignore clcon_text rt buffer))
  #+oduvan-enable-highlight
  (clco::notify-readtable-change clcon_text rt buffer)
  )


(defun send-mode-to-clcon (clcon_text mode buffer)
  "Clone of send-package-to-clcon"
  #-oduvan-enable-highlight
  (declare (ignore clcon_text mode buffer))
  #+oduvan-enable-highlight
  (clco::notify-mode-change clcon_text mode buffer)
  )


(defmethod oi::recompute-tags-up-to (end-line (background (eql t)))
  "В этом методе (в отличие от метода с background nil), we always recompute everything to the end of file. end-line is required to know buffer only. Вызывающая сторона должна была сбросить номер волны раскраски. Родственная функция - oduvanchik::start-background-repaint-after-recomputing-entire-buffer"
  (assert-we-are-in-oduvanchik-thread)
  (assert (sb-thread:holding-mutex-p *invoke-modifying-buffer-lock*))
  (oi::check-something-ok)
  (let* ((buffer (line-buffer end-line))
         (dummy1 (assert buffer))
         (buffer-end (oi::buffer-end-mark buffer))
         (dummy2 (assert buffer-end))
         ;(real-end-line (mark-line buffer-end))
         ;(level (oi::buffer-tag-line-number buffer))
         (start-line 
          (ecase (oi::syntax-highlight-mode buffer)
            (:send-highlight-after-recomputing-entire-buffer
             (oi::first-line-of-buffer buffer))
            (:send-highlight-from-recompute-line-tag
             (oi::НАЙТИ-ПЕРВУЮ-СТРОЧКУ-С-УСТАРЕВШИМ-ТЕГОМ end-line)))))
    (declare (ignore dummy1 dummy2))
    (assert start-line () "Упустили случай пустого буфера? Странно, а где тогда живёт end-line?")
    (let ((new-highlight-wave-id (reset-background-highlight-process buffer)))
      (oi::check-something-ok)
      (clco::order-call-oduvanchik-from-itself
       (list 'recompute-line-tags-starting-from-line-background
             buffer start-line new-highlight-wave-id))
      )))

(defun start-background-repaint-after-recomputing-entire-buffer (buffer)
  (assert-we-are-in-oduvanchik-thread)
  (assert (sb-thread:holding-mutex-p *invoke-modifying-buffer-lock*))
  (oi::check-something-ok)
  (let* ((start-line (oi::first-line-of-buffer buffer))
         (new-highlight-wave-id (reset-background-highlight-process buffer)))
    (when start-line
      (clco::order-call-oduvanchik-from-itself
       (list 'background-repaint-after-recomputing-entire-buffer
             buffer start-line new-highlight-wave-id))
      )))

      

(defun background-repaint-after-recomputing-entire-buffer (buffer start-line highlight-wave-id)
  "Мы перекрасили буфер целиком. Запустим процесс фоновой раскраски. Примерная копия с recompute-line-tags-starting-from-line-background "
  (assert-we-are-in-oduvanchik-thread)
  (cond
    ((not (= highlight-wave-id (oi::buffer-highlight-wave-id buffer)))
     ;; We're obsolete. Die.
     (return-from background-repaint-after-recomputing-entire-buffer nil))
    (t 
     (oi::check-something-ok)
     (oi::maybe-send-line-highlight-to-clcon start-line)
     (let ((next (oi::line-next start-line)))
      (when next
        (clco::order-call-oduvanchik-from-itself
          (list 'background-repaint-after-recomputing-entire-buffer
              buffer next highlight-wave-id)))))))

(defun recompute-line-tags-starting-from-line-background (buffer start-line highlight-wave-id)
  "Создаёт подобие фоновой задачи для раскраски строк до конца файла. Фоновость имитируется через цепочку событий, каждое из которых кладёт событие-продолжение. Смысл состоит в том, чтобы, не отвлекаясь от другой работы, не спеша вычислить, а главное, отправить в tcl раскраску всех строк до конца файла. Родственная функция - background-repaint-after-recomputing-entire-buffer"
  (assert-we-are-in-oduvanchik-thread)
  (let ((check-the-buffer (line-buffer start-line)))
    (cond
      ((not (= highlight-wave-id (oi::buffer-highlight-wave-id buffer)))
       ;; We're obsolete. Die.
       (return-from recompute-line-tags-starting-from-line-background nil))
      ((not (equal buffer check-the-buffer))
       (error "Again we are trying to paint deleted line..."))
      (t 
       (oi::check-something-ok)
       (when start-line
         (let ((prev (line-previous start-line)))
           (when prev
             (assert (oi::line-tag-valid-p prev) () "Ожидалось, что предыдущая строка уже раскрашена")))
         ;; line-tag вызывается ради попбочного эффекта. Поскольку мы уже утвердили, что предыдущая строчка раскрашена (или мы стоим на первой строчке), это не приведёт к большим затратам времени
         (line-tag start-line)
         (when (eq (oi::syntax-highlight-mode buffer) :send-highlight-after-recomputing-entire-buffer)
           ;; В этом режиме мы отключили отправку раскраски непосредственно из тега, т.к. при этом забилась бы очередь отправляемых раскрасок. 
           ;; Поэтому нам приходится отправлять раскраску явно. 
           (oi::maybe-send-line-highlight-to-clcon start-line))
         (let ((next (oi::line-next start-line)))
           (when next
             (clco::order-call-oduvanchik-from-itself
              (list 'recompute-line-tags-starting-from-line-background
                    buffer next highlight-wave-id)))))))))

(defun cl-boolean-to-tcl (x)
  "Returns 0 or 1 in numeric form"
  (if x "1" "0")
  )

(defun clcon-prompt-for-y-or-n (&rest args
                                &key ((:must-exist must-exist) t)
                                  (default nil defaultp)
                                  default-string
                                  ((:prompt prompt) "Y or N? ")
                                  ((:help *parse-help*) "Type Y or N."))
  "For oi::*clcon-prompt-for-y-or-n-hook*. See also oi::prompt-for-y-or-n"
  (declare (ignore args default defaultp default-string))
  (let* ((q-must-exist (cl-boolean-to-tcl must-exist))
         ;; default is ignored now. It is a boolean, if true,
         ;; default value is true, otherwise it is false
         (q-prompt (cl-tk:tcl-escape prompt))
         (q-parse-help (cl-tk:tcl-escape *parse-help*))
         (cmd (format nil "::odu::prompt_for_y_or_n ~A ~A ~A" q-must-exist q-prompt q-parse-help))
         (tcl-result (clco:eval-in-tcl cmd :nowait nil)))
    (cond
      ((string= tcl-result "yes")
       #k"y"
       )
      ((string= tcl-result "no")
       #k"n"
       )
      ((string= tcl-result "cancel")
       (editor-error)
       ))))

(setf oi::*clcon-prompt-for-y-or-n-hook* 'clcon-prompt-for-y-or-n)


; (slot-value (slot-value (slot-value (slot-value (oi::clcon_text-to-buffer ".__edit1.text") 'oi::%region) 'oi::start) 'oi::line) 'oi::marks)

; (clco:eval-in-tcl (with-output-to-string (ou) (format ou "::edt::ApplyHighlightToLine .__edit1.text ") (odu::encode-marks-for-line (odu::numbered-line-of-buffer-by-clcon ".__edit1.text" 3) ou)))

(defun check-display-start-mark-ok (window)
  (let* ((mark (window-display-start window)))
    (oi::check-mark-is-in-its-line mark)))


(defun check-display-start-ok ()
  (dolist (b oi::*buffer-list*)
    (dolist (w (buffer-windows b))
      (check-display-start-mark-ok w))))


(defun count-marks-in-current-buffer ()
  "{linenumber {charpos0 font0} {charpos1 font1} ...} 
  If we know line-number, we can pass it"
  (let* ((b (current-buffer))
         (m (buffer-start-mark b))
         (l (mark-line m))
         (count 0))
    (do ((ll l (line-next ll)))
        ((null ll) count)
      (incf count (length (oi::line-marks ll))))))

(defun check-all-lines-are-disjoint ()
  (let* ((b (current-buffer))
         (all-line-chars nil)
         (m (buffer-start-mark b))
         (l (mark-line m))
         )
    (do ((ll l (line-next ll)))
        ((null ll)
         nil
         )
      (push (oi::line-chars ll) all-line-chars))
    (unless
        (= (length all-line-chars)
           (length (remove-duplicates all-line-chars)))
      (print "Some line-chars coincide"))))


(defun mark-buffer (mark) (odu::line-buffer (odu::mark-line mark)))


