(setq debug-on-error t)

(require 'cl)

(defvar *code* nil)

(defun nargs ()
  (if (eq *args* '\?)
      '\?
      (length *args*)))

(defun get-arg ()
  (let ((arg (pop *args*)))
    (prog1 (if *minusp*
             (if (integerp arg)
                 (- arg)
                 `(- ,arg))
             arg)
      (setq *minusp* nil))))

(defun set-arg (x)
  (ecase (nargs)
    ((\? 0) (setq *args* (list x)))
    (1      (setq *args* (list x)))
    (2      (setf (first *args*) x))))

(defun emit (code)
  (push code *code*))

(defun next-char ()
  (prog1 (following-char)
    (forward-char)))

(defun parse-qreg ()
  (format "%c" (upcase (next-char))))

(defun parse-string ()
  (let ((start (point))
        (end (search-forward "\033")))
    (buffer-substring start (1- end))))

(defmacro qreg (name)
  `(get ,name 'teco-qreg))

(defun oops ()
  (message "Not implemented: %c (%o)" *current-char* *current-char*))

(defvar *dispatch-table* (make-vector 128 'oops))

(defmacro* define-command (name character lambda-list &body body)
  (let ((fn (put name 'teco-function `(lambda ,lambda-list ,@body))))
    (setf (aref *dispatch-table* character) fn)
    (setf (aref *dispatch-table* (upcase character)) fn)))

(define-command carriage-return ?\^M ()
  nil)
  
(define-command tab ?\^I ()
  (emit '(insert ?\^I)))
  
(define-command linefeed ?\^J ()
  nil)
  
(define-command increment ?% ()
  (emit `(incf (qreg ,(parse-qreg)))))

(define-command comma ?, ()
  (ecase (nargs)
    (\?  (setq *args* '\?))
    (0   (setq *args '()))
    (1   (push 0 *args*))
    (2   (error "Too many arguments."))))

(define-command minus ?- ()
  (setq *minusp* t))

(define-command point ?. ()
  (set-arg '(point)))

(defun digit (n)
  (ecase (nargs)
    ((\? 0) (set-arg n))
    ((1 2)  (set-arg (+ (* 10 (first *args*)) n)))))

(define-command zero  ?0 ()  (digit 0))
(define-command one   ?1 ()  (digit 1))
(define-command two   ?2 ()  (digit 2))
(define-command three ?3 ()  (digit 3))
(define-command four  ?4 ()  (digit 4))
(define-command five  ?5 ()  (digit 5))
(define-command six   ?6 ()  (digit 6))
(define-command seven ?7 ()  (digit 7))
(define-command eight ?8 ()  (digit 8))
(define-command nine  ?9 ()  (digit 9))

(define-command colon ?: ()
  (setq *colonp* t))

(define-command semicolon ?; ()
  (emit `(when (plusp ,(get-arg)) (return))))

(define-command loop ?< ()
  (let ((end (save-excursion (search-forward ">"))))
    (ecase (nargs)
      (\?    (emit '(teco-loop)))
      (0     (emit `(loop ,@(compile-until end))))
      ((1 2) (emit `(loop repeat ,(get-arg) do ,@(compile-until end)))
             (setq *args* '())))))

(define-command equals ?= ()
  (ecase (nargs)
    (\?  (emit '(teco-equals)))
    (1   (let ((format (if *colonp* "%s" "%s\r\n")))
           (if *atp*
               (emit `(message ,format ,(get-arg)))
               (emit `(insert (format ,format ,(get-arg)))))))
    (2   (let ((format (if *colonp* "%s,%s" "%s,%s\r\n")))
           (if *atp*
               (emit `(message ,format ,(get-arg) ,(get-arg)))
               (emit `(insert (format ,format ,(get-arg) ,(get-arg)))))))))

(define-command loop ?> ()
  nil)

(define-command space ?  ()
  nil)

(define-command if ?! ()
  (search-forward "!"))

(define-command if ?\" ()
  (let ((condition (upcase (next-char)))
        (end (save-excursion (search-forward "\'"))))
    (setq condition
          (cdr (assoc condition
                      '((?A . asciip)
                        (?B . delimiterp)
                        (?C . not-delimiterp)
                        (?D . digitp)
                        (?E . zerop)
                        (?G . plusp)
                        (?L . minusp)
                        (?N . not-zerop)
                        (?U . uppercasep)))))
    (ecase (nargs)
      (\?  (emit '(teco-conditional)))
      (1   (emit `(when (,condition ,(get-arg)) ,@(compile-until end)))))))

(define-command endif ?\' ()
  nil)

(define-command at ?@ ()
  (setq *atp* t))

(define-command whole-buffer ?b ()
  (set-arg '(point-min)))

(define-command forward ?c ()
  (emit '(forward-char)))

(define-command delete ?d ()
  (ecase (nargs)
    (\?   (emit '(teco-delete)))
    (0    (emit '(delete-char)))
    (1    (emit `(delete-char ,(get-arg))))))

(define-command reverse ?g ()
  (emit `(insert (qreg ,(parse-qreg)))))

(define-command whole-buffer ?h ()
  (setq *args* (list '(point-min) '(point-max))))

(define-command insert ?i ()
  (ecase (nargs)
    (\? (emit '(teco-insert)))
    (0  (if *colonp*
            (emit `(setf (qreg ,(parse-qreg)) ,(parse-string)))
            (emit `(insert ,(parse-string)))))
    (1  (emit `(insert ,(get-arg))))
    (2  (emit `(insert (make-string ,(get-arg) ,(get-arg)))))))

(define-command jump ?j ()
  (ecase (nargs)
    (\? (emit '(teco-jump)))
    (0  (emit '(goto-char (point-min))))
    (1  (emit `(goto-char ,(get-arg))))))

(define-command kill ?k ()
  (ecase (nargs)
    (\? (emit '(teco-kill)))
    (0  (emit '(kill-line)))
    (1  (emit `(kill-line ,(get-arg))))
    (2  (emit `(kill-region ,(get-arg) ,(get-arg))))))

(define-command macro ?m ()
  (emit `(funcall (qreg ,(parse-qreg))))
  (setq *args* '\?))

(define-command quantity ?q ()
  (set-arg `(qreg ,(parse-qreg))))

(define-command reverse ?r ()
  (emit '(backward-char)))

(define-command search ?s ()
  (emit `(search-forward ,(parse-string))))

(define-command update ?u ()
  (emit `(setf (qreg ,(parse-qreg)) ,(get-arg))))

(define-command flush ?w ()
  (setq *args* '()))

(define-command copy ?x ()
  (let ((qreg `(qreg ,(parse-qreg))))
    (ecase (nargs)
      (\? (emit `(teco-copy ,qreg)))
      (0  (emit '(setf ,qreg (buffer-substring (point) (progn (end-of-line) (point))))))
      (1  (emit `(setf ,qreg (buffer-substring (point) (progn (end-of-line ,(get-arg)) (point))))))
      (2  (emit `(setf ,qreg (buffer-substring ,(get-arg) ,(get-arg))))))))

(define-command whole-buffer ?z ()
  (set-arg '(point-max)))

(define-command push ?\[ ()
  (ecase (nargs)
    ((\?)  (emit `(teco-push ,(parse-qreg))))
    (0     (emit `(push (qreg ,(parse-qreg)) *pdl*)))
    (1     (let ((qreg (parse-qreg)))
             (emit `(push (qreg ,qreg) *pdl*))
             (emit `(setf (qreg ,qreg) ,(get-arg)))))))

(define-command backslash ?\\ ()
  (ecase (nargs)
    (\?  (emit '(teco-backslash)))
    (0   (emit `(parse-number-from-buffer)))
    (1   (emit `(insert (format "%s" ,(get-arg)))))))

(define-command push ?\] ()
  (emit `(setf (qreg ,(parse-qreg)) (pop *pdl*))))

(defun compile-until (end)
  (let ((*code* nil)
        (*atp* nil)
        (*colonp* nil)
        (*minusp* nil)
        (*args* '()))
    (while (< (point) end)
      (let ((*current-char* (next-char)))
        (funcall (aref *dispatch-table* *current-char*))))
    *code*))

(defun compile-teco-to-emacs-lisp ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((code (compile-until (point-max))))
      (pop-to-buffer (get-buffer-create "*TECO*"))
      (erase-buffer)
      (loop for x in (nreverse code)
         do (prin1 x (current-buffer))
           (insert "\n")))))
