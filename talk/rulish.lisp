(include declar)

;;; (declare (requiredf '(pidgin fasl dsk pa)))
;;; (requiredf '((dsk pa) pidgin fasl))
;;; (requiredf '((dsk pa) ftalk0 fasl))		; contains Basic Framish.

;;; PROCEDURE
(prefix PROCEDURE
	0 (denfunc '(list (buildfun 'lambda '() right))))

(prefix RUN
	2 (denfun 'eval (kwote right)))


;;; SHOUT
;;   shout "I am a string of tokens."

;; This function is commented because errors occur in reading in the file
;; in Franz. " is a special character. Either change ists use here, or 
;; reset the syntax if this function must be used.

;; (prefix SHOUT
;; 	0 (denfunc '(prog2 (if (nottoken '") (error '|missing ".|))
;;                            (list 'shout0 (kwote (collect-string-until '/")))
;; 			   (read-next-token))))

(defun collect-string-until (terminator)
       (do ((ch    (tyi t)(tyi t))
	    (string nil   (cons ch string))
	    (term (getcharn terminator 1)))
	   ((= ch term)(maknam (nreverse string)))))

;; (defun collect-tokens-until (terminator)
;;        (do ((result))
;; 	   ((eq /:token terminator) (nreverse result))
;; 	   (push /:token result)
;; 	   (read-next-token)))

;;; QUOTE (in the Lisp sense)

;;  Quotes the next expression.
;;  Use it as in "The preference for the time is/: ..."
;;  Alternately, use "that" or "expression" to introduce phrase whose
;;  Lisp equivalent is desired, not their value.

(prefix /:
	2 (denfun 'quote (convert-path right)))

(define-pidgin-character '/:)

; NB infix COLON is used in TIMISH for times/: 3/:45 pm, eg.

(variants '/:  'THAT 'EXPRESSION)			; 



;;; PERCENTSIGN evaluation in frames.

;;  Values and Default facets can have a prefix PERCENTSIGN to indicate
;;  evaluation whenever retrieved.  Use "percentsign" to signal such an expression.
;;  It is a kind of unquote for frames.

(prefix PERCENTSIGN
	2 (denfun 'percentsign < expression > (convert-path right)))


(declare (special /:if-flag))

;;; "the test that the X is a kind of Y"
(prefix TEST
	2 (denfunc '(for (/:if-flag t) right)))

(infix KIND
       10. (denfun 'ako/? (convert-path /:left) < of > (convert-path (kwote-if-atom right))))

(infix LINKED

       10. (denfun 'framish-link (convert-path /:left)
		   < to > (convert-path (kwote-if-atom right))
		   via (kwote-if-atom token)))

(suffix INDIVIDUAL
	15. (denfun 'individual/? (convert-path /:left)))

(suffix GENERIC
	15. (denfun 'generic/? (convert-path /:left)))


(defmacro framish-link (f1 f2 link)
	  ;; Just switch around the arguments.
	  `(flink/? ,link ,f1 ,f2))

(mapc 'comparative-with-is '(kind individual generic linked))


;; "Specify <repair>."
(prefix SPECIFY						; Fill slots
  2 (denfun 'apply ''Schedule
	     (for (rest right)
		  (cond ((path/? rest)(for (path (construct-indicator-path rest))
				      (cond ((eq (2nd path) ':slot) `(list ,(1st path)))
					    (t `(list ,(1st path) ,(2nd path))))))
			(T `(list ,(kwote-if-atom rest)))))))

;; "Describe <repair>."


(prefix DESCRIBE
  2 (denfun 'apply ''Fdiscuss
	    (for (rest right)
		  (cond ((path/? rest)(for (path (construct-indicator-path rest))
				      (cond ((eq (2nd path) ':slot) `(list ,(1st path)))
					    (t `(list ,(1st path) ,(2nd path))))))
			(T `(list ,(kwote-if-atom rest)))))))

;; "Schedule <repair>."
(prefix SCHEDULE					; Just try to assign time.
  2 (denfun 'apply ''framish-schedule
	     (for (rest right)
		  (cond ((path/? rest)(for (path (construct-indicator-path rest))
				      (cond ((eq (2nd path) ':slot)`(list ,(1st path) 'STATUS))
					    (t `(list ,(1st path) ,(2nd path))))))
			(T `(list ,(kwote-if-atom rest) 'STATUS))))))

;; "Instantiate <repair>."
(prefix INSTANTIATE
  2 (denfunc
     '(list 'progn
       (list 'set-framish-context
	     (list 'list (nconc
			  (buildfun < new > 'finstantiate (kwote-if-atom right) < frame >)
			  (if (istoken 'called) (list (kwote right))))
		   nil nil))
       '(individualize /:pathframe)
       '/:pathframe)))

(prefix MENU
  2 (denfun 'menu))


;;; A string is delimited by "|".  |These three words| are a string.
(prefix /|
  2 (denfunc
    '(do ((char (tyi) (tyi))
	  (string))
	 ((= char 124.)				; closing quote sign
	  (maknam (nreverse string)))
	 (push char string))))

(delim /|)

(defun framish-schedule (f s)
       (for (*ask-use-suggestions* t)
            (schedule f s)))


