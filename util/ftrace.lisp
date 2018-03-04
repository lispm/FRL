(include declar)

;;; *****************************************************************
;;; *                                                               *
;;; *                    The FRL Trace Package                      *
;;; *                                                               *
;;; *****************************************************************


;; User Functions: FTRACE (autoloadable), FUNTRACE


;;     The syntax of FTRACE is based on the TRACE package in MACLISP
;; except that the objects being traced are not functions but frame
;; oriented actions: instantiation, frame creation, running attached
;; procedures, etc.
;;     (FTRACE specification1 ...) where a specification is either atomic,
;; and therefore the name of a kind of action to be traced, or a list
;; whose first element is the action and whose remaining elements are
;; chosen from the following options.
;;     Options:
;; (1) BREAK condition
;; (2) COND condition
;; (3) ENTRY list
;; (4) EXIT list
;; 
;;     Actions:
;; IF-ADDED
;; IF-REMOVED
;; IF-NEEDED
;; CREATE
;; DESTROY
;; INSTANTIATE

(declare (special *traceable-actions* *trace-options* errlist))
(setq *traceable-actions* '(if-added if-needed if-removed create destroy instantiate)
      *trace-options* '(break cond entry exit))


(defun FTRACE fexpr (l)
       (ftrace-reset)				  ; reset indentation
       (setq errlist (sadd '(ftrace-reset) errlist))	; reset indentation on error
       (cond((null l)	; print trace objects
             (filter action *traceable-actions* (symeval (generate-trace-name action))))
            (t (do ((spec (car l) (car rest))
                    (rest (cdr l) (cdr rest))
                    (action) (trace-name))
                   ((null spec) (ftrace))
                   (if (atom spec) (setq spec (list spec)))	;  no options supplied
                   (setq action (car spec))
                   (cond((memq action *traceable-actions*)
                         (apply (function funtrace)
				(list action))  ; remove previous trace information.
                         (setq trace-name (generate-trace-name action))
                         (set trace-name t)		; trace it.
                         (do ((options (cdr spec) (cddr options)))
                             ((null options))
                             (cond((memq (car options) *trace-options*)
                                   (putprop trace-name (cadr options) (car options)))
                                  (t (shout0 `(/; ,(car options) |is not a valid option|))
                                     (apply 'funtrace (list action))))))
                        ((shout0 `(/; ,action |can't be traced|))))))))


(defun FUNTRACE fexpr (l)
       (setq errlist (sremove '(ftrace-reset) errlist))	; Installed by FTRACE.
       (if (null l)
           (setq l *traceable-actions*))
       (do ((action (car l) (car rest))
            (rest (cdr l) (cdr rest))
            (trace-name) (untraced-actions))
           ((null action) untraced-actions)	; return actions being untraced.
           (cond((memq action *traceable-actions*)
                 (setq trace-name (generate-trace-name action))
                 (if (symeval trace-name)
                        (set trace-name nil)
                        (push action untraced-actions)
                        (mapc (function (lambda (option) (remprop trace-name option)))
                              *trace-options*)))
                (t (shout0 `(/; ,action |can't be traced|))))))


(declare (special ftrace-indentation delta-ftrace-indentation)
         (fixnum  ftrace-indentation delta-ftrace-indentation))

;; To trace:
;;   If COND clause is T, print standard entry crap and ENTRY
;;   If BREAK, break
;;   Evaluate
;;   IF COND clause is T, print standard exit crap and EXIT


;;; FTRACE/: IF-ADDED and IF-REMOVED

(defun FRUN-AND-TRACE-IF-ADDED (method)
       ;; Assumes :frame :slot and :value are bound.
       (prog (trace-name cond return)
         (setq trace-name '*trace-if-added*)
         (if (setq cond (eval (or (get trace-name 'cond) t)))
             ;; Print the trace information.
             (terpri)
             ;; Set indentation for this instance of tracing.
             (ftrace-indent)
             (shout-indented `(|*** Enter IF-ADDED/:| ,(fname :frame) ,:slot /{ ,:value /}))
             (ftrace-entry-or-exit trace-name 'entry)	; print list of ENTRY values.
             ;; Print the method.
             (ftrace-print-method method)
             ;; boost the indentation for any embedded traces.
             (setq ftrace-indentation (+ ftrace-indentation delta-ftrace-indentation)))
         (break if-added (eval (get trace-name 'break)))
         (setq return (frun method))		; Do it.
         (if cond
             ;; Reset indentation.
             (setq ftrace-indentation (- ftrace-indentation delta-ftrace-indentation))
             (terpri)
             (ftrace-indent)
             (shout-indented `(|*** Exit IF-ADDED/:| ,(fname :frame) ,:slot /{ ,:value /}))
             (ftrace-entry-or-exit trace-name 'exit))	; print list of EXIT values.
         (return return)))

(defun FRUN-AND-TRACE-IF-REMOVED (method)
       ;; Assumes :frame :slot and :value are bound.
       (prog (trace-name cond return)
         (setq trace-name '*trace-if-removed*)
         (if (setq cond (eval (or (get trace-name 'cond) t)))
             ;; Print the trace information.
             (terpri)
             ;; Set indentation for this instance of tracing.
             (ftrace-indent)
             (shout-indented `(|*** Enter IF-REMOVED/:| ,(fname :frame) ,:slot /{ ,:value /}))
             (ftrace-entry-or-exit trace-name 'entry)	; print list of ENTRY values.
             ;; Print the method.
             (ftrace-print-method method)
             (setq ftrace-indentation (+ ftrace-indentation delta-ftrace-indentation)))
         (break if-removed (eval (get trace-name 'break)))
         (setq return (frun method))		; Do it.
         (if cond
             (setq ftrace-indentation (- ftrace-indentation delta-ftrace-indentation))
             (terpri)
             (ftrace-indent)
             (shout-indented `(|*** Exit IF-REMOVED/:| ,(fname :frame) ,:slot /{ ,:value /}))
             (ftrace-entry-or-exit trace-name 'exit))	; print list of EXIT values.
         (return return)))

;;; FTRACE/: IF=NEEDED

(defun FRUN-AND-TRACE-IF-NEEDED (method)
       ;; Assumes :frame and :slot are bound.  Returned value saved in /:VALUE.
       (prog (trace-name cond :value)
         (setq trace-name '*trace-IF-NEEDED*)
         (if (setq cond (eval (or (get trace-name 'cond) t)))
             ;; Print the trace information.
             (terpri)
             ;; Set indentation for this instance of tracing.
             (ftrace-indent)
             (shout-indented `(|*** Enter IF-NEEDED/:| ,(fname :frame) ,:slot))
             (ftrace-entry-or-exit trace-name 'entry)	; print list of ENTRY values.
             ;; Print the method.
             (ftrace-print-method method)
             ;; boost the indentation for any embedded traces.
             (setq ftrace-indentation (+ ftrace-indentation delta-ftrace-indentation)))
         (break if-added (eval (get trace-name 'break)))
         (setq :value (frun method))		; Do it.
         (if cond
             ;; Reset indentation.
             (setq ftrace-indentation (- ftrace-indentation delta-ftrace-indentation))
             (terpri)
             (ftrace-indent)
             (shout-indented `(|*** Exit IF-NEEDED/:| ,(fname :frame) ,:slot /{ ,:value /}))
             (ftrace-entry-or-exit trace-name 'exit))	; print list of EXIT values.
         (return :value)))

;;; FTRACE/: CREATE and DESTROY

(defun FTRACE-CREATE (name)
       (prog (trace-name cond)
         (setq trace-name '*trace-create*)
         (if (setq cond (eval (or (get trace-name 'cond) t)))
             (terpri)
             (ftrace-indent)
             (shout-indented `(|***| |Creating| ,name))
             (ftrace-entry-or-exit trace-name 'entry)
             (setq ftrace-indentation (+ ftrace-indentation delta-ftrace-indentation)))
         (break create (eval (get trace-name 'break)))
         (fcreate1 name)
         (if cond
             (setq ftrace-indentation (- ftrace-indentation delta-ftrace-indentation))
             (terpri)
             (ftrace-indent)
             (shout-indented `(|***| ,name |created|))
             (ftrace-entry-or-exit trace-name 'exit))))

(defun FTRACE-DESTROY (name)
       ;; This is identical to FTRACE-CREATE; when time allows, parameterize them both.
       (prog (trace-name cond)
         (setq trace-name '*trace-destroy*)
         (if (setq cond (eval (or (get trace-name 'cond) t)))
             (terpri)
             (ftrace-indent)
             (shout-indented `(|***| |Destroying| ,name))
             (ftrace-entry-or-exit trace-name 'entry)
             (setq ftrace-indentation (+ ftrace-indentation delta-ftrace-indentation)))
         (break destroy (eval (get trace-name 'break)))
         (fdestroy1 name)
         (if cond
             (setq ftrace-indentation (- ftrace-indentation delta-ftrace-indentation))
             (terpri)
             (ftrace-indent)
             (shout-indented `(|***| ,name |destroyed|))
             (ftrace-entry-or-exit trace-name 'exit))))

(defun FTRACE-INSTANTIATE (frame superordinates)
       (prog (trace-name cond)
         (setq trace-name '*trace-instantiate*)
         (if (setq cond (eval (or (get trace-name 'cond) t)))
             (terpri)
             (ftrace-indent)
             (shout-indented `(|***| |Instantiating| ,frame |as a kind of|
			       ,@(cond ((null (cdr superordinates)) superordinates)
				       (t `(/( ,@superordinates /))))))
             (ftrace-entry-or-exit trace-name 'entry)
             (setq ftrace-indentation (+ ftrace-indentation delta-ftrace-indentation)))
         (break instantiate (eval (get trace-name 'break)))
         (finstantiate1 frame superordinates)
         (if cond
             (setq ftrace-indentation (- ftrace-indentation delta-ftrace-indentation))
             (terpri)
             (ftrace-indent)
             (shout-indented `(|***| ,frame |instantiated|))
             (ftrace-entry-or-exit trace-name 'exit))))

;;; FTRACE Utility Functions

(defun ftrace-reset ()
       (setq ftrace-indentation 0. delta-ftrace-indentation 2.))

(defun ftrace-indent ()
       (indent-to (- (linel t) ftrace-indentation)))

(defun ftrace-print-method (x)
       (for (prinlevel 3. prinlength 5.) (terpri) (ftrace-indent) (prin1 x)))

(defun ftrace-entry-or-exit (name entry-or-exit)
       (for (forms (get name entry-or-exit))
         (if forms (princ '|/|/| |)(sprint (mapcar 'eval forms) (chrct t) 0))))

(defun generate-trace-name (action)
       (concatenate '*trace- action '*))

(declare (unspecial *traceable-actions* *trace-options*
		    ftrace-indentation delta-ftrace-indentation))

;;    FTRACE is FRL's tracer for frame actions.  It's syntax parallels
;; LISP's TRACE except that a predefined set of actions are traced rather
;; than functions and only a limited set of options are available.
;; Traceable actions are IF-ADDED, IF-REMOVED, IF-NEEDED, CREATE, DESTROY,
;; and INSTANTIATE.  Options are COND, BREAK, ENTRY and EXIT.  For
;; example,
;; 
;;    (FTRACE IF-ADDED)
;; 
;; causes trace information to be printed out before and after an if-added
;; method is executed.
;; 
;;     Additional information can be specified using the ENTRY and EXIT
;; options.  The COND options controls whether anything at all is printed;
;; BREAK breaks.  That is,
;; 
;;    (FTRACE (IF-ADDED COND (NOT (MEMQ /:SLOT '(AKO INSTANCE)))
;;                      BREAK (EQ /:SLOT 'FOO)
;;                      ENTRY ( (INDIVIDUAL? /:FRAME) ) ))
;; 
;; prints the usual stuff about if-added methods run on any slots other
;; than AKO and INSTANCE, breaks if an if-added method is run on the FOO
;; slot of a frame, and prints whether or not the frame is an Individual
;; along with the entry information.
;; 
;;     (FTRACE) prints returns a list of actions being traced.
;; 
;;     (FUNTRACE) stops tracing everything.
;; 
;;     (FUNTRACE action1 action2 ...) stops tracing selectively.


