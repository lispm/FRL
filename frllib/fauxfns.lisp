;;-*-Lisp-*-
;;:vi:set lisp ai
;;
(include ldeclar)

(declare (*fexpr frl-file-name))

(defvar *trace-create* nil)

(defmacro defmvar decl
  `(defvar . ,decl))

(defvar interpret-mode nil 
  |True if user wants files loaded in interpretively|)

;;
;;   First some system dependent definitions
;;

#+lispm
(defmacro includef (file) nil)
;;
;; special things for declares not handled in lisp machine lisp
;;
#+lispm
(defun macros fexpr (x))
#+lispm
(defun notype fexpr (x))
#+lispm
(defun localf fexpr (x))

#+lispm
(defmacro pop (x)
  `(let ((answer (car ,x)))
     (setq ,x (cdr ,x))
     answer))

#+lispm
(defmacro push (a b)
  `(setq ,b (cons ,a ,b)))

#+lispm
(defun /$prpr (expression)
  (grind-top-level expression))

#+lispm
(defmacro patom (atom)
 `(princ ,atom))

#+lispm
(defmacro dtpr (x)
  `(or (listp ,x)
       (null ,x)))

#+lispm
(defmacro cadaddr (x)
  `(cadr (caddr ,x)))

#-franz
(defmacro pp (function-name)
  `(grindef ,function-name))

#+franz
(defmacro Macro (name args . body)
  `(defun ,name macro ,args . ,body))

#+franz
(defun cursorpos ())

#+franz
(defvar lisp-file-extension '|.l|)
#+franz
(defvar frl-main-loadfiles-dir nil)
#+franz
(defvar frl-directory-character '//)
#+franz
(defvar frl-end-directory-character '//)

;;#+lispm
;;(defvar frl-main-loadfiles-dir '|lm1:>frl>|)
;;#+lispm
;;(defvar frl-directory-character '|>|)
;;#+lispm
;;(defvar frl-end-directory-character '|>|)

;;
;; declarations when files are stored on the hulk.
;;
#+lispm
(defvar frl-main-loadfiles-dir '|hulk:ps:<frl.|)
#+lispm
(defvar frl-directory-character '|.|)
#+lispm
(defvar frl-end-directory-character '|>|)
#+lispm
(defvar lisp-file-extension '|.lisp|)

;;#+lispm
;;(defvar frl-main-loadfiles-dir '|vax://vb//douglas//frl//|)
;;#+lispm
;;(defvar frl-directory-character '|//|)
;;#+lispm
;;(defvar frl-end-directory-character '|//|)
;;#+lispm
;;(defvar lisp-file-extension '|.lisp|)

#+maclisp
(defvar frl-main-loadfiles-dir '(dsk frl))
#+maclisp
(defvar lisp-file-extension 'lisp)

#+lispm
(defun concat (/&rest n)
  (intern (apply (function string-append)
		 (mapcar (function (lambda (x)
				     (cond ((fixp x)
					    (format:output nil (format:onum x)))
					   ((floatp x)
					    (format:output nil (format:ofloat x)))
					   (t x))))
			 n))))

#+franz
(defmacro frl-load (x)
  `(load ,x))

#+lispm
(defmacro frl-load (x)
  `(load ,x 'frl))

#+maclisp
(defmacro frl-load (x)
  `(load ,x))

#+lispm
(defun copy macro (x)
       `(copylist . , (cdr x)))

#+lispm
(defmacro getd (name)
  `(and (functionp ,name)
	(funcall (function function) ,name)))

#+lispm
(defmacro putd (name definition)
  `(funcall (function deff) ,name ,definition))

#+lispm
(putd 'terpr (getd 'terpri))

#+lispm
(defun makereadtable (ignor) (copy-readtable))

#+lispm
(defun nointerrupt (/&rest ignor))


#-maclisp
(defun loadfiles files
  (mapc (function
	 (lambda (file) 
		 (cond (interpret-mode (patom '|[Loading: |)
				       (patom file)
				       (patom '|]|)
				       (terpri)
				       (frl-load
					(concat file lisp-file-extension)))
		       (t (frl-load file)))))
	(copy (listify files)))
  nil)


#+maclisp
(defun loadfiles files
  (mapc (function
	 (lambda (file) 
		 (cond (interpret-mode (patom '|[Loading: |)
				       (patom file)
				       (patom '|]|)
				       (terpri)
				       (frl-load (append file 
							 (list lisp-file-extension))))
		       (t (frl-load file)))))

	(copy (listify files)))
  nil)

(defmacro frl-concat (arg1 . rest)
  `(cond ((null ,arg1)
	  (apply (function concat) (list . ,rest)))
	 (t (apply (function concat)
		   (list ,arg1 (apply (function concat)
				      (list . ,rest)))))))

#-maclisp
(defun frl-file-name fexpr (args)
       (let ((file (car args))
	     (directory-path (cdr args)))
	    (frl-concat frl-main-loadfiles-dir
			(do ((d directory-path
				(cdr d))
			     (c nil))
			    ((null (cdr d))
			     (frl-concat c (car d)
					 frl-end-directory-character))
			  (setq c (frl-concat c (car d)
					      frl-directory-character)))
			file)))

#+maclisp
(defun frl-file-name fexpr (args)
  (let ((file (car args))
	(directory-path (cdr args)))
    (do ((x (cadr frl-main-loadfiles-dir))
	 (y directory-path (cdr y)))
	((null y)
	 `((,(car frl-main-loadfiles-dir) ,x) ,file))
      (setq x (frl-concat x '/. (car y))))))

(declare (special frl-readtable readtable si:readtable))

(defun initial-syntax ()
  (setq frl-readtable (setq readtable (makereadtable nil)))
  (setsyntax '/[ 'macro (function readbracketlist))
  (setsyntax '/] 'macro (function endbracketlist)))

(defun endbracketlist ()
  '/])


(defun set-frl-syntax ()
  (initial-syntax)
  (frl-syntax))

(defun frl-syntax ()
  ;;;   (setsyntax '/$ 'macro '/$-readmacro) 
  ;;;	This was the "$" readmacro which causes problems.
  (setsyntax '/@ 'macro (function /@-readmacro))
  (setsyntax '/% 'macro (function /%-readmacro))
  (setsyntax '/! 'macro (function /!-readmacro))
  (setsyntax '/& 'macro (function /&-readmacro)))

(Defun linel (tty) 80)

(Defun charpos (tty) 0)

(defun readbracketlist nil
  (do ((x (read) (read)) (list nil))
      ((eq x '/]) (cons 'list (nreverse list)))
      (setq list (cons (cond ((and (not (atom x)) (eq (car x) 'atsign)) (cadr x))
			     ((and (eq (typep x) 'list) (eq (car x) 'list)) x)
			     ((list 'quote x))) list))))

;;; (defun /$-readmacro () (list 'altmode (read)))
(DEFUN /@-READMACRO NIL (LIST 'ATSIGN (READ)))
(DEFUN /%-READMACRO NIL (LIST 'PERCENTSIGN (READ)))
(DEFUN /!-READMACRO NIL (LIST 'EXCLAMATION (READ)))
(DEFUN /&-READMACRO NIL (LIST 'AMPERSAND (READ)))

;;
;;	Modification to the pp to print '@', '%', '!', and '&' .
;;	This version comes with opus 36 franz lisp.
;;
;;      Must figure out how to do this on the lisp machine and maclisp.
;;
(putprop 'atsign '|@| 'printmacrochar)
(putprop 'percentsign '|%| 'printmacrochar)
(putprop 'exclamation '|!| 'printmacrochar)
(putprop 'ampersand '|&| 'printmacrochar)

(defun frl-define-switches ()
  ;; Controls FRL tracing; FTRACE package is autoloadable, but these 
  ;; switches are needed always. Several other flags are used throughout 
  ;; FRL. They are set when the files are read. See INITIALIZE-SWITCHES (in
  ;; FDUMP) for a list of them along with their default values.
  (setq *trace-create* nil   *trace-destroy* nil   *trace-if-added* nil
	*trace-if-removed* nil   *trace-if-needed* nil   *trace-instantiate*
	nil)
  (comment switches)
  ;; LISP Switches
  #-lispm(putd 'prin1 (getd '/#prin1))	; Set top level print to pretty print.
  ;; FRL Switches
  (setq *frames* nil)         ; the system frame list
  (setq *new-frames* nil)     ; the system "new frame" list
  (setq fassert t)            ; interpret fassert forms
  (setq deframe t)            ; interpret DEFRAME forms
  (setq *fgensym* 0)          ; counter used to generate unique frame names.
  (setq *verbose* nil)
  (setq *debug* nil)
  (setq *break-on-warning* nil *break-on-error* nil))

;;
;;    Special initialization needed to make franz like maclisp.
;;
#+franz
(defun loadlisplibrary ()
  ((lambda (interpret-mode)		; always load compiled version of 
					; the lisp library files.
	   ;;(loadfiles  '//usr//lib//lisp//auxfns1
		       ;;'//usr//lib//lisp//backquote
		       ;;'//usr//lib//lisp//auxfns0
		       ;;'//usr//lib//lisp//toplevel
		       ;;'//usr//lib//lisp//machacks)
	   (loadfiles ;'auxfns1
		      ;'backquote
		      ;'auxfns0
		      ;'toplevel
		      'machacks))
   nil))

#-franz
(defun loadlisplibrary ()
  nil)

;;;
;;;
;;;                 FRL Utility Files
;;;

(defun frl-utility-load ()
  (loadfiles (frl-file-name cntrl  util)
	     (frl-file-name set util)
	     (frl-file-name util util)
	     (frl-file-name sutil util)
	     (frl-file-name ftrace util)))

;;
;;	We only need macros at compile time thus, we only load them in.
;;
;;	frl-utility-macro-load loads only the utility files needed at compile time.
;;
(defun frl-utility-macro-load ()
  (loadfiles (frl-file-name cntrl util)
	     (frl-file-name set util)
	     (frl-file-name util util)))
;;;
;;;            Basic FRL  Files
;;;
(defun frl-basic-load ()
  (loadfiles (frl-file-name faccess frl)
	     (frl-file-name fmacro frl)
	     (frl-file-name futil frl)
	     (frl-file-name flist frl)
	     (frl-file-name fherit frl)
	     (frl-file-name fassert frl)
	     (frl-file-name raccess frl)
	     (frl-file-name freq frl)
	     (frl-file-name ttyio frl)
	     (frl-file-name fdump frl)
	     (frl-file-name fask frl)
	     (frl-file-name thing frl)
	     (frl-file-name thing1 frl)))

;;
;;	at compile time for files in frl/ and a few others, we need to
;;	load the following files of macros and initial functions.
;;

(defun frl-basic-macro-load ()
  (loadfiles (frl-file-name faccess frl)
	     (frl-file-name fassert frl)
	     (frl-file-name fmacro frl)
	     (frl-file-name raccess frl)))

(defun dhl-macro-load ()
  (loadfiles (frl-file-name rframes dhl)
	     (frl-file-name rule dhl)
	     (frl-file-name domain dhl)))

(defun dhl-load ()
  (dhl-macro-load)
  (load (frl-file-name satisfy dhl))
  (load (frl-file-name match dhl)))

;;;
;;;                   Oil Stuff - a demo for frl with oil shipments.
;;;

(defun oil-load ()
  (loadfiles (frl-file-name foil oil)
	     (frl-file-name oil oil)
	     (frl-file-name demo oil)))

(defun talk-load ()
  (loadfiles (frl-file-name pidgin talk)
	     (frl-file-name ftalk0 talk)
	     (frl-file-name frmish talk)
	     (frl-file-name rulish talk))
  (talk))

(defun rule-load ()
  (loadfiles (frl-file-name sentin rule)
	     (frl-file-name rule rule)
	     (frl-file-name rtemp rule)
	     ;;;"think"
	     ))


;;; up. Request also does not work until the bug to progv is fixed in
;;; a later version of FRANZ (if then). Also the following alternative
;;; to shout seems as good and is simpler that the set of shout functions
;;; which do not do in FRANZ what they did in maclisp.

(defun /#princ (x) (princ x))

;;; ttyio has a redefinition of prin1 and #prin1 for the same reason

(defun speak (message)
       (prog ()
             (setq message (squash message))
             (terpri)
             loop
             (cond (message (princ (car message))
                            (princ '| |)
                            (setq message (cdr message))
                            (go loop))
                   (t (return t)))))

(defun squash (s)
         (cond ((null s) nil)
               ((atom s) (list s))
               (t (append (squash (car s))
                          (squash (cdr s))))))


;;; There is a request5 function in ttyio to use until progv is fixed.
;;; There is also a request6 function for the same reason.

;;
;; define #print like this for now until it is replaced.
;;
(defun /#print macro (x)
  `(mapc (function (lambda (y)
		     (grind-top-level y)
		     (terpri)))
	 ,@(cdr x)))

#+franz
(defun timer fexpr (request)
  (prog (timein timeout result cpu garbage)
	(setq timein (ptime))
	(prog ()
	      loop (setq result (eval (car request)))
	      (setq request (cdr request))
	      (cond ((null request) (return result))
		    ((go loop))))
	(setq timeout (ptime))
	(setq cpu (quotient (difference (car timeout) 
					(car timein))
			    60.0))
	(setq garbage (quotient (difference (cadr timeout) 
					    (cadr timein)) 
				60.0))
	(terpri)
	(print (list (difference cpu garbage)
		     garbage
		     cpu))
	(terpri)
	(return result)))

#+lispm
(defun timer fexpr (expr)

  ;; return time in seconds to evaluate expr
       (let ((x (minus (quotient (difference
				   (time:microsecond-time)
				   (progn (eval (car expr))
					  (time:microsecond-time)))
				 1000000.0))))
	 (terpri)
	 (print x)
	 (terpri)))
	 

#+franz
(defmacro every (arg-lists func)
  (let ((k (gensym))
	(v (gensym)))
       `(do ((,k ,arg-lists (cdr ,k))
	     (,v))
	    ((null ,k) t)
	    (or (apply ,func (list (car ,k)))
		(return nil)))))

#+lispm
(let ()
  (setq interpret-mode t)
  (frl-define-switches)
  (set-frl-syntax))



