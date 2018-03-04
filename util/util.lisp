(include declar)
;;; -*-lisp-*-
(comment Required Files)

#-lispm
(declare (sstatus macro /~ '(lambda () ((lambda (/~) (eval /~) /~) (read)))))

#-lispm
(setq /~ nil)						; ~ is a NO-OP to the interpreter.

;; Expressions preceded by ~ are read and digested by the compiler, as well as being
;; compiled and left defined in the file.  Use ~ before expressions that will be needed at
;; at compile-time as well as load-time.


(defun REQUIREDF narg
       ;; Arguments are files that are required to be loaded.
       ;; REQUIREDF will halt and load them if the current version is not present.
       ;; Currency is determined using *VERSION*.
       (do ((files (listify narg) (cdr files))
            (f))
           ((null files))
           (setq f (mergef (car files) '((* *) * FASL)))
           ;; if file not  yet in according to version, then load
           (or (equal (mergef '((* *) * t) f)(mergef '((* *) * t)(get '*version* (cadr f))))
               (progn (or (status feature noldmsg)
			  (progn (terpri)(princ '|/;Loading required file |)(prin1 f)))
                      (load f)))
           ;;if has version, then compare, otherwise assume ok
           (cond ((and (get '*version* (cadr f))
                       (not (equal (get '*version* (cadr f))
                                   (probef (mergef '((* *) * >) f)))))
                  (terpri tyo)(princ '|/;Required file latest version not compiled |  tyo)
                  (prin1 f tyo)
                  (break alt-p-to-proceed)))))

(defun REQUIREDF narg nil)

;;; Commented requiredefs, although work, as I make all loadiings explicit.
;;;~(requiredf '((dsk frl) cntrl))			  ; Contains some useful macros.
;;;(requiredf '((dsk frl) set))			  ; Some separate utilities.

(comment Version Numbers)

;;***********************************************************************
;; VERSION NUMBER --
;;   Put (VERSION) in each of the files whose version you wish to record.
;;    This puts the full file name (including version number) on the
;;    plist of *VERSION* under the indicator which is the first file name.
;;************************************************************************


;; * The version stuff is commented. I'm not sure if it can be
;; got to work under vms or not.

;; (macro VERSION (call)
;;        ;; puts the version number (the second filename) of the current file on
;;        ;; property of *VERSION* which is the first file name.
;;        ;; Works for both interpreted and compiled versions.
;;        ((lambda (filename)
;;                 ;; assume file name in newio format
;;                 `(defprop *version* ,filename ,(cadr filename)))
;; 	(truename infile)))
;; 
;; (defun VERSION? (file)
;;        ;; returns the present version number of given file
;;        (caddr (get '*version* file)))
;; 
;; (defun CHECK-VERSIONS ()
;;        ;; runs through all the version numbers on *VERSION* and
;;        ;; makes sure they correspond to the highest version number on dsk.
;;        ;; It returns list of file names that have higher version numbers on
;;        ;; dsk than in present image.
;;        (do ((plist (cdr (plist '*version*)) (cddr plist))
;;             (filename)(hits))
;;            ((null plist) hits)
;;            (setq filename (car plist))
;;            (or (equal filename (probef (mergef '((* *) * >) filename)))
;;                (push filename hits))))
;; 
;; (defun BUMP-VERSION (v)
;;        ;; V is a version number (not a fixnum).
;;        ;; Returns a version number (not a fixnum), which is 1+ V.
;;        ((lambda (base ibase *nopoint)
;;                 (implode (exploden (1+ (readlist (exploden v))))))
;;         10. 10. t))
;; 
;; (version)						; record version of this file

(comment List Manipulation)

;;;
;;;		List Hacking Utilities
;;;


(defun FIRSTN (n list)
       ;; returns a list comprising the first N elements of LIST.
       (do ((l list (cdr l)) (output) (i (cond((minusp n) 0)(n)) (1- i)))
           ((or (zerop i) (null l)) (nreverse output))
           (declare (fixnum i))
           (setq output (cons (car l) output))))


;;;	The following macros extract the Nth field of a list.

(defmacro 1ST (l) `(car ,l))

(defmacro 2ND (l) `(cadr ,l))

(defmacro 3RD (l) `(caddr ,l))

(defmacro 4TH (l) `(cadddr ,l))

(defmacro 5TH (l) `(car (cddddr ,l)))



(defmacro RPLAC1 (l x) `((lambda (ll) (rplaca ll ,x) ll) ,l))

(defmacro RPLAC2 (l x) `((lambda (ll) (rplaca (cdr ll) ,x) ll) ,l))

(defmacro RPLAC3 (l x) `((lambda (ll) (rplaca (cddr ll) ,x) ll) ,l))

(defmacro RPLAC4 (l x) `((lambda (ll) (rplaca (cdddr ll) ,x) ll) ,l))

(defmacro RPLAC5 (l x) `((lambda (ll) (rplaca (cddddr ll) ,x) ll) ,l))

(comment File Manipulation)

;;
;;  this function exists already in the lisp machine
;;
#-lispm
(defun READFILE fexpr (file)
       ;; Fix to use newio file specs
 ((lambda (eof)
   (apply 'uread file)
   (setq ^q t)
   (terpri)
   (princ (status uread))
   (do ((exp (read eof) (read eof)))
       ((eq exp eof))
       (/#prin1 (eval exp)))
   '*)
  (list nil)))

#-lispm
(defun WRITEFILE (sexpr filspc)
       ((lambda (fil prinlevel prinlength)
             (prog2
              ((lambda (outfiles ^R ^W) (/#print sexpr))	;;grinds Sexpr.
	       (cons fil outfiles) T T)
              (truename fil)
              (close fil)))
	(open (mergef filspc defaultf) 'out) NIL NIL))

#+lispm
(defun writefile (sexpr filspc)
  (grind-top-level sexpr nil (open filspc))
  (close filspc)
  filspc)


(comment Printing Utilities)

(defun NEWLINE ()
  ;; puts cursor on a blank line.
  (terpri))


(defun NEWPAGE ()
       ;; prints a formfeed.
       (tyo 12.))

(defun SPACE (N)
       ;; Leaves N blank lines after the current line.
       (declare (fixnum n i))
       (newline)
       (do i n (1- i) (zerop i) (terpri)))

(defun PRINTC (string)
       ;; "PRINT" using PRINC instead of PRIN1.
       (terpri) (princ string) (tyo 32.))

(defun CHRCT (tty)
       (- (linel tty) (charpos tty)))


(comment NUMERICAL LOGICAL and STRING operators)

;;;	Numerical Operators


(defun GE (a b)
       ;; greater-than-or-equal-to predicate for numbers
       (signp ge (difference a b)))

(defun LE (a b)
       (signp le (difference a b)))

(defun ROUND (x)
       ;;rounds off the number X to the nearest integer.
       (fix (plus 0.5 x)))



;;;	String Oriented Utilities.

(defun CONCATENATE args
       ;;args are atoms; returns interned atom formed by concatenation.
       (implode (mapcan 'exploden (delq nil (listify args)))))

(defun STRINGIFY args
       ;; Like concatenate, except does NOT intern atom.
       (maknam (mapcan 'exploden (delq nil (listify args)))))

(defun STRING? (x)
       ;; In MacLisp we must be content with atoms as strings.
       (atom x))

;;;	Miscellany

(defmacro KWOTE (x)
       `(list 'quote ,x))

(defmacro LISTP (x)
	  `(eq (typep ,x) 'list))

(macro LOGAND (form)
       `(boole 1. ,.(cdr form)))


