;;
;;	init - this file is used to both load frl into
;;		lisp and to create FRL (by the file doit.l)
;;		This file can create a compiled or interpretive
;;		version of frl.(see notes below).
;;
;;
;;	set interpret-mode to t if you wish to load an
;;		interpretive version of frl.
;;(setq interpret-mode t)
;;

#+franz
(cvttomaclisp)
;;
;;	Important if you move the frl source, remember
;;	to remove frllib/ftop.o and recompile.
;;
;; (setq frl-main-dir '|//u0//csam//steve//rosenberg|)
#+unix
(process |//bin//csh -c 'rm -f //tmp//frl.dir;pwd /| sed "s//\////\//\////g" > //tmp//frl.dir'|)

#+unix
(let ((frl-dir-file (infile '|//tmp//frl.dir|)))
  (setq frl-main-dir (read frl-dir-file))
  (close frl-dir-file)
  (process |rm -f //tmp//frl.dir|))

#+franz
(apply 'sstatus `(load-search-path ,(append1 (status load-search-path)
					     frl-main-dir)))
#+franz
(load 'frllib//fauxfns)

#+maclisp
(load '((ps frl/.frllib) fauxfns lisp))
(loadlisplibrary)
;;
;;	if interpret-mode is not set, set it too nil.
;;		load everything in compiled.
;;
(defvar interpret-mode nil)

(initial-syntax)

(frl-define-switches)
(frl-syntax)
(frl-utility-load)
;;
;;	This is commented out since it does not mean anything yet
;;	in the franz-lisp version of frl.	-- dhl (6/28/81)
;;
;; These readtable functions require the print file, hence are placed here.
;;;            (DECLARE (REQUIREDEF '((PS FRL) PRINT FASL)) (/#1SET-UP-MACROS))
;;;            (/#MAKE-INVERT-QUOTE-FN EXCLAMATION /!)
;;;            (/#MAKE-INVERT-QUOTE-FN PERCENTSIGN /%)
;;;            (/#MAKE-INVERT-QUOTE-FN AMPERSAND   /&)
;;;            (/#MAKE-INVERT-QUOTE-FN ATSIGN      /@)
(frl-basic-load)
(rule-load)
;;
;;	(set-up-the talk function to load in all the talk files also-
;;			they will only be loaded once.).
;;
(putd 'talk (getd 'talk-load))
;;	(talk-load)	calling (talk) will load the talk files.
(dhl-load)
;;(oil-load)
(load 'frllib//ftop)


