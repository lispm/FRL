From z30083@tansei.cc.u-tokyo.junet Thu Jun  2 13:08:06 1988
Received: by aoyama.cc.aoyama.junet (3.2/6.3Junet-1.0)
	id AA07872; Thu, 2 Jun 88 13:08:05 JST
Received: by ccut.cc.u-tokyo.junet (5.51/6.3Junet-1.0/CSNET-JUNET)
	id AA03737; Thu, 2 Jun 88 12:44:31 JST
Received: by tansei.cc.u-tokyo.junet (4.12/6.3Junet-1.0)
	id AA07993; Thu, 2 Jun 88 12:44:35+0900
Date: Thu, 2 Jun 88 12:44:35+0900
From: z30083@tansei.cc.u-tokyo.junet (Masayuki Ida)
Return-Path: <z30083@tansei.cc.u-tokyo.junet>
Message-Id: <8806020344.AA07993@tansei.cc.u-tokyo.junet>
To: ida@aoyama.junet
Subject: fmacro.l.frl
Status: RO


(include declar)
; -*-Lisp-*-

   
;;; Utility access functions.

(defmacro FEXTRACT-DATUM (d-str)
  ;; D-STR is a data-strucure, consisting of the data plus its comments.
  ;; Returns just the data.
  `(findicator ,d-str))

(defmacro FEXTRACT-DATA (d-strs)
  ;; D-STRS is a list of data-structures.
  ;; Returns a list of only the data, flushing the comments.
  `(findicators1 ,d-strs))

(defmacro FEXTRACT-MESSAGES (d-str label)
  ;; D-STR is a Data-structure.
  ;; Returns the message portion under LABEL.  This is always a list.
  `(fbucket (flistget ,d-str ,label)))

(defmacro FEXTRACT-MESSAGE (d-str label)
  ;; expects only one item in the comment, but in any case, just returns
  ;; the first message.
  `(car (fbucket (flistget ,d-str ,label))))

(defmacro FEXTRACT-COMMENTS (d-str)
  ;; See FCOMMENT/? to select a particular labeled comment.
  `(fbucket ,d-str))

;;; *****************************************************************
;;;			  Macros from FLIST
;;; *****************************************************************
;;
;;   	    FOR TAKING THE INDICATOR AND BUCKET PARTS OF FLISTS.
;;
;;     NB --  (should always be used instead of CAR and CDR !)
;;
;;;>>>>> This is a legacy of the ABBREVIATION storage idea.
;;;      It may be time to flush the conditionals; an flist is always a list.

(defmacro FINDICATOR (flist)
  ;; extract the indicator of an flist.
  `((lambda (f) (cond ((atom f) f)((car f))))
    ,flist))

(defmacro FBUCKET (flist)
  ;; extract the bucket of an flist.
  `((lambda (f) (cond ((atom f) nil)((cdr f))))
    ,flist))

(defmacro FINDICATORS (flist)
  ;; returns list of the indicators of the items in the bucket of Flist.
  `(mapcar
    '(lambda (item) (cond ((atom item) item)((car item))))
    (fbucket ,flist)))

(defmacro FINDICATORS1 (bucket)
  ;; returns list of the indicators of the items in the Bucket.
  `(mapcar
    '(lambda (item) (cond ((atom item) item)((car item))))
    ,bucket))

;;; Auxiliary functions for one-level flists; See FLIST.
(defmacro FASSOC (ind flist)
  ;; returns the item (an flist itself) in Flist with indicator matching Ind.
  ;; Like ASSOC, except jumps over indicator of Flist.
  `(assoc ,ind (fbucket ,flist)))

;;
;;	Use this one when you know ind is an atom.
;;
(defmacro fassq (ind flist)
  `(assoc ,ind (fbucket ,flist)))

(defmacro FDELETE-ASSOC (ind flist)
  ;; deletes from Flist the entire item with given indicator, Ind.
  ;; Returns Flist.
  `((lambda (f) (delq (fassoc ,ind f) f)) ,flist))


;;;*****************************************************************
;;;                  Macros to access properties
;;;*****************************************************************

;; It is convenient to have an abbreviated way of writing "the value the
;; slot S in frame F", which takes as defaults the values for frame and
;; slot in the current Frame Environment.  The ! and & Special Data Forms 
;; supply this mechanism.


;; The following occurs in PA;READTB >.
;; Affected are the PA Readtable/: *READTABLE, and the FRL readtable/: /#READTABLE.
;;;  (setsyntax '/! 'macro '/!-readmacro)
;;;  (setsyntax '/& 'macro '/&-readmacro)
;;;
;;;(defun /!-readmacro nil
;;;       (list 'exclamation (read)))
;;;
;;;(defun /&-readmacro nil
;;;       (list 'ampersand (read)))

;;; ! and & behave like "quote" when read and when printed (see PA; GRIND (INIT) ).
;;;   !foo => (exclamation foo)
;;;   !(meeting time) => (exclamation (meeting time))
;;;   !!x => (exclamation (exclamation x))

;; EXCLAMATION is itself a macro which expands
;;   into fdatum-only or fdata-only (it looks at car of its arg to decide).
;;   A "!!" (which expands when read to (EXCLAMATION (EXCLAMATION arg)) )
;;   indicates fdata-only.
;; AMPERSAND expands into fdatum or fdata, similarly.

;; Arguments are not evaluated although two special kinds are recognized/:
;;    another ! macro form (ie, EXCLAMATION) or the evaluate form (not yet implemented).

;; EXAMPLES/:
;;   !place => (fdatum-only :frame 'place '$value)
;;   !(meeting14 place) => (fdatum-only 'meeting14 'place '$value)
;;   !(!pre place) => (fdatum-only (fdatum-only :frame 'pre '$value) 'place '$value)

;; CHANGES/:
;;   This should pass for the conceptual "get the value of the said slot" and
;;   as such should be user programmable to the extent of supplying the name of the
;;   function to the use.  Also, only Frame and Slot are suitable arguments.
;;   This implies changing !&-arguments to return only F and S.


(macro EXCLAMATION (l)
       (for (arg (2nd l))
          (catch		;special case !VALUE => :value (not yet implemented)
            (cons (cond((and (listp arg)
                             (eq (1ST arg) 'exclamation))
                        (setq arg (2nd arg))
                        'fdata-only)
                       ('fdatum-only))
                  (/!/&-arguments arg))
            value)))

(macro AMPERSAND (l)
       (for (arg (2nd l))
            (cons (cond((and (listp arg)
                             (eq (1ST arg) 'ampersand))
                        (setq arg (2nd arg))
                        'fdata)
                       ('fdatum))
                  (/!/&-arguments arg))))

(defun /!/&-arguments (arg)
       ;; internal to exclamation and ampersand %macros
       (cond((atom arg)
             ;;!foo => [:frame foo '$value]
	     `(:frame ',arg '$value))
            ((for (/#args (length arg))
               (cond((= /#args 1)
                     ;; !(foo) => [:frame foo '$value]
                     ;; !(!foo) => (list ':frame !foo ''$value)
                     `(:frame ,(cond((and (listp (1st arg))
					  (eq (caar arg) 'exclamation))
				     (1st arg))
				    ((kwote (1st arg))))
			      '$value))
                    ((= /#args 2)
                     ;; !(foo bar) => [foo bar]
                     ;; !(!foo bar) => (list !foo 'bar)
                     ;;  etc.
                     `(,(cond((and (listp (1st arg))
				   (eq (caar arg) 'exclamation))
			      (1st arg))
			     ((kwote (1st arg))))
		       ,(cond((and (listp (2nd arg))
				   (eq (caadr arg) 'exclamation))
			      (2nd arg))
			     ((kwote (2nd arg))))
		       '$value)))))))

;;>>>> Improvements (maybe)/: !value => :value, !!value => :values


;;; Two quantifiers for use in requirements -- assume :values is bound and 
;;; map :value over the form.  Responsibility for mapping a requirement over 
;;; all values of a multi-valued slot is in the hands of the requirement itself.
;;; The function which applies requirements to values of a slot need only evaluate
;;; it in all cases -- having properly bound /:VALUES and /:VALUE.

(macro FORALL/? (form)
       (append '(forall :value :values) (cdr form)))

(macro EXISTS/? (form)
       (append '(exists :value :values) (cdr form)))


