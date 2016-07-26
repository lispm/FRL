From z30083@tansei.cc.u-tokyo.junet Thu Jun  2 13:05:17 1988
Received: by aoyama.cc.aoyama.junet (3.2/6.3Junet-1.0)
	id AA07831; Thu, 2 Jun 88 13:05:16 JST
Received: by ccut.cc.u-tokyo.junet (5.51/6.3Junet-1.0/CSNET-JUNET)
	id AA03724; Thu, 2 Jun 88 12:44:12 JST
Received: by tansei.cc.u-tokyo.junet (4.12/6.3Junet-1.0)
	id AA07983; Thu, 2 Jun 88 12:44:30+0900
Date: Thu, 2 Jun 88 12:44:30+0900
From: z30083@tansei.cc.u-tokyo.junet (Masayuki Ida)
Return-Path: <z30083@tansei.cc.u-tokyo.junet>
Message-Id: <8806020344.AA07983@tansei.cc.u-tokyo.junet>
To: ida@aoyama.junet
Subject: fherit.l.frl
Status: RO


(include declar)
;;;-*-LISP-*-
;;; (version)
;;;*****************************************************************
;;;                       AKO INHERITANCE
;;;*****************************************************************


;; If no local datum present in given frame, search proceeds to AKO
;; parent frames until first one found with data under given key.
;;
;; NOTE/: since frame may have more than one AKO parent, all the data
;; found on each path are currently set unioned (on the basis of EQ)
;; together without comment
;; as to their source. (see FHERITAGE for more expensive inheritance
;; with source annotation). This is clearly not a great idea when there
;; are tangled networks of AKO, but it will do for the moment.  Having
;; a step by step operation of inheritance does not make sense, since extra frames
;; can always be meaninglessly interposed, which would then change
;; th inheritance behavior.
;;
;; For inheritance of $value data (values) their is a special check
;; at every level for the presence of $default data (default values).


;; NOTE/:  feature using comments
;;
;; (<datum> (FINHERIT/: CONTINUE)) 
;; 
;; is a comment to FINHERIT not to stop the inheritance lookup just because
;; it found a  value here, but to splice in the current datum to those
;; found further up.



;; Here is a suitable To-add datum (%*UNUSED*) to inhibit the
;; FINHERIT-style inheritance process.  (FDATUM-ONLY ...) will find it
;; and thus stop looking for a value.  Anything which evaluates to nil
;; is acceptable, since the convention  is that if FDATUM-ONLY returns
;; NIL, no values were found.  FDATUM/? however returns T.

(declare (special *unused*))

(setq *UNUSED* nil)


;; The value of *MARK-DATA-FN* is FRL's method for marking a list of data
;; as being in a particular frame.  If you don't want this feature, replace
;; it with a function which only returns the data.  By convention, any 
;; substitute function must accept two arguments/: Data (a list) and Frame.

(defun MARK-DATA-IN-FRAME (data frame)
       (for (fname (fname frame))
	(mapc (function (lambda (datum) (fput-datum-comment datum 'IN/: fname)))
	      data)))

(defmvar *mark-data-fn* 'mark-data-in-frame)


(defun FINHERIT (:frame :slot :facet)
       ;; returns list of datum structures under given frame, slot
       ;; and key by following AKO links.  Stops at the first frame along
       ;; the path which has any non-NIL data (before processing).
       ;; Note/: UNLIKE FINHERIT1 and FINHERIT2, this treats all keys alike
       ;; ie, it does linear inheritance.
       (for (local-props (fbucket (flistget :frame :slot :facet)))

            (cond ((or (null local-props)
                       (exists prop-str local-props
                               (fcomment/? prop-str 'finherit/: 'continue)))

                   ;; continue lookup only if there are no local data 
                   ;; -or- at least one says to continue.
                   (for (parents (*fvalues-only :frame 'ako))
                        ;; First, put together list of all immediate AKOs.
                        (cond ((null parents)
                               (funcall *mark-data-fn* local-props :frame))
                              ((apply 'union
                                 (cons local-props
				       (mapcar '(lambda (:frame)
                                                  (finherit :frame :slot :facet))
                                               parents)))))))

                  ;; otherwise return local data
                  (t (funcall *mark-data-fn* local-props :frame)))))

;;; *** Consider a ADDLIST and DELETELIST implementation of inheritance
;;; *** Consider marking the slot itself as to the type of the inheritance
;;;     desired.

(defun FINHERIT-ALONG (slot :frame :slot :facet)
       (for (local-props (fbucket (flistget :frame :slot :facet)))

            (cond ((or (null local-props)
                       (exists prop-str local-props
                               (fcomment/? prop-str 'finherit/: 'continue)))

                   ;; continue lookup only if there are no local data 
                   ;; -or- one says to continue.
                   (for (parents (*fvalues-only :frame slot))
                        ;; First, put together list of all immediate SLOTs.
                        (cond ((null parents)
                               (funcall *mark-data-fn* local-props :frame))
                              ((apply 'union
                                      (cons  local-props
                                             (mapcar
                                              '(lambda (:frame)
                                                       (finherit-along slot :frame :slot :facet))
                                               parents)))))))

                  ;; otherwise return local data and
		  ;; add the comment (IN/: <:frame>) to each. 
                  (t (funcall *mark-data-fn* local-props :frame)))))




(defun FINHERIT1 (:frame :slot :facet)
       ;; returns list of datum structures under given frame, slot
       ;; and key by following AKO links.
       ;; note special check on $default if key is $value
       (for (local-props (or (fbucket (flistget :frame :slot :facet))
                             (and (eq :facet '$value)
                                  (fbucket (flistget :frame :slot '$default)))))
            (cond ((or (null local-props)
                       (exists prop-str local-props
                               (fcomment/? prop-str 'finherit/: 'continue)))
                   (for (parents (*fvalues-only :frame 'ako))
                        (cond ((null parents) local-props)
                              ((apply 'union
                                      (cons  local-props
                                             (mapcar
                                              '(lambda (:frame)
                                                       (FINHERIT1 :frame :slot :facet))
                                               parents)))))))
                  (t (funcall *mark-data-fn* local-props :frame)))))

;;;>>>>>>>>>>>>>>>> rewrite finherit1 to collect default on the way up the ako chain
;;;                 making one pass up the AKO link sufficient.

(defun FINHERIT2 (:frame :slot :facet)
       ;; Looks first up the AKO as far as it can for a value, and if none is found,
       ;; repeats the process, looking exclusively in the $DEFAULT facet.
       (or (finherit :frame :slot :facet)
	   (and (eq :facet '$value) (finherit :frame :slot '$default))))


(defun FHERITAGE args
       ;; For that level of the frame reached via the indicator path
       ;; specified by the arguments, returns ALL information in the 
       ;; current frame and all its ancestors by way of the AKO link.
       ;;  (FHERITAGE frame slot key datum topic)
       ;;          where any number up to 5 levels are allowed.
       (for (h-str (list (cond ((= args 1) (fname (arg 1)))(t (arg args))))
             path (listify (- 1 args)))
            ;; start out with new dummy flist.
            (do-foreach :frame (ffamily (arg 1) 'ako)
             (for (flist (apply 'flistget (cons :frame path)))
              (do ((i (1-  args) (1- i))
                   (fdummy flist (list 'nil fdummy)))
                  ((zerop i)
                   (do-foreach :slot (fbucket fdummy)
                     (do-foreach :facet (fbucket :slot)
                       (funcall *mark-data-fn* (fbucket :facet) :frame)))
                   (do-foreach item (fbucket flist)
                     (fadd h-str item)))
                  (declare (fixnum i)))))
            h-str))

(defun FHERITAGE/? args
       ;; Like FHERITAGE, but returns non-NIL if ANY information exists in the heritage.
       (for (path (listify (- 1 args)))
         (exists :frame (ffamily (arg 1) 'ako)
		 (apply 'flistget (cons :frame path)))))


(defun FHERITAGE-SLOTS (frame)
       ;; returns a list of slot-names of the frame and all its parents.
       (setify       ;; Multiple AKO is possible, but compound slot names are legit.
         (mapcan '(lambda (f) (fslots f))   (ffamily frame 'ako))))



;;;            Access functions which return the entire heritage


(defun FDATA-HERITAGE (:frame :slot :facet)
       ;; DON'T make this a macro -- the bindings are used by fprocess-indics1.
       (fprocess-indics1 (fbucket (fheritage :frame :slot :facet))))



(defun FDATA-HERITAGE-ONLY (:frame :slot :facet)
       (findicators1 (fdata-heritage :frame :slot :facet)))


