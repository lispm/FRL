(include declar)
;;; (version)
;;;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;;
;;;		FLIST UTILITY FUNCTIONS
;;;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


;; An FLIST is a recursive list structure defined as follows/:
;; 
;; 	<flist> /:/:= ( <indic> . <bucket> )
;; 
;;	<bucket> /:/:= ( <item> <item> ... )
;; 
;; 	<indic> /:/:=  <s-exp>
;; 
;;	<item> /:/:= <flist>
;;
;; This data structure is used to implement the frame data base.
;; 





;; 
;;	AUXILIARY FUNCTIONS FOR ONE-LEVEL FLISTS
;;

(defun FBUILD-ASSOC (ind flist)
       ;; returns the embedded flist under the given indicator,
       ;; building an empty flist if the indicator isn't already present.
       ;; Like FASSOC except guaranteed to return an flist, never nil.
       (cond ((fassoc ind flist))
             (((lambda (item)(nconc flist (ncons item)) item)(ncons ind)))))


(defun FADD (flist item)
       ;; is an auxiliary function for flist access functions.
       ;; It adds Item (which is an flist) to Flist.
       ;;
       ;; eg, (FADD '(f (s1 (k1))) '(s2 (k2))) => (f (s1 (k1))(s2 (k2)))
       ;;
       ;; This calls itself recursively and never loses information
       ;; in Flist (think of it as performing a MERGE!).
       ;; NOTE/: special case of messages in frames, which can be atomic.
       ;; Returns an flist headed by the indicator of Item.
       (do ((flist (cond ((atom item)
                          ;; Adding an atomic item.
                          ;; This is legal in a frame only in the comment flist.
                          (or (fassq item flist)
                              (nconc flist (ncons item))))
                         ((fbuild-assoc (findicator item) flist))))
            (item (fbucket item)(fbucket item)))
           ((null item) flist)
           (fadd flist (findicator item))))



;;
;;	BASIC FLIST ACCESS UTILITIES
;;

(defun FFRAME (flist)
       ;; massages the arguments to each of the following functions.
       ;; If "flist" is an atom, gets its frame property.
       (cond ((atom flist) (get flist 'frame))
	     (t flist)))

(declare (fixnum i))	;used in following functions as counter in DO.

(defun FLISTGET args
       ;;
       ;; (flistget <flist> <ind1> <ind2> ... )
       ;;
       ;; <flist> = flist structure or an atomic frame name
       ;; <ind1> to <indn> = optional indicator path in flist
       ;; 
       ;; returns embedded flist at end of the indicator path (i.e. CAR
       ;; of return value will be last indicator); or in the case of "getting" a
       ;; message in a frame, an s-expression;
       ;; or NIL if last indicator not present.
       (do ((i 2 (1+ i))
            (flist (fframe (arg 1))
		   (fassoc (arg i) flist)))
           ((or (null flist)(> i args)) flist)))

(defun FBUILD args
       ;;
       ;; (FBUILD <flist> <ind1> <ind2> ... )
       ;;
       ;; Like FGET, except guarantees that the indicator path exists in flist,
       ;; by building it if it doesn't.
       ;; Returns an flist whose indicator is the last argument like FGET
       ;; (but never returns NIL, of course).
       (do ((i 2 (1+ i))
            (flist (fframe (arg 1))			       ;can create a frame
                   (fbuild-assoc (arg i) flist)))
           ((> i args) flist)))


(defun FLISTPUT args
       ;; 
       ;; (flistput <flist> <item> <ind1> <ind2> ... )
       ;;
       ;; <flist> = flist structure or atom frame name into which item will go.
       ;; <item> = also an flist structure to be added into <flist>
       ;; <ind1> to <indn> = optional indicator path to point in flist
       ;;                    where item is to be added.
       ;; Returns flist.
       ;;
       ;; Note/: order of arguments reflects similarity 
       ;; to LISP PUT, but with extra indicators.
       (do ((i 3 (1+ i))
            (flist (fframe (arg 1))			       ;can create a frame
                   (fbuild-assoc (arg i) flist)))
           ((> i args)
            (fadd flist (arg 2))
            (arg 1))))





(defun FLISTCLEAR args
       ;;
       ;; (flistclear <flist> <ind1> <ind2> ... )
       ;;
       ;; <flist> = flist structure or atom frame name
       ;; <ind1> to <indn> = optional indicator path
       ;; Clears out bucket under last indicator, but leaves the indicator.
       ;; Returns flist.
       ;; NOTE/:  e.g. if you flistclear the value, the $value key remains.
       ;;        See FDELETE, which actually removes whole flist.
       (do ((i 2 (1+ i))
            (flist (fframe (arg 1))
                   (fassoc (arg i) flist)))
           ((or (null flist)(> i args))
            ;; if flist present, make it empty
            (and flist (rplacd flist nil))
            (arg 1))))
        
       
(defun FLISTDELETE args
       ;;
       ;; (flistdelete <flist> <ind1> <ind2> ... )
       ;;
       ;; <flist> = flist structure or atom frame name  to be modified
       ;; <ind1> = obligatory first indicator
       ;; <ind2> to <indn> = optional indicator path
       ;;
       ;; Deletes entire item under last indicator, including the indicator.
       ;; Returns flist.
       ;; (See FCLEAR)
       (do ((i 2 (1+ i))
            (flist (fframe (arg 1))
                   (fassoc (arg i) flist)))
           ((cond ((null flist))
                  ((= i args)(fdelete-assoc (arg i) flist) t))
            (arg 1))))


(defun FLISTREPLACE args
       ;;
       ;; (flistreplace <flist> <item> <ind1> <ind2> ... )
       ;;
       ;; <flist> = flist structure or atom frame name to be modified
       ;; <item> = flist to be replaced
       ;; <ind1> to <indn> = optional indicator path to where replacement is to take place
       ;; The item displaces all old information (ie, all existing items), as opposed
       ;; to FPUT, which does an FADD.
       (do ((i 3 (1+ i))
            (flist (fframe (arg 1))
                   (fbuild-assoc (arg i) flist)))
           ((> i args)
            (rplacd (fbuild-assoc (findicator (arg 2)) flist)
                    (fbucket (arg 2)))
            (arg 1))))

(declare (notype i))



