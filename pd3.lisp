(in-package :cl-user)

(defpackage pd3
  (:use #:cl #:common-lisp #:cl-ppcre)
  (:shadow #:some)
  (:export #:thing 
           #:arc #:arc-id #:arc-layer #:arc-value #:arc-source #:arc-target
           #:arc-sorceType #:arc-targetType 
           #:action #:container #:EP #:style-prop-exists?
           #:show #:it
           #:read-drawio-file
	         #:order-entities
           #:ensure-external
           #:show-arctype-nil)
  )

(in-package :pd3)

(defun mklist (x)
  "If <x> is a list, return it; otherwise return a singleton list, (<x>)."
  (declare (optimize (speed 3) (safety 0)))
  (if (listp x) x (list x)))

(defun mappend (fn &rest lists)
  "Apply fn to respective elements of list(s), and append results."
  (reduce #'append (apply #'mapcar fn lists) :from-end t))

(defun ensure-external (symbol &optional (package *package*))
  "ensures <symbol> to be external. If it is a string and such a 
   symbol does not exist, it is made."
  (when (not (null symbol))
    (multiple-value-bind (sym status) (intern (string symbol) package)
      (ecase status
        (:external (values sym status))
        (:internal (export sym package)
                   (values sym :external))
        (:inherited (error "Not Yet!"))
        ((nil)
         (export sym package)
         (values sym :external))))))

(defun match (source target &optional (start 0))
  "compares <source> string to <target> string starting <start> in <target>.
   and all characters in <source> are matched to <target> in order, returns true."
  (let ((result (mismatch source target :start2 start :test #'char=)))
    (or (null result)                ; just same string
        (= (length source) result))  ; source is included target and matched
    ))

;;;
;;;
;;;
(defvar *EP* nil)
(defvar *arcs* nil)
(defvar *actions* nil)
(defvar *containers* nil)
(defvar *entities* nil)
(defvar *ordered-entities* nil)
(defvar *mxCell-base-id* nil)

;;;
;;; command style Human User Interface for PD3
;;;

(defvar it nil "pronoun for anaphora")

(defun show (&rest args)
  (setq args (remove :the args))
  (case (car args)
    (:all (ecase (second args)
	    ((:action :actions) (if (equal (third args) :values)
				    (pprint (mapcar #'(lambda (action) (action-value (symbol-value action))) *actions*))
				    (%show *actions*)))
	    ((:arc :arcs) (%show *arcs*))
	    ((:container :containers) (%show *containers*))))
    ((:a :some) (ecase (second args)
		  ((:action :actions) (%show (first *actions*)))
		  ((:arc :arcs) (%show (first *arcs*)))
		  ((:container :containers)(%show (first *containers*)))))
    ((:action :actions)
     (let ((keyword (car (remove :to (remove :related (cdr args))))))
       (mapc #'(lambda (x) (describe (symbol-value x))) (find-by keyword *actions*))))
    ((:arc :arcs)
     (let ((keyword (car (remove :to (remove :related (cdr args))))))
       (mapc #'(lambda (x) (describe (symbol-value x))) (find-by keyword *arcs*))))
    ((:container :containers)
     (let ((keyword (car (remove :to (remove :related (cdr args))))))
       (mapc #'(lambda (x) (describe (symbol-value x))) (find-by keyword *containers*))))
    (otherwise (mapcar #'%show args)))
  (values))
(defun %show (arg)
  (typecase arg
    (null nil)
    (cons (loop for each in arg do (%show each)))
    (symbol (describe (symbol-value arg)))
    (otherwise (describe arg))))

(defun find-by (keyword from-list)
  (loop for x in from-list
     when (%find-by keyword x)
     collect it))
(defun %find-by (keyword x)
  (setq keyword (string keyword))
  (let ((first-char (char keyword 0))
        (value (etypecase (symbol-value x)
		 (action (action-value (symbol-value x)))
		 (arc (arc-value (symbol-value x)))
		 (container (container-value (symbol-value x))))))
    (let ((pos (position first-char value)))
      (when (and pos (match keyword value pos))
        x))))
;;;
;;; End of Interface
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro parse2integer (str)
    (let ((i (gensym)))
      `(let ((,i ,str))
         (when ,i (parse-integer ,i)))
      ))
  
  (defmacro style-prop-exists? (mxCell propstr)
    "returns two values, a list of <propstr> and the value in <style> of <mxCell>, and t.
   This macro returns nil and t, if the property exists but empty value,
   returns nil and nil, if the property does not exists."
    (let ((found (gensym)))
      `(let ((,found
	      (find-if #'(lambda (str) (drawio:starts-with-subseq ,(string propstr) str))
		       (drawio:split-string (drawio:mxCell-style ,mxCell) :item #\;))))
	 (if ,found
	     (values (drawio:split-string ,found :item #\=)
		     t)
	     (values nil nil)))))

  (defmacro style-prop=? (valstr mxCell propstr)
    "returns true if prop-value of <propstr> in <style> of <mxCell> equals to <valstr>."
    `(string=
      ,(string valstr)
      (second (drawio:split-string 
	       (find-if #'(lambda (str) (drawio:starts-with-subseq ,(string propstr) str))
			(drawio:split-string (drawio:mxCell-style ,mxCell) :item #\;))
	       :item #\=))))
  ) ; end of eval-when

(defun drawio-EP-p (mxCell)
  "returns true if <mxCell> in drawio/xml is an action."
  (style-prop-exists? mxCell "URI"))

(defun drawio-container-p (mxCell)
  "returns true if <mxCell> in drawio/xml is a container"
  (style-prop=? "container" mxCell "pd3type"))

(defun drawio-action-p (mxCell)
  "returns true if <mxCell> in drawio/xml is an action."
  (style-prop=? "action" mxCell "pd3type"))

(defun drawio-arc? (mxCell)
  "returns true if <mxCell> in drawio/xml is a arc."
  (style-prop=? "arrow" mxCell "pd3type"))

;;;
;;;
;;;

(defun retlieve-sourcepoint (mxCell)
  (assert (drawio-arc? mxCell))
  (let ((spoint (find-if #'(lambda (p) (string= "sourcePoint" (dio:mxPoint-as p)))
                         (dio:mxGeometry-points (dio:mxCell-geometry mxCell)))))
    (etypecase spoint
      (dio:mxGeometry (cons (dio:mxGeometry-x spoint) (cons (dio:mxGeometry-y spoint) nil)))
      (dio:mxPoint    (cons (dio:mxPoint-x spoint) (cons (dio:mxPoint-y spoint) nil))))))

(defun retlieve-targetpoint (mxCell)
  (assert (drawio-arc? mxCell))
  (let ((tpoint (find-if #'(lambda (p) (string= "targetPoint" (dio:mxPoint-as p)))
                         (dio:mxGeometry-points (dio:mxCell-geometry mxCell)))))
    (etypecase tpoint
      (dio:mxGeometry (cons (dio:mxGeometry-x tpoint) (cons (dio:mxGeometry-y tpoint) nil)))
      (dio:mxPoint    (cons (dio:mxPoint-x tpoint) (cons (dio:mxPoint-y tpoint) nil))))))

(defclass pd3:EP ()
  ((id :accessor EP-id :initarg :id :initform nil)
   (creator :accessor EP-creator :initarg :creator :initform nil)
   (title :accessor EP-title :initarg :title :initform nil)
   (description :accessor EP-description :initarg :description :initform nil)
   (epType :accessor EP-type :initarg :epType :initform nil)
   )
  )

(defclass pd3:action ()
  ((id :accessor action-id :initarg :id :initform nil)
   (layer :accessor action-layer :initarg :layer :initform nil)
   (value :accessor action-value :initarg :value :initform nil)
   (input :accessor action-input :initarg :input :initform nil)
   (output :accessor action-output :initarg :output :initform nil)
   (attribution :accessor action-attribution :initarg :attribution :initform nil)
   (expansion :accessor action-expansion :initarg :expansion :initform nil)
   (actionType :accessor action-type :initarg :actionType :initform nil)
   )
  )

(defclass pd3:arc ()
  ((id :accessor arc-id :initarg :id :initform nil)
   (layer :accessor arc-layer :initarg :layer :initform nil)
   (value :accessor arc-value :initarg :value :initform nil)
   (source :accessor arc-source :initarg :source :initform nil)
   (target :accessor arc-target :initarg :target :initform nil)
   (arcType :accessor arc-type :initarg :arcType :initform nil)
   )
  )

(defclass pd3:container ()
  ((id :accessor container-id :initarg :id :initform nil)
   (layer :accessor container-layer :initarg :layer :initform nil)
   (value :accessor container-value :initarg :value :initform nil)
   (output :accessor action-output :initarg :output :initform nil)
   (contraction :accessor container-contraction :initarg :contraction :initform nil)
   (member :accessor container-member :initarg :member :initform nil)
   (containerType :accessor container-type :initarg :containerType :initform nil)
   )
  )

;;;
;;;
;;;


(defun find-input (object-id)
  (remove-if-not #'(lambda (id) (string= '"information" (arc-type (symbol-value id))))
                 (remove-if-not #'(lambda (id) (string= object-id (arc-target (symbol-value id))))
                                *arcs*)))

(defun find-output (object-id)
  (remove-if-not #'(lambda (id) (string= '"information" (arc-type (symbol-value id))))
                 (remove-if-not #'(lambda (id) (string= object-id (arc-source (symbol-value id))))
                                *arcs*)))

(defun find-output-for-container (object-id)
  (car (remove-if-not #'(lambda (id) (string= '"hierarchization" (arc-type (symbol-value id))))
		      (remove-if-not #'(lambda (id) (string= object-id (arc-source (symbol-value id))))
				     *arcs*))))

(defun find-contraction (object-id)
  (car (remove-if-not #'(lambda (id) (string= object-id (action-expansion (symbol-value id))))
		      *actions*)))

(defun find-expansion (object-id)
  (car (mapcar #'(lambda (id) (arc-source (symbol-value id)))  
	       (remove-if-not #'(lambda (id) (eq 'hierarchization (arc-type (symbol-value id))))
			      (remove-if-not #'(lambda (id) (string= object-id (arc-target (symbol-value id))))
					     *arcs*)))))

(defun find-member (object-id)
  (remove-if-not #'(lambda (id) (string= object-id (action-attribution (symbol-value id))))
		 *actions*))

(defun find-action-type (style-value)
  (cond ((string= style-value '"ECDP") '"define problem")
	((string= style-value '"ECCAI") '"collect/analyze info")
	((string= style-value '"ECESI") '"evaluate/select info")
	((string= style-value '"ECGH") '"generate hypothesis")
	((string= style-value '"ECEX") '"execute")
	((string= style-value '"start") '"start")
	((string= style-value '"end") '"end")
	(t nil)))


(defun find-container-type (mxCell)
  (if (style-prop-exists? mxCell "containertype")
      (let ((style-value (second (style-prop-exists? mxCell "containertype"))))
	(apply #'(lambda (value)
		   (cond ((string= value '"specialization") '"specialization")
			 ((string= value '"whilebox") '"while box")
			 ((string= value '"whilecontainer") '"while container")))
	       style-value nil))))

(defun find-arc-value (mxCell mxCells-nonarcs)
  (let ((arc-value  (string (drawio:mxCell-value mxCell))))
    (apply #'(lambda (value)
	       (cond ((or (string= value '"") (string= value '"NIL"))
		      (find-edgeLabel (dio:mxCell-id mxCell) mxCells-nonarcs))
		     (t arc-value)))
	   arc-value nil)))

(defun find-edgeLabel (object-id mxCells-nonarcs)
  (remove-if-not #'(lambda (mxCell-nonarc) (eq mxCell-nonarc NIL)) mxCells-nonarcs)
  (let ((edgeLabels (remove-if-not #'(lambda (mxCell-nonarc)
				       (string= object-id (dio:mxCell-parent mxCell-nonarc)))
				   mxCells-nonarcs)))
    (if (eq edgeLabels NIL)
	NIL
	(dio:mxCell-value (car edgeLabels)))))

(defun replace-mxCell-value (value)
  (let ((strs (list '"&lt;" '"span" '"&gt;" '"/" '"br" '"style=&quot;" '"font-size: 14px;" '"font-size:14px" '"&quot;" '" ")))
    (dolist (str strs)
      (setq value (cl-ppcre:regex-replace-all str value ""))
      ))
  value)

(defun make-EP-from-mxCell (mxCell)
(when (drawio-EP-p mxCell)
  (let ((id (second (style-prop-exists? mxCell "URI")))
	(title (second (style-prop-exists? mxCell "title")))
	(creator (second (style-prop-exists? mxCell "creator")))
	(description (second (style-prop-exists? mxCell "description")))
	(epType (second (style-prop-exists? mxCell "eptype")))
	(id-sym nil))
      (let ((this-EP
	     (make-instance 'EP :id id :title title :creator creator :description description
			    :epType epType)))
	(setq id-sym (ensure-external (intern (EP-id this-EP) :pd3) :pd3))
	(pushnew id-sym *EP*)
	(set id-sym this-EP)
	(setf (EP-id this-EP) id-sym)
  id-sym))))

(defun make-arc-from-mxCell (mxCell mxCells-nonarcs)
  (when (or (style-prop-exists? mxCell "endArrow")
            (style-prop-exists? mxCell "edgeStyle"))
    (let ((id (drawio:mxCell-id mxCell))
          (layer (second (or (style-prop-exists? mxCell "layer")
                             (style-prop-exists? mxCell "pd3layer"))))
          (value (replace-mxCell-value (find-arc-value mxCell mxCells-nonarcs)))
          (source (drawio:mxCell-source mxCell))
          (target (drawio:mxCell-target mxCell))
          (entryX (second (drawio:split-string
                           (find-if #'(lambda (str) (drawio:starts-with-subseq "entryX" str))
                                    (drawio:split-string (drawio:mxCell-style mxCell) :item #\;))
                           :item #\=)))
          (entryY (second (drawio:split-string
                           (find-if #'(lambda (str) (drawio:starts-with-subseq "entryY" str))
                                    (drawio:split-string (drawio:mxCell-style mxCell) :item #\;))
                           :item #\=)))
          (exitX (second (drawio:split-string 
                          (find-if #'(lambda (str) (drawio:starts-with-subseq "exitX" str))
                                   (drawio:split-string (drawio:mxCell-style mxCell) :item #\;))
                          :item #\=)))
          (exitY (second (drawio:split-string
                          (find-if #'(lambda (str) (drawio:starts-with-subseq "exitY" str))
                                   (drawio:split-string (drawio:mxCell-style mxCell) :item #\;))
                          :item #\=)))
          (this-arc nil)
          (id-sym nil)
          )
					; (format t "~A~%" value)
      (setq this-arc
            (make-instance 'arc :id id :layer layer :value value :source source :target target
			   :arcType (cond 
				      ((and (not (string= entryX "1")) (string= entryY "0")) '"intention")
				      ((and (string= entryX "1") (string= entryY "0")) '"rationale")
				      ((and (string= entryX "1") (string= entryY "0.5")) '"annotation")
				      ((and (string= entryY "1") (string= entryX "0")) '"hierarchization")
				      ((and (string= entryY "1") (not (string= entryX "0"))) '"tool/knowledge")
				      ((or (string= entryX "0") (string= entryY "1")) '"information")
				      (t 'NIL))))
      (setq id-sym (ensure-external (intern (arc-id this-arc) :pd3) :pd3))
      (pushnew id-sym *arcs*)
      (set id-sym this-arc)
      (setf (arc-id this-arc) id-sym)
      id-sym)))

(defun make-action-from-mxCell (mxCell)
  (when (drawio-action-p mxCell)
    ;; (format t "~A~%" (second (style-prop-exists? mxCell "pd3action")))
    (let ((id (dio:mxCell-id mxCell))
          (layer (second (or (style-prop-exists? mxCell "layer")
                             (style-prop-exists? mxCell "pd3layer"))))
	  (action-type (find-action-type (second (style-prop-exists? mxCell "pd3action"))))
          (value  (replace-mxCell-value (dio:mxCell-value mxCell)))
          (attribution (unless (string= *mxCell-base-id* (dio:mxCell-parent mxCell))
			 (dio:mxCell-parent mxCell))))
      (let ((input (find-input id))
            (output (find-output id))
            (expansion (find-expansion id))
            (this-action nil)
            (id-sym nil))
        (setq this-action
              (make-instance 'action :id id :layer layer :value value :input input :output output
			     :attribution attribution :expansion expansion :actionType action-type))
        (setq id-sym (ensure-external (intern (action-id this-action) :pd3) :pd3))
        (pushnew id-sym *actions*)
        (set id-sym this-action)
        (setf (action-id this-action) id-sym)
        (loop for output in output
	   do (cond ((null (arc-source (symbol-value output)))
		     (setf (arc-source (symbol-value output)) id-sym))
		    ((string= (string id-sym) (string (arc-source (symbol-value output)))))
		    ((error "Output ~S of acion ~S mismatch to source of arc ~S." output id-sym output))))
        id-sym))))

(defun make-container-from-mxCell (mxCell)
  (when (style-prop-exists? mxCell "swimlane")
    (let ((id (drawio:mxCell-id mxCell))
          (layer (second (or (style-prop-exists? mxCell "layer")
                             (style-prop-exists? mxCell "pd3layer"))))
          (value (replace-mxCell-value (drawio:mxCell-value mxCell)))
	  (containerType (find-container-type mxCell)))
      (let ((output (find-output-for-container id))
            (contraction (find-contraction id))
            (member (find-member id))
            (id-sym nil))
        (let ((this-container
               (make-instance 'container :id id :layer layer :value value :output output
			      :contraction contraction :member member :containerType containerType)))
          (setq id-sym (ensure-external (intern (container-id this-container) :pd3) :pd3))
          (pushnew id-sym *containers*)
          (set id-sym this-container)
          (setf (container-id this-container) id-sym)
          id-sym)))))

(defun read-drawio-file (file)
  (setq *EP* nil)
  (setq *arcs* nil)
  (setq *actions* nil)
  (setq *containers* nil)
  (setq *ordered-entities* nil)
  (setq *entities* nil)
  (let* ((xml
          (line:with-open-file (stream file :external-format :utf-8)
            (drawio:read-xml-prolog stream)
            (drawio:read-mxfile stream)))
         (mxCells (drawio:query-path xml drawio:mxfile drawio:diagram drawio:mxGraphModel drawio:root))
         (mxCells-nonarcs (remove-if #'drawio-arc? mxCells))
         (these-arcs (loop for mxCell in mxCells
			if (make-arc-from-mxCell mxCell mxCells-nonarcs)
			collect it))
	 )
    (declare (ignore these-arcs))
    (setq *mxCell-base-id* (dio::mxCell-id (second mxCells-nonarcs)))
    (let ((mxCells-nonarcs-nonactions (remove-if #'drawio-action-p mxCells-nonarcs))
	  (these-actions (loop for mxCell in mxCells-nonarcs
			    if (make-action-from-mxCell mxCell)
			    collect it)))
      (declare (ignore these-actions))
      (let ((these-containers (loop for mxCell in mxCells-nonarcs-nonactions
				 if (make-container-from-mxCell mxCell)
				 collect it)))	
        (declare (ignore these-containers))
        (let ((this-EP (loop for mxCell in mxCells-nonarcs-nonactions
				 if (make-EP-from-mxCell mxCell)
				 collect it)))
         (declare (ignore this-EP))
         )	
	)))
  (set-arc-type)
  (order-entities)
  (format t "~A~%" "COMPLETE!")
  )

(defun set-arc-type ()
  (let ((arcs *arcs*))
    (let ((arcs-nontype (remove-if-not #'(lambda (arc) (eq (arc-type (symbol-value arc)) NIL)) arcs))
	  (actions *actions*)
	  (containers *containers*))
      (dolist (arc-nontype arcs-nontype)
	(let ((source-id (arc-source (symbol-value arc-nontype)))
	      (target-id (arc-target (symbol-value arc-nontype)))
	      (source-type nil)
	      (target-type nil)
	      (arc-nontype-id (arc-id (symbol-value arc-nontype))))
	  (dolist (action actions)
	    (when (string= source-id (action-id (symbol-value action))) (setq source-type 'action))
	    (when (string= target-id (action-id (symbol-value action))) (setq target-type 'action)))
	  (dolist (container containers)
            (when (string= source-id (container-id (symbol-value container))) (return (setq source-type 'container))))
	  (cond
	    ((and (equalp source-type 'action) (equalp target-type 'action)) (setf (slot-value (symbol-value (find-arc arc-nontype-id)) 'arcType) '"information"))
	    ((and (equalp source-type 'container) (equalp target-type 'action)) (setf (slot-value (symbol-value (find-arc arc-nontype-id)) 'arcType) '"hierarchization")))
	  )))))

(defun order-entities ()
  (setf *entities* nil)
  (let ((actions *actions*)
	(arcs *arcs*)
	(containers *containers*))
    (dolist (container containers) (setq *entities* (cons container *entities*)))
    (dolist (arc arcs) (setq *entities* (cons arc *entities*)))
    (dolist (action actions) (setq *entities* (cons action *entities*)))
    (%order-entities (find-start-box) *ordered-entities*)
    
    ))

(defun %order-entities (object &rest rest)
					;  (setq ordered-entities (append ordered-entities (list object)))
  (cond ((string= (type-of (symbol-value object)) 'action) ;;when entity is action 
	 (if (string= nil (action-attribution (symbol-value object)))
	     (cond ((string= '"end" (action-type (symbol-value object)))
		    (list (symbol-value object))
		    )	   
		   (t
		    (dolist (output (action-output (symbol-value object))) ;;when action doesn't have attribution
		      (append (list object) (%order-entities output))
		      (%order-entities (find-arc output)))))
	     (%order-entities (find-container (action-attribution (symbol-value object)))) ;;when action has attribution
	     ))
        ((string= (type-of (symbol-value object)) 'arc) ;;when entity is arc
	 (%order-entities (find-action (arc-target (symbol-value object)))))
	))

(defun find-action (id)

  (let ((actions *actions*))
    (car (remove-if-not #'(lambda (action) (string= id (action-id (symbol-value action)))) actions))))

(defun find-container (id)
  (let ((containers *containers*))
    (car (remove-if-not #'(lambda (container) (string= id (container-id (symbol-value container)))) containers))))

(defun find-arc (id)
  (let ((arcs *arcs*))
    (car (remove-if-not #'(lambda (arc) (string= id (string (arc-id (symbol-value arc))))) arcs))))

(defun show-arctype-nil ()
  (let ((arcs *arcs*))
    (show (remove-if-not #'(lambda (arc) (string= nil (arc-type (symbol-value arc)))) arcs))))

(defun find-start-box ()
  (let ((actions *actions*))
    (car (remove-if-not #'(lambda (action) (eq nil (action-attribution (symbol-value action)))) (remove-if-not #'(lambda (action) (string= '"topic" (action-layer (symbol-value action)))) (remove-if-not #'(lambda (action) (string= '"start" (action-type (symbol-value action)))) actions)
													       )))))
