
(in-package :cl)

(cl:defpackage :pd3
  (:use :common-lisp)
  (:shadow #:some)
  (:export #:thing 
           #:flow #:flow-id #:flow-layer #:flow-value #:flow-source #:flow-target
           #:flow-sorceType #:flow-targetType 
           #:action #:container #:style-prop-exists?
           #:show #:it
           #:read-drawio-file
           #:ensure-external
	   
   ))

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

(defvar *flows* nil)
(defvar *actions* nil)
(defvar *containers* nil)

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
           ((:flow :flows) (%show *flows*))
           ((:container :containers) (%show *containers*))))
    ((:a :some) (ecase (second args)
                ((:action :actions) (%show (first *actions*)))
                ((:flow :flows) (%show (first *flows*)))
                ((:container :containers)(%show (first *containers*)))))
    ((:action :actions)
     (let ((keyword (car (remove :to (remove :related (cdr args))))))
       (mapc #'(lambda (x) (describe (symbol-value x))) (find-by keyword *actions*))))
    ((:flow :flows)
     (let ((keyword (car (remove :to (remove :related (cdr args))))))
       (mapc #'(lambda (x) (describe (symbol-value x))) (find-by keyword *flows*))))
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
                   (flow (flow-value (symbol-value x)))
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
;;;       (print ,found)
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

(defun drawio-container-p (mxCell)
  "returns true if <mxCell> in drawio/xml is a container"
  (style-prop=? "container" mxCell "pd3type"))

(defun drawio-action-p (mxCell)
  "returns true if <mxCell> in drawio/xml is an action."
  (style-prop=? "action" mxCell "pd3type"))

(defun drawio-flow? (mxCell)
  "returns true if <mxCell> in drawio/xml is a flow."
  (style-prop=? "arrow" mxCell "pd3type"))

;;;
;;;
;;;

(defun retlieve-sourcepoint (mxCell)
  (assert (drawio-flow? mxCell))
  (let ((spoint (find-if #'(lambda (p) (string= "sourcePoint" (dio:mxPoint-as p)))
                         (dio:mxGeometry-points (dio:mxCell-geometry mxCell)))))
    (etypecase spoint
      (dio:mxGeometry (cons (dio:mxGeometry-x spoint) (dio:mxGeometry-y spoint)))
      (dio:mxPoint    (cons (dio:mxPoint-x spoint) (dio:mxPoint-y spoint))))))

(defun retlieve-targetpoint (mxCell)
  (assert (drawio-flow? mxCell))
  (let ((tpoint (find-if #'(lambda (p) (string= "targetPoint" (dio:mxPoint-as p)))
                         (dio:mxGeometry-points (dio:mxCell-geometry mxCell)))))
    (etypecase tpoint
      (dio:mxGeometry (cons (dio:mxGeometry-x tpoint) (dio:mxGeometry-y tpoint)))
      (dio:mxPoint    (cons (dio:mxPoint-x tpoint) (dio:mxPoint-y tpoint))))))

(defclass pd3:action ()
  ((id :accessor action-id :initarg :id :initform nil)
   (layer :accessor action-layer :initarg :layer :initform nil)
   (value :accessor action-value :initarg :value :initform nil)
   (inputs :accessor action-inputs :initarg :inputs :initform nil)
   (outputs :accessor action-outputs :initarg :outputs :initform nil)
   (controls :accessor action-controls :initarg :controls :initform nil)
   (mechanisms :accessor action-mechanisms :initarg :mechanisms  :initform nil)
   (parent :accessor action-parent :initarg :parent :initform nil)
   (child :accessor action-child :initarg :child :initform nil)
   (actionType :accessor action-type :initarg :actionType :initform nil)
   )
  )

(defclass pd3:flow ()
  ((id :accessor flow-id :initarg :id :initform nil)
   (layer :accessor flow-layer :initarg :layer :initform nil)
   (value :accessor flow-value :initarg :value :initform nil)
   (source :accessor flow-source :initarg :source :initform nil)
   (target :accessor flow-target :initarg :target :initform nil)
   (sourceType :accessor flow-sourceType :initarg :sourceType :initform nil)
   (targetType :accessor flow-targetType :initarg :targetType :initform nil)
   (sourcepoint :accessor flow-sourcepoint :initform nil)
   (targetpoint :accessor flow-targetpoint :initform nil)
   )
  )

(defclass pd3:container ()
  ((id :accessor container-id :initarg :id :initform nil)
   (layer :accessor container-layer :initarg :layer :initform nil)
   (value :accessor container-value :initarg :value :initform nil)
   (parent :accessor container-parent :initarg :parent :initform nil)
   (child :accessor container-child :initarg :child :initform nil)
   (containerType :accessor container-type :initarg :containerType :initform nil)
   )
  )

;;;
;;;
;;;


(defun find-inputs (target-id)
  (remove-if-not #'(lambda (id) (eq 'input (flow-targetType (symbol-value id))))
                 (remove-if-not #'(lambda (id) (string= target-id (flow-target (symbol-value id))))
                                *flows*)))

(defun find-outputs (source-id)
  (remove-if-not #'(lambda (id) (eq 'output (flow-sourceType (symbol-value id))))
                 (remove-if-not #'(lambda (id) (string= source-id (flow-source (symbol-value id))))
                                *flows*)))

(defun find-controls (target-id)
  (remove-if-not #'(lambda (id) (eq 'control (flow-targetType (symbol-value id))))
                 (remove-if-not #'(lambda (id) (string= target-id (flow-target (symbol-value id))))
                                *flows*)))

(defun find-mechanisms (target-id)
  (remove-if-not #'(lambda (id) (eq 'mechanism (flow-targetType (symbol-value id))))
                 (remove-if-not #'(lambda (id) (string= target-id (flow-target (symbol-value id))))
                                *flows*)))

(defun find-parent (source-id)
  (remove-if-not #'(lambda (id) (eq 'child (flow-sourceType (symbol-value id))))
                 (remove-if-not #'(lambda (id) (string= source-id (flow-source (symbol-value id))))
                                *flows*)))

(defun find-child (target-id)
  (remove-if-not #'(lambda (id) (eq 'parent (flow-targetType (symbol-value id))))
                 (remove-if-not #'(lambda (id) (string= target-id (flow-target (symbol-value id))))
                                *flows*)))

(defun find-child-for-container (source-id)
  (remove-if-not #'(lambda (id) (string= source-id (action-parent (symbol-value id))))
		 *actions*))

(defun set-action-type (style-value)
  (cond ((string= style-value '"ECDP") '"difine problem")
	((string= style-value '"ECCD") '"collect data")
	((string= style-value '"ECAI") '"analyze information")
	((string= style-value '"ECEV") '"evaluate")
	((string= style-value '"ECLS") '"list-up solutions")
	((string= style-value '"ECDE") '"decide")
	((string= style-value '"ECEX") '"execute")
	(t nil)))

(defun set-container-type (mxCell)
  (if (style-prop-exists? mxCell "containertype")
    (let ((style-value (second (style-prop-exists? mxCell "containertype"))))
	 (apply #'(lambda (value)
	      (cond ((string= value '"specialization") '"specialization")
	       ((string= value '"whilebox") '"while box")
	       ((string= value '"whilecontainer") '"while container")))
		style-value nil))))

(defun make-flow-from-mxCell (mxCell)
  (when (or (style-prop-exists? mxCell "endArrow")
            (style-prop-exists? mxCell "edgeStyle"))
    (let ((id (drawio:mxCell-id mxCell))
          (layer (second (or (style-prop-exists? mxCell "layer")
                             (style-prop-exists? mxCell "pd3layer"))))
          (value (drawio:mxCell-value mxCell))
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
          (this-flow nil)
          (id-sym nil)
          )
      (setq this-flow
            (make-instance 'flow :id id :layer layer :value value :source source :target target
              :sourceType (cond ((string= exitX "1") 'output)
                                ((string= exitY "1") 'child)
                                ((or (null exitX) (null exitY)) 'output)
                                (t nil))
              :targetType (cond ((string= entryX "0") 'input)
                                ((string= entryY "0") 'intention)
				((and (string= entryX "1") (string= entryY "0")) 'rationale)
				((and (string= entryX "1") (string= entryY "0.5")) 'annotation)
                                ((and (string= entryY "1") (style-prop-exists? mxCell "edgeStyle"))
                                 'parent)
                                ((and (string= entryY "1") (style-prop-exists? mxCell "endArrow"))
                                 'tool/knowledge
				 )
                                ((and (or (null entryX) (null entryY)) (style-prop-exists? mxCell "edgeStyle"))
                                 'parent)
                                ((and (or (null entryX) (null entryY)) (style-prop-exists? mxCell "endArrow"))
                                 'input)
                                ((error "Cant happen 2")))
              ))
      (when (null source)
        (setf (flow-sourcepoint this-flow) (retlieve-sourcepoint mxCell)))
      (when (null target)
        (setf (flow-targetpoint this-flow) (retlieve-targetpoint mxCell)))
      (setq id-sym (ensure-external (intern (flow-id this-flow) :pd3) :pd3))
      (pushnew id-sym *flows*)
      (set id-sym this-flow)
      (setf (flow-id this-flow) id-sym)
      id-sym)))

(defun search-output-by-point (geometry)
  (let ((x (read (make-string-input-stream (dio:mxGeometry-x geometry)))) ; read as real number
        (y (read (make-string-input-stream (dio:mxGeometry-y geometry))))
        (width (read (make-string-input-stream (dio:mxGeometry-width geometry))))
        (height (read (make-string-input-stream (dio:mxGeometry-height geometry)))))
;;;    (format t "~S ~S ~S ~S" x y width height)
    (let ((output-x (+ x width))
          (output-y (+ y (floor (/ height 2)))))
      (find-if #'(lambda (aflow)
                   (let((p (flow-sourcepoint (symbol-value aflow))))
                     (and p (car p) (cdr p)
                          (< (abs (- output-x (read (make-string-input-stream (car p))))) 2) ; two means small epsilon
                          (< (abs (- output-y (read (make-string-input-stream (cdr p))))) 2))))
               *flows*))))

(defun make-action-from-mxCell (mxCell)
  (when (drawio-action-p mxCell)
    (let ((id (dio:mxCell-id mxCell))
          (layer (second (or (style-prop-exists? mxCell "layer")
                             (style-prop-exists? mxCell "pd3layer"))))
	  (type (set-action-type (second (style-prop-exists? mxCell "pd3action"))))
          (value (dio:mxCell-value mxCell))
          (parent (dio:mxCell-parent mxCell)))
      
      (let ((inputs (find-inputs id))
            (outputs (or (find-outputs id) (mklist (search-output-by-point (dio:mxCell-geometry mxCell)))))
            (controls (find-controls id))
            (mechanisms (find-mechanisms id))
            (child (find-child id))
            (this-action nil)
            (id-sym nil))
;;;        (format t "~%:id ~S :layer ~S :value ~S :inputs ~S :outputs ~S :controls ~S :mechanisms ~S"
;;;          id layer value inputs outputs controls mechanisms)
        (setq this-action
              (make-instance 'action :id id :layer layer :value value :inputs inputs :outputs outputs
                :controls controls :mechanisms mechanisms :parent parent :child child :actionType type))
        (setq id-sym (ensure-external (intern (action-id this-action) :pd3) :pd3))
        (pushnew id-sym *actions*)
        (set id-sym this-action)
        (setf (action-id this-action) id-sym)
        (loop for output in outputs
            do (cond ((null (flow-source (symbol-value output)))
                      (setf (flow-source (symbol-value output)) id-sym))
                     ((string= (string id-sym) (string (flow-source (symbol-value output)))))
                     ((error "Output ~S of acion ~S mismatch to source of flow ~S." output id-sym output))))
        id-sym))))

(defun make-container-from-mxCell (mxCell)
  (when (style-prop-exists? mxCell "swimlane")
    (let ((id (drawio:mxCell-id mxCell))
          (layer (second (or (style-prop-exists? mxCell "layer")
                             (style-prop-exists? mxCell "pd3layer"))))
          (value (drawio:mxCell-value mxCell))
	  (containerType (set-container-type mxCell)))
      (let ((parent (find-parent id))
            (child (find-child-for-container id))
            (id-sym nil))
;;;        (format t "~%:id ~S :layer ~S :value ~S :parent ~S :child ~S"
;;;          id layer value parent child)
        (let ((this-container
               (make-instance 'container :id id :layer layer :value value 
                 :parent parent :child child :containerType containerType)))
          (setq id-sym (ensure-external (intern (container-id this-container) :pd3) :pd3))
          (pushnew id-sym *containers*)
          (set id-sym this-container)
          (setf (container-id this-container) id-sym)
          id-sym)))))

;;;
;;;
;;;

(defun read-drawio-file (file)
  (let* ((xml
          (line:with-open-file (stream file :external-format :utf-8)
            (drawio:read-xml-prolog stream)
            (drawio:read-mxfile stream)))
         (mxCells (drawio:query-path xml drawio:mxfile drawio:diagram drawio:mxGraphModel drawio:root))
         (mxcells-nonflows (remove-if #'drawio-flow? mxCells))
         (these-flows (loop for mxCell in mxCells
                    if (make-flow-from-mxCell mxCell)
                    collect it))
         )
    (declare (ignore these-flows))
    ;; (format t "~%The number of flows is ~S." (length *flows*))
    ;; (format t "~%Do you want to show them?")
    ;; (when (y-or-n-p ) (loop for aflow in *flows* do (describe (symbol-value aflow))))
    (let ((these-actions (loop for mxCell in mxcells-nonflows
                       if (make-action-from-mxCell mxCell)
                       collect it)))
      (declare (ignore these-actions))
      ;; (format t "~%The number of actions is ~S." (length *actions*))
      ;; (format t "~%Do you want to show them?")
      ;; (when (y-or-n-p ) (loop for anaction in *actions* do (describe (symbol-value anaction))))
      (let ((these-containers (loop for mxCell in mxcells-nonflows
                            if (make-container-from-mxCell mxCell)
                            collect it)))
        (declare (ignore these-containers))
        ;; (format t "~%The number of containers is ~S." (length *containers*))
        ;; (format t "~%Do you want to show them?")
        ;; (when (y-or-n-p ) (loop for acontainer in *containers* do (describe (symbol-value acontainer))))
        ;; (format t "~%Type '(??)', if you want help.")
       

        ))))
  
#|
:cd /home/jumpei/common-lisp/pd3/
:ld pd3.asd
(asdf:load-system :pd3)
(in-package :pd3)
(read-drawio-file "PD3プラントエンジニア例題_作業プロセス記述.xml")
|#
