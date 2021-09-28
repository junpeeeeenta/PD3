(in-package :pd3)

(defun null-string-p (s)
  (or (null s) (zerop (length s))))

(defparameter *keywords&stopwords* '(:the :a :some :all :every :action :actions :flow :flows :container :containers
                                     :parent :parents :child :childs :children :relevant :related :to 
                                     :diagram :value :values :edgeLabel :edgeLabels :memory :exit :quit :stop))

(defun ?? ()
  (format t "You are in interactive mode for PD3.~%")
  (format t "You can invoke PD3 commands without parens such as Linax/Unix commands.~%")
  (format t "For example, if you want to show all actions, type~%")
  (format t "show all actions.~%")
  (format t "The folowings are other commands.~%")
  (format t "-----------------------~%")
  (format t "show all containers~%")
  (format t "show all flows~%")
  (format t "show <ID>~%")
  (format t "show [the] action(s) related to <日本語>~%")
  (format t "show [the] flow(s) related to <日本語>~%")
  (format t "show [the] container(s) related to <日本語>~%")
  (format t "show all action values~%")
  (format t "verify112~%")
  (format t "verify113~%")
  (format t "verify1312~%")
  (format t "verify1313~%")
  (format t "verify1314~%")
  (format t "exit~%")
  (loop
    (print '>)(force-output)
    (let ((line (read-line *standard-input*)))
      (setq line (string-trim '(#\Space #\Tab #\. #\．) line))
      (let ((form (drawio:split-string line)))
        (setq form
              (cons (intern (string-upcase (car form)) :pd3)
                    (mapcar #'(lambda (str) (intern (string-upcase str) :keyword)) (cdr form))))
        (when (member (car form) '(exit quit stop)) (return-from ??))
        (print form)
        (print (eval (cons (car form) (mapcar #'(lambda (x) (list 'quote x)) (cdr form)))))
        ))))

(defun verify112 ()
  (setq it
  (loop for aflow in *flows*
      when (%verify112 aflow)
      collect aflow)))
(defun %verify112 (aflow)
  (etypecase aflow
    (cons (mapcar #'%verify112 aflow))
    (symbol (%verify112 (symbol-value aflow)))
    (flow (null-string-p (flow-value aflow)))))

(defun verify113 ()
  (setq it
  (loop for aflow in *flows*
      when (%verify113 aflow)
      collect aflow)))
(defun %verify113 (aflow)
  (etypecase aflow
    (cons (mapcar #'%verify113 aflow))
    (symbol (%verify113 (symbol-value aflow)))
    (flow (null (flow-target aflow)))))

(defun verify1312 ()
  (setq it
  (loop for anaction in *actions*
      when (%verify1312 anaction)
      collect anaction)))
(defun %verify1312 (anaction)
  (etypecase anaction
    (cons (mapcar #'%verify1312 anaction))
    (symbol (%verify1312 (symbol-value anaction)))
    (action (and (consp (action-parent anaction))(cdr (action-parent anaction))))))

(defun verify1313 ()
  (setq it
  (loop for action in *actions*
      when (%verify1313 action)
      collect action)))
(defun %verify1313 (action)
  (etypecase action
    (cons (mapcar #'%verify1313 action))
    (symbol (%verify1313 (symbol-value action)))
    (action (cond ((or (string= "[Start]" (action-value action))
                       (string= "[End]" (action-value action)))
                   nil)
                  ((string= (action-parent action) "1") nil)
                  ((action-parent action)
                   (cond ((not (consp (action-parent action)))
                          (let ((pa (action-parent action)))
                            (typecase pa
                              (string (let ((pa-sym (intern pa :pd3)))
                                        (cond ((boundp pa-sym)
                                               (cond ((typep (symbol-value pa-sym) 'container) nil)
                                                     (t action)))
                                              (t action))))
                              (symbol (cond ((boundp pa)
                                             (cond ((typep (symbol-value pa) 'container) nil)
                                                   (t action)))
                                            (t action)))
                              (t action))))
                         (t action)))
                  (t action)))))

;;;(defun sibling-p (action1 action2)
;;;  (when (symbolp action1) (setq action1 (symbol-value action1)))
;;;  (let ((parent1 (action-parent action1))
;;;        (parent2 (action-parent action2)))
;;;    (string= (string parent1) (string parent2))))

(defun input-output-relation-p (action1 action2)
  (intersection (action-outputs (symbol-value action1))
                (action-inputs (symbol-value action2))))
(defun diff-rank-p (action1 action2 &optional (rank1 0) (rank2 0))
  (when (stringp action1) (setq action1 (intern action1 :pd3)))
  (when (stringp action2) (setq action2 (intern action2 :pd3)))
  (cond ((eq action1 action2) nil) ;in the same rank
        ((null action1) t)
        ((null action2) t)
        (t (diff-rank-pp (action-parent (symbol-value action1))
                        (action-parent (symbol-value action2))
                        (+ rank1 1)
                        (+ rank2 1)))))
(defun diff-rank-pp (parent1 parent2 &optional (rank1 0) (rank2 0))
  (cond ((equal parent1 parent2) nil) ;in the same rank
        ((null parent1) t)
        ((null parent2) t)
        ((string= parent1 "1") t)
        ((string= parent2 "1") t)
        (t (when (stringp parent1) (setq parent1 (intern parent1 :pd3)))
           (when (stringp parent2) (setq parent2 (intern parent2 :pd3)))
         (diff-rank-p (container-parent (symbol-value parent1))
                        (container-parent (symbol-value parent2))
                        (+ rank1 1)
                        (+ rank2 1)))))

(defun verify1314 ()
  (loop for action1 in *actions* do
        (loop for action2 in *actions*
            when (violate-1314rule action1 action2)
            collect (cons action1 action2))))
(defun violate-1314rule (action1 action2)
  (unless (eq action1 action2)
    (cond ((input-output-relation-p action1 action2)
           (diff-rank-p action1 action2))
          ((input-output-relation-p action2 action1)
           (diff-rank-p action2 action1))
          )))

