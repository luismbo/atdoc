;;;; -*- mode:Lisp; package:user -*- ;;;;
;;;; Created: 10 December 1992
;;;; Copyright 1992 Patrick H. Winston and Berthold K. P. Horn.
;;;; All rights reserved.
;;;;
;;;; Version 1.0.1, copied from master file on 23 Apr 93       
;;;; 
;;;; This software is licensed by Patrick H. Winston and Berthold K. P. Horn
;;;; (licensors) for instructional use with the textbooks ``Lisp,'' by Patrick
;;;; H. Winston and Berthold K. P. Horn, and ``Artificial Intelligence,'' by
;;;; Patrick H. Winston.  Your are free to make copies of this software and
;;;; modify it for such instructional use as long as:
;;;; 1. You keep this notice intact.
;;;; 2. You cause any modified files to carry a prominent notice stating
;;;;    that you modified the files and the date of your modifications.
;;;; This software is licensed ``AS IS'' without warranty and the licensor
;;;; shall have no liability for any alleged defect or damages.

;;;; Changed by David Lichteblau 2007:
;;;;   - added package.lisp, put each file into its own package
;;;;   - removed the #-clos and #+gclisp conditionalizations
;;;;   - wrapped an eval-when around remove-specializers

(in-package :blocks-world-goals)

;;;; PROCEDURES

(defclass node ()
  ((parent :accessor node-parent :initform nil)
   (children :accessor node-children :initform nil)
   (action :accessor node-action :initform nil)))

(defvar *current-node* (make-instance 'node))

(defmethod attach-parent ((child node) (parent node))
  (setf (node-parent child) parent)     ;Attach parent to child.
  (setf (node-children parent)          ;Attach child to parent.
        (append (node-children parent)
                (list child))))

(defmethod attach-action ((node node) action)
  (setf (node-action node) action))

(defmacro define-history-method (name parameters &rest body)
  (declare (ignore body))
  `(defmethod ,name :around ,parameters
     (let* ((parent *current-node*)
	    (*current-node* (make-instance 'node))
	    (primary-method-value (call-next-method)))
       (when primary-method-value
	 (attach-parent *current-node* parent)
	 (attach-action *current-node*
			(list ',name ,@(remove-specializers parameters))))
       primary-method-value)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun remove-specializers (parameter-list)
    (mapcar #'(lambda (element) (if (listp element) (first element) element))
	    parameter-list)))

(define-history-method put-on ((object movable-block)
			       (support basic-block)))

(define-history-method get-rid-of ((object movable-block)))

(define-history-method make-space ((object movable-block)
				   (support basic-block)))

(define-history-method clear-top ((support load-bearing-block)))

(define-history-method move ((object movable-block)
                             (support basic-block)))

(defmethod print-object ((x basic-block) stream)
  (format stream "~a" (block-name x)))

(defun show-simple-tree (node &optional (indentation 0))
  (format t "~&~vt~a" 
          indentation
          (or (node-action node) 'top-of-tree))
  (dolist (node (node-children node))
    (show-simple-tree node (+ 2 indentation))))

(defun find-action (given-form &optional (node *current-node*))
  (let ((node-form (node-action node)))
    (if (equal given-form node-form)
        node
        (dolist (child (node-children node))
          (let ((result (find-action given-form child)))
            (when result (return result)))))))

(defmacro tell-why (name &rest parameters)
  `(tell-why-aux (list ',name ,@parameters)))

(defun tell-why-aux (given-action)
  (let ((node (find-action given-action)))
    (if (not (null node))
	(cond ((node-action (node-parent node))
	       (format t "~&I did ~a because I wanted to ~a."
		       given-action
		       (node-action (node-parent node))))
	      (t (format t "~&I did ~a because you told me to."
			 given-action)))
      (format t "~&I did not ~a." given-action))
    'done))
