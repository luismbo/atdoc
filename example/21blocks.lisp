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
;;;;   - added defvars for *HAND* and TABLE as well as B1 to L8
;;;;   - moved 21blocks.dta into this file
;;;;   - added documentation

(in-package :blocks-world)

(defvar *hand*)

(defvar table nil "@unexport{}")
(defvar b1)
(defvar b2)
(defvar b3)
(defvar b4)
(defvar w5)
(defvar b6)
(defvar w7)
(defvar l8)


;;;; PROCEDURES

(defgeneric block-name (instance)
  (:documentation
   "@arg[instance]{a @class{basic-block}}
    @return{a symbol}
    @short{Returns the block's name, a symbol.}

    In the examples from the book, a global variable of this name is used
    to refer to @code{instance}."))

(defgeneric block-width (instance)
  (:documentation
   "@arg[instance]{a @class{basic-block}}
    @return{an integer}
    @short{Returns the block's width.}

    The size of a block is specified as width and height, and determines which
    parts of the world this block occupies.  No other objects can be
    placed to an overlapping position.

    @see{block-position}
    @see{block-height}"))

(defgeneric block-height (instance)
  (:documentation
   "@arg[instance]{a @class{basic-block}}
    @return{an integer}
    @short{Returns the block's height.}

    The size of a block is specified as width and height, and determines which
    parts of the world this block occupies.  No other objects can be
    placed to an overlapping position.

    @see{block-position}
    @see{block-width}"))

(defgeneric block-position (instance)
  (:documentation
   "@arg[instance]{a @class{basic-block}}
    @return{a list of two integers}
    @short{Returns the block's position.}

    The position of a block is specified as a list of its x and y coordinates,
    where the first axis runs along the table, and the second axis points
    upwards towards the hand.

    Together with the block's width and height, the position
    determines which
    parts of the world this block occupies.  No other objects can be
    placed to an overlapping position.

    @see{block-height}
    @see{block-width}
    @see{hand-position}"))

(defgeneric block-supported-by (instance)
  (:documentation
   "@arg[instance]{a @class{basic-block}}
    @return{nil, or a block}
    @short{Returns the block this instance has been placed onto.}

    All blocks except for the table sit on top of another block, which
    supports them.

    @see{block-support-for}"))

(defclass basic-block ()
  ((name :accessor block-name :initarg :name)
   (width :accessor block-width :initarg :width)
   (height :accessor block-height :initarg :height)
   (position :accessor block-position :initarg :position)
   (supported-by :accessor block-supported-by :initform nil))
  (:documentation
   "@short{The superclass of all objects in the Blocks World
    (not including the hand).}

    Subclasses of @code{basic-block} characterize different kinds of objects,
    and have different properties.

    They all have a name, given as
    @slot{block-name} and in the examples from the book, a global variable
    of that name is used to refer to them.

    Since this chapter is an explanation of CLOS, no specific
    constructor function is defined, and users may call @code{make-instance}
    directly.

    @see-slot{block-width}
    @see-slot{block-height}
    @see-slot{block-position}
    @see-slot{block-supported-by}"))

(defclass movable-block (basic-block) ()
  (:documentation
   "@short{The superclass of objects in the Blocks World that can be moved
    by the hand.}

    This class is mixed into all blocks except for the @class{table}."))

(defgeneric block-support-for (instance)
  (:documentation
   "@arg[instance]{a @class{load-bearing-block}}
    @return{a list of blocks}
    @short{Returns the blocks that have been placed onto this instance.}

    @see{block-supported-by}"))

(defclass load-bearing-block (basic-block) 
  ((support-for :accessor block-support-for :initform nil))
  (:documentation
   "@short{The superclass of objects in the Blocks World that other blocks
    can be placed onto.}

    This class is mixed into most blocks, except for the @class{wedge}
    and the @class{ball}.

    @see-slot{block-support-for}"))

(defclass brick (movable-block load-bearing-block) ()
  (:documentation
   "@short{A useful movable building block with a flat top.}

    Because this block has a flat top, it supports other blocks."))

(defclass wedge (movable-block) ()
  (:documentation
   "@short{An interesting movable building block.}

    Because this block doesn't have a flat top, it cannot
    support other blocks."))
(defclass ball (movable-block) ()
  (:documentation
   "@short{The block is a sphere.}

    Because this block doesn't have a flat top, it cannot
    support other blocks."))

(defclass table (load-bearing-block) ()
  (:documentation
   "@short{The table supporting the rest of the world.}

    The entire rest of the world sits on this table.  The table itself
    cannot be moved.

    For each world, this class is meant to be a singleton."))

(defgeneric hand-name (instance)
  (:documentation
   "@arg[instance]{a @class{hand}}
    @return{a symbol}
    @short{Returns the hand's name, a symbol.}

    @begin{implementation-node}
    The hand is always called @code{blocks-world::*hand*}.
    @end{implementation-node}"))

(defgeneric hand-position (instance)
  (:documentation
   "@arg[instance]{a @class{hand}}
    @return{a list of two integers}
    @short{Returns the hand's position.}

    The position of a hand is specified as a list of its x and y coordinates,
    where the first axis runs along the table, and the second axis points
    upwards towards the hand.

    @see{block-position}"))

(defgeneric hand-grasping (instance)
  (:documentation
   "@arg[instance]{a @class{hand}}
    @return{a @class{movable-block}, or nil}
    @short{Returns the block the hand is currently holding.} "))

(defclass hand ()
  ((name :accessor hand-name :initarg :name)
   (position :accessor hand-position :initarg :position)
   (grasping :accessor hand-grasping :initform nil))
  (:documentation
   "@short{The hand that moves the world.}

    This hand can be used to move every @class{movable-block}.

    @see-slot{hand-name}
    @see-slot{hand-position}
    @see-slot{hand-grasping}"))

(defmethod block-support-for ((object basic-block))
  nil)

(defgeneric put-on (object support)
  (:documentation
   "@arg[object]{a @class{movable-block}}
    @arg[support]{a @class{basic-block}}
    @return{a boolean}
    @short{Move block @code{object} onto block @code{support}.}

    Prints the steps taken and returns T or prints an error 
    message and returns nil.

    @see{get-space}
    @see{grasp}
    @see{move}
    @see{ungrasp}"))

(defmethod put-on ((object movable-block) (support basic-block))
  (if (get-space object support)
      (and (grasp object)
           (move object support)
           (ungrasp object))
      (format t "~&Sorry, there is no room for ~a on ~a."
              (block-name object)
              (block-name support))))

(defgeneric get-space (object support)
  (:documentation
   "@arg[object]{a @class{movable-block}}
    @arg[support]{a @class{basic-block}}
    @return{undocumented, but non-nil}
    @short{Find or make space on support for object.}

    @see{find-space}
    @see{make-space}"))

(defmethod get-space ((object movable-block) (support basic-block))
  (or (find-space object support)
      (make-space object support)))

(defgeneric grasp (object)
  (:documentation
   "@arg[object]{a @class{movable-block}}
    @return{t}
    @short{Grasps the block using the hand.}

    Makes sure to ungrasp the block currently grasped, if any.

    @see{ungrasp}
    @see{hand}"))

(defmethod grasp ((object movable-block))
  (unless (eq (hand-grasping *hand*) object)
    (when (block-support-for object) (clear-top object))
    (when (hand-grasping *hand*)
      (get-rid-of (hand-grasping *hand*)))
    (format t "~&Move hand to pick up ~a at location ~a."
            (block-name object)
            (top-location object))
    (setf (hand-position *hand*) (top-location object))
    (format t "~&Grasp ~a." (block-name object))
    (setf (hand-grasping *hand*) object))
  t)

(defgeneric ungrasp (object)
  (:documentation
   "@arg[object]{a @class{movable-block}}
    @return{a boolean}
    @short{Ungrasps the block if hand is holding it.}

    Returns t if successful, or nil if hand didn't hold this block.

    @see{grasp}
    @see{hand}"))

(defmethod ungrasp ((object movable-block))
  (when (block-supported-by object)
    (format t "~&Ungrasp ~a." (block-name object))
    (setf (hand-grasping *hand*) nil)
    t))

(defgeneric get-rid-of (object)
  (:documentation
   "@arg[object]{a @class{movable-block}}
    @return{unspecified}
    @short{Moves @code{object} onto the @class{table}.}

    @see{put-on}"))

(defmethod get-rid-of ((object movable-block))
  (put-on object table))

(defgeneric make-space (object support)
  (:documentation
   "@arg[object]{a @class{movable-block}}
    @arg[support]{a @class{basic-block}}
    @return{undocumented, but non-nil}
    @short{Make space on support for object.}

    Takes all necessary actions to make space available.

    @see{get-space}
    @see{find-space}"))

(defmethod make-space ((object movable-block) (support basic-block))
  (dolist (obstruction (block-support-for support))
    (get-rid-of obstruction)
    (let ((space (find-space object support)))
      (when space (return space)))))

(defgeneric clear-top (support)
  (:documentation
   "@arg[support]{a @class{load-bearing-block}}
    @return{nil}
    @short{Make space on top of this object.}

    Removes all blocks @code{support} is supporting.

    @see{get-rid-of}
    @see{block-support-for}"))

(defmethod clear-top ((support load-bearing-block))
  (dolist (obstacle (block-support-for support) t)
    (get-rid-of obstacle)))

(defgeneric remove-support (object)
  (:documentation
   "@arg[object]{a @class{movable-block}}
    @return{a boolean}
    @short{Note that @code{object} has been taken from @code{support}.}

    This function maintains the slots @fun{block-supported-by}
    and @fun{block-support-for}."))

(defmethod remove-support ((object movable-block))
  (let ((support (block-supported-by object)))
    (when support
      (setf (block-support-for support)
            (remove object (block-support-for support)))
      (setf (block-supported-by object) nil)
      t)))

(defgeneric add-support (object support)
  (:documentation
   "@arg[object]{a @class{movable-block}}
    @arg[support]{a @class{basic-block}}
    @return{a boolean}
    @short{Note that @code{object} has been put onto @code{support}.}

    This function maintains the slots @fun{block-supported-by}
    and @fun{block-support-for}."))

(defmethod add-support ((object movable-block)
                        (support basic-block))
  t)

(defmethod add-support ((object movable-block)
                        (support load-bearing-block))
  (push object (block-support-for support))
  (setf (block-supported-by object) support)
  t)

(defgeneric move (object support)
  (:documentation
   "@arg[object]{a @class{movable-block}}
    @arg[support]{a @class{load-bearing-block}}
    @return{a boolean}
    @short{Move block @code{object} onto block @code{support}.}

    This is a helper function for @fun{put-on}."))

(defmethod move :before ((object movable-block) ignored-parameter)
  (let ((support (block-supported-by object)))
    (when support
      (format t "~%Removing support relations between ~a and ~a."
          (block-name object) (block-name support))
      (setf (block-support-for support)
            (remove object (block-support-for support)))
      (setf (block-supported-by object) nil)
      t)))

(defmethod move :after ((object movable-block)
                        (support load-bearing-block))
  (format t "~%Adding support relations between ~a and ~a."
          (block-name object) (block-name support))
  (setf (block-support-for support)
        (cons object (block-support-for support)))
  (setf (block-supported-by object) support)
  t)

(defmethod move ((object movable-block) (support basic-block))
  (let ((newplace (get-space object support)))
    (format t "~&Move ~a to top of ~a at location ~a."
            (block-name object)
            (block-name support)
            newplace)
    (setf (block-position object) newplace))
  t)

(defmethod (setf block-position)
           :after
           (new-position (object movable-block))
  (setf (hand-position *hand*) (top-location object)))

(defun find-space (object support)
  "@arg[object]{a @class{movable-block}}
    @arg[support]{a @class{basic-block}}
    @return{undocumented or nil}
    @short{Find space on support for object.}

    Returns nil if no space could be found.

    @see{get-space}
    @see{make-space}"
  (dotimes (offset (+ 1 (- (block-width support)
                           (block-width object))))
    (unless (intersections-p object offset
                             (first (block-position support))
                             (block-support-for support))
      (return (list (+ offset (first (block-position support)))
                    (+ (second (block-position support))
                       (block-height support)))))))

(defun intersections-p (object offset base obstacles)
  (dolist (obstacle obstacles)
    (let* ((ls-proposed (+ offset base))
           (rs-proposed (+ ls-proposed (block-width object)))
           (ls-obstacle (first (block-position obstacle)))
           (rs-obstacle (+ ls-obstacle (block-width obstacle))))
      (unless (or (>= ls-proposed rs-obstacle)
                  (<= rs-proposed ls-obstacle))
        (return t)))))

(defun top-location (object)
  (list (+ (first (block-position object))
           (/ (block-width object) 2))
        (+ (second (block-position object))
           (block-height object))))

(defmethod print-object ((x basic-block) stream)
  (format stream "#<block ~a>" (block-name x)))


;;;; from 21blocks.dta

;;;; Created: 8 December 1992
(defparameter *blocks*
 (list 
  (make-instance 'table :name 'table :width 20 :height 0 :position '(0 0))
  (make-instance 'brick :name 'b1 :width 2 :height 2 :position '(0 0))
  (make-instance 'brick :name 'b2 :width 2 :height 2 :position '(2 0))
  (make-instance 'brick :name 'b3 :width 4 :height 4 :position '(4 0))
  (make-instance 'brick :name 'b4 :width 2 :height 2 :position '(8 0))
  (make-instance 'wedge :name 'w5 :width 2 :height 4 :position '(10 0))
  (make-instance 'brick :name 'b6 :width 4 :height 2 :position '(12 0))
  (make-instance 'wedge :name 'w7 :width 2 :height 2 :position '(16 0))
  (make-instance 'ball  :name 'l8 :width 2 :height 2 :position '(18 0))
 ))

(dolist (l *blocks*) (set (block-name l) l))

(dolist (l (remove table *blocks*)) 
  (pushnew l (block-support-for table))
  (setf (block-supported-by l) table))

(setf *hand* (make-instance 'hand :name '*hand* :position '(0 6)))
