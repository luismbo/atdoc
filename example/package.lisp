;;; -*- mode: lisp; indent-tabs: nil; show-trailing-whitespace: t -*-

(in-package :cl-user)

(defpackage :blocks-world
  (:use :cl)
  (:export #:basic-block
	   #:block-name
	   #:block-width
	   #:block-height
	   #:block-position
	   #:block-supported-by

	   #:movable-block

	   #:load-bearing-block
	   #:block-support-for

	   #:brick
	   #:wedge
	   #:ball
	   #:table

	   #:*hand*
	   #:hand
	   #:hand-name
	   #:hand-position
	   #:hand-grasping

	   #:put-on
	   #:get-space
	   #:grasp
	   #:ungrasp
	   #:get-rid-of
	   #:make-space
	   #:clear-top
	   #:remove-support
	   #:add-support
	   #:move
	   #:find-space
	   #:intersections-p
	   #:top-location))

(defpackage :blocks-world-goals
  (:use :cl :blocks-world)
  (:export #:node
	   #:node-parent
	   #:node-children
	   #:node-action
	   #:*current-node*
	   #:attach-parent
	   #:attach-action
	   #:define-history-method
	   #:show-simple-tree
	   #:find-action
	   #:tell-why))
