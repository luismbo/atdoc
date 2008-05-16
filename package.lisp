;;; -*- mode: lisp; indent-tabs: nil; show-trailing-whitespace: t -*-

(in-package :cl-user)

(defpackage :atdoc
  (:use :cl)
  (:export #:generate-html-documentation
	   #:generate-latex-documentation
	   #:generate-documentation
	   #:extract-documentation))
