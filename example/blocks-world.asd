;;; -*- mode: lisp; indent-tabs: nil; show-trailing-whitespace: t -*-

(defpackage :blocks-world-system
  (:use :asdf :cl))
(in-package :blocks-world-system)

(defclass blocks-world-source-file (cl-source-file) ())

#+sbcl
(defmethod perform :around ((o compile-op) (s blocks-world-source-file))
  (handler-bind ((sb-ext:compiler-note #'muffle-warning))
    (let ((*compile-print* nil))
      (call-next-method))))

(asdf:defsystem :blocks-world
    :default-component-class blocks-world-source-file
    :components ((:file "package")
		 (:file "21blocks" :depends-on ("package"))
		 (:file "22goals" :depends-on ("21blocks")))
    :depends-on ())
