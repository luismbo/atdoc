;;; -*- mode: lisp; indent-tabs: nil; show-trailing-whitespace: t -*-

(defpackage :atdoc-system
  (:use :asdf :cl))
(in-package :atdoc-system)

(defclass atdoc-source-file (cl-source-file) ())

#+sbcl
(defmethod perform :around ((o compile-op) (s atdoc-source-file))
  (handler-bind ((sb-ext:compiler-note #'muffle-warning))
    (let ((*compile-print* nil))
      (call-next-method))))

(asdf:defsystem :atdoc
    :default-component-class atdoc-source-file
    :components ((:file "package")
		 (:file "atdoc" :depends-on ("package")))
    :depends-on (:cxml :split-sequence :swank :xuriella :closer-mop))
