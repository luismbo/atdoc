;;; -*- show-trailing-whitespace: t; indent-tabs: nil -*-

;;; Copyright (c) 2006,2007 David Lichteblau. All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :atdoc)

(defun function-arglist (fun)
  (swank::arglist fun))

(defun magic-namestring (file)
  (let ((atdoc-directory (asdf:component-pathname (asdf:find-system :atdoc))))
    (unless (and (stringp file) (char= (char file 0) #\.))
      (let* ((kind (pathname-type file))
	     (base (merge-pathnames (format nil "~A/" kind) atdoc-directory)))
	(setf file (merge-pathnames file base))))
    (namestring file)))

(defun xsltproc (stylesheet input output)
  (let* ((asdf::*verbose-out* (make-string-output-stream))
	 (code (asdf:run-shell-command
		"cd ~S && xsltproc ~S ~S >~S"
		(magic-namestring *default-pathname-defaults*)
		(magic-namestring stylesheet)
		(magic-namestring input)
		(magic-namestring output))))
    (unless (zerop code)
      (error "running xsltproc failed with code ~A [~%~A~%]"
	     code
	     (get-output-stream-string asdf::*verbose-out*)))))

(defun copy-file (a b &key (if-exists :error))
  (with-open-file (in a :element-type '(unsigned-byte 8))
    (with-open-file (out b
			 :direction :output
			 :if-exists if-exists
			 :element-type '(unsigned-byte 8))
      (let ((buf (make-array #x2000 :element-type '(unsigned-byte 8))))
	(loop
	   for pos = (read-sequence buf in)
	   until (zerop pos)
	   do (write-sequence buf out :end pos))))))

(defun generate-documentation
    (packages directory &key (index-title "No Title")
                             (heading "No Heading")
                             css
                             (logo nil)
                             (run-xsltproc t))
  (unless css
    (warn "no CSS stylesheet specified, falling back to default.css")
    (setf css "default.css"))
  (setf packages (mapcar #'find-package packages))
  (with-open-file (s (merge-pathnames ".atdoc.xml" directory)
		     :direction :output
		     :if-exists :rename-and-delete)
    (cxml:with-xml-output (cxml:make-octet-stream-sink s)
      (cxml:with-element "documentation"
	(cxml:attribute "logo" logo)
	(cxml:attribute "index-title" index-title)
	(cxml:attribute "css" "index.css")
	(cxml:attribute "heading" heading)
	(dolist (package packages)
	  (emit-package package packages)))))
  (when run-xsltproc
    (let ((*default-pathname-defaults* (merge-pathnames directory)))
      (copy-file (magic-namestring css) "index.css"
		 :if-exists :rename-and-delete)
      (xsltproc "macros.xsl" "html.xsl" ".atdoc.html.xsl.out")
      (xsltproc "cleanup.xsl" ".atdoc.xml" ".atdoc.tmp1")
      (xsltproc ".atdoc.html.xsl.out" ".atdoc.tmp1" ".atdoc.tmp2")
      (xsltproc "paginate.xsl" ".atdoc.tmp2" (merge-pathnames "index.html")))))

(defun munge-name (name)
  (format nil "~(~A~)__~(~A~)"
	  (package-name (symbol-package name))
	  (substitute #\_ #\* (symbol-name name))))

(defun name (name)
  (cxml:attribute "id" (munge-name name))
  (unexported-name name))

(defun unexported-name (name)
  (cxml:attribute "name" (string-downcase (symbol-name name)))
  (cxml:attribute "package"
		  (string-downcase (package-name (symbol-package name)))))

(defun symbol-status (symbol)
  (nth-value 1 (find-symbol (symbol-name symbol) (symbol-package symbol))))

(defun good-symbol-p (symbol other-packages)
  (and (find (symbol-package symbol) other-packages)
       (not (eq (symbol-status symbol) :internal))))

(defun random-name (name other-packages)
  (cxml:attribute "status" (symbol-name (symbol-status name)))
  (if (good-symbol-p name other-packages)
      (name name)
      (unexported-name name)))

(defun emit-package (package other-packages)
  (cxml:with-element "package"
    (cxml:attribute "name" (string-downcase (package-name package)))
    (cxml:attribute "id" (string-downcase (package-name package)))
    (emit-docstring package (or (documentation package t)
				"no documentation string found"))
    (cxml:with-element "symbols"
      (do-external-symbols (sym package)
	(when (boundp sym)
	  (emit-variable sym))
	(when (fboundp sym)
	  (if (macro-function sym)
	      (emit-macro sym)
	      (emit-function sym)))
	(when (find-class sym nil)
	  (emit-class (find-class sym) other-packages))))))

(defun emit-variable (name)
  (cxml:with-element "variable-definition"
    (name name)
    (emit-docstring name (documentation name 'variable))))

(defun emit-function (name)
  (cxml:with-element "function-definition"
    (name name)
    (cxml:with-element "lambda-list"
      (dolist (arg (function-arglist (symbol-function name)))
	(cxml:with-element "elt"
	  (cxml:text (write-to-string arg
				      :pretty t
				      :escape nil
				      :case :downcase)))))
    (emit-docstring name (documentation name 'function))))

(defun emit-macro (name)
  (cxml:with-element "macro-definition"
    (name name)
    (cxml:with-element "lambda-list"
      (dolist (arg (function-arglist (macro-function name)))
	(cxml:with-element "elt"
	  (cxml:text (write-to-string arg
				      :pretty t
				      :escape nil
				      :case :downcase)))))
    (emit-docstring name (documentation name 'function))))

(defun emit-class (class other-packages)
  (cxml:with-element "class-definition"
    (name (class-name class))
    (sb-pcl:finalize-inheritance class)
    (cxml:with-element "cpl"
      (dolist (super (cdr (sb-pcl:class-precedence-list class)))
	(cxml:with-element "superclass"
	  (random-name (class-name super) other-packages))))
    (cxml:with-element "subclasses"
      (labels ((recurse (c)
		 (dolist (sub (sb-pcl:class-direct-subclasses c))
		   (if (good-symbol-p (class-name sub) other-packages)
		       (cxml:with-element "subclass"
			 (random-name (class-name sub) other-packages))
		       (recurse sub)))))
	(recurse class)))
    (emit-docstring (class-name class) (documentation class t))))

(defun emit-docstring (package-designator str)
  (let ((package (etypecase package-designator
		   (symbol (symbol-package package-designator))
		   (package package-designator))))
    (when str
      (cxml:with-element "documentation-string"
	(cxml::maybe-emit-start-tag)
	(parse-docstring str (make-instance 'docstring-parser
			       :docstring-package package
			       :chained-handler cxml::*sink*))))))

(defun parse-docstring (str handler)
  (with-input-from-string (s str)
    (parse-docstring-1 s handler nil)))

(defun characters (handler str)
  (let ((lines (coerce (split-sequence:split-sequence #\newline str) 'vector))
	(ignore nil))
    (sax:characters handler (elt lines 0))
    (when (> (length lines) 1)
      (loop
	  for i from 1 below (1- (length lines))
	  for line = (elt lines i)
	  do
	    (cond
	      ((zerop (length (string-trim " " line)))
		(unless ignore
		  (sax:start-element handler nil "break" "break" nil)
		  (sax:end-element handler nil "break" "break"))
		(setf ignore t))
	      (t
		(sax:characters handler (string #\newline))
		(sax:characters handler line)
		(setf ignore nil))))
      (sax:characters handler (elt lines (1- (length lines)))))))

(defun parse-docstring-1 (stream handler close)
  (let ((out (make-string-output-stream)))
    (loop for c = (read-char stream nil) do
	  (cond
	    ((null c)
	      (when close
		(error "unexpected end of documentation string"))
	      (return))
	    ((eql c #\@)
	      (cond
		((eql (peek-char nil stream nil) #\})
		  (write-char (read-char stream) out))
		((eql (peek-char nil stream nil) #\@)
		  (write-char c out))
		(t
		  (characters handler (get-output-stream-string out))
		  (let ((name (read-delimited-string stream "[{")))
		    (when (equal name "end")
		      (read-char stream)
		      (unless
			  (equal (read-delimited-string stream "}" t) close)
			(error "invalid close tag"))
		      (return))
		    (parse-docstring-element stream handler name)))))
	    ((eql c #\})
	      (when (eq close t)
		(return))
	      (error "unexpected closing brace"))
	    (t
	      (write-char c out))))
    (characters handler (get-output-stream-string out))))

(defun read-delimited-string (stream bag &optional eat-limit)
  (let ((out (make-string-output-stream)))
    (loop
	for c = (read-char stream nil)
	do
	  (when (null c)
	    (error "unexpected end of documentation string"))
	  (when (find c bag)
	    (unless eat-limit
	      (unread-char c stream))
	    (return (get-output-stream-string out)))
	  (write-char c out))))

(defun parse-docstring-element (stream handler name)
  (let ((close t)
	(arg nil)
	(attrs '()))
    (when (eql (read-char stream) #\[)
      (setf arg (read-delimited-string stream "]" t))
      (unless (eql (read-char stream) #\{)
	(error "expected opening brace after closing bracket")))
    (when (equal name "begin")
      (setf name (read-delimited-string stream "}" t))
      (setf close name))
    (when arg
      (push (sax:make-attribute :qname name :value arg) attrs))
    (sax:start-element handler nil name name attrs)
    (parse-docstring-1 stream handler close)
    (sax:end-element handler nil name name)))

(defclass docstring-parser (cxml:sax-proxy)
    ((docstring-package :initarg :docstring-package
			:accessor docstring-package)
     (current-name :initform nil :accessor current-name)
     (current-attributes :accessor current-attributes)
     (current-text :accessor current-text)))

(defmethod sax:start-element ((handler docstring-parser) uri lname qname attrs)
  (cond
    ((or (equal qname "fun")
	 (equal qname "class")
	 (equal qname "variable")
	 (equal qname "slot")
	 (equal qname "see")
	 (equal qname "see-slot")
	 (equal qname "see-constructor"))
      (setf (current-name handler) qname)
      (setf (current-attributes handler) attrs)
      (setf (current-text handler) ""))
    (t
      (call-next-method))))

(defmethod sax:characters ((handler docstring-parser) data)
  (if (current-name handler)
      (setf (current-text handler)
	    (concatenate 'string (current-text handler) data))
      (call-next-method)))

(defmethod sax:end-element ((handler docstring-parser) uri lname qname)
  (let ((name (current-name handler)))
    (when (equal qname name)
      (let* ((next (cxml:proxy-chained-handler handler))
	     (attrs (current-attributes handler))
	     (text (current-text handler))
	     (munged-name
	      (munge-name
	       (let ((*package* (docstring-package handler)))
		 (read-from-string text)))))
	(push (sax:make-attribute :qname "id" :value munged-name) attrs)
	(sax:start-element next nil name name attrs)
	(sax:characters next text)
	(setf (current-name handler) nil))))
  (call-next-method))
