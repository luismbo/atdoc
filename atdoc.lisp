;;; -*- show-trailing-whitespace: t; indent-tabs: nil -*-

;;; Copyright (c) 2006,2007,2008 David Lichteblau. All rights reserved.

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

#+sbcl
(defun run-shell-command (directory output command &rest args)
  ;; fixme: escape the namestrings properly, or use a function calling
  ;; exec rather system.
  (let* ((asdf::*verbose-out* (make-string-output-stream))
	 (code (asdf:run-shell-command
		"cd ~S && ~A~{ ~S~}~@[ >~S~]"
		directory
		command
		args
		output))
	 (output (get-output-stream-string asdf::*verbose-out*)))
    (unless (zerop code)
      (print output)
      (error "running ~A failed with code ~A [~%~A~%]"
	     command
	     code
	     output))
    output))

#+allegro
(defun run-shell-command (directory output command &rest args)
  ;; fixme: escape the namestrings properly, or use a function calling
  ;; exec rather system.
  (multiple-value-bind (stdout stderr exitcode)
      (excl.osi:command-output
       (format nil "~A~{ ~S~}~@[ >~S~]"
	       command
	       args
	       output)
       :directory directory
       :whole T)
    (declare (ignore stdout))
    (unless (zerop exitcode)
      (error "running ~A failed with code ~A [~%~A~%]"
	     command exitcode stderr))))

(defvar *stylesheets*)

(defun flush-cache ()
  (setf *stylesheets* (make-hash-table :test 'equal)))

(flush-cache)

(defun grovel-stylesheet-dependencies (namestring)
  (let ((dependencies '()))
    (klacks:with-open-source (source (cxml:make-source (pathname namestring)))
      (loop
	 for event = (klacks:peek-next source)
	 while event
	 do
	 (when (and (eq event :start-element)
		    (equal (klacks:current-uri source)
			   "http://www.w3.org/1999/XSL/Transform")
		    (or (equal (klacks:current-lname source) "import")
			(equal (klacks:current-lname source) "include")))
	   (push (make-pathname
		  :type "xsl"
		  :defaults (merge-pathnames
			     (klacks:get-attribute source "href")
			     namestring))
		 dependencies))))
    dependencies))

(defun find-stylesheet (namestring)
  (let ((cache-entry (gethash namestring *stylesheets*)))
    (unless (and cache-entry
		 (every (lambda (dependency)
			  (eql (file-write-date (car dependency))
			       (cdr dependency)))
			(cdr cache-entry)))
      (if cache-entry
	  (format t "~&Stylesheet has changed, reloading: ~A~%" namestring)
	  (format t "~&Loading stylesheet: ~A~%" namestring))
      (let ((dependencies (grovel-stylesheet-dependencies namestring)))
	(dolist (dependency dependencies)
	  (with-open-file (s (make-pathname :type "tmp" :defaults dependency)
			     :direction :output
			     :if-exists :rename-and-delete)
	    (xuriella:apply-stylesheet
	     (pathname (magic-namestring "macros.xsl"))
	     dependency
	     :output s)))
	(setf cache-entry
	      (cons (xuriella:parse-stylesheet
		     (xuriella:apply-stylesheet
		      (pathname (magic-namestring "macros.xsl"))
		      (pathname namestring)))
		    (mapcar (lambda (file)
			      (cons file (file-write-date file)))
			    (list* namestring
				   (magic-namestring "macros.xsl")
				   dependencies))))
	(setf (gethash namestring *stylesheets*)
	      cache-entry)))
    (car cache-entry)))

(defun apply-stylesheet-chain (input stylesheets output)
  (loop
     for input-designator = (pathname (magic-namestring input)) then result
     for (current-stylesheet . rest) on stylesheets
     for output-designator = (if rest nil (pathname (magic-namestring output)))
     for result = (xuriella:apply-stylesheet
		   (find-stylesheet (magic-namestring current-stylesheet))
		   input-designator
		   :output output-designator)))

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

(defvar *include-slot-definitions-p* nil)

(defun extract-documentation (packages directory
			      &rest keys
			      &key include-slot-definitions-p
			      &allow-other-keys)
  "@arg[packages]{List of package designators.
     Documentation will be generated for these packages.}
   @arg[directory]{A pathname specifying a directory.
     The output file will be written to this directory, which must already
     exist.}
   @arg[include-slot-definitions-p]{A boolean.}
   @arg[keys]{Extra parameters for stylesheets.}
   Extracts docstrings from @code{packages} and writes them in XML syntax
   to .atdoc.xml in the specified directory.

   With @code{include-slot-definitions-p}, class documentation will include
   a list of direct slots.

   Extra parameters will be inserted as attributes on the root element."
  (setf packages (mapcar #'find-package packages))
  (let ((*include-slot-definitions-p* include-slot-definitions-p))
    (with-open-file (s (merge-pathnames ".atdoc.xml" directory)
		       :element-type '(unsigned-byte 8)
		       :direction :output
		       :if-does-not-exist :create
		       :if-exists :rename-and-delete)
      (cxml:with-xml-output (cxml:make-octet-stream-sink s)
	(cxml:with-element "documentation"
	  (loop for (key value) on keys :by #'cddr do
	       (when value
		 (cxml:attribute
		     (format nil "~A"	;argh... string ./. simple-string
			     (string-downcase key))
		   value)))
	  (dolist (package packages)
	    (emit-package package packages)))))))

(defun generate-documentation (&rest args)
  "A deprecated alias for @fun{generate-html-documentation}."
  (warn "deprecated function GENERATE-DOCUMENTATION called, replaced by GENERATE-HTML-DOCUMENTATION")
  (apply #'generate-html-documentation args))

(defun generate-html-documentation
    (packages directory &key (index-title "No Title")
                             (heading "No Heading")
                             (css "default.css")
                             (logo nil)
                             (single-page-p nil)
                             (include-slot-definitions-p nil)
                             (include-internal-symbols-p t))
  "@arg[packages]{List of package designators.
     Documentation will be generated for these packages.}
   @arg[directory]{A pathname specifying a directory.
     All output files and temporary data will be written
     to this directory, which must already exist.}
   @arg[index-title]{This string will be
     used as the title of the main page, index.html.
     (Other pages will be named according to the object they are
     documenting.)}
   @arg[heading]{This string will be used as a visible title on top of
     every page.}
   @arg[logo]{Deprecated.}
   @arg[css]{A pathname or string pointing to a cascading stylesheet (CSS)
     file. This file will be copied to the target directory under the name
     index.css. If this argument is a string and does not start with a dot, it
     will be taken as namestring relative to the atdoc/css  directory.}
   @arg[single-page-p]{A boolean.}
   @arg[include-slot-definitions-p]{A boolean.}
   @arg[include-internal-symbols-p]{A boolean.}
   @return{The pathname of the generated file index.xml.}
   Generates HTML documentation for @code{packages}.

   With @code{single-page-p}, all documentation is assembled as a single
   page called index.html.  Otherwise, index.html will include only
   a symbol index and a summary of each package, with links to other pages.

   With include-slot-definitions-p, pages for symbols that are not exported
   will be included, so that documentation for exported symbols can safely
   refer to internal pages (but internal symbols will not be included in
   the symbol index automatically).  This option has no effect if
   single-page-p is enabled.

   With @code{include-slot-definition}, class documentation will include
   a list of direct slots."
  (setf include-slot-definitions-p (and include-slot-definitions-p "yes"))
  (setf include-internal-symbols-p (and include-internal-symbols-p "yes"))
  (setf single-page-p (and single-page-p "yes"))
  (extract-documentation packages
			 directory
			 :include-slot-definitions-p include-slot-definitions-p
			 :include-internal-symbols-p include-internal-symbols-p
			 :single-page-p single-page-p
			 :logo logo
			 :index-title index-title
			 :css "index.css"
			 :heading heading)
  (let ((*default-pathname-defaults* (merge-pathnames directory)))
    (copy-file (magic-namestring css) "index.css"
	       :if-exists :rename-and-delete)
    (copy-file (magic-namestring "header.gif") "header.gif"
	       :if-exists :rename-and-delete)
    (apply-stylesheet-chain ".atdoc.xml"
			    (list "cleanup.xsl"
				  (if single-page-p
				      "html-singlepage.xsl"
				      "html.xsl")
				  "paginate.xsl")
			    (merge-pathnames "index.html"))))

(defun generate-latex-documentation
    (packages directory
     &key (title "No Title")
          (run-tex-p "pdflatex")
          (include-slot-definitions-p nil))
  "@arg[packages]{List of package designators.
     Documentation will be generated for these packages.}
   @arg[directory]{A pathname specifying a directory.
     All output files and temporary data will be written
     to this directory, which must already exist.}
   @arg[title]{This string will be used as the document's title.}
   @arg[include-slot-definitions-p]{A boolean.}
   @arg[run-tex-p]{A boolean.}
   @return{The pathname of the generated file documentation.pdf, or nil.}
   Generates TeX documentation for @code{packages}.

   With @code{run-tex-p} (the default), pdflatex is run automatically to
   generate a PDF file.

   With @code{include-slot-definition}, class documentation will include
   a list of direct slots."
  (setf include-slot-definitions-p (and include-slot-definitions-p "yes"))
  (format t "Extracting docstrings...~%")
  (extract-documentation packages
			 directory
			 :title title
			 :include-slot-definitions-p include-slot-definitions-p)
  (format t "Generating .tex...~%")
  (let ((*default-pathname-defaults* (merge-pathnames directory)))
    (copy-file (magic-namestring "defun.tex") (merge-pathnames "defun.tex")
	       :if-exists :rename-and-delete)
    (apply-stylesheet-chain ".atdoc.xml"
			    (list "cleanup.xsl"
				  "latex.xsl")
			    (merge-pathnames "documentation.tex"))
    (let ((i 0))
      (labels ((latex1 ()
		 (format t "Running pdflatex (~D)...~%" (incf i))
		 (run-shell-command (magic-namestring directory)
				    nil
				    run-tex-p
				    "documentation"))
	       (latex ()
		 (loop while
		      (search "Rerun to get cross-references right"
			      (latex1)))))
	(when run-tex-p
	  (latex)
	  (format t "Running makeindex...~%")
	  (run-shell-command (magic-namestring directory)
			     nil
			     "makeindex"
			     "documentation.idx")
	  (latex)))
      (merge-pathnames "documentation.pdf"))))

(xpath-sys:define-extension :atdoc "http://www.lichteblau.com/atdoc/")

(xpath-sys:define-xpath-function/eager :atdoc :escape-latex-string (x)
  ;; fixme:
  ;; \ -> $\backslash$
  ;; & -> ???
  (setf x (cl-ppcre:regex-replace-all "([#$~_^{}%])"
				      (xpath:string-value x)
				      "\\\\\\1"))
  (setf x (cl-ppcre:regex-replace-all "&"
				      (xpath:string-value x)
				      "$\\\\&$")))

(xpath-sys:define-xpath-function/eager :atdoc :base-uri (x)
  (xpath-protocol:base-uri (xpath:first-node x)))

(xuriella:define-extension-group :atdoc "http://www.lichteblau.com/atdoc/")

(defun munge-name (name kind)
  (format nil "~(~A~)__~A__~(~A~)"
	  (package-name (symbol-package name))
	  kind
	  (cl-ppcre:regex-replace-all "[/*%]" (symbol-name name) "_")))

(defun name (name kind)
  (cxml:attribute "id" (munge-name name kind))
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

(defun random-name (name other-packages kind)
  (cxml:attribute "status" (symbol-name (symbol-status name)))
  (if (good-symbol-p name other-packages)
      (name name kind)
      (unexported-name name)))

(defun emit-package (package other-packages)
  (flet ((handle-symbol (sym)
	   (when (boundp sym)
	     (emit-variable sym))
	   (when (fboundp sym)
	     (if (macro-function sym)
		 (emit-macro sym)
		 (emit-function sym)))
	   (when (find-class sym nil)
	     (emit-class (find-class sym) other-packages))
	   (when (and (documentation sym 'type)
		      (not (find-class sym nil)))
	     (emit-type sym)))
	 (internalp (sym)
	   "Check whether SYM is internal to some package, but not external
            to any other documented package."
	   (and (loop
		   for package in (cons package other-packages)
		   thereis
		   (eq (nth-value 1 (find-symbol (symbol-name sym) package))
		       :internal))
		(loop
		   for package in (cons package other-packages)
		   never
		   (eq (nth-value 1 (find-symbol (symbol-name sym) package))
		       :external)))))
    (cxml:with-element "package"
      (cxml:attribute "name" (string-downcase (package-name package)))
      (cxml:attribute "id" (string-downcase (package-name package)))
      (emit-docstring package (or (documentation package t)
				  "no documentation string found"))
      (cxml:with-element "external-symbols"
	(do-external-symbols (sym package)
	  (handle-symbol sym)))
      (cxml:with-element "internal-symbols"
	(do-symbols (sym package)
	  (when (internalp sym)
	    (handle-symbol sym)))))))

(defun emit-variable (name)
  (cxml:with-element "variable-definition"
    (name name "variable")
    (emit-docstring name (documentation name 'variable))))

(defun emit-type (name)
  (cxml:with-element "type-definition"
    (name name "type")
    (emit-docstring name (documentation name 'type))))

(defun emit-function (name)
  (cxml:with-element "function-definition"
    (name name "fun")
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
    (name name "fun")
    (cxml:with-element "lambda-list"
      (dolist (arg (function-arglist (macro-function name)))
	(cxml:with-element "elt"
	  (cxml:text (write-to-string arg
				      :pretty t
				      :escape nil
				      :case :downcase)))))
    (emit-docstring name (documentation name 'function))))

(defun emit-slot (slot-def)
  (cxml:with-element "slot"
    (name (closer-mop:slot-definition-name slot-def) "slot")
    (cxml:attribute "allocation" (munge-name (closer-mop:slot-definition-allocation slot-def) "symbol"))
    (cxml:attribute "type" (format nil "~A" (closer-mop:slot-definition-type slot-def))) ;; may be a complicated typespec
    (cxml:with-element "initargs"
      (dolist (ia (closer-mop:slot-definition-initargs slot-def))
	(cxml:with-element "initarg" (name ia "symbol"))))
    (cxml:with-element "readers"
      (dolist (reader (closer-mop:slot-definition-readers slot-def))
	(cxml:with-element "reader" (name reader "symbol"))))
    ;; FIXME: writer methods will be of the form (setf name) which breaks in munge-name
    ;;     (cxml:with-element "writers"
    ;;       (dolist (writer (closer-mop:slot-definition-writers slot-def))
    ;; 	(cxml:attribute "writer" (munge-name writer "writer"))))
    (emit-docstring (closer-mop:slot-definition-name slot-def)
		    (documentation slot-def T))))

(defun emit-class (class other-packages)
  (cxml:with-element "class-definition"
    (name (class-name class) "class")
    #+sbcl (sb-pcl:finalize-inheritance class)
    #+allegro (unless (typep class 'structure-class)
		(aclmop:finalize-inheritance class))
    (cxml:with-element "cpl"
      (dolist (super (cdr #+sbcl (sb-pcl:class-precedence-list class)
			  #+allegro (aclmop:class-precedence-list class)))
	(cxml:with-element "superclass"
	  (random-name (class-name super) other-packages "class"))))
    (cxml:with-element "subclasses"
      (labels ((recurse (c)
		 (dolist (sub #+sbcl (sb-pcl:class-direct-subclasses c)
			      #+allegro (aclmop:class-direct-subclasses c))
		   (if (good-symbol-p (class-name sub) other-packages)
		       (cxml:with-element "subclass"
			 (random-name (class-name sub) other-packages "class"))
		       (recurse sub)))))
	(recurse class)))
    (when (and *include-slot-definitions-p*
	       (not (typep class 'structure-class)))
      (cxml:with-element "direct-slots"
	(dolist (slot (closer-mop:class-direct-slots class))
	  (emit-slot slot))))
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
		  (let ((name (read-delimited-string stream "[{ :")))
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
	(attrs '())
	(first-char (read-char stream)))
    (when (eql first-char #\[)
      (setf arg (read-delimited-string stream "]" t))
      (setf first-char (read-char stream)))
    (case first-char
      (#\{)
;;;       (#\space)
;;;       (#\: )
      (t (error "expected opening brace, space, or colon")))
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
     (current-kind :accessor current-kind)
     (current-attributes :accessor current-attributes)
     (current-text :accessor current-text)))

(defmethod sax:start-element ((handler docstring-parser) uri lname qname attrs)
  (declare (ignore lname uri))
  (cond
    ((or (equal qname "fun")
	 (equal qname "class")
	 (equal qname "type")
	 (equal qname "variable")
	 (equal qname "slot")
	 (equal qname "see")
	 (equal qname "see-slot")
	 (equal qname "see-constructor")
	 (equal qname "see-condition"))
      (setf (current-name handler) qname)
      (setf (current-kind handler)
	    (case (intern qname :atdoc)
	      ((|fun| |class| |type| |variable|) qname)
	      ((|see| |see-slot| |slot|) "fun")
	      (|see-constructor| "fun")
	      (|see-condition| "class")))
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
  (declare (ignore lname uri))
  (let ((name (current-name handler)))
    (block give-up
      (when (equal qname name)
	(let* ((next (cxml:proxy-chained-handler handler))
	       (attrs (current-attributes handler))
	       (text (current-text handler))
	       (munged-name
		(handler-case
		    (munge-name
		     (let ((*package* (docstring-package handler)))
		       (read-from-string text))
		     (current-kind handler))
		  (error (c)
		    (warn "ignoring ~A" c)
		    nil))))
	  (when munged-name
	    (push (sax:make-attribute :qname "id" :value munged-name) attrs))
	  (sax:start-element next nil name name attrs)
	  (sax:characters next text)
	  (setf (current-name handler) nil)))))
  (call-next-method))
