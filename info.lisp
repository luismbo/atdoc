;;; -*- show-trailing-whitespace: t; indent-tabs: nil -*-

;;; Copyright (c) 2008 David Lichteblau. All rights reserved.

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

(defun generate-info-documentation
    (packages directory
     &key name
          (title "No Title")
          (include-slot-definitions-p nil))
  "@arg[packages]{List of package designators.
     Documentation will be generated for these packages.}
   @arg[directory]{A pathname specifying a directory.
     All output files and temporary data will be written
     to this directory, which must already exist.}
   @arg[title]{This string will be used as the document's title.}
   @arg[include-slot-definitions-p]{A boolean.}
   @return{The pathname of the generated file documentation.pdf, or nil.}
   Generates TeX documentation for @code{packages}.

   With @code{run-tex-p} (the default), pdflatex is run automatically to
   generate a PDF file.

   With @code{include-slot-definition}, class documentation will include
   a list of direct slots."
  (setf name (or name
		 (when packages (string-downcase (car packages)))
		 "documentation"))
  (setf include-slot-definitions-p (and include-slot-definitions-p "yes"))
  (extract-documentation packages
			 directory
			 :title title
			 :name name
			 :include-slot-definitions-p include-slot-definitions-p)
  (let* ((*default-pathname-defaults* (merge-pathnames directory))
	 (pathname (merge-pathnames (concatenate 'string name ".info"))))
    (apply-stylesheet "macros.xsl" "info.xsl" ".info.xsl")
    (apply-stylesheet "macros.xsl" "info-paginate.xsl" ".info-paginate.xsl")
    (apply-stylesheet "cleanup.xsl" ".atdoc.xml" ".atdoc.tmp1")
    (apply-stylesheet ".info.xsl" ".atdoc.tmp1" ".atdoc.tmp2")
    (apply-stylesheet ".info-paginate.xsl" ".atdoc.tmp2" pathname)
    pathname))

(xpath-sys:define-xpath-function/eager :atdoc :stars (x)
  (map 'string (constantly #\*) (xpath:string-value x)))

(xpath-sys:define-xpath-function/eager :atdoc :date ()
  (multiple-value-bind (second minute hour date month year)
      (get-decoded-time)
    (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
	    year month date hour minute second)))

(xpath-sys:define-xpath-function/eager :atdoc :indent
    (data indentation initial-indentation)
  (setf indentation (round indentation))
  (setf initial-indentation (round initial-indentation))
  (let* ((n (length data))
	 (pos (position #\space data :test-not #'eql))
	 (need-whitespace-p nil)
	 (column indentation)
	 (width 72))
    (cond
      ((zerop n)
       "")
      (pos
       (with-output-to-string (s)
	 (loop repeat initial-indentation do (write-char #\space s))
	 (loop while (< pos n) do
	      (let* ((w (or (position #\space data :start (1+ pos)) n))
		     (next
		      (or (position #\space data :start w :test-not #'eql) n)))
		(when need-whitespace-p
		  (cond
		    ((< (+ column w (- pos)) width)
		     (write-char #\space s)
		     (incf column))
		    (t
		     (write-char #\newline s)
		     (loop repeat indentation do (write-char #\space s))
		     (setf column indentation))))
		(write-string data s :start pos :end w)
		(incf column (- w pos))
		(setf need-whitespace-p (< w n))
		(setf pos next)))))
      (t
       " "))))

(xuriella:define-extension-parser :atdoc "write-character" (node)
  (stp:with-attributes (code) node
    `(xslt:text ,(string (code-char (parse-integer code))))))

(xuriella:define-extension-parser :atdoc "avt" (node)
  (stp:do-children (child node)
    (when (typep child 'stp:text)
      (let ((avt1
	     (stp:make-element "avt1" "http://www.lichteblau.com/atdoc/")))
	(stp:replace-child node child avt1)
	(stp:append-child avt1 child))))
  `(progn ,@(xuriella:parse-body node)))

(xuriella:define-extension-parser :atdoc "avt1" (node)
  `(avt1 ,(stp:string-value node)))

(xuriella:define-extension-compiler avt1
    (str &environment env)
  (let ((thunk (xuriella::compile-avt str env)))
    (lambda (ctx)
      (xuriella::write-text (funcall thunk ctx)))))
