;;; -*- mode: lisp; indent-tabs: nil; show-trailing-whitespace: t -*-

(in-package :cl-user)

(defpackage :atdoc
  (:use :cl)
  (:export #:generate-html-documentation
	   #:generate-latex-documentation
	   #:generate-documentation
	   #:extract-documentation)
  (:documentation
   "@a[http://www.lichteblau.com/atdoc/]{Atdoc}
    generates documentation for Common Lisp packages.

    It extracts documention strings
    written using a custom markup language and generates HTML pages or
    TeX documents.

    @begin[Generating formatted documentation]{section}
    Only two functions in atdoc will be called by the casual user,
    one for HTML output, and one for LaTeX output.  There is also an
    older function called generate-documentation, which in now an
    alias for generate-html-documentation.

    @aboutfun{generate-html-documentation}
    @aboutfun{generate-latex-documentation}
    @aboutfun{generate-documentation}
    @end{section}

    @begin[Generating unformatted XML]{section}
    Power users might want to extract docstrings into XML and then
    send that XML through their own XSLT stylesheets.  The following
    function can be used for that purpose.

    @aboutfun{extract-documentation}
    @end{section}"))
