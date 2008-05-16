(asdf:operate 'asdf:load-op :atdoc)

;;
;; HTML (single page)
;;
(let* ((base (asdf:component-pathname (asdf:find-system :atdoc)))
       (output-directory (merge-pathnames "doc/api/" base)))
  (ensure-directories-exist output-directory)
  (atdoc:generate-html-documentation
   '(:atdoc)
   output-directory
   :index-title "Atdoc API reference"
   :heading "Atdoc"
   :single-page-p t))
