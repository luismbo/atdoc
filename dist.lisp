(asdf:operate 'asdf:load-op :atdoc)
(asdf:operate 'asdf:load-op :blocks-world)

(atdoc:generate-html-documentation
 '(:atdoc)
 (merge-pathnames
  "doc/atdoc/"
  (asdf:component-relative-pathname (asdf:find-system :atdoc)))
 :heading "atdoc")

