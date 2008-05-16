(asdf:operate 'asdf:load-op :blocks-world)
(asdf:operate 'asdf:load-op :atdoc)

;;
;; HTML (multipage)
;;
(let* ((base (asdf:component-pathname (asdf:find-system :blocks-world)))
       (output-directory (merge-pathnames "multi-page/" base)))
  (ensure-directories-exist output-directory)
  (atdoc:generate-html-documentation
   '(:blocks-world :blocks-world-goals)
   output-directory
   :index-title "Blocks World API reference"
   :heading "The Blocks World"
   :include-internal-symbols-p nil))

;;
;; HTML (single page)
;;
(let* ((base (asdf:component-pathname (asdf:find-system :blocks-world)))
       (output-directory (merge-pathnames "single-page/" base)))
  (ensure-directories-exist output-directory)
  (atdoc:generate-html-documentation
   '(:blocks-world :blocks-world-goals)
   output-directory
   :index-title "Blocks World API reference"
   :heading "The Blocks World"
   :single-page-p t))

;;
;; LaTeX/PDF
;;

(let* ((base (asdf:component-pathname (asdf:find-system :blocks-world)))
       (output-directory (merge-pathnames "tex/" base)))
  (ensure-directories-exist output-directory)
  (atdoc:generate-latex-documentation
   '(:blocks-world :blocks-world-goals)
   output-directory
   :title "The Blocks World"))
