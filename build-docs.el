(add-to-list 'load-path ".")
(require 'ssg)

(ssg-build
 :base-dir "docs/"
 :out-dir "output/")
