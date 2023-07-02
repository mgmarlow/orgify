(add-to-list 'load-path ".")
(require 'ssg)

(ssg-build
 :base-dir "docs/"
 :static-dir "docs/public/"
 :out-dir "output/")
