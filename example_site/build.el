;; Add ssg.el from root directory to the load path. You won't need to
;; do this, instead download the package as instructed in README.md.
(add-to-list 'load-path "../")
(require 'ssg)

(ssg-config
 :base-dir "."
 :out-dir "output/")


