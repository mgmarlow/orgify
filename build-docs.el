;;; build-docs.el  -*- lexical-binding: t; -*-

(add-to-list 'load-path ".")
(require 'orgify)

(orgify-build
 :base-dir "docs/"
 :static-dir "docs/public/"
 :out-dir "output/")
