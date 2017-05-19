;;;; package.lisp - Package definition

(defpackage #:my-little-fts
  (:use #:cl
        #:alexandria
        #:clack
        #:lack
        #:lass
        #:plump
        #:quri
        #:spinneret
        #:sqlite)
  (:export #:main))
