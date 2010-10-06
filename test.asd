;;; -*- Lisp; encoding: utf-8 -*-

(asdf:defsystem #:gauche.test
  :name "gauche.test"
  :author "Toshihisa Abe <toshihisa.abe@gmail.com>"
  :licence "BSD"
  :description "A test framework: ported from Gauche (a scheme interpreter)"

  :components ((:file "test")))