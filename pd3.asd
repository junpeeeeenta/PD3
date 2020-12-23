(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :pd3-system)
    (defpackage :pd3-system
      (:use :common-lisp :asdf))))

(in-package :pd3-system)

(defsystem :pd3
  :name "pd3"
  :author "Seiji Koide <koide@ontolonomy.co.jp>"
  :maintainer"Seiji Koide <koide@ontolonomy.co.jp>"
  :version "0.0.1"
  :licence "MIT"
  :description "reader and chercker for pd3 file made by Drawio"
  :depends-on ("drawio" "xmlreader" "line-reader")
  :components
  ((:file "pd3")
   (:file "pd3inter"))
  :serial t)
