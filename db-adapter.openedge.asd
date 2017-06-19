#|
  This file is a part of db-adapter.openedge project.
|#

(in-package :cl-user)
(defpackage db-adapter.openedge-asd
  (:use :cl :asdf))
(in-package :db-adapter.openedge-asd)

(defsystem db-adapter.openedge
  :version "0.1"
  :author ""
  :license ""
  :depends-on (#:db-adapter #:plain-odbc #:sxql #:trivial-features #:uiop)
  :components ((:module "src"
                :components
                ((:file "db-adapter.openedge"))))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq))))
