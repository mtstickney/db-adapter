(defpackage #:db-adapter
  (:use #:cl)
  (:export #:db-adapter
           #:connect
           #:disconnect
           #:reconnect
           #:activep
           #:execute
           #:exec-update
           #:start-transaction
           #:commit
           #:rollback
           #:get-table-area
           #:get-index-area
           #:move-table
           #:move-index
           #:move-table-and-indexes))
(in-package #:db-adapter)

(defclass db-adapter () ())

(defgeneric connect (adapter))
(defgeneric disconnect (adapter))
(defgeneric reconnect (adapter))
(defgeneric activep (adapter))
(defgeneric execute (adapter query))
(defgeneric exec-update (adapter query))
(defgeneric start-transaction (adapter))
(defgeneric commit (adapter))
(defgeneric rollback (adapter))
;; Maybe-TODO: add some methods for (optional) savepoint support.
(defgeneric get-table-area (adapter table &key schema))
(defgeneric get-index-area (adapter table index &key table-schema index-schema))
(defgeneric move-table (adapter table new-area &key schema))
(defgeneric move-index (adapter table index new-area &key schema))
(defgeneric move-table-and-indexes (adapter table new-area &key schema index-area))
