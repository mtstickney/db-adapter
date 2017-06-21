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
           #:table-exists-p
           #:column-exists-p
           #:index-exists-p
           #:get-table-area
           #:get-index-area
           #:area-exists-p
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
(defgeneric table-exists-p (adapter table &key schema)
  (:method ((adapter db-adapter) table &key (schema "PUB"))
    (if (execute adapter (sxql:select ((:raw "1"))
                           (sxql:from :information_schema.tables)
                           (sxql:where (:and (:= :table_name table)
                                             (:= :table_schema schema)))))
        t
        nil)))
(defgeneric column-exists-p (adapter table column &key schema)
  (:method ((adapter db-adapter) table column &key (schema "PUB"))
    (if (execute adapter (sxql:select ((:raw "1"))
                           (sxql:from :information_schema.columns)
                           (sxql:where (:and (:= :table_schema schema)
                                             (:= :table_name table)
                                             (:= :column_name column)))))
        t
        nil)))
;; Maybe-TODO: add some methods for (optional) savepoint support.
(defgeneric get-table-area (adapter table &key schema))
(defgeneric get-index-area (adapter table index &key table-schema index-schema))
(defgeneric area-exists-p (adapter area))
(defgeneric move-table (adapter table new-area &key schema))
(defgeneric move-index (adapter table index new-area &key schema))
(defgeneric move-table-and-indexes (adapter table new-area &key schema index-area))
