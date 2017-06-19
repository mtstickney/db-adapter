(in-package :cl-user)
(defpackage db-adapter.openedge
  (:use #:cl #:db-adapter)
  (:export #:openedge-adapter))
(in-package :db-adapter.openedge)

;;; Db-adapter API


;;; OpenEdge Adapter
(defclass openedge-adapter (db-adapter)
  ((connection :initarg :connection :initform nil :accessor raw-connection)
   (host :initarg :host :accessor db-host)
   (port :initarg :port :accessor db-port)
   (database-name :initarg :db-name :accessor db-name)
   (database-path :initarg :db-path :accessor db-path)
   (username :initarg :user :accessor username)
   (password :initarg :pass :accessor password)
   (isolation-level :initarg :isolation :accessor isolation-level)
   (driver :initarg :driver :accessor driver-name)
   (workarounds :initarg :workarounds :accessor connection-workarounds)
   (default-schema :initarg :schema :accessor default-schema))
  (:default-initargs
   :isolation nil
    :schema "PUB"
    :workarounds "1048576"))

(defmethod initialize-instance :after ((instance openedge-adapter) &key &allow-other-keys)
  (let ((path (db-path instance)))
    (unless (and (slot-boundp instance 'database-name) (db-name instance))
      (setf (db-name instance)
            (pathname-name (if (stringp path)
                               (parse-namestring path)
                               path))))))

;; TODO: This belongs in a proper statement implementation, not here
;; (and not as a regular function).
(defstruct raw-query
  query
  params)

(defun raw-query (query &rest params)
  (make-raw-query :query query :params params))

(defmethod sxql:yield ((obj raw-query))
  (values (raw-query-query obj)
          (raw-query-params obj)))

(defun set-schema-statement (adapter schema)
  (declare (ignore adapter))
  ;; FIXME: broken, injectable.
  (raw-query (format nil "SET SCHEMA '~A'" schema)))

(defparameter +quote-char+ #\")

(defun quote-ident (ident)
  (check-type ident keyword)
  (let ((sxql:*quote-character* +quote-char+))
    (nth-value 0 (sxql:yield (sxql.operator:detect-and-convert ident)))))

(defun connection-params (adapter)
  (flet ((iso-level-val (level)
           (etypecase level
             ((or string null) level)
             (integer (write-to-string level))
             (keyword (ecase level
                        (:read-uncommitted "READ UNCOMMITTED")
                        (:read-committed "READ COMMITTED")
                        (:repeatable-read "REPEATABLE READ")
                        (:serializable "SERIALIZABLE"))))))
    (loop for param on (list
                        :host (db-host adapter)
                        :port (if (stringp (db-port adapter))
                                  (db-port adapter)
                                  (write-to-string (db-port adapter)))
                        :db (db-name adapter)
                        :uid (username adapter)
                        :pwd (password adapter)
                        :dil (iso-level-val (isolation-level adapter))
                        :driver (driver-name adapter)
                        :workarounds (connection-workarounds adapter))
       by #'cddr
       when (second param)
       collect (first param)
       when (second param)
       collect (second param))))

(defmethod connect ((adapter openedge-adapter))
  (when (raw-connection adapter)
    (ignore-errors (plain-odbc:close-connection (raw-connection adapter))))
  (setf (raw-connection adapter)
        (apply #'plain-odbc:connect-generic (connection-params adapter)))
  (execute adapter (set-schema-statement adapter (default-schema adapter)))
  (values))

(defmethod disconnect ((adapter openedge-adapter))
  (when (raw-connection adapter)
    (plain-odbc:close-connection (raw-connection adapter)))
  (setf (raw-connection adapter) nil))

(defmethod reconnect ((adapter openedge-adapter))
  (disconnect adapter)
  (connect adapter))

(defmethod activep ((adapter openedge-adapter))
  (handler-bind
      ((plain-odbc::sql-error (lambda (c)
                                (declare (ignore c))
                                (return-from activep nil))))
    (if (and (raw-connection adapter)
             (execute adapter (raw-query "SELECT 1 FROM SYSPROGRESS.SYSCALCTABLE")))
        t
        nil)))

(defmethod execute ((adapter openedge-adapter) query)
  (let ((sxql:*quote-character* +quote-char+))
    (multiple-value-bind (query params)
        (sxql:yield query)
      (plain-odbc:exec-query* (raw-connection adapter) query params))))

(defmethod exec-update ((adapter openedge-adapter) query)
  (let ((sxql:*quote-character* +quote-char+))
    (multiple-value-bind (query params)
        (sxql:yield query)
      (plain-odbc:exec-update* (raw-connection adapter) query params))))

(defmethod start-transaction ((adapter openedge-adapter))
  ;; Any statement will start a transaction, so use a NO-OP.
  (execute adapter (raw-query "SELECT 1 FROM SYSPROGRESS.SYSCALCTABLE")))

(defmethod commit ((adapter openedge-adapter))
  (plain-odbc:commit (raw-connection adapter)))

(defmethod rollback ((adapter openedge-adapter))
  (plain-odbc:rollback (raw-connection adapter)))

(defmethod get-table-area ((adapter openedge-adapter) table &key schema)
  ;; FIXME: allowing keywords here doesn't actually work.
  (check-type table (or string keyword))
  (check-type schema (or string keyword null))
  (let* ((schema (or schema (default-schema adapter)))
         (areas (execute adapter
                         (sxql:select :area._area-name
                           (sxql:from (:as :_file :tbl))
                           (sxql:inner-join (:as :_storageobject :so)
                                            :on (:and (:= :tbl._file-number :so._object-number)
                                                      (:= :so._object-type 1)))
                           (sxql:inner-join (:as :_area :area)
                                            :on (:= :so._area-number :area._area-number))
                           (sxql:where (:and (:= :tbl._file-name table)
                                             (:= :tbl._tbl-type "T")
                                             (:= :tbl._owner schema)))))))
    (unless areas
      (error "There is no storage area for table ~A.~A" schema table))
    (first (first areas))))

(defmethod get-index-area ((adapter openedge-adapter) table index &key table-schema index-schema)
  (check-type table (or keyword string))
  (check-type index (or keyword string))
  (check-type table-schema (or keyword string null))
  (check-type index-schema (or keyword string null))
  (let* ((table-schema (or table-schema (default-schema adapter)))
         (index-schema (or index-schema (default-schema adapter)))
         (areas (execute adapter
                         (sxql:select :area._area-name
                           (sxql:from (:as (sxql:select ((:as :rowid :tbl-rowid) :_file-number)
                                             (sxql:from :_file)
                                             (sxql:where (:and (:= :_file-name table)
                                                               (:= :_tbl-type "T")
                                                               (:= :_owner table-schema))))
                                           :tbl))
                           (sxql:inner-join (:as :_index :idx)
                                            :on (:and (:= :_index-name index)
                                                      (:= :_idxowner index-schema)
                                                      (:= :_file-recid :tbl.tbl-rowid)))
                           (sxql:inner-join (:as :_storageobject :so)
                                            :on (:and (:= :idx._idx-num :so._object-number)
                                                      (:= :so._object-type 2)))
                           (sxql:inner-join (:as :_area :area)
                                            :on (:= :so._area-number :area._area-number))))))
    (unless areas
      (error "There is no storage area for index ~A.~A of table ~A.~A."
             index-schema
             index
             table-schema
             table))
    (first (first areas))))

(defparameter *default-dlc-dir*
  #+windows #P"C:\\Progress\\OpenEdge\\"
  #-windows #P"/usr/dlc/")

(defun script-path (binary)
  (check-type binary pathname)
  (merge-pathnames binary (or (uiop:getenv "DLC")
                              *default-dlc-dir*)))

(defmethod move-table ((adapter openedge-adapter) table new-area &key schema)
  (let ((schema (or schema (default-schema adapter))))
    (multiple-value-bind (output error status)
        (uiop:run-program (list (uiop:native-namestring (script-path #P"bin/_proutil"))
                                ;; For some inexplicable reason, Progress db
                                ;; tools won't accept db names with a file
                                ;; extension.
                                (uiop:native-namestring (make-pathname :type nil
                                                                       :defaults (db-path adapter)))
                                "-C"
                                "tablemove"
                                (format nil "~A.~A" schema table)
                                new-area)
                          :ignore-error-status t
                          ;; :error-output '(:string :stripped t)
                          :output '(:string :stripped t))
      (declare (ignore error))
      (cond
        ((= status 0) t)
        ;; All errors seem to return code -1.
        (t (error "Error moving table ~A.~A. Report:~&~A"
                  schema
                  table
                  output))))))

(defmethod move-index ((adapter openedge-adapter) table index new-area &key schema)
  (let ((schema (or schema (default-schema adapter))))
    (multiple-value-bind (output error status)
        (uiop:run-program (list (uiop:native-namestring (script-path #P"bin/_proutil"))
                                ;; For some inexplicable reason, Progress
                                ;; db tools won't accept db names with a
                                ;; file extension.
                                (uiop:native-namestring (make-pathname :type nil
                                                                       :defaults (db-path adapter)))
                                "-C"
                                "idxmove"
                                (format nil "~A.~A.~A" schema table index)
                                new-area)
                          :ignore-error-status t
                          :output '(:string :stripped t))
      (declare (ignore error))
      (cond
        ((= status 0) t)
        ;; All errors seem to return code -1.
        (t (error "Error moving index ~A of table ~A.~A and its indexes. Report:~&~A"
                  index
                  schema
                  table
                  output))))))

(defmethod move-table-and-indexes ((adapter openedge-adapter) table new-area &key schema (index-area new-area))
  (let ((schema (or schema (default-schema adapter))))
    (multiple-value-bind (output error status)
        (uiop:run-program (list (uiop:native-namestring (script-path #P"bin/_proutil"))
                                ;; For some inexplicable reason, Progress db
                                ;; tools won't accept db names with a file
                                ;; extension.
                                (uiop:native-namestring (make-pathname :type nil
                                                                       :defaults (db-path adapter)))
                                "-C"
                                "tablemove"
                                (format nil "~A.~A" schema table)
                                new-area
                                index-area)
                          :ignore-error-status t
                          :output '(:String :stripped t))
      (declare (ignore error))
      (cond
        ((= status 0) t)
        ;; All errors seem to return code -1.
        (t (error "Error moving table ~A.~A and its indexes. Report:~&~A"
                  schema
                  table
                  output))))))
