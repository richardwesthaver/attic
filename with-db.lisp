(defvar *database-backend* nil)
(defparameter *save-database-backend-on-load* t)
;;; Backends
(defvar *database-backend-table* (make-hash-table)
  "Hash Table where keys are a database backend designator and values
are a list of functions which are responsible for doing all initialization
such as loading shared libraries and setting variables.")

(defvar *database-backend-options* (make-hash-table)
  "Hash Table where keys are a database backend designator and values are a
lambda-list which will be interpreted by PARSE-DATABASE-BACKEND-OPTIONS within
the body of WITH-DB forms.")

(defvar *database-backend-close-options* '(close destroy))

(defun add-database-loader (backend thunk)
  (let ((flist (gethash backend *database-backend-table*)))
    (setf (gethash backend *database-backend-table*) (pushnew thunk flist :test 'equalp))))

(defun add-database-backend-option (backend option)
  "Add a new database backend option."
  (let ((olist (gethash backend *database-backend-options*)))
    (setf (gethash backend *database-backend-options*) (pushnew option olist))))

(defun set-database-backend (backend options &rest thunks)
  "Set the loaders (a sequence of thunks) and options for the designated database
backend keyword BACKEND."
  (setf (gethash backend *database-backend-table*) thunks
        (gethash backend *database-backend-options*) options))

(declaim (inline %load-database-backend))
(defun %load-database-backend (backend)
  (when-let ((be (gethash backend *database-backend-table*)))
    (dolist (th be)
      (funcall th))))

(defun load-database-backend (backend &optional save)
  "Load database BACKEND and set value of *DATABASE-BACKEND*. When SAVE is
non-nil also arrange for the BACKEND to be loaded on init when this core is
saved."
  (let ((*save-database-backend-on-load* save))
    (%load-database-backend backend)
    (setq *database-backend* backend)))

(defun %database-backend-option-key (item)
  (keywordicate (if (atom item) item (car item))))

;; TODO 2024-11-10: should we handle &rest/&optional too?
(defun parse-database-backend-options (initargs)
  "Parse INITARGS as a plist of database options for current *DATABASE-BACKEND*."
  (mapcar ;; for each registered database backend option..
   (lambda (opt)
     (let ((key (%database-backend-option-key opt)))
       (if (member key initargs)
           (let ((match (getf initargs key)))
             (if (atom opt) (cons opt match) (cons (car opt) match)))
           opt)))
   (gethash *database-backend* *database-backend-options*)))

(defgeneric set-database-backend-option (db key val)
  (:method (db (key (eql :open)) val)
    (when val
      (open-db db)))
  (:method (db (key (eql :close)) val)
    (when val
      (close-db db)))
  (:method (db (key (eql :destroy)) val)
    (when val
      (close-db db)
      (destroy-db db)))
  (:method (db (key (eql :path)) val)
    (setf (path db) val))
  (:method (db (key (eql :name)) val)
    (setf (name db) val))
  (:method (db (key (eql :id)) val)
    (setf (id db) val))
  (:method (db (key (eql :sap)) val)
    (setf (sap db) val))
  (:method (db (key (eql :opts)) val)
    (setf (options db) val))
  (:method (db (key (eql :opt)) (val cons))
    (setf (opt db (car val)) (cdr val)))
  (:method (db (key (eql :shutdown)) val)
    (shutdown-db db :wait (eql val :wait))))

(defun set-database-backend-options (db &rest options)
  (mapc (lambda (opt)
          (set-database-backend-option
           db
           (keywordicate (car opt))
           ;; WARNING eval here
           (eval (cdr opt))))
        options))

(defun do-database-backend-init-options (db &rest options)
  (apply 'set-database-backend-options
         db
         (remove-if
          (lambda (x) 
            (or (atom x)
                (null (cdr x))
                (member (car x) *database-backend-close-options*)))
          options)))

(defun do-database-backend-close-options (db &rest options)
  (apply 'set-database-backend-options
         db
         (remove-if
          (lambda (x)
            (or (atom x)
                (null (cdr x))
                (not (member (car x) *database-backend-close-options*))))
          options)))


;; TODO 2025-08-12: call-with
;; (defun call-with-db (db fn &rest args))

(defmacro with-db ((var &rest initargs &key (db '*db*) &allow-other-keys) 
                   &body body)
  "Bind VAR to a DATABASE instance produced by parsing INITARGS for the extent
  of BODY which may contain any of the *DATABASE-BACKEND-OPTIONS* available
  for the current *DATABASE-BACKEND*."
  (with-gensyms (opts)
    `(let ((,opts ',(parse-database-backend-options initargs))
           (,var ,db))
       ;; ,@(when open (remf initargs :open) `((open-db ,var)))
       (apply 'do-database-backend-init-options ,var ,opts)
       (unwind-protect (progn ,@body)
         ;; ,@(when close (remf initargs :close) `((close-db ,var)))
         ;; ,@(when destroy (remf initargs :destroy) `((destroy-db ,var)))
         (apply 'do-database-backend-close-options ,var ,opts)))))

;;; RDB
(defmethods set-database-backend-option
  (((db rdb) (key (eql :close)) (val (eql :auto)))
   "Arrange for SHUTDOWN-DB to be called when there are no more references to DB."
   (sb-ext:finalize db (lambda () (shutdown-db db))))
  (((db rdb) (key (eql :merge-op)) val)
   "Assign a MERGE-OP to this database."
   (setf (opt db :merge-operator) val))
  (((db rdb) (key (eql :comparator)) val)
   "Assign a custom COMPARATOR to this database."
   (setf (opt db :comparator) val))
  (((db rdb) (key (eql :prefix-op)) val)
   "Assign a custom SLICETRANSFORM to this database to be used as a prefix
extractor."
   (setf (opt db :prefix-extractor) val))
  (((db rdb) (key (eql :event-listener)) val)
   "Assign an EVENT-LISTENER to this database."
   (setf (opt db :event-listener) val))
  (((db rdb) (key (eql :logger)) val)
   (setf (opt db :info-log) val)))

;; HACK 2026-08-06: 
;; (defun rdb (path &key))
(defmethods set-database-backend-option
  (((db rdb) (key (eql :close)) (val (eql :auto)))
   "Arrange for SHUTDOWN-DB to be called when there are no more references to DB."
   (sb-ext:finalize db (lambda () (close-db db))))
  (((db rdb) (key (eql :merge-op)) val)
   "Assign a MERGE-OP to this database."
   (setf (opt db :merge-operator) val))
  (((db rdb) (key (eql :comparator)) val)
   "Assign a custom COMPARATOR to this database."
   (setf (opt db :comparator) val))
  (((db rdb) (key (eql :prefix-op)) val)
   "Assign a custom SLICETRANSFORM to this database to be used as a prefix
extractor."
   (setf (opt db :prefix-extractor) val))
  (((db rdb) (key (eql :event-listener)) val)
   "Assign an EVENT-LISTENER to this database."
   (setf (opt db :event-listener) val)))
