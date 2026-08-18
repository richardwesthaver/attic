;;; kv.lisp --- deprecated support for KV types.

;; the original idea here was to combine KEY and VALUE types in the same
;; class, in the hopes of simplifying/optimizing FFI calls which require
;; foreign buffers to be allocated for both. We now have much better IO
;; support via IO-STREAMs (STATIC-STREAMs and BUFFER-STREAMs) and have gone in
;; a different direction which is more flexible.

;; In retrospect, this design doesn't quite fit into any existing database
;; architecture. It becomes a nuisance to manage yet-another object or struct
;; for this purpose and modern database libraries do not get any measurable
;; benefit from keys and values being allocated next to each other.

;; The two concepts which require the most attention are zero-copy and
;; index/cache alignment. Our new IO-STREAM classes help with both while KVs
;; help with neither. KVs make sense as a syntax concept only, and thus
;; naturally should be isolated to the realm of _macros_.

;;; Code:
(defvar *default-kv* (make-kv))
(defvar *default-kv-size* 8)
;;; KV
(defstruct (kv (:constructor make-kv (&optional key val))) 
  (key (make-octets *default-kv-size*) :type octet-vector) 
  (val (make-octets *default-kv-size*) :type octet-vector))

(defmethod put-kv ((self rdb-database) (kv kv))
  (put-kv (db self) kv))

(defmethod merge-kv ((self rdb-database) kv &key (opts (rocksdb-writeoptions-create)))
  (%merge-kv (sap self) (kv-key kv) (kv-val kv) opts))

(defmethod insert-kv ((self rdb) (kv kv) &key column (opts (rocksdb-writeoptions-create)))
  (if column
      (let ((column (etypecase column
                  (rdb-cf column)
                  (t (find column (columns self)
                           :key 'name
                           :test 'equal)))))
        (%put-cf (sap self)
                    (sap column)
                    (kv-key kv)
                    (kv-val kv)
                    opts))
      (put-kv self kv)))

(defun wbwi-put-kv-cf (wbwi column kv)
  (%wbwi-put-cf (sap wbwi) (sap column) (kv-key kv) (kv-val kv)))

(defmethod kv ((self rdb-iter))
  (make-kv (skey self) (sval self)))

(defmethod merge-kv ((self rdb-cf) kv &key db (opts (rocksdb-writeoptions-create)))
  (%merge-cf (sap db) (sap self) (kv-key kv) (kv-val kv) opts))

(defmethod put-kv ((self sst-file-writer) (kv kv))
  (%sst-put (sst-file-writer-sap self)
            (kv-key kv) (kv-val kv)))

(defmethod put-kv ((self rdb) (kv kv))
  (%put-kv
   (sap self)
   (kv-key kv)
   (kv-val kv)))

(defmethod merge-kv ((self rdb) kv &key (opts (rocksdb-writeoptions-create)))
  (%merge-kv (sap self) (kv-key kv) (kv-val kv) opts))

(defmethod put-kv ((self rdb-wbwi) (kv kv))
  (put-key self (kv-key kv) (kv-val kv)))

;;; kv
(defmacro with-kv ((k v kv) &body body)
  `(let ((,k (kv-key ,kv))
         (,v (kv-val ,kv)))
     ,@body))

(defmacro do-kvs ((k v kvs) &body body)
  "Do BODY for each K and V in the array KVS."
  (with-gensyms (%kv)
    `(loop for ,%kv across ,kvs
           do (with-kv (,k ,v ,%kv) ,@body))))

(defmethod put-key ((self keyring) (key number) (val kv))
  (add-key "user" (octets-to-alien (kv-key val)) (octets-to-alien (kv-val val)) (length (kv-val val)) (id self)))
