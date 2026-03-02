;;; b3.lisp --- BLAKE3 Hasher

;; 

;;; Code:
(defpackage :cry/b3
  (:nicknames :b3)
  (:use :cl :std :blake3 :sb-alien)
  (:export :b3hash :b3sum
           :b3hash-string))

(in-package :cry/b3)

(defun b3hash (in &optional (len +blake3-out-len+))
  "Hash the sequence IN using blake3 returning an OCTET-VECTOR of length LEN."
  (declare (optimize (speed 3) (safety 0)))
    (let ((out (make-octets len)))
      (with-blake3-hasher h
        (with-alien ((input (* unsigned-char) (octets-to-alien in))
                     (output (* unsigned-char) (make-alien unsigned-char len)))
          (blake3-hasher-update (addr h) input (length in))
          (blake3-hasher-finalize (addr h) output len)
          (clone-octets-from-alien output out len)
          out))))

(defun b3hash-string (in &key (length +blake3-out-len+) (hex t))
  "Hash a string using BLAKE3. When HEX is T (the default) return a hex-encoded
string instead of octets."
  (let ((hash (b3hash (sb-ext:string-to-octets in) length)))
    (if hex
        (octet-vector-to-hex-string hash)
        hash)))
  
(defun b3sum (path &key (hex t))
  (declare (optimize (speed 3) (safety 0)))
  (with-open-file (f path :element-type 'octet)
    (let ((out (make-octets (file-length f))))
      (read-sequence out f)
      (let ((hash (b3hash out)))
        (if hex
            (octet-vector-to-hex-string hash)
            hash)))))

#+test
(deftest b3 ()
  (blake3:load-blake3)
  (isequal
   (b3hash-string "1234")
   (b3hash-string "1234")))

#+nil
(defun checksum-bench ()
  (blake3:load-blake3)
  (labels ((.md5sum () (crypto:digest-file (crypto:make-digest :md5) *load-truename*))
           (.sha1sum () (crypto:digest-file (crypto:make-digest :sha1) *load-truename*))
           (.sha256sum () (crypto:digest-file (crypto:make-digest :sha256) *load-truename*))
           (.sha512sum () (crypto:digest-file (crypto:make-digest :sha512) *load-truename*))
           (.b3sum () (b3sum *load-truename* :hex nil))
           (.crc64sum () (crc64-file *load-truename*)))
    (init-crc64 +improved-polynomial+)
    (let ((n 1000))
      (time (dotimes (i n) (.sha1sum))) ;; 20 bytes
      (time (dotimes (i n) (.sha256sum))) ;; 32 bytes
      (time 
       (sb-sprof:with-profiling (:report :graph)
         (dotimes (i n) (.b3sum)))) ;; 32 bytes ; incredibly slow
      (time (dotimes (i n) (.sha512sum))) ;; 64 bytes
      (time (dotimes (i n) (.md5sum))) ;; 16 bytes
      (time (dotimes (i n) (.crc64sum)))))) ;; 8 bytes
