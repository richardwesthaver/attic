;;; lapack-ffi-gen.lisp --- LAPACK groveller

;; From core/skelfile, used to generate FFI/BLAS/LAPACK-FFI.LISP at
;; build-time. Nowadays we keep it directly in the source tree and include in
;; the default build, so no need to generate on each build.

;;; Code:
;; lapack-ffi.lisp ()
(tree-sitter:load-tree-sitter)
(tree-sitter:load-tree-sitter-c)
(let (lapack lapack.h code)
  (setf
   lapack
   (let* ((str (read-file #p"/usr/include/openblas/lapack.h"))
          (tree (flatten (syn/ts::parse-string :c str :consume t :produce-cst nil)))
          (ret))
     (setf lapack.h str)
     (loop with dx = 0
           ;; loop through all declarations
           for di = (position :declaration tree :start dx)
           if (null di) 
           do (return ret)
           else
           do (progn
                (setf dx di)
                (incf dx)
                (let ((ei (position :declaration tree :start dx)))
                  (push (subseq tree di ei) ret))))))
  (labels ((lastr (x start end)
             (apply 'subseq lapack.h (subseq x start end)))
           (lasym (str) (symbolicate (string-upcase (substitute #\- #\_ (string-trim '(#\*) str)))))
           (lasym* (str)
             (let ((len (length str)))
               (list '* (lasym (subseq str 0 
                                       (if (char= (char str (1- len)) #\*)
                                           (1- len)
                                           len))))))
           (lasyms (x start end)
             (symbolicate (string-upcase (substitute #\- #\_ (lastr x start end)))))
           (lasymf (x start end)
             (symbolicate (string-upcase (substitute #\- #\_ (remove-string "_base" (remove-string "LAPACK_" (lastr x start end)))))))
           (laparm (str)
             (let ((a (nreverse (split-sequence #\space str))))
               `(,(if (string-equal (first a) "T")
                      '%t
                      (lasym (first a)))
                 ,(if (third a)
                      ;; type param (could be X const* or const X*)
                      (if (string-equal (third a) "const")
                          (lasym (second a))
                          (lasym (third a)))
                      (when (second a)
                        (lasym* (second a))))
                 ,@(when (third a) '(:copy)))))
           (gen-lapack-args (x)
             (let* ((pa (position :parameter-list x)) ;; param start
                    (pb (position :preproc-ifdef x)) ;; param end
                    (params (subseq x pa pb)))
               (loop with p1 = 0
                     for pos = (position :parameter-declaration params :start p1)
                     while pos
                     do (setf p1 (1+ pos))
                     collect (laparm (lastr params (+ pos 1) (+ pos 3)))))))
    (setf code
          (mapcar
           (lambda (x)
             (let ((p (position :function-declarator x)))
               (list* 'deflapack
                      ;; routine name
                      (lasymf x (+ p 5) (+ p 7))
                      ;; return-type
                      (lasyms x (- p 3) (- p 1))
                      ;; list of function parameters (NAME TYPE)
                      (remove-if (lambda (x) (eql (car x) 'fortran-strlen)) (gen-lapack-args x)))))
           lapack)))
  (with-output-to-file (f (asdf:system-relative-pathname :lapack "lapack-ffi.lisp") :if-exists :supersede)
    (write-line ";;; lapack-ffi.lisp --- LAPACK Alien Routines -*- buffer-read-only:t -*-" f)
    (write-line "(in-package :lapack)" f)
    (mapc (lambda (x) (write x :stream f :case :downcase) (terpri f)) code)))
