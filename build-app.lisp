;;; build-app.lisp --- Interactively build a Lisp app

;; 

;;; Code:
(pkg:defpkg :demo/build-app
  (:use :core-lisp)
  (:import-from :linedit :formedit :install-repl))
(in-package :demo/build-app)
(install-repl)

(print-in-box "build-app.lisp")

(tagbody
   :main
   (restart-case
       (let (cli)
         (string-case ((formedit :prompt1 "App Type: "
                                 :completions '("core" "simple")))
           ("core" 
            (let ((sys (string-upcase
                        (formedit :prompt1 "App Thunk: "
                                  :completions '("homer" "krypt" "mpk" "packy" "skel")))))
              (setf cli (find-symbol* (format nil "START-~A" sys) (format nil "BIN/~A" sys)))))
           ("simple"
            (setf cli (compile nil `(lambda () ,(read-from-string (linedit:formedit :prompt1 "App Thunk (form): ")))))))
         (save-lisp-and-die (formedit :prompt1 "App Name: ")
                            :toplevel cli 
                            :executable t 
                            :compression t))
    (restart () (go :main))))
