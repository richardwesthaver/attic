;;; ppcre.asd --- PPCRE System Definitions

;;; Code:
(defsystem :ppcre
  :version "2.1.2"
  :description "Perl-compatible regular expression library"
  :author "Dr. Edi Weitz"
  :license "BSD"
  :serial t
  :components ((:file "pkg")
               (:file "var")
               (:file "util")
               (:file "errors")
               (:file "charset")
               (:file "charmap")
               (:file "chartest")
               (:file "lexer")
               (:file "parser")
               (:file "regex-class")
               (:file "regex-class-util")
               (:file "convert")
               (:file "optimize")
               (:file "closures")
               (:file "repetition-closures")
               (:file "scanner")
               (:file "api"))
  :in-order-to ((test-op (test-op :ppcre/tests))))

(defsystem :ppcre/tests
  :description "Perl-compatible regular expression library tests"
  :author "Dr. Edi Weitz"
  :license "BSD"
  :depends-on (:ppcre :rt)
  :components ((:module "tests"
                        :serial t
                        :components ((:file "pkg")
                                     (:file "tests")
                                     (:file "perl-tests"))))
  :perform (test-op (o c)
             (funcall (intern (symbol-name :run-all-tests)
                              (find-package :ppcre/tests)))))
