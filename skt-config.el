;; Skt Config
(use-package skt
  :defer t
  :custom
  skt-enable-tempo-elements t
  skt-delete-duplicate-marks t
  :init (defvar skt-default-version "0.1.0")
  :bind (:map skt-minor-mode-map
              ("b" . tempo-backward-mark)
              ("f" . tempo-forward-mark)
              ("SPC" . tempo-complete-tag)
              ("t" . skt-add-tag))
  :custom
  (defvar skt-skeleton-path-function #'abbreviate-file-name
    "Function to be called when expanding file-header skeletons. Useful to
rebind locally inside a project or module, where you want to delete some
prefix or replace it.")

  (defun skt-buffer-path (&optional function)
    (let ((path (or buffer-file-name (format "%s.lisp" (gensym "scratch-")))))
      (funcall (or function skt-skeleton-path-function) path)))

  (defun skt-skelfile-path ()
    (if (string= (file-name-nondirectory buffer-file-name) "skelfile")
        "skelfile"
      (skt-buffer-path)))
  ;; functions
  (skt-define-function capture (:abbrev "capture" :tag t) org-capture)
  (skt-define-function agenda (:abbrev "agenda" :tag t) org-agenda)
  (skt-define-function mjump (:abbrev "mjump" :tag t) bookmark-jump)
  (skt-define-function bjump (:abbrev "bjump" :tag t) ibuffer-jump)
  (skt-define-function rjump (:abbrev "rjump" :tag t)
    (lambda () (jump-to-register (read-char "register: "))))
  (skt-define-function pjump (:abbrev "pjump" :tag t) (lambda () (project-switch-project default-directory)))

  ;; templates
  (skt-define-template readme (:mode org-mode :tag t)
    "#+title: " (p "title: ") n
    "#+description: " (p "description: ") n
    "#+author: " user-full-name n
    "#+email:" user-mail-address n
    "#+setupfile: clean.theme" n
    "#+export_file_name: index" n>
    p n> n>
    ":info:" n>
    "+ version :: " skt-default-version n
    ":end:" n>)

  (skt-define-template clean.theme (:mode org-mode :tag t)
    "#+setupfile: " (expand-file-name "org/clean.theme" company-cdn-url))

  ;; TODO 2024-06-04: 
  ;; (skt-define-template defsystem (:mode lisp-mode :tag t :abbrev "defsystem"))
  ;; (skt-define-template defpackage (:mode lisp-mode :tag t :abbrev "defpackage"))
  ;; (skt-define-template defpkg (:mode lisp-mode :tag t :abbrev "defpkg"))

  (skt-define-template defmacro (:abbrev "(defmacro" :tag t :mode lisp-mode)
    "(defmacro " (p "Name: ") " (" (p "Args: ") ")" > n> r ")")

  (skt-define-template defun (:abbrev "(defun" :tag t :mode lisp-mode)
    "(defun " (p "Name: ") " (" (p "Args: ") ")" > n> r ")")

  (skt-define-template defvar (:abbrev "(defvar" :tag t :mode lisp-mode)
    > "(defvar " > r ")")

  ;; skeletons
  (skt-define-skeleton head (:abbrev "head" :mode lisp-mode)
    "description: "
    ";;; " (skt-buffer-path 'file-name-nondirectory) " --- " str \n \n ";; " _ \n \n ";;; Code:" \n >)

  (skt-define-skeleton head (:abbrev "head" :mode skel-mode)
    "description: "
    ";;; " (skt-skelfile-path) " --- " str " -*- mode: skel; -*-" \n _)

  (skt-define-skeleton head (:abbrev "head" :mode org-mode)
    "title: "
    "#+title: " str \n
    "#+author: " (skeleton-read "author: ") \n
    "#+description: " (skeleton-read "description: ") \n
    "#+setupfile: clean.theme" \n > _)

  (skt-define-skeleton head (:abbrev "head" :mode rust-mode)
    "description: "
    "//! " (skt-buffer-path 'file-name-nondirectory) " --- " str \n \n "// " _ \n \n "//! Code: " \n >)

  (skt-define-skeleton system-head (:abbrev "system-head" :mode lisp-mode)
    "system-name: "
    ";;; " (skt-buffer-path) " --- "
    '(setq v1 (file-name-base (skt-buffer-path))) (capitalize v1)
    " Sytem Definitions" \n
    > "(defsystem :" v1 \n
    > ":depends-on (:std :log)" \n
    > ":components ((:file \"pkg\")" _ "))")

  (skt-define-skeleton sys-head (:abbrev "sys-head" :mode skel-mode)
    "system-name: "
    ";;; " (skt-buffer-path) " --- "
    '(setq v1 (file-name-base (skt-buffer-path))) (capitalize v1)
    " Sytem Definitions" \n
    > "(defsys :" v1 \n
    > ":depends-on (:std :log)" \n
    > ":components ((:file \"pkg\")" _ "))")

  (skt-define-skeleton pkg-head (:abbrev "pkg-head" :mode lisp-mode)
    "ignored"
    ";;; " (skt-buffer-path 'file-name-nondirectory) " --- "
    '(setq v1 (skeleton-read "name: ")) v1 " Package Definitions" \n
    > "(defpkg :" v1 \n
    > ":use (:std :log))" \n \n
    > "(in-package :" v1 ")" \n >)

  (skt-define-skeleton crate-head (:abbrev "crate-head" :mode conf-toml-mode)
    "ignored"
    "### " (skt-buffer-path 'file-name-nondirectory) " --- " 
    '(setq v1 (skeleton-read "name: ")) v1 " Cargo Manifest" \n >
    "[package]" \n
    "name = \"" v1 "\"" \n
    "version = \"" skt-default-version "\"" \n
    "[dependencies]" \n >)
  ;; autoinsert
  (skt-register-auto-insert "skelfile" #'skt-template-skel-head)
  (skt-register-auto-insert "readme.org" #'skt-template-org-readme)
  (skt-register-auto-insert "Cargo.toml" #'skt-template-conf-toml-crate-head)
  (skt-register-auto-insert "pkg.lisp" #'skt-template-lisp-pkg-head)
  (skt-register-auto-insert ".*[.]asd" #'skt-template-lisp-system-head)
  (skt-register-auto-insert ".*[.]lisp" #'skt-template-lisp-head)
  (skt-register-auto-insert ".*[.]rs" #'skt-template-rust-head)
  (skt-register-auto-insert ".*[.]sys" #'skt-template-skel-sys-head)
  ;; (keymap-set skel-minor-mode-map "C-<return>" 'company-tempo)
  (auto-insert-mode t))
