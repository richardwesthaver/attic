;;; old.el --- Old elisp code form core/etc/emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Richard Westhaver

;; Author: Richard Westhaver <richard.westhaver@gmail.com>

;;; Code:

;;; Tags
;;;###autoload
(defun refresh-tags ()
  "Refresh TAGS database in `user-emacs-directory'."
  (interactive)
  (let ((default-directory user-emacs-directory))
    (async-shell-command 
     "etags ./*.el \\
./lib/*.el \\
~/comp/core/emacs/*.el \\
~/comp/core/emacs/lib/*.el \\
-o TAGS")))
