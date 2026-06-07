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

;; emms-directory (expand-file-name "emms" user-emacs-directory)
;; shr-cookie-policy nil
;; NOTE 2023-11-04: EXPERIMENTAL
;; ediff-floating-control-frame t
(setq url "https://compiler.company"
      packy-url "https://packy.compiler.company"
      company-domain "compiler.company"
      company-cdn-url "https://cdn.compiler.company")

;; (let ((grammar-dir "/usr/share/tree-sitter/"))
;;   (when (file-exists-p grammar-dir)
;;     (setq treesit-extra-load-path
;;           (append
;;            (flatten
;;             (mapcar
;;              (lambda (f)
;;                (unless (or (string= "." f) (string= ".." f))
;;                  (concat grammar-dir f)))
;;              (directory-files "/usr/share/tree-sitter")))
;;            treesit-extra-load-path))))
;; (setq multisession-storage 'files)

;; (use-package kind-icon
;;   :ensure t
;;   :after corfu
;;                                         ;:custom
;;                                         ; (kind-icon-blend-background t)
;;                                         ; (kind-icon-default-face 'corfu-default) ; only needed with blend-background
;;   :config
;;   (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; ;; TODO 2024-08-05: infer logbook column-titles/props
;; (defun column-display-value-transformer (column-title value)
;;   "Modifies the value to display in column view."
;;   (let ((title (upcase column-title)))
;;     (when (and (member title '("UPDATED" "NOTE")))
;;       (org-back-to-heading)
;;       (re-search-forward
;;        "Note taken on \\[\\(.*\\)\\] \\\\\\\\\\\n +\\(.*\\) *$"
;;        (org-entry-end-position) t))
;;     (if (equal column-title "UPDATED")
;;         (match-string-no-properties 1)
;;       (match-string-no-properties 2))))

;; (setq org-columns-modify-value-for-display-function
;;       #'column-display-value-transformer)

;; (require 'slime-company "slime-company")
;; (require 'slime-cape "slime-cape")
;; (require 'slime-repl-ansi-color "slime-repl-ansi-color")

;; X11-only (mcclim requires clx)
(defun clouseau-inspect (string)
  "Inspect a lisp value with Clouseau. make sure to load clouseau
with a custom core or in your init file before using this
function: '(ql:quickload :clouseau)'."
  (interactive
   (list (slime-read-from-minibuffer
	  "Inspect value (evaluated): "
	  (slime-sexp-at-point))))
  (let ((inspector 'cl-user::*clouseau-inspector*))
    (slime-eval-async
	`(cl:progn
	  (cl:defvar ,inspector nil)
	  ;; (Re)start the inspector if necessary.
	  (cl:unless (cl:and (clim:application-frame-p ,inspector)
			     (clim-internals::frame-process ,inspector))
		     (cl:setf ,inspector (cl:nth-value 1 (clouseau:inspect nil :new-process t))))
	  ;; Tell the inspector to visualize the correct datum.
	  (cl:setf (clouseau:root-object ,inspector :run-hook-p t)
		   (cl:eval (cl:read-from-string ,string)))
	  ;; Return nothing.
	  (cl:values)))))

;; lisp font-lock defaults: https://www.n16f.net/blog/custom-font-lock-configuration-in-emacs/
;; (defface cl-character-face
;;   '((default :inherit font-lock-constant-face))
;;   "The face used to highlight Common Lisp character literals.")

;; (defface cl-standard-function-face
;;   '((default :inherit font-lock-keyword-face))
;;   "The face used to highlight standard Common Lisp function symbols.")

;; (defface cl-standard-value-face
;;   '((default :inherit font-lock-variable-name-face))
;;   "The face used to highlight standard Common Lisp value symbols.")

;; (defvar cl-font-lock-keywords
;;   (let* ((character-re (concat "#\\\\" lisp-mode-symbol-regexp "\\_>"))
;;          (function-re (concat "(" (regexp-opt cl-function-names t) "\\_>"))
;;          (value-re (regexp-opt cl-value-names 'symbols)))
;;     `((,character-re . 'cl-character-face)
;;       (,function-re
;;        (1 'cl-standard-function-face))
;;       (,value-re . 'cl-standard-value-face))))

;;; Eglot
;; (with-eval-after-load 'eglot
;;   (unless (package-installed-p 'eglot-x)
;;     (package-vc-install '(eglot-x :url "https://vc.compiler.company/packy/eglot-x")))
;;   (require 'eglot-x)
;;   (with-eval-after-load 'eglot-x
;;     (add-to-list 'eglot-server-programs
;;                  '((rust-ts-mode rust-mode) .
;;                    ("rust-analyzer" :initializationOptions (:check (:command "clippy")))))
;;     (eglot-x-setup)))

;;; Graphviz
;; (use-package graphviz-dot-mode
;;   :ensure t
;;   :config
;;   (setq graphviz-dot-indent-width 2))

;; (add-hook 'after-init-hook #'org-clock-persistence-insinuate)

;; Patch org-mode to use vertical splitting
(defadvice org-prepare-agenda (after org-fix-split)
  (toggle-window-split))
(ad-activate 'org-prepare-agenda)

(defun org-time-string-to-seconds (s)
  "Convert a string HH:MM:SS to a number of seconds."
  (cond
   ((and (stringp s)
	 (string-match "\\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\)" s))
    (let ((hour (string-to-number (match-string 1 s)))
	  (min (string-to-number (match-string 2 s)))
	  (sec (string-to-number (match-string 3 s))))
      (+ (* hour 3600) (* min 60) sec)))
   ((and (stringp s)
	 (string-match "\\([0-9]+\\):\\([0-9]+\\)" s))
    (let ((min (string-to-number (match-string 1 s)))
	  (sec (string-to-number (match-string 2 s))))
      (+ (* min 60) sec)))
   ((stringp s) (string-to-number s))
   (t s)))

(defun org-time-seconds-to-string (secs)
  "Convert a number of seconds to a time string."
  (cond ((>= secs 3600) (format-seconds "%h:%.2m:%.2s" secs))
	((>= secs 60) (format-seconds "%m:%.2s" secs))
	(t (format-seconds "%s" secs))))

(defmacro with-time (time-output-p &rest exprs)
  "Evaluate an org-table formula, converting all fields that look
like time data to integer seconds.  If TIME-OUTPUT-P then return
the result as a time value."
  (list
   (if time-output-p 'org-time-seconds-to-string 'identity)
   (cons 'progn
	 (mapcar
	  (lambda (expr)
	    `,(cons (car expr)
		    (mapcar
		     (lambda (el)
		       (if (listp el)
			   (list 'with-time nil el)
			 (org-time-string-to-seconds el)))
		     (cdr expr))))
	  `,@exprs))))

(defun org-hex-strip-lead (str)
  (if (and (> (length str) 2) (string= (substring str 0 2) "0x"))
      (substring str 2) str))

(defun org-hex-to-hex (int)
  (format "0x%x" int))

(defun org-hex-to-dec (str)
  (cond
   ((and (stringp str)
	 (string-match "\\([0-9a-f]+\\)" (setf str (org-hex-strip-lead str))))
    (let ((out 0))
      (mapc
       (lambda (ch)
	 (setf out (+ (* out 16)
		      (if (and (>= ch 48) (<= ch 57)) (- ch 48) (- ch 87)))))
       (coerce (match-string 1 str) 'list))
      out))
   ((stringp str) (string-to-number str))
   (t str)))

(defmacro with-hex (hex-output-p &rest exprs)
  "Evaluate an org-table formula, converting all fields that look
    like hexadecimal to decimal integers.  If HEX-OUTPUT-P then
    return the result as a hex value."
  (list
   (if hex-output-p 'org-hex-to-hex 'identity)
   (cons 'progn
	 (mapcar
	  (lambda (expr)
	    `,(cons (car expr)
		    (mapcar (lambda (el)
			      (if (listp el)
				  (list 'with-hex nil el)
				(org-hex-to-dec el)))
			    (cdr expr))))
	  `,@exprs))))

(defun check-for-clock-out-note ()
  (interactive)
  (save-excursion
    (org-back-to-heading)
    (let ((tags (org-get-tags)))
      (and tags (message "tags: %s " tags)
	   (when (member "clocknote" tags)
	     (org-add-note))))))

(add-hook 'org-clock-out-hook 'check-for-clock-out-note)

(defun org-word-count (beg end
			   &optional count-latex-macro-args?
			   count-footnotes?)
  "Report the number of words in the Org mode buffer or selected region.
Ignores:
- comments
- tables
- source code blocks (#+BEGIN_SRC ... #+END_SRC, and inline blocks)
- hyperlinks (but does count words in hyperlink descriptions)
- tags, priorities, and TODO keywords in headers
- sections tagged as 'not for export'.

The text of footnote definitions is ignored, unless the optional argument
COUNT-FOOTNOTES? is non-nil.

If the optional argument COUNT-LATEX-MACRO-ARGS? is non-nil, the word count
includes LaTeX macro arguments (the material between {curly braces}).
Otherwise, and by default, every LaTeX macro counts as 1 word regardless
of its arguments."
  (interactive "r")
  (unless mark-active
    (setf beg (point-min)
	  end (point-max)))
  (let ((wc 0)
	(latex-macro-regexp "\\\\[A-Za-z]+\\(\\[[^]]*\\]\\|\\){\\([^}]*\\)}"))
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
	(cond
	 ;; Ignore comments.
	 ((or (org-at-comment-p) (org-at-table-p))
	  nil)
	 ;; Ignore hyperlinks. But if link has a description, count
	 ;; the words within the description.
	 ((looking-at org-bracket-link-analytic-regexp)
	  (when (match-string-no-properties 5)
	    (let ((desc (match-string-no-properties 5)))
	      (save-match-data
		(cl-incf wc (length (remove "" (org-split-string
						desc "\\W")))))))
	  (goto-char (match-end 0)))
	 ((looking-at org-any-link-re)
	  (goto-char (match-end 0)))
	 ;; Ignore source code blocks.
	 ((org-between-regexps-p "^#\\+BEGIN_SRC\\W" "^#\\+END_SRC\\W")
	  nil)
	 ;; Ignore inline source blocks, counting them as 1 word.
	 ((save-excursion
	    (backward-char)
	    (looking-at org-babel-inline-src-block-regexp))
	  (goto-char (match-end 0))
	  (setf wc (+ 2 wc)))
	 ;; Count latex macros as 1 word, ignoring their arguments.
	 ((save-excursion
	    (backward-char)
	    (looking-at latex-macro-regexp))
	  (goto-char (if count-latex-macro-args?
			 (match-beginning 2)
		       (match-end 0)))
	  (setf wc (+ 2 wc)))
	 ;; Ignore footnotes.
	 ((and (not count-footnotes?)
	       (or (org-footnote-at-definition-p)
		   (org-footnote-at-reference-p)))
	  nil)
	 (t
	  (let ((contexts (org-context)))
	    (cond
	     ;; Ignore tags and TODO keywords, etc.
	     ((or (assoc :todo-keyword contexts)
		  (assoc :priority contexts)
		  (assoc :keyword contexts)
		  (assoc :checkbox contexts))
	      nil)
	     ;; Ignore sections marked with tags that are
	     ;; excluded from export.
	     ((assoc :tags contexts)
	      (if (intersection (org-get-tags-at) org-export-exclude-tags
				:test 'equal)
		  (org-forward-same-level 1)
		nil))
	     (t
	      (cl-incf wc))))))
	(re-search-forward "\\w+\\W*")))
    (format "%d words in %s." wc
	    (if mark-active "region" "buffer"))))

;; (defun org-agenda-log-mode-colorize-block ()
;;   "Set different line spacing based on clock time duration."
;;   (save-excursion
;;     (let* ((colors (cl-case (alist-get 'background-mode (frame-parameters))
;;                      (light
;;                       (list "#F6B1C3" "#FFFF9D" "#BEEB9F" "#ADD5F7"))
;;                      (dark
;;                       (list "#aa557f" "DarkGreen" "DarkSlateGray" "DarkSlateBlue"))))
;;            pos
;;            duration)
;;       (nconc colors colors)
;;       (goto-char (point-min))
;;       (while (setq pos (next-single-property-change (point) 'duration))
;;         (goto-char pos)
;;         (when (and (not (equal pos (pos-bol)))
;;                    (setq duration (org-get-at-bol 'duration)))
;;           ;; larger duration bar height
;;           (let ((line-height (if (< duration 15) 1.0 (+ 0.5 (/ duration 30))))
;;                 (ov (make-overlay (pos-bol) (1+ (pos-eol)))))
;;             (overlay-put ov 'face `(:background ,(car colors) :foreground "black"))
;;             (setq colors (cdr colors))
;;             (overlay-put ov 'line-height line-height)
;;             (overlay-put ov 'line-spacing (1- line-height))))))))

;; (add-hook 'org-agenda-finalize-hook #'org-agenda-log-mode-colorize-block)

;; org-sbx [[https://list.orgmode.org/d429d29b-42fa-7d7b-6f3a-9fe692fd6dc7@grinta.net/T/]]
(defun %org-sbx (name header args)
  (let* ((args (mapconcat
		(lambda (x)
		  (format "%s=%S" (symbol-name (car x)) (cadr x)))
		args ", "))
	 (ctx (list 'babel-call (list :call name
				      :name name
				      :inside-header header
				      :arguments args
				      :end-header ":results silent")))
	 (info (org-babel-lob-get-info ctx)))
    (when info (org-babel-execute-src-block nil info))))

(defmacro org-sbx (name &rest args)
  (let* ((header (if (stringp (car args)) (car args) nil))
	 (args (if (stringp (car args)) (cdr args) args)))
    (unless (stringp name)
      (setq name (symbol-name name)))
    (let ((result (%org-sbx name header args)))
      (org-trim (if (stringp result) result (format "%S" result))))))

(defmacro add-packages (&rest pkgs)
  "add list of packages PKGS to `package-selected-packages'"
  `(mapc (lambda (x) (add-to-list 'package-selected-packages x)) ',pkgs))

;;; OS
(defmacro when-sys= (name body)
  "(when (string= (system-name) NAME) BODY)"
  `(when ,(string= (system-name) name) ,body))

(defun wc ()
  "Return a 3-element list with lines, words and characters in
region or whole buffer."
  (interactive)
  (let ((n 0)
	(start (if mark-active (region-beginning) (point-min)))
	(end (if mark-active (region-end) (point-max))))
    (save-excursion
      (goto-char start)
      (while (< (point) end) (if (forward-word 1) (setq n (1+ n)))))
    (list (count-lines start end) n (- end start))))

(use-package elfeed 
  :ensure t
  :custom
  elfeed-feeds 
  '(("http://threesixty360.wordpress.com/feed/" blog math)
    ("http://www.50ply.com/atom.xml" blog dev)
    ("http://blog.cryptographyengineering.com/feeds/posts/default" blog)
    ("http://abstrusegoose.com/feed.xml" comic)
    ("http://accidental-art.tumblr.com/rss" image math)
    ("http://researchcenter.paloaltonetworks.com/unit42/feed/" security)
    ("http://curiousprogrammer.wordpress.com/feed/" blog dev)
    ("http://feeds.feedburner.com/amazingsuperpowers" comic)
    ("http://amitp.blogspot.com/feeds/posts/default" blog dev)
    ("http://pages.cs.wisc.edu/~psilord/blog/rssfeed.rss" blog)
    ("http://www.anticscomic.com/?feed=rss2" comic)
    ("http://feeds.feedburner.com/blogspot/TPQSS" blog dev)
    ("http://techchrunch.com/feeds" tech news)
    ("https://rss.nytimes.com/services/xml/rss/nyt/Technology.xml" tech news)
    ("https://static.fsf.org/fsforg/rss/news.xml" tech news)
    ("https://feeds.npr.org/1001/rss.xml" news)
    ("https://search.cnbc.com/rs/search/combinedcms/view.xml?partnerId=wrss01&id=10000664" fin news)
    ("https://search.cnbc.com/rs/search/combinedcms/view.xml?partnerId=wrss01&id=19854910" tech news)
    ("https://search.cnbc.com/rs/search/combinedcms/view.xml?partnerId=wrss01&id=100003114" us news)
    ("http://arxiv.org/rss/cs" cs rnd)
    ("http://arxiv.org/rss/math" math rnd)
    ("http://arxiv.org/rss/q-fin" q-fin rnd)
    ("http://arxiv.org/rss/stat" stat rnd)
    ("http://arxiv.org/rss/econ" econ rnd)
    ;; John Wiegley
    ("http://newartisans.com/rss.xml" dev blog)
    ("https://www.reddit.com/r/listentothis/.rss" music reddit)
    ("https://www.ftc.gov/feeds/press-release-consumer-protection.xml" gov ftc)
    ("https://api2.fcc.gov/edocs/public/api/v1/rss/" gov fcc))
  :init
  (defun yt-dl-it (url)
    "Downloads the URL in an async shell"
    (let ((default-directory user-stash-directory))
      (async-shell-command (format "yt-dlp %s" url))))

  (defun elfeed-youtube-dl (&optional use-generic-p)
    "Youtube-DL link"
    (interactive "P")
    (let ((entries (elfeed-search-selected)))
      (cl-loop for entry in entries
	       do (elfeed-untag entry 'unread)
	       when (elfeed-entry-link entry)
	       do (yt-dl-it it))
      (mapc #'elfeed-search-update-entry entries)
      (unless (use-region-p) (forward-line))))
  :config
  (keymap-set elfeed-search-mode-map "d" 'elfeed-youtube-dl)
  (keymap-set user-map "e f" #'elfeed)
  (keymap-set user-map "e F" #'elfeed-update))

(use-package elfeed-tube
  :ensure t
  :after elfeed
  ;; :config
  ;; (elfeed-tube-setup)
  ;; (elfeed-tube-add-feeds '("detroit techno" "boiler room dj" "brad mehldau" "chris 'daddy' dave"))
  :bind (:map elfeed-show-mode-map
	      ("F" . elfeed-tube-fetch)
	      ([remap save-buffer] . elfeed-tube-save)
	      :map elfeed-search-mode-map
	      ("F" . elfeed-tube-fetch)
	      ([remap save-buffer] . elfeed-tube-save)))

(use-package elfeed-tube-mpv
  :ensure t
  :bind (:map elfeed-show-mode-map
	      ("C-c C-f" . elfeed-tube-mpv-follow-mode)
	      ("C-c C-w" . elfeed-tube-mpv-where)))

;; go install github.com/rjhorniii/ical2org@latest
(defun ical2org (file)
  "Convert ics FILE to an org-mode heading."
  (interactive "ffile: ")
  (shell-command (format "ical2org %s -a %s" file org-inbox-file)))

;; The point of this package is to enable an Emacs-native scrum
;; workflow. Many years ago I used to use the org-jira package and
;; mirror an external scrum/agile system (Jira).

;; Mind you, I wouldn't dare take a shot at Jira. As far as Products
;; go, when you need to work with hundreds of humans on software and
;; are given a short list you must choose from, it's often the best of
;; the worst.

;; The problem is however, that we don't need Products. What we need,
;; is a plan. How we achieve that end should be via the best and most
;; powerful tools possible.

;; In my opinion, Emacs Org Mode is the most powerful tool
;; available. It is not quite the best tool for the job, but this
;; isn't a problem because it is not a Product. We are given the
;; opportunity to make it the best tool possible, in the only way
;; possible - by doing it ourselves.

;; And yes, the aura of NIH syndrome may be strong here. Most of the
;; time you need to work with lots of folks who don't have the need or
;; patience to learn Org-mode. This package isn't for them. It's for
;; small groups of like-minded Lispers :).

;;;; Refs
;; scrum: https://www.scrum.org/resources/what-scrum-module

;; roadmap: https://compiler.company/plan/roadmap
;; tasks: https://compiler.company/plan/tasks

  ;; (skt-define-skeleton local-vars
  ;;     (:tag t :abbrev "local-vars" :docstring "Insert a local variables section.  Use current comment syntax if any.")
  ;;     (completing-read "Mode: " obarray
  ;;   	     (lambda (symbol)
  ;;   	       (if (commandp symbol)
  ;;   		   (string-match "-mode$" (symbol-name symbol))))
  ;;   	     t)
  ;;   '(save-excursion
  ;;      (if (re-search-forward page-delimiter nil t)
  ;;      (error "Not on last page"))) comment-start "Local Variables:" comment-end \n
  ;;      comment-start "mode: " str
  ;;      & -5 | '(kill-line 0) & -1 | comment-end \n
  ;;      ( (completing-read (format "Variable, %s: " skeleton-subprompt)
  ;;   	                  obarray
  ;;   	                  (lambda (symbol)
  ;;   	                    (or (eq symbol 'eval)
  ;;   		                    (custom-variable-p symbol)))
  ;;   	                  t)
  ;;        comment-start str ": "
  ;;        (read-from-minibuffer "Expression: " nil read-expression-map nil
  ;;   		                   'read-expression-history) | _
  ;;        comment-end \n)
  ;;      resume:
  ;;      comment-start "End:" comment-end \n)
