;;; ratpoison.lisp --- StumpWM Ratpoison support code

;; event.lisp

;;; Code:
(in-package :wm)
(defun bytes-to-window (bytes)
  "Combine a list of 4 8-bit bytes into a 32-bit number. This is because
ratpoison sends the rp_command_request window in 8 byte chunks."
  (logior (first bytes)
          (ash (second bytes) 8)
          (ash (third bytes) 16)
          (ash (fourth bytes) 24)))

(defun handle-rp-commands (root)
  "Handle a ratpoison style command request."
  (labels ((one-cmd ()
             (multiple-value-bind (win type format bytes-after) (xlib:get-property root :rp_command_request :end 4 :delete-p t)
               (declare (ignore type format))
               (setf win (xlib::lookup-window *display* (bytes-to-window win)))
               (when (xlib:window-p win)
                 (let* ((data (xlib:get-property win :rp_command))
                        (interactive-p (car data))
                        (cmd (map 'string 'code-char (nbutlast (cdr data)))))
                   (declare (ignore interactive-p))
                   (eval-command cmd)
                   (xlib:change-property win :rp_command_result (map 'list 'char-code "0TODO") :string 8)
                   (xlib:display-finish-output *display*)))
               bytes-after)))
    (loop while (> (one-cmd) 0))))

;; :property-notify (case)
#+nil
(:rp_command_request
     ;; we will only find the screen if window is a root window, which
     ;; is the only place we listen for ratpoison commands.
 (let* ((screen (find-screen window)))
   (when (and (eq state :new-value)
              screen)
     (handle-rp-commands window))))
