;;; xlib-old.lisp --- Old XLIB Code

;; 

;;; Code:

;;; translate.lisp (keysyms)
(defvar *keysym-sets* nil) ;; Alist of (name first-keysym last-keysym)

(defun define-keysym-set (set first-keysym last-keysym)
  ;; Define all keysyms from first-keysym up to and including
  ;; last-keysym to be in the keysym set named SET. SET is a keyword
  ;; (i.e., a symbol in the package named KEYWORD). When the function
  ;; KEYSYM-SET is called with a keysym, the SET of the keysym set to
  ;; which the keysym belongs is returned.

  ;; If the range of keysyms defined by first-keysym and last-keysym
  ;; overlaps the range of an existing keysym set, then an error is
  ;; signaled.
  (declare (type keyword set)
           (type keysym first-keysym last-keysym))
  (when (> first-keysym last-keysym)
    (rotatef first-keysym last-keysym))
  (setq *keysym-sets* (delete set *keysym-sets* :key #'car))
  (dolist (set *keysym-sets*)
    (let ((first (second set))
          (last (third set)))
      (when (or (<= first first-keysym last)
                (<= first last-keysym last))
        (error "Keysym range overlaps existing set ~s" set))))
  (push (list set first-keysym last-keysym) *keysym-sets*)
  set)

(defun keysym-set (keysym)
  ;; Return the character code set name of keysym
  (declare (type keysym keysym)
           (clx-values keyword))
  (dolist (set *keysym-sets*)
    (let ((first (second set))
          (last (third set)))
      (when (<= first keysym last)
        (return (first set))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro keysym (keysym &rest bytes)
    ;; Build a keysym.

    ;; If KEYSYM is an integer, it is used as the most significant
    ;; bits of the keysym, and BYTES are used to specify low order
    ;; bytes. The last parameter is always byte4 of the keysym. If
    ;; KEYSYM is not an integer, the keysym associated with KEYSYM is
    ;; returned.

    ;; This is a macro and not a function macro to promote
    ;; compile-time lookup. All arguments are evaluated.

    ;; FIXME: The above means that this shouldn't really be a macro at
    ;; all, but a compiler macro. Probably, anyway.
    (declare (type t keysym)
             (type list bytes)
             (clx-values keysym))
    (typecase keysym
      ((integer 0 *)
       (dolist (b bytes keysym) (setq keysym (+ (ash keysym 8) b))))
      (otherwise
       (or (car (character->keysyms keysym))
           (error "~s Isn't the name of a keysym" keysym))))))

;; Keysym-mappings are a list of the form (object translate lowercase modifiers mask)
;; With the following accessor macros. Everything after OBJECT is optional.
(defmacro keysym-mapping-object (keysym-mapping)
  ;; Parameter to translate
  `(first ,keysym-mapping))

(defmacro keysym-mapping-translate (keysym-mapping)
  ;; Function to be called with parameters (display state OBJECT)
  ;; when translating KEYSYM and modifiers and mask are satisfied.
  `(second ,keysym-mapping))

(defmacro keysym-mapping-lowercase (keysym-mapping)
  ;; LOWERCASE is used for uppercase alphabetic keysyms. The value
  ;; is the associated lowercase keysym.
  `(third ,keysym-mapping))

(defmacro keysym-mapping-modifiers (keysym-mapping)
  ;; MODIFIERS is either a modifier-mask or list containing intermixed
  ;; keysyms and state-mask-keys specifying when to use this
  ;; keysym-translation.
  `(fourth ,keysym-mapping))

(defmacro keysym-mapping-mask (keysym-mapping)
  ;; MASK is either a modifier-mask or list containing intermixed
  ;; keysyms and state-mask-keys specifying which modifiers to look at
  ;; (i.e. modifiers not specified are don't-cares)
  `(fifth ,keysym-mapping))

(defun define-keysym (object keysym &key lowercase translate modifiers mask display)
  ;; Define the translation from keysym/modifiers to a (usually
  ;; character) object. Any previous keysym definition with KEYSYM
  ;; and MODIFIERS is deleted before the new definition is added.

  ;; MODIFIERS is either a modifier-mask or list containing intermixed
  ;; keysyms and state-mask-keys specifying when to use this
  ;; keysym-translation. The default is NIL.

  ;; MASK is either a modifier-mask or list containing intermixed
  ;; keysyms and state-mask-keys specifying which modifiers to look at
  ;; (i.e. modifiers not specified are don't-cares).
  ;; If mask is :MODIFIERS then the mask is the same as the modifiers
  ;; (i.e. modifiers not specified by modifiers are don't cares)
  ;; The default mask is *default-keysym-translate-mask*

  ;; NOTE 2026-01-24: this option is unused in our code
  ;; If DISPLAY is specified, the translation will be local to DISPLAY,
  ;; otherwise it will be the default translation for all displays.

  ;; LOWERCASE is used for uppercase alphabetic keysyms. The value
  ;; is the associated lowercase keysym. This information is used
  ;; by the keysym-both-case-p predicate (for caps-lock computations)
  ;; and by the keysym-downcase function.

  ;; TRANSLATE will be called with parameters (display state OBJECT)
  ;; when translating KEYSYM and modifiers and mask are satisfied.
  ;; [e.g (zerop (logxor (logand state (or mask *default-keysym-translate-mask*))
  ;;                     (or modifiers 0)))
  ;;      when mask and modifiers aren't lists of keysyms]
  ;; The default is #'default-keysym-translate
  (declare (type (or base-char t) object)
           (type keysym keysym)
           (type (or null mask16 (clx-list (or keysym state-mask-key)))
                 modifiers)
           (type (or null (member :modifiers) mask16 (clx-list (or keysym state-mask-key)))
                 mask)
           (type (or null display) display)
           (type (or null keysym) lowercase)
           (type (or null (function (display card16 t) t)) translate))
  (flet ((merge-keysym-mappings (new old)
           ;; Merge new keysym-mapping with list of old mappings.
           ;; Ensure that the mapping with no modifiers or mask comes first.
           (let* ((key (keysym-mapping-modifiers new))
                  (merge (delete key old :key #'cadddr :test #'equal)))
             (if key
                 (nconc merge (list new))
                 (cons new merge))))
         (mask-check (mask)
           (unless (or (numberp mask)
                       (dolist (element mask t)
                         (unless (or (find element +state-mask-vector+)
                                     (gethash element *keysym->character-map*))
                           (return nil))))
             (x-type-error mask '(or mask16 (clx-list (or modifier-key modifier-keysym)))))))
    (let ((entry
            ;; Create with a single LIST call, to ensure cdr-coding
            (cond
              (mask
               (unless (eq mask :modifiers)
                 (mask-check mask))
               (when (or (null modifiers) (and (numberp modifiers) (zerop modifiers)))
                 (error "Mask with no modifiers"))
               (list object translate lowercase modifiers mask))
              (modifiers (mask-check modifiers)
                         (list object translate lowercase modifiers))
              (lowercase	(list object translate lowercase))
              (translate	(list object translate))
              (t	(list object)))))
      (if display
          (let ((previous (assoc keysym (display-keysym-translation display))))
            (if previous
                (setf (cdr previous) (merge-keysym-mappings entry (cdr previous)))
                (push (list keysym entry) (display-keysym-translation display))))
          (setf (gethash keysym *keysym->character-map*)
                (merge-keysym-mappings entry (gethash keysym *keysym->character-map*)))))
    object))

(defun undefine-keysym (object keysym &key display modifiers &allow-other-keys)	              
  ;; Undefine the keysym-translation translating KEYSYM to OBJECT with MODIFIERS.
  ;; If DISPLAY is non-nil, undefine the translation for DISPLAY if it exists.
  (declare (type (or base-char t) object)
           (type keysym keysym)
           (type (or null mask16 (clx-list (or keysym state-mask-key)))
                 modifiers)
           (type (or null display) display))
  (flet ((match (key entry)
           (let ((object (car key))
                 (modifiers (cdr key)))
             (or (eql object (keysym-mapping-object entry))
                 (equal modifiers (keysym-mapping-modifiers entry))))))
    (let* (entry
           (previous (if display
                         (cdr (setq entry (assoc keysym (display-keysym-translation display))))
                         (gethash keysym *keysym->character-map*)))
           (key (cons object modifiers)))
      (when (and previous (find key previous :test #'match))
        (setq previous (delete key previous :test #'match))
        (if display
            (setf (cdr entry) previous)
            (setf (gethash keysym *keysym->character-map*) previous))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant character-set-switch-keysym (keysym 255 126))
  (defconstant left-shift-keysym (keysym 255 225))
  (defconstant right-shift-keysym (keysym 255 226))
  (defconstant left-control-keysym (keysym 255 227))
  (defconstant right-control-keysym (keysym 255 228))
  (defconstant caps-lock-keysym (keysym 255 229))
  (defconstant shift-lock-keysym (keysym 255 230))
  (defconstant left-meta-keysym (keysym 255 231))
  (defconstant right-meta-keysym (keysym 255 232))
  (defconstant left-alt-keysym (keysym 255 233))
  (defconstant right-alt-keysym (keysym 255 234))
  (defconstant left-super-keysym (keysym 255 235))
  (defconstant right-super-keysym (keysym 255 236))
  (defconstant left-hyper-keysym (keysym 255 237))
  (defconstant right-hyper-keysym (keysym 255 238)))

(defun keysym-downcase (keysym)
  ;; If keysym has a lower-case equivalent, return it, otherwise return keysym.
  (declare (type keysym keysym))
  (declare (clx-values keysym))
  (let ((translations (gethash keysym *keysym->character-map*)))
    (or (and translations (keysym-mapping-lowercase (first translations))) keysym)))

(defun keysym-uppercase-alphabetic-p (keysym)
  ;; Returns T if keysym is uppercase-alphabetic.
  ;; I.E. If it has a lowercase equivalent.
  (declare (type keysym keysym))
  (declare (clx-values (or null keysym)))
  (let ((translations (gethash keysym *keysym->character-map*)))
    (and translations
         (keysym-mapping-lowercase (first translations)))))

(defun character->keysyms (character &optional display)
  ;; Given a character, return a list of all matching keysyms.
  ;; If DISPLAY is given, translations specific to DISPLAY are used,
  ;; otherwise only global translations are used.
  ;; Implementation dependent function.
  ;; May be slow [i.e. do a linear search over all known keysyms]
  (declare (type t character)
           (type (or null display) display)
           (clx-values (clx-list keysym)))
  (let ((result nil))
    (when display
      (dolist (mapping (display-keysym-translation display))
        (when (eql character (second mapping))
          (push (first mapping) result))))
    (maphash #'(lambda (keysym mappings)
                 (dolist (mapping mappings)
                   (when (eql (keysym-mapping-object mapping) character)
                     (pushnew keysym result))))
             *keysym->character-map*)
    result))
