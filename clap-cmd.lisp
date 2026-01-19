;;; clap-cmd.lisp --- Pre-COMMAND CLI/CLAP code

;; 

;;; Code:

(defvar *args* nil
  "Current command arguments.
Bound for the lifetime of a DEFCMD function.")

(defvar *opts* nil
  "Current command options.
Bound for the lifetime of a DEFCMD function.")

(declaim (unsigned-byte *argc* *optc*))
(defvar *argc* 0
  "Current count of command arguments.
This value may be updated throughout the lifetime of a function defined with
DEFCMD.")

(defvar *optc* 0
  "Current count of command options.
This value may be updated throughout the lifetime of a function defined with
DEFCMD.")

(defvar *arg* nil
  "Current option argument.
Bound for the lifetime of a function defined with DEFOPT.")

(declaim ((vector symbol) *cli-opt-types*))
(defvar *cli-opt-types* ;; make sure to keep this in sync with the list of parsers above
  (let ((types '(boolean string form list symbol keyword number file directory pathname)))
    (make-array (length types) :element-type 'symbol :initial-contents types)))

;; (defmacro gen-cli-thunk (pvars &rest thunk)
;;   "Generate and return a function based on THUNK suitable for the :thunk
;; slot of cli objects with pandoric bindings PVARS.")

;; DEFOPTS are much simpler - they always take a single optional argument and
;; have no lambda-list that needs to be applied.
(defmacro defopt (name &body body)
  "Define a CLI-OPT."
  (multiple-value-bind (body decl doc-string) (parse-body body :documentation t)
    `(defun ,name (&optional arg)
       ,(let ((%d '(ignorable arg)))
          (if decl 
              (append decl (list %d))
              `(declare ,%d)))
       ,@(when doc-string (list doc-string))
       (let ((*arg* arg))
         ,@body))))

(defmacro defopts (&body body)
  "Define multiple CLI-OPTs."
  (unless (null body)
    `(progn ,@(mapcar (lambda (x) `((defopt ,@x))) body))))

(defmacro make-opt-parser (spec &body body)
  "Return a TYPE-opt-parser function based on SPEC which is either a
symbol from *CLI-OPT-TYPES* or a list, and optional BODY which
is a list of handlers for the opt-val."
  (let* ((type (if (consp spec) (car spec) spec))
         (super (when (consp spec) (cadr spec)))
         (fn-name (symbolicate 'parse- type '-opt)))
    ;; thread em
    (let ((fn1 (unless (null super) (symbolicate "PARSE-" super "-OPT"))))
      `(defun ,fn-name (&optional arg)
         "Parse the cli-opt-val *ARG*."
         (declare (ignorable arg))
         ,@(if fn1
               `((setf *arg* (funcall #',fn1 arg)))
               `((setf *arg* arg)))
         ,@body))))

(define-constant +cli-lambda-list-keywords+ '(&rest &optional &opt &key) :test 'equal)

;; TODO 2025-01-05: env? for shell env
(defun parse-cli-lambda-list (ll)
  "Parse a specialized CLI lambda-list, returning as multiple values:

- required ARGs
- optional ARGs
- rest ARG
- OPTs
- key OPTs"
  (let ((state :required)
        (required)
        (optional)
        (rest)
        (opts)
        (key-opts))
    (labels ((%fail (l)
               (simple-program-error "Misplaced ~S in ordinary lambda-list:~%  ~S"
                                     l ll))
             (%check-var (l what)
               (unless (and (or (symbolp l)
                                (and (consp l) (= 2 (length l)) (symbolp (first l))))
                            (not (constantp l)))
                 (simple-program-error "Invalid ~A ~S in ordinary lambda-list:~%  ~S"
                                       what l ll)))
             (%check-spec (spec what)
               (destructuring-bind (init suppliedp) spec
                 (declare (ignore init))
                 (%check-var suppliedp what))))
      (dolist (l ll)
        (case l
          (&optional
           (if (eq state :required)
               (setf state l)
               (%fail l)))
          (&rest
           (if (member state '(:required &optional))
               (setf state l)
               (%fail l)))
          (&opt
           (if (member state '(:required &optional :after-rest &key))
               (setf state l)
               (%fail l)))
          (&key
           (if (member state '(:required &optional :after-rest &opt))
               (setf state l)
               (%fail l)))
          (t
           (when (member l '#.(set-difference lambda-list-keywords
                                              '(&optional &rest &key &allow-other-keys &aux &opt)))
             (simple-program-error
              "Bad lambda-list keyword ~S in ordinary lambda-list:~%  ~S"
              l ll))
           (case state
             (:required
              (%check-var l "required parameter")
              (push l required))
             (&optional
              (if (consp l)
                  (destructuring-bind (name &rest tail) l
                    (%check-var name "optional parameter")
                    (cond ((cdr tail)
                           (%check-spec tail "optional-supplied-p parameter"))))
                  (%check-var l "optional parameter"))
              (push (ensure-list l) optional))
             (&opt
              (if (consp l)
                  (destructuring-bind (name &rest tail) l
                    (%check-var name "opt parameter")
                    (when (cdr tail) (%check-spec tail "opt parameter")))
                  (%check-var l "opt parameter"))
              (push (ensure-list l) opts))
             (&rest
              (%check-var l "rest parameter")
              (setf rest l
                    state :after-rest))
             (&key
              (if (consp l)
                  (destructuring-bind (var-or-kv &rest tail) l
                    (if (consp var-or-kv)
                        (destructuring-bind (keyword var) var-or-kv
                          (unless (symbolp keyword)
                            (simple-program-error "Invalid key name ~S in ordinary ~
                                                         lambda-list:~%  ~S"
                                                  keyword ll))
                          (%check-var var "key parameter"))
                        (%check-var var-or-kv "key parameter"))
                    (when (cdr tail)
                      (%check-spec tail "key parameter"))
                    (setf l (cons var-or-kv tail)))
                  (%check-var l "key parameter"))
              (push (ensure-list l) key-opts))
             (t (simple-program-error "invalid cli lambda-list:~%  ~S" ll)))))))
    (values (nreverse required) 
            (nreverse optional) 
            rest 
            (nreverse opts)
            (nreverse key-opts))))

#+nil (parse-cli-lambda-list '(arg1 arg2 &optional (arg3 "foo") &rest rest &opt opt1 opt2 &key key1 key2))

;; TODO 2025-09-05: Luke.. use the lambda-list, Luke..

;; DEFCMD always returns a function of two argument ARGS and OPTS - the
;; cli-lambda-list is applied to the BODY instead of closing over the
;; function.
(defmacro defcmd (name cli-lambda-list &body body)
  "Bind NAME to a functions which accepts a CLI-LAMBDA-LIST containing a
specialized lambda-list with the following keywords:

- &OPTIONAL is an optional positional argument in ARGS
- &OPT specifies a set of cli options (--foo val, -f)
- &KEY specifies a set of cli keywords (:bar val)
- &REST specifies the remainder of the ARGS passed to the CLI after all args,
  options, and keywords

CLI-LAMBDA-LIST is a list which automatically destructures and binds the
values of parsed CLI objects for the duration of BODY. The forms accepted are
the same as the SLOTS args to WITH-SLOTS - the CAR is used as the name of the
local symbol binding and the CDR is the actual name of the CLI-OPT. An atom
counts as both.

Additionally, the following special variables are bound for the duration of
BODY:

- *ARGC* : the count of arguments passed to this command
- *ARGS* : the actual list of args
- *OPTC* : the count of options passed to this command
- *OPTS* : the actual list of options"
  (multiple-value-bind (required optional rest opts keys) (parse-cli-lambda-list cli-lambda-list)
    (multiple-value-bind (body decl doc-string) (parse-body body :documentation t)
      `(defun ,name (args opts)
         ,(let ((%d '(ignorable args opts)))
            (if decl 
                (append decl (list %d))
                `(declare ,%d)))
         ,doc-string
         (let ((*argc* (length args))
               (*optc* (length opts))
               (*args* args)
               (*opts* opts))
           (symbol-macrolet
               ,(mapcar (lambda (x)
                          (unless (typep x
                                         '(or symbol
                                           (cons symbol (cons symbol null))))
                            (error "Malformed CLI-OPT binding: ~s, should either a symbol or (variable-name opt-name)" x))
                          (destructuring-bind (name &optional (opt-name name)) (ensure-list x)
                            `(,name
                              (when-let ((val (find ,(string-downcase opt-name) *opts* 
                                                    :test 'equal
                                                    :key 'cli/clap/obj:cli-opt-name)))
                                (cli-opt-val val)))))
                 opts)
             ,@body))))))

;; currently not in use
#+nil
(defun gen-thunk-ll (origin args)
  (let ((a0 (list (symbolicate '$a 0) origin)))
    (group 
     (nconc (loop for i from 1 for a in args nconc (list (symbolicate '$a (the fixnum i)) a)) a0)
     2)))

(defun default-cmd-thunk (args opts)
  (declare (ignore args opts))
  (values))

(defun default-opt-thunk (arg)
  (identity arg))

(defun cli-opt-type-p (s)
  (declare (type symbol s))
  (find s *cli-opt-types* :test 'string-equal))

;;; Parsers
(make-opt-parser string *arg*)

(make-opt-parser boolean (when *arg* t))

(make-opt-parser (form string) (read-from-string *arg*))

(make-opt-parser (list form) (when (listp *arg*) *arg*))

(make-opt-parser (symbol form) (when (symbolp *arg*) *arg*))

(make-opt-parser (keyword form) (when (keywordp *arg*) *arg*))

(make-opt-parser number (when *arg* (parse-number *arg*)))

(make-opt-parser integer (when *arg* (parse-integer *arg*)))

(make-opt-parser (file string) 
  (parse-native-namestring *arg* nil *default-pathname-defaults* :as-directory nil))

(make-opt-parser (directory string)
  (sb-ext:parse-native-namestring *arg* nil *default-pathname-defaults* :as-directory t))

(make-opt-parser (pathname string)
  (pathname *arg*))

(defmethod push-cmd ((self cli-cmd) (place cli-cmd))
  (vector-push self (cmds place)))

(defmethod push-opt ((self cli-opt) (place cli-cmd))
  (vector-push self (opts place)))

(defmethod pop-cmd ((self cli-cmd))
  (vector-pop (cmds self)))

(defmethod pop-opt ((self cli-opt))
  (vector-pop (opts self)))

(defun solop (self)
  "A CLI object is considered 'solo' if there are no ACTIVE-CMDS parsed - there
are only OPTS and ARGS which should be used with the default command."
  (= 0 (length (active-cmds self))))

(defmethod proc-args ((self cli-cmd) args)
  "Process ARGS into an ast. Each element of the ast is a node with a
:type slot, indicating the type of node and an AST slot which stores
an object."
  (flatten
   (loop
     with skip
     with exit
     for (a . args) on args
     if skip
     do (setq skip nil)
     else if exit
     do (loop-finish)
     else if (short-opt-p a) ;; SHORT OPT

                             ;; TODO 2025-01-01: handle opt-group-p
     collect
        (let* ((has-eq (short-opt-has-eq-p a))
               (names (or (car has-eq) (string-left-trim "-" a)))
               (opts (find-short-opts names self :recurse nil)))
          (cond
            ((and (= (length opts) 1) (not has-eq))
             (let ((o (car opts)))
               (if (eql (cli-opt-type o) 'boolean)
                   (%compose-flag-opt o)
                   (prog1
                       (%compose-value-opt o (pop args))
                     (setq skip t)))))
            ((and has-eq opts)
             (loop for o in opts
                   do (activate-opt o)
                   do (setf (cli-opt-val o) (cdr has-eq))
                   collect (cli-node 'opt o)))
            ((and (not has-eq) opts)
             (loop for o in opts
                   collect (%compose-flag-opt o)))
            (t ;; if nothing else, we usually want to pass it as an arg, but
             ;; it may also be useful to enable the debugger and handle
             ;; with restarts.
             (sb-ext:enable-debugger)
             ;; (with-opt-restart-case a
             ;; (clap-unknown-argument a 'cli-opt))
             a)))
     else if (long-opt-p a) ;; LONG OPT
     collect           
        (let* ((has-eq (long-opt-has-eq-p a))
               (name (or (car has-eq) (string-left-trim "-" a)))
               (o (car (find-opts name self :recurse nil))))
          (cond
            ((and has-eq o)
             (activate-opt o)
             (setf (cli-opt-val o) (cdr has-eq))
             (cli-node 'opt o))
            ((and (not has-eq) o)
             (prog1
                 (%compose-value-opt o (pop args))
               (setq skip t)))
            (t ;; (not o) (not has-eq)
             (with-opt-restart-case a
               (clap-unknown-argument a 'cli-opt)))))
     ;; OPT GROUP
     else if (group-opt-p a)
     collect 
        (cli-node 'group nil)
     ;; OPT KEYWORD (experimental)
     else if (opt-keyword-p a)
     collect (if-let ((o (car (find-opts (string-left-trim ":" a) self :recurse t))))
               (prog1 (%compose-keyword-opt o (pop args))
                 (setq exit t))
               (cli-node 'arg a))
     else ;; CMD or ARG
     collect
        (if-let ((cmd (find-cmd a self)))
          (progn (setq exit t)
                 ;; command forms are another AST
                 (setf cmd (parse-args cmd args))
                 (cli-node 'cmd cmd))
          ;; just a plain arg - move to next
          (cli-node 'arg a)))))

(defmethod wrap ((self cli-cmd) ast)
  "Install the given AST, recursively filling in value slots."
    ;; we assume all nodes in the ast have been validated and the ast
    ;; itself is consumed. validation is performed in proc-args.

    ;; before doing anything else we lock SELF, which should remain
    ;; locked until all subcommands have completed
    (activate-cmd self)
    (loop named install
          for (node . tail) on ast
          while node
          do 
             (let ((type (cli-node-type node))
                   (form (ast node)))
               (case type
                 ;; opts
                 (opt
                  (setf (find-opt (name form) self) form))
                 (cmd
                  (setf (find-cmd (name form) self) form))
                 (arg (push-arg form self)))))
  (setf (cli-args self) (nreverse (cli-args self)))
  self)

;;; Objects
(defstruct cli-opt
  ;; note that cli-opts can have a nil or unbound name slot
  (name "" :type string)
  (type 'boolean :type (or symbol list))
  (thunk 'default-opt-thunk :type symbol)
  (val nil)
  (description nil :type (or null string)))

(defaccessor cli-thunk ((self cli-opt)) (cli-opt-thunk self))
(defaccessor name ((self cli-opt)) (cli-opt-name self))
(defaccessor lock ((self cli-opt)) (cli-opt-lock self))

(defmethod activate-opt ((self cli-opt))
  (setf (cli-opt-lock self) t))

(defun %compose-flag-opt (o)
  (activate-opt o)
  (setf (cli-opt-val o) t)
  (cli-node 'opt o))

(defun %compose-flag-opts (&rest os)
  (let ((ret))
    (dolist (o os ret)
      (%compose-flag-opt o))))

(defun %compose-value-opt (o &optional val)
  (activate-opt o)
  (setf (cli-opt-val o) val)
  (cli-node 'opt o))

(defun %compose-keyword-opt (o val)
  (activate-opt o)
  (setf (cli-opt-val o) val)
  (cli-node 'opt o))

(defmethod initialize-instance :after ((self cli-opt) &key)
  (with-slots (name thunk) self
    (unless (stringp name) (setf name (format nil "~(~A~)" name)))
    self))

(defmethod make-load-form ((obj cli-opt) &optional env)
  (make-load-form-saving-slots
   obj
   :slot-names '(name type thunk val description lock)
   :environment env))

(defmethod install-thunk ((self cli-opt) (lambda function) &optional compile)
  "Install THUNK into the corresponding slot in cli-cmd SELF."
  (let ((%thunk (if compile (compile nil lambda) lambda)))
    (setf (cli-thunk self) %thunk)
    self))

(defmethod print-object ((self cli-opt) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~A :active ~A :val ~A"
            (cli-opt-name self)
            (cli-opt-lock self)
            (cli-opt-val self))))

(defmethod print-usage ((self cli-opt) &optional stream)
  (format stream "-~(~{~A~^/--~}~)~@[ :value ~A~]~24t~@[~A~]~@[~%~4t:doc ~A~]"
          (let ((n (cli-opt-name self)))
            (declare (simple-string n))
            (list (schar0 n) n))
          (and (slot-boundp self 'val) (cli-opt-val self))
          (and (slot-boundp self 'description) (cli-opt-description self))
          (when (fboundp (cli-thunk self))
            (documentation (symbol-function (cli-thunk self)) 'function))))

(defmethod equiv ((a cli-opt) (b cli-opt))
  (with-slots (name type) a
    (with-slots ((bn name) (bk type)) b
      (and (equal name bn)
           (equal type bk)))))

(defmethod equiv ((a t) (b cli-opt))
  (equalp (cli-opt-val b) a))

(defmethod equiv ((a cli-opt) (b t))
  (equalp (cli-opt-val a) b))

(defmethod call-opt ((self cli-opt) arg)
  (funcall (cli-opt-thunk self) arg))

(defmethod do-opt ((self cli-opt))
  (prog1 (setf (cli-opt-val self) (call-opt self (cli-opt-val self)))
    (setf (cli-opt-lock self) nil)))

(defmethod do-opts ((self vector))
  (loop for opt across self
        do (do-opt opt)))

(defmethods find-opt 
  (((name string) (self list) &key active default)
   (if-let ((found (find name self :key 'cli-opt-name :test 'equal)))
     (if active
         (when (lock found)
           found)
         found)
     default))
  (((name string) (self vector) &key active default)
   (if-let ((found (find name self :key 'cli-opt-name :test 'equal)))
     (if active
         (when (lock found)
           found)
         found)
     default)))

(defun getopt (name &optional (default :error) (opts *opts*))
  "Retrieve a CLI-OPT-VAL by name from a vector of CLI-OPTs."
  (let ((opts (or opts (opts *cli*))))
    (cli-opt-val (find-opt 
                  (string-downcase name) opts 
                  :default (if (eql default :error)
                               (clap-unknown-argument name 'opt)
                               default)))))

(defun setopt (name val &optional (default :error) (opts *opts*))
  (let ((opts (or opts (opts *cli*))))
    (setf (cli-opt-val 
           (find-opt 
            (string-downcase name) opts 
            :default (if (eql default :error)
                         (clap-unknown-argument name 'opt)
                         default)))
          val)))

(defsetf getopt setopt)

(defmacro with-opt-restart-case (arg expression)
  "Bind restarts 'use-as-arg' and 'discard-arg' for duration of EXPRESSION."
  `(restart-case ,expression
     (use-as-arg () () (cli-node 'arg ,arg))
     (discard-arg () () (setf ,arg nil))))

(defmethod find-opts ((name string) (self cli-command) &key active recurse)
  (let ((ret))
    (flet ((%find (o obj)
             (when-let ((found (find o (opts obj) :key #'name :test 'equal)))
               (push found ret))))
      (when (and recurse (cmds self))
        (loop for c across (cmds self)
              do (%find name c)))
      (%find name self)
      (when active
        (setf ret (remove-if-not #'lock ret)))
      ret)))

(defun find-short-opts (flag cmd &key recurse)
  "Find and return all CLI-OPTs matching character or string FLAG in CMD.

- recurse :: optionally check nested commands as well."
  (let ((ret))
    (flet ((%find (ch obj)
             (when-let ((found (find (coerce ch 'character) obj 
                                     :key #'name
                                     :test #'opt-string-prefix-eq)))
               (push found ret))))
      (flet ((%recurse-ch (ch vec)
               (loop for c across vec
                     do (%find ch (opts c))))
             (%recurse-str (str vec)
               (loop for c across vec
                     for ch across str
                     do (%find ch (opts c)))))
        (etypecase flag
          (character
           (when recurse (%recurse-ch flag (cmds cmd)))
           (%find flag (opts cmd)))
          (string
           (when recurse (%recurse-str flag (cmds cmd)))
           (%find flag (opts cmd))))
        ret))))

;; old tests
#+nil
(progn
(in-package :cli/tests)
(in-suite :cli)
;; make sure we don't quit lisp in the middle of a test
(setf *no-exit* t)
(defparameter *cmd1* (make-cli :name "foo" :description "cmd1 description"))
(defparameter *cmd2* (make-cli :name "ayo" :description "cmd1 description"))
(defparameter *cmd3* (make-cli :cmd :name "flub" :opts *test-opts* :thunk 'flub-thunk))
(defparameter *cmds* (make-cmds (list `(:name "baz" :description "baz" :opts ,*test-opts*) *cmd1* *cmd2* *cmd3*)))

(defparameter *test-cli* (make-cli :cli :opts *test-opts* :cmds *cmds* :description "test cli"))

(deftest mixed-args ()
  (with-cli (*test-cli* :exit nil) '("--foo" "bar" "flub")
    (is (string= "bar" (aref (opts *test-cli*) 0)))
    (is (null (cli-args *test-cli*)))
    (do-cmd *test-cli*)))

(deftest cli-ast ()
  "Validate the CLI/CLAP/AST parser."
  (is (string= (cli-opt-name (ast (car (proc-args *test-cli* '("--foo" "1")))))
               "foo"))
  (signals clap-unknown-argument
    (proc-args *test-cli* '("--log" "default" "--foo=11"))))

(defmain foo-main (:exit nil)
  (isnt (do-cmd *test-cli*)))

(deftest clap-main ()
  (is (null (funcall #'foo-main))))

(deftest clap-basic ()
  "test basic CLAP functionality."
  (with-cli ((make-cli :cli :opts *test-opts* :cmds *cmds* :description "test cli") :exit nil) *args*
    (is (eq (cli/clap/macs::schar0 "test") #\t))
    (let ((ast (proc-args *cli* '("-f" "baz" "--bar=fax")))) ;; not eql
      (is= 2 (length ast))
      (is (cli-node 'opt (find-short-opts #\f *cli*)))
      (is (cli-node 'opt (find-opts "bar" *cli*))))
    (parse-args *cli* '("--bar" "baz" "-f" "yaks"))
    (is (stringp
         (with-output-to-string (s)
           (print-version *cli* s)
           (print-usage *cli* s)
           (print-help *cli* s))))
    (is (string= "foobar" (cli/clap:parse-string-opt "foobar")))
    (do-cmd *cli*)))

(make-opt-parser trivial *arg*)

(deftest clap-opts ()
  "CLAP opt tests."
  (is (reduce (lambda (x y) (and x y))
              (loop for k across *cli-opt-types* collect (cli-opt-type-p k))))
  (is (parse-trivial-opt t))
  (is (null (parse-trivial-opt nil)))))
