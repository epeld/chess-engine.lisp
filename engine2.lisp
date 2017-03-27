
(in-package :chess-engine)

(defun prefixed-p (string prefix)
  "Returns t if 'string' starts with prefix"
  (search string prefix :end1 (length prefix)))

(defclass engine ()
  ((process :documentation "A handle to the process the engine is running in"
            :reader engine-process
            :initform (error "A process is required")
            :initarg :process)
   (log :documentation "A log of the conversation between lisp and the engine"
        :accessor engine-log
        :initform nil)
   (options :documentation "A cache of the options that the engine supports, once it sends it"
            :accessor engine-options-cache
            :initform nil))
  (:documentation "A chess engine process"))


(defun make-engine (process)
  "Construct a new engine instance"
  (make-instance 'engine :process process))


(defun engine-input (engine)
  "Returns the engine's input stream"
  (sb-ext:process-input (engine-process engine)))


(defun engine-output (engine)
  "Returns the engine's output stream"
  (sb-ext:process-output (engine-process engine)))


(defun make-command-log-entry (cmd args)
  "Construct a log entry suitable for insertion into engine log, containing a GUI command"
  `(gui . ,(format nil cmd args)))


(defun make-engine-log-entry (reply)
  "Construct a log entry suitable for insertion into engine log, containing an engine reply"
  `(gui . ,reply))


(defun log-entry-content (entry)
  "Return the string content of a log entry"
  (cdr entry))


(defun engine-command (engine cmd &rest args)
  "Send a command to the engine. Cmd is a format string constructing the command"
  (let ((ws (engine-input engine)))
    (format ws cmd args)
    (push (make-command-log-entry cmd args) (engine-log engine))))


(defun engine-read-line (engine &key (wait t))
  "Read a single line, appending it to the log before returning"
  (let ((rs (engine-output engine))
        line)
    (when (or wait (listen rs))
      (setq line (read-line rs))
      (push (make-engine-log-entry line) (engine-log engine))
      line)))


(defun engine-read-reply (engine &optional multiple)
  "Collect the replies from the engine, blocking until first line appears"
  (do ((lines nil (push line lines))
       (line (engine-read-line engine :wait t) (engine-read-line engine :wait nil)))
      ((null line) (not multiple))))


(defun engine-quit (engine)
  "Quit the engine"
  (engine-command engine "quit~%")
  (sb-ext:process-wait (engine-process engine)))


(defun engine-uci (engine)
  "Send the 'uci' UCI engine-command to the engine. Telling it to init UCI mode"
  (engine-command engine "uci~%")
  (engine-read-reply engine t))


(defun engine-ucinewgame (engine)
  "Tell the engine to forget everything and start a new game"
  (engine-command engine "ucinewgame~%"))


(defun engine-isready (engine)
  "Ask the engine isready. Also tells the engine to initialize its state the first time it is sent."
  (engine-command engine "isready~%")
  (unless (string= (engine-read-reply engine) "readyok")
    (error "Engine not ready")))


(defun engine-position (engine fen &rest moves)
  "Tell the engine to set up a given position on its internal board"
  (engine-command engine "position ~a~@[ moves~{ ~a~}~]~%" fen moves))


(defun engine-go (&key ponder infinite mate movetime
                    nodes depth movestogo searchmoves wtime btime winb binc)
  "Tell the engine to start thinking. Use ponder to let the engine think when it is not its turn.

All time engine-commands use milliseconds"
  (when (and ponder infinite)
    (error "PONDER and INFINITE cannot both be active"))

  (format nil "go~:[~; infinite~]~:[~; ponder~]~:{ ~a ~a~}~%"
          infinite ponder
          (remove-if #'null
                     (mapcar (lambda (name value)
                               (when value
                                 `(,(string-downcase name) ,value)))
                             '(mate movetime nodes depth movestogo searchmoves wtime btime winb binc)
                             (list mate movetime nodes depth movestogo searchmoves wtime btime winb binc)))))


(defun engine-stop (engine)
  "Stop the engine thinking"
  (engine-command engine "stop~%"))


(defun engine-ponderhit (engine)
  "Tell the engine to carry out the pondered move. (But not to start thinking)"
  (engine-command engine "ponderhit~%"))


(defun engine-logs-starting-with (engine prefix &optional count)
  "Return log entries starting-with a given prefix"
  (let ((logs (engine-log engine)))
    (remove-if-not (lambda (entry)
                     (prefixed-p (log-entry-content entry) prefix))
                   logs
                   :count count)))

(defun engine-log-position (engine prefix)
  "Return the index in the engine log of the last entry with a given prefix. (0 being the most recent index)"
  (position-if (lambda (entry)
                 (prefixed-p (log-entry-content entry) prefix))
               (engine-log engine)))


(defclass engine-option ()
  ((name :documentation "The name of the option"
         :type string
         :accessor option-name
         :initarg :name
         :initform (error "Name required"))
   (type :documentation "The type of option this is"
         :type keyword
         :accessor option-type
         :initarg :type
         :initform (error "Type required"))
   (initial-value :documentation "The initial value this option has"
                  :accessor option-initial-value
                  :initarg :initial-value)
   (vars :documentation "The list of possible values for a combo-type option"
         :accessor option-vars
         :initarg :vars))
  (:documentation "A configurable option that a chess engine supports"))


(defvar option-types '(:check :spin :combo :button :string)
  "All allowed option types")


(defun parse-engine-type (string)
  "Identify the option type from its string representation"
  (or (loop for type in option-types
         when (string-equal type string)
         do (return type))
      (error "Invalid option type ~a" string)))


(defun parse-key-value-pairs (stream)
  "Construct an alist of key value pairs from stream"
  (let (pairs)
    (do ((curr (read-word stream :string) (read-word stream :string))
         (name nil))
        ((null curr))
      (if name
          (progn (setq pairs (acons name curr pairs))
                 (setq name nil))
          (setq name curr)))

    (nreverse pairs)))


(defun parse-option (string)
  "Parse an option out of a string"
  (with-input-from-string (stream string)
    (unless (string-equal (read-word stream :string) "option")
      (error "Invalid format ~a" string))
    
    (let ((alist (parse-key-value-pairs stream)))
      (flet ((get-value (name &optional required)
               (or (cdr (assoc name alist :test #'string-equal))
                   (when required
                     (error "Option ~a required: '~a'" name string)))))

        (let ((name (get-value "name" t))
              (type (parse-engine-type (get-value "type" t))))
          
          (ecase type
            (:button (make-instance 'engine-option
                                    :name name
                                    :type type))

            (:check (make-instance 'engine-option
                                   :name name
                                   :type type
                                   :initial-value (string-equal "true"
                                                                (get-value "default" t))))

            (:spin (make-instance 'engine-option
                                  :name name
                                  :type type
                                  :min (get-value "min" t)
                                  :max (get-value "max" t)
                                  :initial-value (get-value "default" t)))

            (:combo (make-instance 'engine-option
                                   :name name
                                   :type type
                                   :initial-value (get-value "default" t)
                                   :vars (loop for pair in alist
                                            when (string-equal "var" (car pair))
                                            collect (cdr pair))))

            (:string (make-instance 'engine-option
                                    :name name
                                    :type type
                                    :initial-value (get-value "default" t)))))))))


(defun engine-options (engine)
  "Return the parsed options, once supplied by the engine"
  (unless (engine-logs-starting-with engine "uci" 1)
    (error "Engine hasn't been sent UCI yet"))
  (unless (engine-options-cache engine)
    (setf (engine-options-cache engine)
          (mapcar (lambda (entry)
                    (parse-key-value-pairs (log-entry-content entry)))
                  (engine-logs-starting-with engine  "option"))))
  (engine-options-cache engine))


(defun read-word (stream &optional (type :list))
  "Read a single word from stream, returning the characters as a list.
Only white space is accepted as word delimiter. Skips the trailing space"
  (let ((word (loop
                 until (char= #\Space (peek-char nil stream nil #\Space))
                 collect (read-char stream)
                 finally (read-char stream nil))))
    (if (and word (eq type :string))
        (coerce word 'string)
        word)))


(defun engine-thinking-p (engine)
  "Return T if engine is currently analyzing or pondering a position"

  (let ((bestmove-pos  (engine-log-position engine "bestmove"))
        (go-pos  (engine-log-position engine "go")))

    ;; The engine is thinking if we told it 'go' and it hasn't reported a best move yet
    (and go-pos
         (or (null bestmove-pos)
             (< go-pos bestmove-pos)))))


(defun engine-current-position (engine)
  "Get the position the engine is supposed to have on its internal board"
  (let ((log (engine-logs-starting-with engine "position" 1)))
    (when log
      (subseq (log-entry-content log) (length "position ")))))
