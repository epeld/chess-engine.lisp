
(in-package :chess-engine)

(defclass engine ()
  ((process :documentation "A handle to the process the engine is running in"
            :initform (error "A process is required")
            :initarg :process)
   (log :documentation "A log of the conversation between lisp and the engine"
        :accessor engine-log
        :initform nil))
  (:documentation "A chess engine process"))

(defun make-engine (process)
  "Construct a new engine instance"
  (make-instance 'engine :process process))

(defun engine-input (engine)
  "Returns the engine's input stream"
  (process-input engine))

(defun engine-output (engine)
  "Returns the engine's output stream"
  (process-output engine))

(defun make-command-log-entry (cmd args)
  "Construct a log entry suitable for insertion into engine log, containing a GUI command"
  `(gui . ,(format nil cmd args)))

(defun make-engine-log-entry (reply)
  "Construct a log entry suitable for insertion into engine log, containing an engine reply"
  `(gui . ,reply))

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


(defun engine-quit (process)
  "Quit the engine"
  (engine-command process "quit~%")
  (process-wait process))

(defun engine-uci (process)
  "Send the 'uci' UCI engine-command to the engine. Telling it to init UCI mode"
  (engine-command process "uci~%")
  (engine-read-reply process t))

(defun engine-isready (process)
  "Ask the engine isready. Also tells the engine to initialize its state the first time it is sent."
  (engine-command process "isready~%")
  (unless (string= (engine-read-reply process) "readyok")
    (error "Engine not ready")))

(defun engine-position (process fen &rest moves)
  "Tell the engine to set up a given position on its internal board"
  (engine-command process "position ~a~{ ~a~}~%" fen moves))

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

(defun engine-stop (process)
  "Stop the engine thinking"
  (engine-command process "stop~%"))

(defun engine-ponderhit (process)
  "Tell the engine to carry out the pondered move. (But not to start thinking)"
  (engine-command process "ponderhit~%"))


