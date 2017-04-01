
(defvar *chess-analysis-process*
  nil
  "A handle to the currently running chess engine process")

(defvar chess-analysis-go-args
  nil
  "Arguments to go as a string. Try e.g \"infinite\"")

(defun chess-analysis-engine-running ()
  (unless (and *chess-analysis-process*
               (eq 'run (process-status *chess-analysis-process*)))
    (chess-analysis-init)))

(defun uci-command (command)
  (assert *chess-analysis-process*)
  (unless (s-ends-with-p "\n" command)
    (error "Command should be terminated with newline: %s" command))


  (with-current-buffer (process-buffer *chess-analysis-process*)
    ;; Position ourselves at the end,
    ;; add a ">"-prompt to indicate we are issuing a command,
    ;; ..and update mark
    (goto-char (point-max))
    (unless (eolp) }
      (insert-string "\n"))
    (insert-string "> " command)
    (set-marker (process-mark *chess-analysis-process*) (point))


    ;; Send command and receive output
    (comint-send-string *chess-analysis-process* command)))


(defun chess-analysis-init ()
  "Start up a new chess engine for analysis"
  (let* ((buf (generate-new-buffer "*Chess Analysis*"))
         
         (proc (make-comint "chess-analysis-engine"
                            (executable-find "stockfish"))))
    
    (setq *chess-analysis-process* (get-process "chess-analysis-engine"))

    ;; Add a hook to be notified of output:
    (with-current-buffer buf
      (add-hook 'comint-output-filter-functions
                'uci-handle-engine-output
                nil
                t))

    (uci-command "uci\n")
    (uci-command "isready\n")))

(with-current-buffer (process-buffer *chess-analysis-process*)
  ;(setq comint-output-filter-functions nil)
  (add-hook 'comint-output-filter-functions
          'uci-handle-engine-output
          nil
          t)
  (add-hook 'comint-output-filter-functions
            'comint-postoutput-scroll-to-bottom
            nil
            t))

(defun uci-set-position (fen &rest moves)
  "See the UCI 'position'-command"
  (let (parts)
    (push "position" parts)
    (push "fen" parts)
    (push fen parts)
    (when moves
      (push "moves" parts)
      (loop for move in moves do (push move parts)))
    (push "\n" parts)

    (let ((cmd (json-join (nreverse parts) " ")))
      ;(message cmd)
      (uci-command cmd))))


(defun uci-go ()
  "See the UCI 'go'-command"
  ;; TODO add all the options
  (uci-command (if chess-analysis-go-args
                   (format "go %s\n")
                 "go\n")))


(defun uci-stop ()
  (interactive)
  "See the UCI 'go'-command"
  ;; TODO add all the options
  (uci-command "stop\n"))


(defun uci-handle-engine-output (output)
  (with-current-buffer (process-buffer *chess-analysis-process*)
    (save-excursion
      (goto-char (point-max))
      (beginning-of-line)
      ;; move backwards if current line is empty
      (when (eolp)
        (beginning-of-line 0))
      (when (eq 'bestmove (symbol-at-point))
        (message (buffer-substring-no-properties (point) (line-end-position)))))))


(defun chess-analysis-current-fen ()
  (chess-pos-to-fen (chess-game-pos chess-module-game chess-display-index)
                    t))


;; High Level Interface:
(defun chess-analysis-analyse ()
  (interactive)
  (chess-analysis-engine-running)
  (let ((fen (chess-analysis-current-fen)))
    (uci-set-position fen)
    (uci-go)))

(defun chess-analysis-fen ()
  (interactive)
  (message (chess-analysis-current-fen)))

(provide 'chess-analysis)
