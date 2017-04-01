
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
    (with-current-buffer (process-buffer *chess-analysis-process*)
      (setq comint-output-filter-functions nil)
      (add-hook 'comint-output-filter-functions
                'uci-handle-engine-output
                nil
                t)
      (add-hook 'comint-output-filter-functions
                'comint-postoutput-scroll-to-bottom
                nil
                t))
    
    (uci-command "uci\n")
    (uci-command "isready\n")))

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

(defun uci-quick-go ()
  "Like uci-go but never spends time thinking (at least that is the idea)"
  (uci-command "go\n"))


(defun uci-stop ()
  (interactive)
  "See the UCI 'stop'-command"
  ;; TODO add all the options
  (uci-command "stop\n"))

(defun uci-bestmove ()
  "Find the suggested bestmove from the engine"
  (with-current-buffer (process-buffer *chess-analysis-process*)
    (save-excursion
      (goto-char (point-max))
      (beginning-of-line)
      ;; move backwards if current line is empty
      (when (eolp)
        (beginning-of-line 0))
      (when (eq 'bestmove (symbol-at-point))
        (buffer-substring-no-properties (point) (line-end-position))))))


(defun uci-score ()
  "Find the latest score reported from the engine"
  (with-current-buffer (process-buffer *chess-analysis-process*)
    (save-excursion
      (goto-char (point-max))
      (let ((start (search-backward "score cp"))
            (skip (length "score cp ")))
        (when start
          (loop for i from (+ skip (point)) 
                until (eq (char-after i) 32)
                finally
                return (buffer-substring-no-properties (+ skip start) i)))))))


(defun uci-handle-engine-output (output)
  (let ((bestmove (uci-bestmove)))
    (when bestmove
      (message "%s (score %s)" bestmove (uci-score)))))


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


(defun chess-analysis-move-forward ()
  (interactive)
  (chess-display-move-forward)
  (chess-analysis-analyse))

(defun chess-analysis-move-backward ()
  (interactive)
  (chess-display-move-backward)
  (chess-analysis-analyse))

(defun chess-analysis-fen ()
  (interactive)
  (message (chess-analysis-current-fen)))


(defun chess-analysis-bindings ()
  "Setup bindings for chess analyis"
  (interactive)
  (local-set-key (kbd "p") 'chess-analysis-move-backward)
  (local-set-key (kbd "n") 'chess-analysis-move-forward)
  (local-set-key (kbd "C-c C-e") 'chess-analysis-analyse)
  (local-set-key (kbd "C-c C-g") 'uci-stop))
 

 

(provide 'chess-analysis)
