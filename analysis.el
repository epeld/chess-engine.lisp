
(defvar *chess-analysis-current-game* nil)
(defvar *chess-analysis-current-index* nil)
(defvar chess-analysis-move-type :san)

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
                   (format "go %s\n" chess-analysis-go-args)
                 "go\n")))

(defun uci-quick-go ()
  "Like uci-go but never spends time thinking (at least that is the idea)"
  (uci-command "go\n"))


(defun uci-stop ()
  (interactive)
  "See the UCI 'stop'-command"
  ;; TODO add all the options
  (uci-command "stop\n"))

(defun next-word ()
  "Return the next word found in buffer"
  (let ((start (point)))
    (forward-word 1)
    (buffer-substring start (point))))

(defun next-move (pos)
  (let ((move (next-word)))
    (unless (eq 4 (length move))
      (error "Strange move %s" move))
    (chess-algebraic-to-ply pos move)))

(defun uci-bestmove ()
  "Find the suggested bestmove from the engine"
  (let ((pos (chess-analysis-current-pos)))
    (with-current-buffer (process-buffer *chess-analysis-process*)
      (save-excursion
        (goto-char (point-max))
        (beginning-of-line)
        ;; move backwards if current line is empty
        (when (eolp)
          (beginning-of-line 0))
        (when (eq 'bestmove (symbol-at-point))
          (forward-word 1)              ; move past "bestmove"
          (forward-char 1)              ; move past space
          (let ((move (next-move pos))
                (end (line-end-position)))
            ;; TODO this whole sequence is "optional" if engine doesn't want to ponder
            (let ((ponder (search-forward "ponder ")))
              (when (and ponder
                         (< ponder end))
                (let ((ponder-move (next-move (chess-ply-next-pos move))))
                  (unless ponder-move
                    (error "Something went wrong"))
                  (list (chess-ply-to-algebraic move chess-analysis-move-type)
                        (chess-ply-to-algebraic ponder-move chess-analysis-move-type)))))))))))


(defun uci-depth ()
  (with-current-buffer (process-buffer *chess-analysis-process*)
    (save-excursion
      (let ((go (search-backward "go" nil)))
        (goto-char (point-max))

        (let ((depth (search-backward "depth" go nil)))
          (when depth
            (forward-word 1)
            (string-to-number (next-word))))))))

(defun uci-score ()
  "Find the latest score reported from the engine"
  (with-current-buffer (process-buffer *chess-analysis-process*)
    (save-excursion
      (goto-char (point-max))
      
      ; find the start of the current search session
      (let ((go (search-backward "go" nil)))
        (goto-char (point-max))

        ; check for mate
        (let ((mate (search-backward "score mate" go t)))
          (if mate
              (progn
                (forward-word 2)   ; skip the words "score" and "mate"
                `(mate . ,(abs (string-to-number (next-word)))))
            
            ;; check for centipawn score
            (let ((start (search-backward "score cp" go t))
                  (skip (length "score cp ")))
              (when start
                (loop for i from (+ skip (point)) 
                      until (eq (char-after i) 32)
                      finally
                      return `(score .
                                     ,(/ (string-to-number
                                          (buffer-substring (+ skip start) i))
                                         (float 100))))))))))))


(defun chess-analysis-summary ()
  ;; TODO we can actually extract a lot more info from here.
  ;; most interesting is probably the pv-line in e.g SAN-format
  (let ((bestmove (uci-bestmove))
        score)
    (when bestmove
      (setq score (uci-score))
      (format "%s %s %s (depth: %d)"
              (car bestmove)
              (cadr bestmove)
              (cond ((eq 'mate (car score))
                     (format "MATE IN %s" (cdr score)))

                    (t (format "score: %s" (cdr score))))
              (uci-depth)))))

(defun uci-handle-engine-output (output)
  (let ((summary (chess-analysis-summary)))
    (when summary
      (message summary))))


(defun chess-analysis-current-pos ()
  (interactive)
  (or (chess-game-pos *chess-analysis-current-game* *chess-analysis-current-index*)
      (error "No current position in game %s" *chess-analysis-current-index*)))

(defun chess-analysis-current-fen ()
  (chess-pos-to-fen (chess-analysis-current-pos) t))


;; High Level Interface:
(defun chess-analysis-analyse (&optional args)
  (interactive "s")
  (setq *chess-analysis-current-game* chess-module-game)
  (setq *chess-analysis-current-index* chess-display-index)
  (chess-analysis-engine-running)
  
  (let ((chess-analysis-go-args args)
        (fen (chess-analysis-current-fen)))
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
