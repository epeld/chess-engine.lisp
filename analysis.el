
(defvar *chess-analysis-current-game* nil)
(defvar *chess-analysis-current-index* nil)
(defvar chess-analysis-move-type :san)


(defvar *chess-analysis-process*
  nil
  "A handle to the currently running chess engine process")


(defvar chess-analysis-go-args
  nil
  "Arguments to go as a string. Try e.g \"infinite\"")


(defvar *chess-analysis-summary-buffer*
  "Chess Analysis Overview"
  "Buffer where we report summaries of the engine analysis")


(defun skip-whitespace ()
  (skip-chars-forward " \t"))



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
    (unless (eolp)
      (insert-string "\n"))
    (insert-string "> " command)
    (set-marker (process-mark *chess-analysis-process*) (point))


    ;; Send command and receive output
    (comint-send-string *chess-analysis-process* command)))


(defun chess-analysis-init ()
  "Start up a new chess engine for analysis"
  (let* (;(buf (generate-new-buffer "*Chess Analysis*"))
         (proc (make-comint "chess-analysis-engine"
                            (executable-find "stockfish"))))

    ;; Ensure buffer exists
    (unless (get-buffer *chess-analysis-summary-buffer*)
      (generate-new-buffer *chess-analysis-summary-buffer*))
    
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
  ;; Be careful not to change position while engine is calculating
  ;; because that will confuse our gui code..
  (with-current-analysis-session
   (unless (uci-bestmove)
     (uci-stop)))
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


(defun parse-ply (pos)
  (let ((move (next-word)))
    (unless (eq 4 (length move))
      (error "Strange move %s" move))
    (chess-algebraic-to-ply pos move)))


(defun moves-to-plies (pos moves)
  (let ((ply (chess-algebraic-to-ply pos (car moves))))
    (cons ply
          (if (rest moves)
              (moves-to-plies (chess-ply-next-pos ply) (rest moves))
            nil))))


(defun uci-bestmove ()
  "Find the suggested bestmove from the engine"
  (goto-char (point-max))

  (when (search-backward "bestmove" nil t)
    (forward-word 1)
    (skip-whitespace)
        
    (let ((move (next-word)))
      (if (search-forward "ponder" (line-end-position) t)
          (progn
            (skip-whitespace)
            (list move (next-word)))
        (list move)))))


(defun uci-statistic (word &optional fn unit)
  "Find a single statistic"
  (goto-char (point-max))
  (let ((id (lambda (x) x)))
    (or (when (search-backward word nil t)
        (forward-word 1)
        (format "%s%s" (funcall (or fn id)
                                (string-to-number (next-word)))
                (or unit "")))
      "N/A")))


(defun uci-pv ()
  "Find the primary variation sequence"
  ;; Example line:
  ;; info depth 11 seldepth 16 multipv 1 score cp 481 nodes 11409 nps 814928 tbhits 0 time 14 pv h4g5 a8f8
  (goto-char (point-max))
  (when (search-backward "multipv" nil t)
    (search-forward " pv" nil nil)
    (skip-whitespace)
    (split-string (buffer-substring (point) (line-end-position))
                  " " t " ")))


(defun uci-score ()
  "Find the latest score reported from the engine"
  ;; Examples:
  ;; info depth 27 seldepth 38 multipv 1 score mate 10 nodes 23104622 nps 1681436 hashfull 999 tbhits 0 time 13741 pv f1f6 b6f6 e5f6 f8g7 h5g6 e7f7 c1f1 b7c6 g6h7 g8f8 f6g7 f8e7 f1f7 e7f7 g7e5 f7e8 d3g6 e8f8 e5d6
  (goto-char (point-max))

  ;; check for mate
  (let ((mate (search-backward "score mate" nil t)))
    (if mate
        (progn
          (forward-char (length "score mate")) ; skip the words "score" and "mate"
          (skip-whitespace)                    ;skip whitespace
          `(mate . ,(abs (string-to-number (next-word)))))
            
      ;; check for centipawn score
      (let ((start (search-backward "score cp" nil t))
            (skip (length "score cp ")))
        (when start
          (loop for i from (+ skip (point)) 
                until (eq (char-after i) 32)
                finally
                return `(score .
                               ,(/ (string-to-number
                                    (buffer-substring (+ skip start) i))
                                   (float 100)))))))))


(defun chess-analysis-format-score (score)
  (cond ((eq 'mate (car score))
         (format "MATE IN %s" (cdr score)))

        (t (format "%s" (cdr score)))))

(defun format-moves (pos moves)
  (json-join (loop for ply in (moves-to-plies pos moves)
                   collect (chess-ply-to-algebraic ply chess-analysis-move-type))
             " "))


(defun chess-analysis-summary ()
  "foo"
  ;; Ex info depth 27 seldepth 38 multipv 1 score mate 10 nodes 23104622 nps 1681436 hashfull 999 tbhits 0 time 13741 pv f1f6 b6f6 e5f6 f8g7 h5g6 e7f7 c1f1 b7c6 g6h7 g8f8 f6g7 f8e7 f1f7 e7f7 g7e5 f7e8 d3g6 e8f8 e5d6
  (with-current-analysis-session
   (format "\nPV: %s\nScore: %s\n\nTime: %s, Depth: %s\nNodes: %s (%s nps), Hash: %s, Hits: %s\n\n%s"
           (format-moves (chess-analysis-current-pos) (uci-pv))
           (chess-analysis-format-score (uci-score))
           (uci-statistic "time" nil "ms")
           (uci-statistic " depth") ; the space is important. otherwise we match "seldepth"
           (uci-statistic "nodes")
           (uci-statistic "nps")
           (uci-statistic "hashfull" (lambda (x) (/ x 10)) " %")
           (uci-statistic "tbhits")
           (if (uci-bestmove) "Stopped" "Running"))))


(defmacro with-engine-buffer (&rest body)
  `(with-current-buffer (process-buffer *chess-analysis-process*)
     ,@body))


(defmacro with-narrowed-region (bounds &rest body)
  (let ((bs (gensym "bounds")))
    `(let (,bs)

       (widen)
       (setq ,bs ,bounds)
       (narrow-to-region (first ,bs) (second ,bs))
       
       (unwind-protect
           (progn ,@body)
         (widen)))))


(defun last-go-bounds ()
  ;; Look for "go" and (the last) "\n"
  ;; Because any input without a trailing "\n" is not complete
  (let (last-go last-line)
       
    (goto-char (point-max))
    (setq last-go (search-backward "go" nil t))
       
    (goto-char (point-max))
    (setq last-line (search-backward "\n" nil nil)) ;; raise error if not found!
       
    (list (or last-go 1) last-line)))


(defmacro with-current-analysis-session (&rest body)
  "Execute body in a context where the current buffer contains the last 'go'-session only"
  `(with-engine-buffer
    (save-excursion
      (with-narrowed-region (last-go-bounds)
                            ,@body))))


(defun chess-analysis-update-summary ()
  (let ((summary (chess-analysis-summary)))
    (with-current-buffer *chess-analysis-summary-buffer*
      (erase-buffer)
      (insert-string summary))))

(defun uci-handle-engine-output (output)
  (when (find ?\C-j output)
    (chess-analysis-update-summary)))


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
