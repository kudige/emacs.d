(define-derived-mode smb3-logfile-mode fundamental-mode "SMB3-Logfile"
  "Major mode for editing SMB3 log files."

  ;; Syntax highlighting
  (setq font-lock-defaults
        '((
           ("DispatchCommand received.*messageId [0-9]+ [A-Z_]+" . font-lock-keyword-face)
           ("DispatchResponse sending response.*messageId [0-9]+ [A-Z_]+" . font-lock-keyword-face)
           ("^<.*?>" . font-lock-constant-face)
           ("[a-z_A-Z]+\\.[a-z]+:[0-9]+:[0-9]+\\.[0-9]+" . font-lock-comment-face)
		   ("Error throw.*" . font-lock-warning-face)
           )))

  ;; Mode variables
  (setq next-error-function 'smb3log-next-error)

  ;; Key bindings
  (define-key smb3-logfile-mode-map (kbd "f") 'font-lock-fontify-buffer)
  (define-key smb3-logfile-mode-map (kbd "h") 'highlight-lines-with-same-prefix)
  (define-key smb3-logfile-mode-map (kbd "<up>") 'on-up-arrow-key-press)
  (define-key smb3-logfile-mode-map (kbd "<down>") 'on-down-arrow-key-press)
  (define-key smb3-logfile-mode-map (kbd "RET") 'smb3log-goto-file)
  (define-key smb3-logfile-mode-map (kbd "e") 'smb3log-next-error)
  (define-key smb3-logfile-mode-map (kbd "E") 'smb3log-prev-error)
  )
;; Function to detect specific text pattern in .log files
(defun smb3-detect-pattern ()
  (when (and (string= (file-name-extension buffer-file-name) "log")
             (save-excursion
               (goto-char (point-min))
               (re-search-forward "main.c:[0-9]+:[0-9]+\\.[0-9]+ \\./smb3server:" nil t)))
    (smb3-logfile-mode)))

(defvar my-highlight-overlays '()
  "List of overlays used to highlight lines.")

(defun highlight-lines-with-same-prefix ()
  "Highlight all lines in the visible buffer with the same prefix between < and > as the current line."
  (interactive)
  (remove-highlight-overlays)
  (let* ((current-line (thing-at-point 'line t))
         (prefix (if (string-match "<\\(.*?\\)>" current-line)
                     (match-string 1 current-line)
                   nil)))
	(message prefix)
    (when prefix
      (save-excursion
        (goto-char (window-start))
        (while (< (point) (window-end))
          (when (re-search-forward (concat "<" (regexp-quote prefix) ">") (line-end-position) t)
            (setq my-highlight-overlays
                  (cons (highlight-line) my-highlight-overlays)))
          (forward-line 1))))))

(defun highlight-line ()
  "Highlight the current line."
  (let ((overlay (make-overlay (line-beginning-position) (line-end-position))))
    (overlay-put overlay 'face 'highlight)
    overlay))

(defun remove-highlight-overlays ()
  "Remove all highlight overlays."
  (dolist (overlay my-highlight-overlays)
    (delete-overlay overlay))
  (setq my-highlight-overlays '())
  )

(defun on-up-arrow-key-press ()
  "Unhighlight current highlights and rehighlight based on the current line."
  (interactive)
  (previous-line)
  (highlight-lines-with-same-prefix)
  )

(defun on-down-arrow-key-press ()
  "Unhighlight current highlights and rehighlight based on the current line."
  (interactive)
  (next-line)
  (highlight-lines-with-same-prefix)
  )


(defun smb3log-goto-file ()
  "Goto the source file for the current log line"
  (interactive)
  (let* ((line (thing-at-point 'line t))
		 )
	(message line)
	(when (string-match "<\\([^/]+\\)/[0-9]+>\\([^:]+\\):\\([0-9]+\\):" line)
      (let ((filename (match-string 2 line))
			(linenumber (string-to-number (match-string 3 line))))
		(open-file-at-line filename linenumber)
		)))
  )


(defun smb3log-next-error(&optional arg reset)
  "Goto the next line that matches the pattern 'Error thrown'"
  (interactive)
  (search-forward "Error thrown" nil t))

(defun smb3log-prev-error()
  "Goto the previous line that matches the pattern 'Error thrown'"
  (interactive)
  (search-backward "Error thrown" nil t))

;; Add detection function to `find-file-hook`
(add-hook 'find-file-hook #'smb3-detect-pattern)

;; Associate .smb3log files with smb3-logfile-mode
(add-to-list 'auto-mode-alist '("\\.smb3log\\'" . smb3-logfile-mode))

; Move global-set-key to mode map
(fset 'move-global-set-key
   (kmacro-lambda-form [?\C-s ?g ?l ?o ?b ?a ?l ?- ?s ?e ?t ?- ?k ?e ?y ?\C-m ?\C-a ?\C-@ ?\C-\[ ?> ?\C-r ?g ?l ?o ?b ?a ?l ?- ?\C-m ?\C-\[ ?O ?B ?\C-a ?\C-w ?\C-\[ ?\[ ?O ?\C-\[ ?\[ ?I ?\C-r ?s ?m ?b ?3 ?- ?l ?o ?g ?f ?i ?l ?e ?- ?m ?o ?d ?e ?- ?m ?a ?p ?\C-m ?\C-\[ ?O ?B ?\C-a ?\C-o ?\C-y ?\C-x ?\C-x ?\C-\[ ?x ?r ?e ?p ?l ?\C-i ?s ?t ?r ?i ?n ?\C-i ?\C-m ?g ?l ?o ?b ?a ?l ?- ?s ?e ?t ?- ?k ?e ?y ?\C-m ?d ?e ?f ?i ?n ?e ?- ?k ?e ?y ?  ?s ?m ?b ?3 ?- ?l ?o ?g ?i ?f ?\C-? ?\C-? ?f ?i ?l ?e ?- ?m ?o ?d ?e ?- ?m ?a ?p ?\C-m ?\C-\[ ?O ?B ?\C-x ?\C-x ?\C-\[ ?x ?i ?n ?d ?e ?n ?t ?- ?r ?e ?g ?\C-i ?\C-m ?\C-x ?\C-x] 0 "%d"))

(provide 'smb3log)
