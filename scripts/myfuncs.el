(provide 'myfuncs)

(defun ck-is-string-in-current-line (search-string)
  "Check if SEARCH-STRING is present in the current line."
  (interactive "sEnter search string: ")
  (save-excursion  ; Preserve the point position
    (let ((line-start (line-beginning-position))
          (line-end (line-end-position))
          line-contents)
      ;; Extract the line contents
      (setq line-contents (buffer-substring-no-properties line-start line-end))
      ;; Check if the line contains the search string
      (if (string-match-p (regexp-quote search-string) line-contents)
          (progn
            (message "String '%s' found in the current line." search-string)
            t)  ; Return t for true
		(progn
		  (beep)  ; Ring the bell
          (error "String '%s' not found in the current line." search-string))))))

(defun ck-split-window-right-if-not ()
  "Split the window right if there is only one window in the current frame."
  (interactive)
  (unless (> (length (window-list)) 1) ; Check if more than one window is open
    (split-window-right)))             ; Split window right if only one window




(defun open-file-at-line (filename linenumber)
  "Open FILENAME and go to LINENUMBER.
If the buffer already exists, switch to it. Otherwise, open the file and go to the line."
  (interactive "fFilename: \nnLine number: ")
  (let ((buffer (get-buffer (file-name-nondirectory filename))))
    (if buffer
        (switch-to-buffer buffer)
	  (find-file (read-file-name (concat "Open file" filename ": ")))
	  ))
  (goto-line linenumber))

