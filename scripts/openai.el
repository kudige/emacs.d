(provide 'openai)
(require 'json)
(require 'url)

(defcustom openai-server-url "shallow.local:4000/chat/completions"
  "URL of the OpenAI server endpoint."
  :type 'string
  :group 'openai)

(defcustom openai-api-key ""
  "OpenAI api key (optional)."
  :type 'string
  :group 'openai)

(defcustom openai-model "llama3.1"
  "OpenAI api model name"
  :type 'string
  :group 'openai)

(defcustom openai-system-prompt ""
  "System prompt for OpenAI."
  :type 'string
  :group 'openai)
(make-variable-buffer-local 'openai-system-prompt)

;;;;
;;;; Mode Specific Prompts
;;;;

(defcustom openai-system-prompt-global 
  "Never use a single backtick. Always use triple backticks and ensure there is an extra line at the beginning of the code block"
  "Appended to al system prompts"
  :type 'string
  :group 'openai)

(defcustom openai-c-system-prompt "You are a C coding assistant. You will help me write C code. When editing a function, always return the full edited version of the function in the code block."
  "System prompt for OpenAI in C mode"
  :type 'string
  :group 'openai)

(defun openai-c-mode-common-hook ()
  ;; my customizations for all of c-mode and related modes
  (setf openai-system-prompt (concat openai-system-prompt-global ", " openai-c-system-prompt))
  )
(add-hook 'c-mode-common-hook 'openai-c-mode-common-hook)
(add-hook 'c++-mode-hook 'openai-c++-mode-hook)

(defcustom openai-python-system-prompt "You are a Python coding assistant. You will help me write and edit python code. Always return only the code and nothing else. When writing scripts take arguments from command line, and always add #!/usr/bin/env python3 to the scripts"
  "System prompt for OpenAI in Python mode"
  :type 'string
  :group 'openai)

(defun openai-python-mode-hook ()
  ;; my customizations for all of c-mode and related modes
  (setf openai-system-prompt (concat openai-python-system-prompt ", " openai-system-prompt-global))
  )
(add-hook 'python-mode-hook 'openai-python-mode-hook)

(defcustom openai-emacs-lisp-system-prompt "You are a Emacs Lisp coding assistant. You will help me write emacs lisp code. You only return code and nothing else."
  "System prompt for OpenAI in ELisp mode"
  :type 'string
  :group 'openai)

(defun openai-emacs-lisp-mode-hook ()
  (setf openai-system-prompt (concat openai-emacs-lisp-system-prompt ", " openai-system-prompt-global))
  )
(add-hook 'emacs-lisp-mode-hook 'openai-emacs-lisp-mode-hook)

(defcustom openai-shell-system-prompt "You are a Bash coding assistant. You will help me write bash scripts. You may be asked to write small commands or whole scipts."
  "System prompt for OpenAI in Shell mode"
  :type 'string
  :group 'openai)

(defun openai-shell-mode-hook ()
  (setf openai-system-prompt (concat openai-shell-system-prompt ", " openai-system-prompt-global))
  )
(add-hook 'shell-mode-hook 'openai-shell-mode-hook)
(add-hook 'sh-mode-hook 'openai-shell-mode-hook)
(add-hook 'eshell-mode-hook 'openai-shell-mode-hook)
(add-hook 'term-mode-hook 'openai-shell-mode-hook)

(defcustom openai-text-system-prompt "You are a writing assistant. You will generate text based on what I ask and always return the content within a code block"
  "System prompt for OpenAI in Text mode"
  :type 'string
  :group 'openai)

(defun openai-text-mode-hook ()
  (setf openai-system-prompt (concat openai-text-system-prompt ", " openai-system-prompt-global))
  )
(add-hook 'text-mode-hook 'openai-text-mode-hook)

(defcustom openai-system-prompt-powershell-mode "You are a PowerShell assistant. You will help me write and edit PowerShell scripts. Always return only the code and nothing else. Provide useful commands and functions for managing the system."
  "System prompt for OpenAI in PowerShell mode"
  :type 'string
  :group 'openai)

(defun openai-powershell-mode-hook ()
  ;; customizations specific to PowerShell mode
  (setf openai-system-prompt (concat openai-system-prompt-powershell-mode ", " openai-system-prompt-global))
  )
(add-hook 'powershell-mode-hook 'openai-powershell-mode-hook)


;; Buffer specific variable to store the current context
(defvar openai-context `[]
  "The current conversation context excluding system message")
(make-variable-buffer-local 'openai-context)

;; Any additional info to be sent with the query
(defvar openai-extra-context ""
  "Extra context (text, code block etc) for the request")

(defun string-has-two-ticks (str)
  (let ((count 0)
        (start 0))
    (while (and (string-match "```" str start)
                (< count 2))
      (setq count (1+ count))
      (setq start (match-end 0)))
    (= count 2)))

(defun extract-between-ticks (string)
  "Extracts the contents between the first two occurrences of triple backticks in STRING and removes the first line within the extracted text."
  (let* ((start-pos (string-match "```" string))
         (end-pos (and start-pos (string-match "```" string (+ 3 start-pos)))))
    (when (and start-pos end-pos)
      (let ((extracted (substring string (+ 3 start-pos) end-pos)))
        ;; Find the end of the first line within the extracted string
        (let ((first-line-end-pos (string-match "\n" extracted)))
          ;; Return the substring starting from the character after the first newline
          (when first-line-end-pos
            (substring extracted (+ 1 first-line-end-pos))))))))

(defun do-openai-query (prompt callback)
  (if (not (functionp callback))
      (error "Must provide a callback function to do-openai-query"))

  (when (and openai-extra-context
           (not (string= "" openai-extra-context)))
	(setq prompt (concat prompt "\n```" openai-extra-context "\n```"))
	(setq openai-extra-context "")
	)

  
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("Accept" . "application/json")
            ("Authorization" . ,(concat "Bearer " openai-api-key))
			))

         (context (vconcat openai-context `[((role . "user") (content . ,prompt))]))
		 (messages (vconcat `[((role . "system") (content . ,openai-system-prompt))]
							context))
         (body-content `(:messages ,messages :model ,openai-model))
         (request-data (json-encode `(:messages ,messages
                                                :model ,openai-model
												)))
         (url-request-data request-data)
         (url (format "%s" openai-server-url))
		 )
	(setq openai-tmp-buffer (current-buffer))
    (url-retrieve
     url
     (lambda (status context callback)
	   (let ((http-status (if (plist-get status :error)
                                         (concat "Error: " (format "%s" (plist-get status :error)))
                            nil)))
		 (if http-status
			 (message (format "%s" http-status))

		   
		   (goto-char (point-min))
		   (re-search-forward "\n\n")
		   
		   (let* ((json-response (buffer-substring-no-properties (point) (point-max)))
				  (parsed-response (json-parse-string json-response))
				  (result (gethash "content" (gethash "message" (elt (gethash "choices" parsed-response) 0))))
				  (result-code (extract-between-ticks result))
				  )
										; Uncomment next line to debug the response
										;(message (concat "AI JSON: " json-response))
			 (kill-buffer (current-buffer))
			 (with-current-buffer openai-tmp-buffer
			   (setq openai-context (vconcat context
											 `[((role . "assistant") (content . ,result))]))
			   (apply callback (list result-code))
			   )
			 )
		   )
		 )
	   )
	 (list context callback) nil nil
	)
	)
  )

(defun openai-query-insert(prompt)
  "Send a prompt to the OpenAI server and insert the response at the cursor position."
  (interactive "sEnter your prompt: ")

  (setq my-point (point)) ; Save current cursor position
  (setq my-buffer (current-buffer))
  (do-openai-query prompt (lambda (result)
  		 (with-current-buffer my-buffer
		   (goto-char my-point)
		   (set-mark my-point)
           (insert result)
		   )
		 )
				   )
  )

(defun openai-query-file(prompt)
  "Send a prompt to the OpenAI server and replace the entire file with the new response."
  (interactive "sEnter your prompt: ")

  (setq my-buffer (current-buffer))
  (do-openai-query prompt (lambda (result)
  		 (with-current-buffer my-buffer
		   (let ((inhibit-read-only t))  ; Allow editing of read-only buffers
			 (erase-buffer)              ; Erase the existing contents
			 (set-mark (point))
			 (insert result))))
				   )
  )

(defun openai-query-region(prompt)
  "Send a prompt to the OpenAI server and replace the current region with the new response."
  (interactive "sEnter your prompt: ")

  (setq my-beg (region-beginning))
  (setq my-end (region-end))
  (setq my-buffer (current-buffer))

  (do-openai-query prompt (lambda (result)
							(with-current-buffer my-buffer
							  (let ((inhibit-read-only t))  ; Allow editing of read-only buffers
								(delete-region my-beg my-end)       ; Delete the current region
								(goto-char my-beg)               ; Move back to region start
								(set-mark my-beg)
								(insert result)))))
  )


(defun openai-query-modify(prompt)
  "Send a prompt to the OpenAI server and replace the current region with the new response."
  (interactive "sEnter your prompt: ")
  (openai-reset)
  (openai-set-extra)
  (openai-query-region prompt)
  )

(defun openai-reset()
  (interactive)
  (setq openai-context `[])
  (message "AI Chat reset")
  )

(defun openai-set-extra()
  (interactive)
  (setq openai-extra-context (buffer-substring-no-properties (mark) (point)))
  (message "Extra context saved. Will be used in your next query")
  )

(defun openai-toggle-server ()
  (interactive)
  (if (string-match "openai" openai-server-url)
      (progn
        (setq openai-server-url "http://192.168.1.62:4000/chat/completions")
        (setq openai-model "llama3.1"))
    (progn
      (setq openai-server-url "https://api.openai.com/v1/chat/completions")
      (setq openai-model "gpt-4o")))
  (if (string-match "openai" openai-server-url)
      (message "Switching to OpenAI server")
    (message "Switching to Local")))


(global-set-key (kbd "C-c i") 'openai-query-insert)
(global-set-key (kbd "C-c f") 'openai-query-file)
(global-set-key (kbd "C-c r") 'openai-query-region)
(global-set-key (kbd "C-c m") 'openai-query-modify)
(global-set-key (kbd "C-c x") 'openai-reset)
(global-set-key (kbd "C-c e") 'openai-set-extra)
(global-set-key (kbd "C-c t") 'openai-toggle-server)

										;(message prompt)
