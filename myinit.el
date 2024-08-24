(setq load-path (cons "~/.emacs.d/modes" load-path))
(setq load-path (cons "~/.emacs.d/scripts" load-path))
(require 'myfuncs)
(require 'openai)
(require 'smb3log)

(when (file-directory-p "~/.emacs.d/modes")
  (load "~/.emacs.d/modes/php-mode-autoloads.el"))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset 4)
 '(js-indent-level 2)
 '(openai-api-key "")
 '(openai-server-url "http://shallow.local:4000/chat/completions")
 '(python-indent-offset 4)
 '(tab-width 4))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(defun kbd1-disabled ()
  (interactive)
  (mykbd1)
  )
(defun kbddisabled2 ()
  (interactive)
  (mykbd2)
  )
(defun kbddisabled3 ()
  (interactive)
  (mykbd3)
  )
(defun kbddisabled4 ()
  (interactive)
  (mykbd4)
  )
(defun kbddisabled5 ()
  (interactive)
  (mykbd5)
  )
(defun kbddisabled6 ()
  (interactive)
  (mykbd6)
  )
(defun kbddisabled7 ()
  (interactive)
  (mykbd7)
  )
(defun kbddisabled8 ()
  (interactive)
  (mykbd8)
  )
(defun kbddisabled9 ()
  (interactive)
  (mykbd9)
  )
(defun kbddisabled0 ()
  (interactive)
  (mykbd0)
  )

(global-set-key (kbd  "C-z") 'undo) 
(global-set-key (kbd  "C-M-g") 'global-set-key)

(define-key input-decode-map "\e\eOA" [(meta up)])
(define-key input-decode-map "\e\eOB" [(meta down)])
(define-key input-decode-map "\e\eOD" [(meta left)])
(define-key input-decode-map "\e\eOC" [(meta right)])

(define-key input-decode-map "\e1" [(meta one)])
(define-key input-decode-map "\e2" [(meta two)])
(define-key input-decode-map "\e3" [(meta three)])
(define-key input-decode-map "\e4" [(meta four)])
(define-key input-decode-map "\e5" [(meta key5)])
(define-key input-decode-map "\e6" [(meta key6)])
(define-key input-decode-map "\e7" [(meta key7)])
(define-key input-decode-map "\e8" [(meta key8)])
(define-key input-decode-map "\e9" [(meta key9)])
(define-key input-decode-map "\e0" [(meta key0)])

(global-set-key [(meta up)] 'start-kbd-macro)
(global-set-key [(meta down)] 'end-kbd-macro)
(global-set-key [(meta right)] 'call-last-kbd-macro)
(global-set-key [(meta left)]  'name-last-kbd-macro)
  
(global-set-key [(meta one)] 'kbd1)
(global-set-key [(meta two)] 'kbd2)
(global-set-key [(meta three)] 'kbd3)
(global-set-key [(meta four)] 'kbd4)
(global-set-key [(meta key5)] 'kbd5)
(global-set-key [(meta key6)] 'kbd6)
(global-set-key [(meta key7)] 'kbd7)
(global-set-key [(meta key8)] 'kbd8)
(global-set-key [(meta key9)] 'kbd9)
(global-set-key [(meta key0)] 'kbd0)



(global-set-key (kbd "<f2>") 'save-buffer)
(global-set-key (kbd "<f3>") 'find-file)
(global-set-key (kbd "<f5>") 'goto-line)


				
(provide 'myinit)
