;;;
;;; common.el
;;;
;;; Author: Shine Song
;;;
;;; Create Date: 2016-8-29
;;;
;;; This file is auto load by init.el
;;; List of Packages:
;;;

;; set packages with configuration
(setq
 el-get-sources
 (append
  el-get-sources
  '(
    (:name buffer-move			; have to add your own keys
	   :after (progn
		    (global-set-key (kbd "<C-S-up>")     'buf-move-up)
		    (global-set-key (kbd "<C-S-down>")   'buf-move-down)
		    (global-set-key (kbd "<C-S-left>")   'buf-move-left)
		    (global-set-key (kbd "<C-S-right>")  'buf-move-right)))

    (:name goto-last-change		; move pointer back to last change
	   :after (progn
		    ;; when using AZERTY keyboard, consider C-x C-_
		    (global-set-key (kbd "C-x C-/") 'goto-last-change)))

    (:name pdf-tools)			; pdf reader
    
    (:name sr-speedbar)			; keep speedbar as a buffer

    (:name multi-term			; terminal emulator
	   :after (progn
		    (setq multi-term-program "bin/bash")
		    (setq system-uses-terminfo nil)))
    
    (:name company-mode			; complete framework
	   :after (progn
		    (require 'company)
		    (require 'cc-mode)
		    (add-hook 'after-init-hook 'global-company-mode)
		    (setq company-minimum-prefix-length 0)
		    (setq company-async-timeout 5)
		    (setq company-idle-delay 1)

		    (setq company-backends (delete 'company-semantic company-backends))
		    (define-key c-mode-map [(tab)] 'company-complete)
		    (define-key c++-mode-map [(tab)] 'compan-complete)))
    
    (:name helm				; interactive window to show much things		
	   :after (progn
		    (require 'helm-config)
		    (global-set-key (kbd "C-c h") 'helm-command-prefix)			; default C-x c is too close to  C-x C-c
		    (global-unset-key (kbd "C-x c"))

		    (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)	; rebind tab to run persistent
		    (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)	; make TAB work in terminal
		    (define-key helm-map (kbd "C-z") 'helm-select-action)		; list actions using C-x C-z
		    (when (executable-find "curl")
		      (setq helm-google-suggest-use-curl-p t))

		    (setq helm-split-window-in-side-p t	; open helm buffer inside current window, not occupy whole other window
			  helm-move-to-line-cycle-in-source t	; move to end or beginning of source when reaching top or bottom of source.
			  helm-ff-search-library-in-sexp t	; search for library in `require` and `declare-function` sexp.
			  helm-scroll-amount 8			; scroll 8 lines other window using M-<next>/M-<prior>
			  helm-ff-file-name-history-use-recentf t)
		    (helm-mode t)

		    ))

    (:name ibuffer-vc			; group files by project
	   :after (progn
		    (add-hook 'ibuffer-hook
			      (lambda()
				(ibuffer-vc-set-filter-groups-by-vc-root)
				(unless (eq ibuffer-sorting-mode 'alphabetic)
				  (ibuffer-do-sort-by-alphabetic))))
		    (setq ibuffer-formats
			  '((mark modified read-only vc-status-mini " "
				  (name 18 18 :left :elide)
				  " "
				  (size 9 -1 :right)
				  " "
				  (mode 16 16 :left :elide)
				  " "
				  (vc-status 16 16 :left)
				  " "
				  filename-and-process)))))

    (:name smex				; a better (ido like) M-x
	   :after (progn
		    (setq smex-save-file "~/.emacs.d/.smex-items")
		    (global-set-key (kbd "M-x") 'smex)
		    (global-set-key (kbd "M-X") 'smex-major-mode-commands)))
    
    (:name helm-shell)			; shell in helm
    
    (:name helm-shell-history)
    )))

;; set packages without configuration
(setq my:el-get-packages
      (append
       my:el-get-packages
       '(
	 ;; uncomment below line to add new package
	 el-get				; el-get is self-hosting
	 escreen				; screen for emacs, C-\ C-h
	 switch-window			; take over C-x o
	 yasnippet				; powerful snippet mode
	 magit				; ma git
	 color-theme			        ; theme
	 wc					; word count
	 )))
