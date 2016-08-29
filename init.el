;; emacs configuration
;; Copyright (C) 2016 Shine Song
;;
;; Author: Shine Song <shinesong_sxs@foxmail.com
;; URL: https://shinesong.github.io
;; Created: 2016-08-27
;; Keywords: emacs setup el-get
;; Licence: MIT
;; This file is NOT part of GNU emacs

(global-set-key [f12] (lambda()(interactive)(load-file "~/.emacs.d/init.el")))	; useful reload key
(require 'cl)		; common lisp goodies
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil t)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://github.com/dimitri/el-get/raw/master/el-get-install.el")
    (end-of-buffer)
    (eval-print-last-sexp)))

;; now either el-get is `require'd already, or have been `load'ed by the
;; el-get installer.

;; set local recipes
(setq
 el-get-sources
 '((:name buffer-move			; have to add your own keys
	  :after (progn
		   (global-set-key (kbd "<C-S-up>")     'buf-move-up)
		   (global-set-key (kbd "<C-S-down>")   'buf-move-down)
		   (global-set-key (kbd "<C-S-left>")   'buf-move-left)
		   (global-set-key (kbd "<C-S-right>")  'buf-move-right)))

   (:name smex				; a better (ido like) M-x
   	  :after (progn
   		   (setq smex-save-file "~/.emacs.d/.smex-items")
   		   (global-set-key (kbd "M-x") 'smex)
   		   (global-set-key (kbd "M-X") 'smex-major-mode-commands)))
   
   (:name goto-last-change		; move pointer back to last change
	  :after (progn
		   ;; when using AZERTY keyboard, consider C-x C-_
		   (global-set-key (kbd "C-x C-/") 'goto-last-change)))
   
   (:name yaml-mode                     ; yaml edit mode
	  :after (progn
		   (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
		   (add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))))

   (:name cmake-mode
	  :after (progn
		   (defun maybe-cmake-project-hook()
		     (if (file-exists-p "CMakeLists.txt") (cmake-project-mode)))
		   (add-hook 'c-mode-hook 'maybe-cmake-project-hook)
		   (add-hook 'c++-mode-hook 'maybe-cmake-project-hook)
		   (setq auto-mode-alist
			 (append '(("CMakeLists\\.txt\\'" . cmake-mode))
				 '(("\\.cmake\\'" . cmake-mode))
				 auto-mode-alist))))
   
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
   
   (:name irony-mode			; complete backends of c/c++
	  :after (progn
		   (defun my-irony-mode-hook()
		     (define-key irony-mode-map [remap completion-at-point]
		       'irony-completion-at-point-async)
		     (define-key irony-mode-map [remap complete-symbol]
		       'irony-completion-at-point-async))
		   
		   (add-hook 'c-mode-hook 'irony-mode)
		   (add-hook 'c++-mode-hook 'irony-mode)
		   (add-hook 'objc-mode-hook 'irony-mode)
		   
		   (add-hook 'irony-mode-hook 'my-irony-mode-hook)
		   (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
		   (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)))

   (:name company-irony			; irony adaptor to company
   	  :after (progn   
   		   (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
   		   (eval-after-load 'company
   		     '(add-to-list 'company-backends 'company-irony))))
   
   (:name company-irony-c-headers	; c/c++ header files of irony
   	  :type git
   	  :url "git://github.com/hotpxl/company-irony-c-headers.git"
   	  :feature  "company-irony-c-headers"
   	  :compile "company-irony-c-headers"
   	  :after (progn
   		   (eval-after-load 'company
   		     '(add-to-list
   		       'company-backends 'company-irony-c-headers))))
   
   (:name function-args			; hint for function arg
	  :after (progn
		   (fa-config-default)))
  
   (:name flycheck			; fly check
	  :after (progn
		   (add-hook 'c++-mode-hook 'flycheck-mode)
		   (add-hook 'c-mode-hook 'flycheck-mode)))

   (:name flycheck-irony		; flycheck of irony
	  :after (progn
		   (eval-after-load 'flycheck
		     '(add-hook 'flycheck-mode-hook 'flycheck-irony-setup))))
   (:name ggtags			; gnu global tags
	  :after (progn
		   (require 'ggtags)
		   (add-hook 'c-mode-common-hook
			     (lambda()
			       (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
				 (ggtags-mode 1))))
		   (define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
		   (define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
		   (define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
		   (define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
		   (define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
		   (define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)

		   (define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark)
		   ; support imenu
		   (setq-local imenu-create-index-function #'ggtags-build-imenu-index)
		   (setq-local eldoc-documentation-function #'ggtags-eldoc-function)
		   ))
		   
   (:name rtags				; jump between source/header files
	  :after (progn
		   (require 'company-rtags)
		   (setq rtags-completions-enabled t)
		   (eval-after-load 'company
		     '(add-to-list
		       'company-backends 'company-rtags))
		   (setq rtags-autostart-diagnostics t)
		   (rtags-enable-standard-keybindings)
		   (setq rtags-use-helm t)
		   (require 'flycheck-rtags)
		   (defun my-flycheck-rtags-setup()
		     (flycheck-select-checker 'rtags)
		     (setq-local flycheck-highlighting-mode nil)	; RTags create more accurate overlays-at
		     (setq-local flycheck-check-syntax-automatically nil))
		   (add-hook 'c-mode-common-hook 'my-flycheck-rtags-setup)))

   (:name company-auctex		; complete latex
   	  :after (progn
   		   (company-auctex-init)))
   
   (:name company-math			; complete latex math equation
   	  :after (progn
		   (eval-after-load 'company
		     '(add-hook 'company-backends 'company-math-symbols-latex))))

   (:name reftex			; reference management of tex
	  )

   (:name magic-latex-buffer
	  )
   
   (:name markdown-mode)		; markdown written mode

   (:name org-mode			; organize mode
   	  :after (progn
   		   (global-set-key (kbd "C-c l") 'org-store-link)
   		   (global-set-key (kbd "C-c a") 'org-agenda)
   		   (global-set-key (kbd "C-c c") 'org-capture)
   		   (global-set-key (kbd "C-c b") 'org-iswitchb)
   		   (transient-mark-mode 1)
		   (add-hook 'org-mode-hook (lambda() (setq truncate-lines nil)))
		   ))
   
   (:name projectile			; project management
	  :after (progn
		   (projectile-global-mode)
		   (setq projectile-require-project-root nil)
		   (setq project-enable-caching t)
		   (global-set-key [f5] 'projectile-find-file)
		   (global-set-key [f6] 'projectile-find-other-file)
		   (global-set-key (kbd "<f5>") (lambda ()
                               (interactive)
                               (setq-local compilation-read-command nil)
                               (call-interactively 'projectile-compile-project)))

		   ))
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
   (:name helm-gtags
	  :after (progn
		   (require 'helm-gtags)
		   (setq
		    helm-gtags-ignore-case t
		    helm-gtags-auto-update t
		    helm-gtags-use-input-at-cursor t
		    helm-gtags-pulse-at-cursor t
		    helm-gtags-prefix-key "\C-cg"
		    helm-gtags-suggested-key-mapping t
		    )

		   ;; Enable helm-gtags-mode
		   (add-hook 'dired-mode-hook 'helm-gtags-mode)
		   (add-hook 'eshell-mode-hook 'helm-gtags-mode)
		   (add-hook 'c-mode-hook 'helm-gtags-mode)
		   (add-hook 'c++-mode-hook 'helm-gtags-mode)
		   (add-hook 'asm-mode-hook 'helm-gtags-mode)

		   (define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
		   (define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
		   (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
		   (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
		   (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
		   (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)))
   
   (:name helm-projectile		; integrate projectile into helm
	  :after (progn
		   (projectile-global-mode)
		   (setq projectile-completion-system 'helm)
		   (helm-projectile-on)))

   (:name helm-bibtex			
	  )
   
   (:name helm-ros
   	  :type git
   	  :url "git://github.com/davidlandry93/helm-ros.git")

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

   (:name helm-shell			; shell in helm
	  )

   (:name helm-shell-history
	  )
   
   (:name multi-term			; terminal emulator
   	  :after (progn
   		   (setq multi-term-program "bin/bash")
   		   (setq system-uses-terminfo nil)))

   (:name pdf-tools			; pdf reader
	  )
   (:name sr-speedbar)			; keep speedbar as a buffer
		   
   (:name flyspell-correct
	  :after (progn
		   (require 'flyspell-correct-helm)
		   (define-key flyspell-mode-map (kbd "C-;") 'flyspell-correct-previous-word-generic)
		   ))
   ))

;; set packages without config
(setq
 my:el-get-packages
 '(el-get				; el-get is self-hosting
   escreen				; screen for emacs, C-\ C-h
   switch-window			; take over C-x o
   yasnippet				; powerful snippet mode
   magit				; ma git
   color-theme			        ; theme
   wc					; word count
   ))

;;
;; Some recipes require extra tools to be installed
;;
;; Note: el-get-install requires git, so we know we have at least that.
;;
(when (ignore-errors (el-get-executable-find "cvs"))
  (add-to-list 'my:el-get-packages 'emacs-goodies-el)) ; the debian addons for emacs

(when (ignore-errors (el-get-executable-find "svn"))
  (loop for p in '(psvn    		; M-x svn-status
		   )
	do (add-to-list 'my:el-get-packages p)))

(setq my:el-get-packages
      (append
       my:el-get-packages
       (loop for src in el-get-sources collect (el-get-source-name src))))

;; install new packages and init already installed packages
(el-get 'sync my:el-get-packages)

;;;;;;;;;;;;;;;;;;;;;;;;
;; System configuration
;;;;;;;;;;;;;;;;;;;;;;;;
(setq inhibit-splash-screen t)		; no splash screen
(setq visible-bell t)			; diable error bell
(line-number-mode t)			; have line number and
(column-number-mode t)			; column numbers in the model line
(tool-bar-mode -1)			; no tool bar with icons
(scroll-bar-mode -1)			; no scroll bars
(unless (string-match "apple-darwin" system-configuration)
  ;; on mac, there's always a menu bar drown, don't have it empty
  (menu-bar-mode -1))

;; choose your own fonts, in a system dependant way
(if (string-match "apple-darwin" system-configuration)
    (set-face-font 'default "Monaco-13")
  (set-face-font 'default "Monospace-10"))

(global-hl-line-mode)			; highlight current line
(global-linum-mode t)			; add line numbers on the left

(setq auto-save-default nil)		; disable auto save
(setq-default make-backup-files nil)	; disable backup
(display-time-mode t)			; display time
(setq display-time-24h-format t)
(setq display-time-day-and-date t)	

(mouse-avoidance-mode 'animate)		; mouse avoid cursor when it approach
(add-to-list 'default-frame-alist '(alpha . 100))	; avoid compiz manager rendering bugs
;;(cua-mode)				; copy/past with C-c C-v and C-x, check out C-RET too
(when (string-match "apple-darwin" system-configuration)	;; under mac, have Command as Meta and keep Option for localized input

  (setq mac-allow-anti-aliasing t)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none))
(setq x-select-enable-clipboard t)	; enable clipboard
(windmove-default-keybindings 'meta)	; Navigate windows with M-<arrow>
(setq windmove-wrap-around t)		; ..
(winner-mode 1)				; winner-mode provides C-<left> to get back to previous window layout

(global-auto-revert-mode t)		; whenever an external process changes a file, revert its content to reflect what's on-disk
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)	; Make M-x shell colorful
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(require 'ido)				; active ido mode
(ido-mode t)
(setq ido-save-directory-list-file "~/.emacs.d/.ido.last")
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point 'guess)
(setq ido-show-dot-for-dired t)
(setq ido-default-buffer-method 'selected-window)
(global-set-key (kbd "C-x C-b") 'ido-switch-buffer)
(global-set-key (kbd "C-x B") 'ibuffer)
(setq ido-decorations			; have vertical ido completion lists
      '("\n-> " "" "\n   " "\n   ..." "[" "]"
	" [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]"))

(add-hook 'c-mode-common-hook 'hs-minor-mode)	; fold and hide blocks of text

(require 'dired-x)			; C-x C-j opens dires with cursor right on the file

(defun fullscreen()			; full screen
  (interactive)
  (set-frame-parameter nil 'fullscreen
		       (if (frame-parameter nil 'fullscreen) nil 'fullboth)))
(global-set-key [f11] 'fullscreen)

(unless (string-match "apple-darwin" system-configuration)
  ;; on mac, there's always a menu bar drown, don't have it empty
  (menu-bar-mode -1))

;; choose your own fonts, in a system dependant way
(if (string-match "apple-darwin" system-configuration)
    (set-face-font 'default "Monaco-13")
  (set-face-font 'default "Monospace-10"))
(color-theme-deep-blue)			; load theme
(defun comment-or-uncomment-region-or-line()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
	(progn
	  (setq beg (region-beginning) end (region-end))
	  (save-excursion
	    (setq beg (progn (goto-char beg) (line-beginning-position))
		  end (progn (goto-char end) (line-end-position)))))
      (setq beg (line-beginning-position)
	    end (line-end-position)))
    (comment-or-uncomment-region beg end)
    (next-line)))
(global-set-key (kbd "C-/") 'comment-or-uncomment-region-or-line)	; comment line or region
(global-set-key (kbd "C-z") 'undo)

(setq
 gdb-many-windows t	; use gdb-many-windows by default
 gdb-show-main t	; non-nil means display source file containing the main routine at startup-echo-area-message
 )
;;;;;;;;;;;;;;;;;;;;;;;;
;; ROS
;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "/opt/ros/kinetic/share/emacs/site-lisp")
(require 'rosemacs-config)
(global-set-key (kbd "C-x C-r") ros-keymap)
(eval (ros-current-pkg-modeline-entry)) ; current package in the mode line
(setq ros-completion-function 'ido-completing-read)
