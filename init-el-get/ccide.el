;;;
;;; ccide.el
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
    ;; uncomment below line to add new package
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

    (:name helm-ros
	   :type git
	   :url "git://github.com/davidlandry93/helm-ros.git")
    )))

;; set packages without configuration
(setq
 my:el-get-packages
 (append
  my:el-get-packages
  '(
    ;; uncomment below line to add new package
    ;; template
    )))
