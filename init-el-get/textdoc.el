;;;
;;; textdoc.el
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
    (:name yaml-mode                     ; yaml edit mode
	   :after (progn
		    (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
		    (add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))))

    (:name org-mode			; organize mode
	   :after (progn
		    (global-set-key (kbd "C-c l") 'org-store-link)
		    (global-set-key (kbd "C-c a") 'org-agenda)
		    (global-set-key (kbd "C-c c") 'org-capture)
		    (global-set-key (kbd "C-c b") 'org-iswitchb)
		    (transient-mark-mode 1)
		    (add-hook 'org-mode-hook (lambda() (setq truncate-lines nil)))
		    ))
    
    (:name markdown-mode)		; markdown written mode

    (:name flyspell-correct
	   :after (progn
		    (require 'flyspell-correct-helm)
		    (define-key flyspell-mode-map (kbd "C-;") 'flyspell-correct-previous-word-generic)
		    ))
    
    (:name company-auctex		; complete latex
	   :after (progn
		    (company-auctex-init)))
    
    (:name company-math			; complete latex math equation
	   :after (progn
		    (eval-after-load 'company
		      '(add-hook 'company-backends 'company-math-symbols-latex))))

    (:name reftex)			; reference management of tex

    (:name magic-latex-buffer)

    (:name helm-bibtex)
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
