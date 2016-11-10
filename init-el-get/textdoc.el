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
    (:name auctex
	   :after (progn
		    (add-hook 'LaTeX-mode-hook 'flyspell-mode)
		    (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
		    (add-hook 'LaTeX-mode-hook '(lambda()
						  (setq TeX-auto-untabify t
							TeX-engin 'xetex
							TeX-show-compilation t)
						  (setq TeX-save-query nil)
						  (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
						  (setq TeX-command-default "XeLaTeX")
						  ))))
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

    (:name cdlatex-mode)

    (:name chinese-pyim
	   :type git
	   :url "git://github.com/tumashu/chinese-pyim.git"
	   :after (progn
		    (require 'chinese-pyim)
		    (setq defaut-input-method "chinese-pyim")
		    (global-set-key (kbd "C-\\") 'toggle-input-method)
		    (setq pyim-use-tooltip 'popup)
		    (setq-default pyim-english-input-switch-functions
				  '(pyim-probe-program-mode pyim-probe-osg-speed-commands pyim-probe-isearch-mode pyim-probe-org-structure-template))
		    (setq-default pyim-punctuation-half-width-functions
				  '(pyim-probe-punctuation-line-beginning pyim-probe-punctuation-after-punctuation))
		    ))
    (:name chinese-pyim-greatdict
	   :type git
	   :url "git://github.com/tumashu/chinese-pyim-greatdict.git"
	   :after (progn
		    (require 'chinese-pyim-greatdict)
		    (chinese-pyim-greatdict-enable)
		    ))
    )))

;; set packages without configuration
(setq
 my:el-get-packages
 (append
  my:el-get-packages
  '(
    ;; uncomment below line to add new package
    ;; template
    pos-tip
    popup
    )))
