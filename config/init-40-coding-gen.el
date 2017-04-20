;;; init-40-coding-gen.el --- Code for general programming

;; Copyright (C) 2017 Gregory J Stein

;; Author: Gregory J Stein <gregory.j.stein@gmail.com>
;; Maintainer: Gregory J Stein <gregory.j.stein@gmail.com>
;; Created: 20 Aug 2015

;; Keywords: configuration, company, magit, git, flycheck
;; Homepage: https://github.com/gjstein/emacs.d
;; License: GNU General Public License (see init.el for details)

;;; Commentary:
;; General tools for programming across languages.  This consists of:
;;   Company :: used for code completion
;;   Projectile :: used for searching projects
;;   Magit :: used for interfacing with git/github
;;   Flycheck :: code syntax/convention checking

;;; Code:

;; === Code Completion ===

;; == company-mode ==
(use-package company
  :ensure t
  :defer t
  :diminish company-mode
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config
  (use-package company-irony :ensure t :defer t)
  (setq company-idle-delay              nil
	company-minimum-prefix-length   2
	company-show-numbers            t
	company-tooltip-limit           20
	company-dabbrev-downcase        nil
	)
  (use-package company-quickhelp
    :config
    (company-quickhelp-mode 1)
    (eval-after-load 'company
      '(define-key company-active-map (kbd "C-c h") #'company-quickhelp-manual-begin))
    )
  :bind ("C-;" . company-complete-common)
  )

;; === Tools ===

;; == YASnippet ==
(use-package yasnippet
  :ensure t
  :defer t
  :config (yas-global-mode t)
  )

;; == Projectile ==
(use-package projectile
  :ensure t
  :defer t
  :diminish projectile-mode
  :init
  (projectile-mode)
  (use-package helm-projectile
    :ensure t
    :defer t
    :after helm
    :config
    (helm-projectile-on)
    )
  )

;; == ag ==

;; Note that 'ag' (the silver searcher) needs to be installed.
;; Ubuntu: sudo apt-get install ag
;; OSX: brew install ag

(use-package ag
  :ensure t
  :defer t
  )
(use-package helm-ag
  :ensure t
  :defer t
  :after helm
  )

;; == compile ==

;; https://emacs.stackexchange.com/questions/8135/why-does-compilation-buffer-show-control-characters
(use-package ansi-color
  :ensure t
  :defer t
  :config (progn
            (defun my/ansi-colorize-buffer ()
              (let ((buffer-read-only nil))
                (ansi-color-apply-on-region (point-min) (point-max))))
            (add-hook 'compilation-filter-hook 'my/ansi-colorize-buffer)))


;; == magit ==
(use-package magit
  :ensure t
  :defer t
  :bind ("C-x g" . magit-status)
  :init
  (setq magit-diff-options (quote ("--word-diff")))
  (setq magit-diff-refine-hunk 'all)
  )

;; == flycheck ==
(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :defer t
  :init

  (add-hook 'after-init-hook #'global-flycheck-mode)
  ;; check OS type
  (if (string-equal system-type "gnu/linux")
      (progn
	(custom-set-variables
	 '(flycheck-c/c++-clang-executable "clang-3.8")
	 )))
  (add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++11")))
  (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-gcc))
  )

;; == highlight-parentheses ==
(use-package highlight-parentheses
  :ensure highlight-parentheses
  :config
  (global-highlight-parentheses-mode t)
  (setq hl-paren-colors '("orange1" "pink1"  "red1" "royalblue1" "firebrick1"))
  )

;; == OTHER LANGUAGES ==

(provide 'init-40-coding-gen)
;;; init-40-coding-gen.el ends here
