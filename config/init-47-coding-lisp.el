;;; init-47-coding-lisp.el -- Code for LISP

;; Copyright (C) 2017 Shuang Song

;; Author: Shuang Song (shinesong_sxs@foxmail.com)
;;
;; Created: 16 Apr 2017
;;; Commentary:
;;; Code:
(use-package paredit
  :ensure t
  :config
  (autoload 'enable-paredit-mode "paredit"
    "Turn on pseudo-structural editing of Lisp code."
    t)
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
  (add-hook 'scheme-mode-hook 'enable-paredit-mode)
  :diminish "()")


(use-package eldoc :diminish "")

(use-package elisp-slime-nav
  :ensure t
  :config
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook 'turn-on-elisp-slime-nav-mode))
  :diminish " π")

(use-package macrostep
  :ensure t
  :bind ("C-c e" . macrostep-expand))

(use-package lisp-mode
  :config
  ;; the SBCL configuration file is in Common Lisp
  (add-to-list 'auto-mode-alist '("\\.sbclrc\\'" . lisp-mode))
  ;; Open files with .cl extension in lisp-mode
  (add-to-list 'auto-mode-alist '("\\.cl\\'" . lisp-mode))
  
  )

(use-package slime
  :ensure slime-company
  :commands (slime slime-lisp-mode-hook)
  :bind ("C-c s" . slime-selector)
  :config
  (setq inferior-lisp-program "sbcl")
  (slime-setup '(slime-repl slime-fancy slime-company slime-indentation slime-banner slime-highlight-edits))
  (require 'slime-autoloads)
  ;; Add fancy slime contribs
  (add-to-list 'slime-contribs 'slime-fancy)
  (setq slime-net-coding-system 'utf-8-unix)
  )

(provide 'init-47-coding-lisp)
;;; init-47-coding-lisp.el ends here
