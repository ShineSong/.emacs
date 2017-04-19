

(use-package lisp-mode
  :config

  (use-package elisp-slime-nav
    :ensure t
    :commands elisp-slime-nav-mode)

  (use-package macrostep
    :ensure t
    :bind ("C-c e" . macrostep-expand))
  
  (use-package slime
    :ensure t
    :commands (slime slime-lisp-mode-hook)
    :config

    (slime-setup '(slime-repl))
    (require 'slime-autoloads)
    
    ;; the SBCL configuration file is in Common Lisp
    (add-to-list 'auto-mode-alist '("\\.sbclrc\\'" . lisp-mode))
    ;; Open files with .cl extension in lisp-mode
    (add-to-list 'auto-mode-alist '("\\.cl\\'" . lisp-mode))
    ;; Add fancy slime contribs
    (add-to-list 'slime-contribs 'slime-fancy)

    (add-hook 'slime-repl-mode-hook (lambda()
				      (smartparens-strict-mode +1)
				      (whitespace-mode -1)))

    (setq slime-complete-symble-function 'slime-fuzzy-compelete-symbol
 	  slime-fuzzy-completion-in-place t
	  slime-enable-evaluate-in-emacs t
	  slime-autodoc-use-multiline-p t
	  inferior-lisp-program "sbcl"
	  slime-auto-start 'always)

    (define-key slime-mode-map (kbd "TAB") 'slime-indent-amd-complete-symbol)
    (define-key slime-mode-map (kbd "C-c C-s") 'slime-selector)
    (define-key slime-mode-map (kbd "[") (lambda() (interactive) (insert "(")))
    (define-key slime-mode-map (kbd "]") (lambda() (interactive) (insert ")")))
    (define-key slime-mode-map (kbd "(") (lambda() (interactive) (insert "[")))
    (define-key slime-mode-map (kbd ")") (lambda() (interactive) (insert "]")))
  ))
