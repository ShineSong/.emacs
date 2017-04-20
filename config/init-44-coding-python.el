;;; init-44-coding-python.el --- Python configuration

;; Copyright (C) 2017 Gregory J Stein

;; Author: Gregory J Stein <gregory.j.stein@gmail.com>
;; Maintainer: Gregory J Stein <gregory.j.stein@gmail.com>
;; Created: 20 Aug 2015

;; Keywords: configuration, python
;; Homepage: https://github.com/gjstein/emacs.d
;; License: GNU General Public License (see init.el for details)

;;; Commentary:
;; Simply runs python-mode

;;; Code:


;; ensure:
;;; pip install jedi
;;  pip install flake8
;;  pip install importmagic
;;  pip install autopep8
;;  pip install yapf
(use-package elpy
  :ensure t
  :after python
  :config
  (elpy-enable)
  (setq python-shell-interpreter "ipython"
  	python-shell-interpreter-args "--pylab=osx --pdb --nosep --classic"
  	python-shell-prompt-regexp ">>> "
  	python-shell-prompt-output-regexp ""
  	python-shell-completion-setup-code "from IPython.core.completerlib import module_completion"
  	python-shell-completion-string-code "';'.join(module_completion('''%s'''))\n"
  	python-shell-completion-string-code "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

  (setq elpy-rpc-backend "jedi")
  (when (executable-find "ipython")
    (elpy-use-ipython))
  (setq elpy-modules '(elpy-module-sane-defaults
		       elpy-module-company
		       elpy-module-eldoc
		       elpy-module-highlight-indentation
		       elpy-module-pyvenv
		       elpy-module-yasnippet
		       ))
  (define-key python-mode-map (kbd "RET")
    'newline-and-indent)
  (add-hook 'python-mode-hook
	    (lambda ()
	      (set (make-local-variable 'comment-inline-offset) 2)
	      ))
  
  (use-package company-jedi
    :ensure jedi-core
    :config
    (add-to-list 'company-backends 'company-jedi)
    )
  
  )


(provide 'init-44-coding-python)
;;; init-44-coding-python.el ends here
