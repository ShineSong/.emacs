;;; package --- Summary
;;
;; Copyright (C) Shuang Song
;;
;; Author: Shuang Song (shinesong_sxs@foxmail.com)
;;
;;; Commentary:
;;; Code:

(use-package multi-term
  :ensure t
  :config
  (setq multi-term-program "/bin/bash")
  (global-set-key (kbd "C-c t") 'multi-term-next)
  (global-set-key (kbd "C-c T") 'multi-term))
(provide 'init-11-term)
;;; init-11-term.el ends here
