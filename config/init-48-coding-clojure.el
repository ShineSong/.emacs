;;; init-48-coding-clojure.el -- Code for Clojure

;; Copyright (C) 2017 Shuang Song

;; Author: Shuang Song (shinesong_sxs@foxmail.com)
;;
;; Created: 3 May 2017
;;; Commentary:
;;; Code:

(use-package clojure-mode
  :ensure t)

(use-package cider
  :ensure t
  :config
  (use-package helm-cider
    :ensure t)
  )

(provide 'init-48-coding-clojure)
;;; init-48-coding-clojure.el ends here
