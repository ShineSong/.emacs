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

;; init package list
(setq el-get-sources ())
(setq my:el-get-packages ())
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Autoload files in init-el-get dir ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq scriptfiles (directory-files "~/.emacs.d/init-el-get" t "\\.el$"))
(loop for file in scriptfiles
      do (load-file file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Autoload files in init dir         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq scriptfiles (directory-files "~/.emacs.d/init" t "\\.el$"))
(loop for file in scriptfiles
      do (load-file file))



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
;; Look
;;;;;;;;;;;;;;;;;;;;;;;;
(setq inhibit-splash-screen t)		; no splash screen
(setq visible-bell t)			; diable error bell
(tool-bar-mode -1)			; no tool bar with icons
(scroll-bar-mode -1)			; no scroll bars
(unless (string-match "apple-darwin" system-configuration)
  ;; on mac, there's always a menu bar drown, don't have it empty
  (menu-bar-mode -1))
;; choose your own fonts, in a system dependant way
(if (string-match "apple-darwin" system-configuration)
    (set-face-font 'default "Monaco-13")
  (set-face-font 'default "Monospace-10"))

(display-time-mode t)			; display time
(setq display-time-24h-format t)
(setq display-time-day-and-date t)	

(add-to-list 'default-frame-alist '(alpha . 100))	; avoid compiz manager rendering bugs
;;(cua-mode)				; copy/past with C-c C-v and C-x, check out C-RET too
(when (string-match "apple-darwin" system-configuration)	;; under mac, have Command as Meta and keep Option for localized input
  (setq mac-allow-anti-aliasing t)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none))

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




