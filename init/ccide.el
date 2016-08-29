(line-number-mode t)			; have line number and
(column-number-mode t)			; column numbers in the model line
(global-hl-line-mode)			; highlight current line
(global-linum-mode t)			; add line numbers on the left

(add-hook 'c-mode-common-hook 'hs-minor-mode)	; fold and hide blocks of text

(require 'dired-x)			; C-x C-j opens dires with cursor right on the file

(defun comment-or-uncomment-region-or-line()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
	(progn
	  (setq beg (region-beginning) end (region-end))
	  (save-excursion
	    (setq beg (progn (goto-char beg) (line-beginning-position))
		  end (progn (goto-char end) (line-end-position)))))
      (setq beg (line-beginning-position)
	    end (line-end-position)))
    (comment-or-uncomment-region beg end)
    (next-line)))

(global-set-key (kbd "C-/") 'comment-or-uncomment-region-or-line)	; comment line or region
(global-set-key (kbd "C-z") 'undo)

(setq
 gdb-many-windows t	; use gdb-many-windows by default
 gdb-show-main t	; non-nil means display source file containing the main routine at startup-echo-area-message
 )

;;;;;;;;;;;;;;;;;;;;;;;;
;; ROS
;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "/opt/ros/kinetic/share/emacs/site-lisp")
(require 'rosemacs-config)
(global-set-key (kbd "C-x C-r") ros-keymap)
(eval (ros-current-pkg-modeline-entry)) ; current package in the mode line
(setq ros-completion-function 'ido-completing-read)
