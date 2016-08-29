(setq auto-save-default nil)		; disable auto save
(setq-default make-backup-files nil)	; disable backup
(global-auto-revert-mode t)		; whenever an external process changes a file, revert its content to reflect what's on-disk
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)	; Make M-x shell colorful
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(mouse-avoidance-mode 'animate)		; mouse avoid cursor when it approach

(setq x-select-enable-clipboard t)	; enable clipboard
(windmove-default-keybindings 'meta)	; Navigate windows with M-<arrow>
(setq windmove-wrap-around t)		; ..
(winner-mode 1)				; winner-mode provides C-<left> to get back to previous window layout

(require 'ido)				; active ido mode
(ido-mode t)
(setq ido-save-directory-list-file "~/.emacs.d/.ido.last")
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point 'guess)
(setq ido-show-dot-for-dired t)
(setq ido-default-buffer-method 'selected-window)
(global-set-key (kbd "C-x C-b") 'ido-switch-buffer)
(global-set-key (kbd "C-x B") 'ibuffer)
(setq ido-decorations			; have vertical ido completion lists
      '("\n-> " "" "\n   " "\n   ..." "[" "]"
	" [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]"))
