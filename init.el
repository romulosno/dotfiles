(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
	 [default default default italic underline success warning error])
 '(ansi-color-names-vector
	 ["#2e3436" "#a40000" "#4e9a06" "#c4a000" "#204a87" "#5c3566" "#729fcf" "#eeeeec"])
 '(ansi-term-color-vector
	 [unspecified "#FFFFFF" "#d15120" "#5f9411" "#d2ad00" "#6b82a7" "#a66bab" "#6b82a7" "#505050"] t)
 '(custom-safe-themes
	 (quote
		("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "36f17556e827b41b63fe9375dbeeb4989d4976fe51cd0506968c6a41d2a7c9f8")))
 '(package-selected-packages
	 (quote
		(doom-modeline command-log-mode dired rainbow-delimiters dired-single all-the-icons-dired all-the-icons org-brain smart-mode-line-atom-one-dark-theme smart-mode-line: atom-one-dark smart-mode-line atom-one-dark-theme zenburn-theme org-mind-map helm-rg evil-mode lsp-ui-peek-mode lsp-ui-peek magit maggit helm-projectile helm projectile ## spacemacs-dark org-mode spacemacs-theme npm-mode lsp-dart emmet-mode which-key js2-mode js-mode typescript-mode yasnippet dap-mode lsp-treemacs lsp-ui flycheck company lsp-mode use-package)))
 '(pdf-view-midnight-colors (quote ("#b2b2b2" . "#292b2e")))
 '(tetris-x-colors
	 [[229 192 123]
		[97 175 239]
		[209 154 102]
		[224 108 117]
		[152 195 121]
		[198 120 221]
		[86 182 194]]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(line-number ((t (:inherit default)))))

(load-theme 'wombat)

(global-linum-mode t)
(global-visual-line-mode t)
(show-paren-mode t)
(setq inhibit-startup-screen t)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(visual-line-mode 1)
(electric-pair-mode 1)
(desktop-save-mode 1)

(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

(global-set-key "\M-f" 'forward-to-word)
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)
(global-set-key "\C-x2" (lambda () (interactive)(split-window-vertically) (other-window 1)))
(global-set-key "\C-x3" (lambda () (interactive)(split-window-horizontally) (other-window 1)))

(define-key global-map [remap forward-word] 'forward-to-word)

(use-package helm
	:config
	(define-key global-map [remap find-file] 'helm-find-files)
	(define-key global-map [remap occur] 'helm-occur)
	(define-key global-map [remap list-buffers] 'helm-buffers-list)
	(define-key global-map [remap dabbrev-expand] 'helm-dabbrev)
	(define-key global-map [remap execute-extended-command] 'helm-M-x)
	(define-key global-map [remap apropos-command] 'helm-apropos)
	:ensure t)

(use-package ido
  :config 
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  (setq ido-use-filename-at-point 'guess)
  (ido-mode 1)
  :ensure t)

(use-package company
	:config
	(setq company-minimum-prefix-length 1)
	:ensure t)
(add-hook 'after-init-hook 'global-company-mode)
(use-package dap-mode
  :ensure t)

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-l")
  (setq lsp-idle-delay 0.500)
	(setq lsp-lens-enable t)
	(setq lsp-signature-auto-activate nil)
	(add-hook 'lsp-mode-hook 'lsp-ui-mode)
  :config 
  (define-key lsp-mode-map (kbd "C-l") lsp-command-map)
  :hook (
				 (typescript-mode . lsp-deferred)
				 (js-mode . lsp-deferred)
				 (scss-mode . lsp-deferred)
				 (lsp-deferred . lsp-enable-which-key-integration)
				 (lsp-deferred . lsp-ui-mode)
				 (lsp-deferred . lsp-ui-peek-mode)
				 (lsp-deferred . dap-mode)
				 (lsp-deferred . company-mode))
  :commands 
	(lsp lsp-deferred)
  :ensure t)

(use-package lsp-ui
  :commands
	lsp-ui-mode
	lsp-ui-si
	:config
	(setq lsp-ui-sideline-show-diagnostics t)
	(setq lsp-ui-sideline-show-symbol nil)
	:hook (lsp-mode-hook . lsp-ui-mode)
  :ensure t)

(use-package lsp-treemacs
	:config
	(local-set-key (kbd "C-l T v") 'lsp-treemacs-symbols)
  :commands
	lsp-treemacs-errors-list
	:init
	(lsp-treemacs-sync-mode 1)
  :ensure t)

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3)
	:ensure t)

(use-package term
  :config
  (setq explicit-shell-file-name "bash")
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

(use-package yasnippet
  :ensure t)

(use-package typescript-mode
  :config
  (setq-default typescript-indent-level 2)
  (setq-default tab-width 2)
  :ensure t)

(use-package js2-mode
  :config
  (setq-default js-indent-level 2)
  (setq tab-width 2)
  :ensure t)
(put 'dired-find-alternate-file 'disabled nil)

(use-package emmet-mode
	:config
	(setq emmet-move-cursor-between-quotes t)
  :ensure t)

(use-package mhtml-mode
	:hook
	(mhtml-mode . emmet-mode))

(use-package org
	:ensure t)

(use-package npm-mode
	:ensure t)

(use-package helm-projectile
	:ensure t)

(use-package projectile
	:config
	(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
	:hook
	(projectile . lsp-dired-mode)
	:init
	(helm-projectile-on)
	:ensure t)
(projectile-mode +1)

(use-package magit
	:ensure t)

(use-package flymake
	:commands
	(flymake-show-diagnostic)
	:ensure t)

(use-package vimrc-mode
	:ensure t)

(use-package atom-one-dark-theme
	:ensure t)
(load-theme 'atom-one-dark t)

(use-package smart-mode-line
	:ensure t
	:config
	(setq sml/theme 'respectful))
(smart-mode-line-enable)

(when (display-graphic-p)
  (require 'all-the-icons))
;; or
(use-package all-the-icons
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode)
	:ensure t)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
	:ensure t)

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
	:bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-agho --group-directories-first")))

(use-package dired-single
	:ensure t)

(use-package command-log-mode
	:ensure t)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

;; Functions
(defun npm-start()
	(interactive)
	(npm-mode--exec-process "npm start"))





