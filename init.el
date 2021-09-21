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
 '(custom-enabled-themes '(spacemacs-dark))
 '(custom-safe-themes
	 '("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" default))
 '(hl-todo-keyword-faces
	 '(("TODO" . "#dc752f")
		 ("NEXT" . "#dc752f")
		 ("THEM" . "#2d9574")
		 ("PROG" . "#4f97d7")
		 ("OKAY" . "#4f97d7")
		 ("DONT" . "#f2241f")
		 ("FAIL" . "#f2241f")
		 ("DONE" . "#86dc2f")
		 ("NOTE" . "#b1951d")
		 ("KLUDGE" . "#b1951d")
		 ("HACK" . "#b1951d")
		 ("TEMP" . "#b1951d")
		 ("FIXME" . "#dc752f")
		 ("XXX+" . "#dc752f")
		 ("\\?\\?\\?+" . "#dc752f")))
 '(package-selected-packages
	 '(evil evil-mode lsp-ui-peek-mode lsp-ui-peek magit maggit helm-projectile helm projectile ## spacemacs-dark org-mode spacemacs-theme npm-mode lsp-dart emmet-mode which-key js2-mode js-mode typescript-mode yasnippet dap-mode lsp-treemacs lsp-ui flycheck company lsp-mode use-package))
 '(pdf-view-midnight-colors '("#b2b2b2" . "#292b2e")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq column-number-mode t)
(global-display-line-numbers-mode t)
(show-paren-mode t)
(setq inhibit-startup-screen t)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(visual-line-mode 1)
(electric-pair-mode 1)

(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)
(global-set-key "\C-x2" (lambda () (interactive)(split-window-vertically) (other-window 1)))
(global-set-key "\C-x3" (lambda () (interactive)(split-window-horizontally) (other-window 1)))

(use-package helm
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
				 (js-mode . lsp)
				 (scss-mode . lsp)
				 (lsp-deferred . lsp-enable-which-key-integration)
				 (lsp . lsp-ui-mode)
				 (lsp . lsp-ui-peek-mode)
				 (lsp . dap-mode)
				 (lsp . company-mode))
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
  :commands
	lsp-treemacs-errors-list
	:init
	(lsp-treemacs-sync-mode 1)
  :ensure t)

(use-package which-key
  :config
  (which-key-mode)
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
  :ensure t)

(use-package mhtml-mode
	:hook
	(mhtml-mode . emmet-mode))

(use-package spacemacs-theme
  :defer t
  :init (load-theme 'spacemacs-dark t)
	:ensure t)

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

(use-package evil
	:ensure t)
(evil-mode 1)
