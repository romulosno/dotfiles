(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(use-package auto-package-update
  :ensure t
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

(use-package no-littering
  :ensure t)
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(setq inhibit-startup-screen t)		; Remove a tela inicial padrão
(global-linum-mode 1)			; Número das linhas
(setq visible-bell 1)			; Remove o beep infernal
(toggle-scroll-bar -1)			; Remove scroll
(tool-bar-mode -1)			; Remove barra de ferramenta
(menu-bar-mode -1)			; Remove menus
(set-fringe-mode 10)			; Padding

;; Desabilita números das linhas em alguns modos
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (global-linum-mode nil))))

(setq gc-cons-threshold (* 50 1000 1000)) ; Performance
(setq read-process-output-max (* 1024 1024)) ; Performance
(setq enable-recursive-minibuffers t) ; Permite minibuffers recursivos
(defalias 'yes-or-no-p 'y-or-n-p)     ; Define y e n para sim e não

(global-set-key (kbd "<f5>") 'kill-buffer-and-window)
(global-set-key "\C-x2" (lambda () (interactive)(split-window-vertically) (other-window 1)))
(global-set-key "\C-x3" (lambda () (interactive)(split-window-horizontally) (other-window 1)))

(electric-pair-mode 1)			; Fechar parenteses
(show-paren-mode 1)			; Mostra o parenteses par

(use-package which-key
  :ensure t
  :config (which-key-mode 1)
  :init (setq which-key-idle-delay 0.3))

(use-package org
  :ensure t
  :init (add-hook 'org-mode-hook (lambda() (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))
  :custom ((setq org-return-follows-link  t)
           (setq org-src-tab-acts-natively t)
           (setq org-latex-toc-command "\\tableofcontents \\clearpage"))
  :hook ((org-mode . org-indent-mode)
         (org-mode . org-bullets-mode)))

(defun efs/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/dotfiles/.emacs.d/Emacs.org"))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle)
      (load "~/dotfiles/.emacs.d/init.el"))))

(use-package lsp
  :custom ((setq lsp-log-io nil) ; Performance
           (setq lsp-idle-delay 0.500) 
           (setq lsp-lens-enable t)
           (setq lsp-signature-auto-activate nil))
  :init (setq lsp-keymap-prefix "C-l")
  :commands (lsp lsp-deferred)
  :bind-keymap ("C-l" . lsp-command-map)
  :hook ((typescript-mode . lsp-deferred)
         (js-mode . lsp-deferred)
         (scss-mode . lsp-deferred)
         (java-mode . lsp-deferred)
         (python-mode . lsp-deferred)
         (lsp-mode . lsp-enable-whick-key-integration)))

(use-package dap-mode
  :ensure t
  :hook (lsp-mode . dap-mode))

(use-package ido
  :ensure t
  :custom ((setq ido-enable-flex-matching t)
           (setq ido-everywhere t)
           (setq ido-ignore-extensions t))
  :config (ido-mode 1))
(push "~" completion-ignored-extensions)

(use-package ido-vertical-mode
  :ensure t
  :config (ido-vertical-mode 1)
  :init (setq ido-vertical-define-keys 'C-n-and-C-p-only)
  :hook (ido-mode . ido-vertical-mode))

(use-package dired
  :init (setq dired-listing-switches "-agho --group-directories-first")
  :custom (setq dired-omit-files
                (rx (or (seq bol (? ".") "#")     ;; emacs autosave files
                        (seq bol "." (not (any "."))) ;; dot-files
                        (seq "~" eol)                 ;; backup-files
                        (seq bol "CVS" eol)           ;; CVS dirs
                        ))))
(put 'dired-find-alternate-file 'disabled nil)

(use-package typescript-mode
  :ensure t
  :custom ((setq typescript-indent-level 2)
           (setq tab-width 2)))

(use-package js3-mode
  :ensure t
  :custom ((setq js-indent-level 2)
           (setq tab-width 2)))

(use-package company
  :ensure t
  :custom ((setq company-minimum-prefix-length 1)
           (setq company-dabbrev-downcase nil))
  :config (global-company-mode 1))

(use-package markdown-mode
  :bind ("C-c RET" . markdown-toggle-gfm-checkbox))

(use-package web-mode
  :ensure t
  :mode "\\.html\\'")

(use-package emmet-mode
  :ensure t
  :init (setq emmet-move-cursor-between-quotes t)
  :hook ((web-mode . emmet-mode)
         (scss-mode . emmet-mode)))

(use-package projectile
  :ensure t
  :config (projectile-mode 1)
  :bind-keymap  ("C-c p" . projectile-command-map)
  :hook (projectile . lsp-dired-mode))

(use-package smex
  :ensure t
  :bind ("M-x" . smex))

(use-package pdf-tools
  :ensure t
  :mode "\\.pdf\\'")

(global-set-key (kbd "C-c <left>") 'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>") 'windmove-up)
(global-set-key (kbd "C-c <down>") 'windmove-down)
