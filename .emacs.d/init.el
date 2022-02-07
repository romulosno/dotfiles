;;; init.el --- Arquivo de configuração do Emacs
;;; Commentary:
;; Configuração inicial do Emacs

;;; Code:
(setq gc-cons-threshold most-positive-fixnum)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")))

(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'package)
(require 'keyb)
(require 'rom)
(package-initialize)



(setq next-line-add-newlines t)
(setq inhibit-startup-screen t)
(setq backup-directory-alist '(("" . "~/.emacs.d/backup")))
(setq enable-recursive-minibuffers t)

(add-hook 'after-init-hook #'global-company-mode)
(add-hook 'after-init-hook #'global-flycheck-mode)

(put 'dired-find-alternate-file 'disabled nil)


(with-eval-after-load 'company
  (setq company-minimum-prefix-length 1))

(with-eval-after-load 'typescript-mode
  (setq-default typescript-indent-level 2)
  (setq-default tab-width 2))

(with-eval-after-load 'js-mode
  (setq-default js-indent-level 2)
  (setq-default tab-width 2))

(with-eval-after-load 'lsp
  (setq lsp-idle-delay 0.500)
  (setq lsp-lens-enable t)
  (setq lsp-signature-auto-activate nil))
(add-hook 'lsp-before-initialize-hook (lambda ()
					(setq lsp-keymap-prefix "C-l")
					(define-key lsp-mode-map (kbd "C-l") lsp-command-map)))
(add-hook 'lsp-mode-hook 'lsp-ui-mode)
(add-hook 'lsp-mode-hook 'lsp-enable-which-key-integration)
(add-hook 'lsp-mode-hook 'dap-mode)
(add-hook 'lsp-mode-hook (lambda ()
			   (lsp-enable-which-key-integration)))
(add-hook 'typescript-mode-hook #'lsp-deferred)
(add-hook 'js-mode-hook #'lsp-deferred)
(add-hook 'scss-mode-hook #'lsp-deferred)
(add-hook 'python-mode #'lsp-deferred)

(with-eval-after-load 'flycheck
  (define-key flycheck-mode-map flycheck-keymap-prefix nil)
  (setq flycheck-keymap-prefix (kbd "C-l c"))
  (define-key flycheck-mode-map flycheck-keymap-prefix flycheck-command-map))

(with-eval-after-load 'lsp-ui-mode
  (setq lsp-ui-doc-position 'bottom)
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-sideline-show-symbol nil))

(with-eval-after-load 'projectile
  (define-key projectile-mode-map (kbd "C-c p") projectile-command-map))
(add-hook 'projectile-hook 'lsp-dired-mode)
(add-hook 'projectile-hook 'counsel-projectile-mode)

(with-eval-after-load 'which-key
  (setq which-key-idle-delay 0.3))

(with-eval-after-load 'term
  (setq explicit-shell-file-name "bash")
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

(add-hook 'mhtml-mode-hook 'emmet-mode)
(with-eval-after-load 'emmet-mode
  (setq emmet-move-cursor-between-quotes t))

(require 'yasnippet)
(yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)

(rom-setup)
(projectile-mode 1)
(show-paren-mode t)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(visual-line-mode 1)
(electric-pair-mode 1)
(ivy-mode 1)
(counsel-mode 1)
(which-key-mode)
(global-linum-mode)

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
 '(custom-enabled-themes (quote (tango)))
 '(custom-safe-themes
   (quote
    ("a00b7ba0fb438c944f435c9a2c8cfca704d7bbd72b9e65418cfd5f9291ca8253" "665258494d95f08242562063d6a709001ef8d7a7c120ef9ac31b2112ad851eba" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "36f17556e827b41b63fe9375dbeeb4989d4976fe51cd0506968c6a41d2a7c9f8")))
 '(package-selected-packages
   (quote
    (yasnippet-snippets jedi lsp-jedi impatient-mode flycheck typescript-mode which-key))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;;; init.el ends here
