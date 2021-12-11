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
  (setq tab-width 2))


(with-eval-after-load 'js-mode
  (setq-default js-indent-level 2)
  (setq tab-width 2))

(with-eval-after-load 'lsp
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-idle-delay 0.500)
  (setq lsp-lens-enable t)
  (setq lsp-signature-auto-activate nil)
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map))
(add-hook 'lsp-mode-hook 'lsp-ui)
(add-hook 'lsp-mode-hook 'lsp-enable-which-key-integration)
(add-hook 'lsp-mode-hook 'dap-mode)
(add-hook 'lsp-mode-hook (lambda () (lsp-enable-which-key-integration)))
(add-hook 'typescript-mode-hook #'lsp)
(add-hook 'js-mode-hook #'lsp)
(add-hook 'scss-mode-hook #'lsp)


(with-eval-after-load 'lsp-ui
  (setq lsp-ui-doc-position 'bottom)
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-sideline-show-symbol nil))

(with-eval-after-load 'projectile
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))
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
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#2e3436" "#a40000" "#4e9a06" "#c4a000" "#204a87" "#5c3566" "#729fcf" "#eeeeec"])
 '(ansi-term-color-vector
   [unspecified "#FFFFFF" "#d15120" "#5f9411" "#d2ad00" "#6b82a7" "#a66bab" "#6b82a7" "#505050"] t)
 '(custom-safe-themes
   (quote
    ("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "36f17556e827b41b63fe9375dbeeb4989d4976fe51cd0506968c6a41d2a7c9f8")))
 '(package-selected-packages (quote (flycheck typescript-mode which-key))))
(custom-set-faces )


;;; init.el ends here
