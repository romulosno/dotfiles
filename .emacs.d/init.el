(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(setq inhibit-startup-screen t)				    ; Remove a tela inicial padrão
(setq backup-directory-alist '(("" . "~/.emacs.d/backup"))) ; Pasta para salvar backups
(setq enable-recursive-minibuffers t)	; Permite minibuffers recursivos
(defalias 'yes-or-no-p 'y-or-n-p)	; Define y e n para sim e não

(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ; 1mb

(setq package-list			; Lista de pacotes
      '(company
        which-key
        lsp-mode
        projectile
        typescript-mode
        org-bullets
        emmet-mode
        yasnippet
        ido-vertical-mode
        pdf-tools
        smex))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))		;Atualiza os pacotes

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))		;Instala lista de pacotes

(global-set-key (kbd "<f5>") 'kill-buffer-and-window)
(global-set-key "\C-x2" (lambda () (interactive)(split-window-vertically) (other-window 1)))
(global-set-key "\C-x3" (lambda () (interactive)(split-window-horizontally) (other-window 1)))

(electric-pair-mode 1) 			; Fechar parenteses
(global-company-mode 1)			; Autocomplete
(global-linum-mode 1)			; Número das linhas
(ido-mode 1)				; LOVE LOVE
(setq visible-bell 1)			; Remove o beep infernal
(show-paren-mode 1)			; Mostra o parenteses par
(visual-line-mode 1)			; Quebra linha quando a tela é pequena
(yas-global-mode)			; Inicia snippets
(electric-pair-mode 1)			; Fecha os pares

(tool-bar-mode -1)			; Remove barra de ferramenta
(menu-bar-mode -1)			; Remove menus
(toggle-scroll-bar -1)			; Remove scroll

(which-key-mode 1)			; Habilita modo
(with-eval-after-load 'which-key
  (setq which-key-idle-delay 0.3))	; Define o delay para aparecer

(with-eval-after-load 'org 
  (setq org-src-tab-acts-natively t))
(defun my-org-hook ()
  (org-bullets-mode 1)
  (org-indent-mode 1))
(add-hook 'org-mode-hook 'my-org-hook)

(defun efs/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/dotfiles/.emacs.d/Emacs.org"))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle)
      (load "~/dotfiles/.emacs.d/init.el"))))
(add-hook 'org-mode-hook (lambda() (add-hook 'after-save-hook #'efs/org-babel-tangle-config))) ; Chama a função ao salvar o arquivo

(global-set-key (kbd "M-n") 'flymake-goto-next-error)
(global-set-key (kbd "M-p") 'flymake-goto-prev-error)
(global-set-key (kbd "<f6>") 'flymake-show-buffer-diagnostics)

(with-eval-after-load 'lsp
  (setq lsp-log-io nil) ; Performance
  (setq lsp-idle-delay 0.500) 
  (setq lsp-lens-enable t)
  (setq lsp-signature-auto-activate nil))
(add-hook 'lsp-before-initialize-hook (lambda ()
                                        (setq lsp-keymap-prefix "C-l")
                                        (define-key lsp-mode-map (kbd "C-l") lsp-command-map))) ; Tecla de atalho padrão

(add-hook 'typescript-mode-hook #'lsp-deferred) 
  (add-hook 'js-mode-hook #'lsp-deferred)
  (add-hook 'scss-mode-hook #'lsp-deferred)
  (add-hook 'python-mode #'lsp-deferred)

(setq lsp-clients-angular-language-server-command
      '("node"
        "/usr/lib/node_modules/@angular/language-server"
        "--ngProbeLocations"
        "/usr/lib/node_modules"
        "--tsProbeLocations"
        "/usr/lib/node_modules"
        "--stdio"))			; Funcionar quando o npm install foi feito com su
(add-hook 'lsp-mode-hook 'lsp-enable-which-key-integration) ; Adiciona sugestões para o which-key
(add-hook 'lsp-mode-hook 'dap-mode) ; Adiciona chamada do modo de depuração

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-ignore-extensions t)
(push "~" completion-ignored-extensions)

(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)

(add-hook 'dired-load-hook
          (function (lambda () (load "dired-x"))))
(with-eval-after-load 'dired
  (setq dired-listing-switches "-agho --group-directories-first")
  (setq dired-omit-files
        (rx (or (seq bol (? ".") "#")     ;; emacs autosave files
                (seq bol "." (not (any "."))) ;; dot-files
                (seq "~" eol)                 ;; backup-files
                (seq bol "CVS" eol)           ;; CVS dirs
                )))
  (put 'dired-find-alternate-file 'disabled nil))

(with-eval-after-load 'typescript-mode
  (setq-default typescript-indent-level 2)
  (setq-default tab-width 2))

(with-eval-after-load 'js-mode
  (setq-default js-indent-level 2)
  (setq-default tab-width 2))

(with-eval-after-load 'company
  (setq company-dabbrev-downcase nil))

(setq company-minimum-prefix-length 1)
(defun mars/company-backend-with-yas (backends)
  "Add :with company-yasnippet to company BACKENDS.
Taken from https://github.com/syl20bnr/spacemacs/pull/179."
  (if (and (listp backends) (memq 'company-yasnippet backends))
      backends
    (append (if (consp backends)
                backends
              (list backends))
            '(:with company-yasnippet))))
;; add yasnippet to all backends
(setq company-backends
      (mapcar #'mars/company-backend-with-yas company-backends))

(add-hook 'after-init-hook #'global-company-mode)

(with-eval-after-load 'markdown-mode
  (global-set-key (kbd "C-c RET") 'markdown-toggle-gfm-checkbox))

(add-hook 'mhtml-mode-hook 'emmet-mode)
(with-eval-after-load 'emmet-mode
  (setq emmet-move-cursor-between-quotes t))

(add-hook 'mhtml-mode-hook 'sgml-electric-tag-pair-mode)
(add-hook 'html-mode-hook
          (lambda()
            (setq sgml-basic-offset 2)
            (setq indent-tabs-mode t)))

(projectile-mode 1)			; Inicia pacote
(with-eval-after-load 'projectile
  (define-key projectile-mode-map (kbd "C-c p") projectile-command-map)) ; Define tecla de atalho
(add-hook 'projectile-hook 'lsp-dired-mode)
(add-hook 'projectile-hook 'counsel-projectile-mode)

(require 'yasnippet)
(yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)

(eval-after-load 'smerge-mode  (lambda ()
                                 (define-key smerge-mode-map (kbd "<f8>") smerge-basic-map)))

(global-set-key (kbd "M-x") 'smex)

(global-set-key (kbd "C-c <left>") 'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>") 'windmove-up)
(global-set-key (kbd "C-c <down>") 'windmove-down)
