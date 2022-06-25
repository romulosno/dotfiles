(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(use-package emacs
  :init
  (setq native-comp-async-report-warnings-errors nil) ;; Remove avisos do native-comp
  (setq gc-cons-threshold (* 50 1000 1000)) ; Performance
  (setq read-process-output-max (* 1024 1024)) ; Performance
  (defalias 'yes-or-no-p 'y-or-n-p)     ; Define y e n para sim e não

  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
        modus-themes-region '(bg-only no-extend))

  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  (setq enable-recursive-minibuffers t)
  :config (load-theme 'modus-vivendi t nil)
  :bind (("<f6>" . modus-themes-toggle)
         ("<escape>" . 'keyboard-escape-quit)))

(setq inhibit-startup-screen t)		; Remove a tela inicial padrão
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
  (add-hook mode (lambda () (global-linum-mode -1))))

(global-set-key (kbd "<f5>") 'kill-buffer-and-window)
(global-set-key "\C-x2" (lambda () (interactive)(split-window-vertically) (other-window 1)))
(global-set-key "\C-x3" (lambda () (interactive)(split-window-horizontally) (other-window 1)))

(use-package no-littering
  :ensure t)
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(use-package auto-package-update
  :ensure t
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

(electric-pair-mode 1)			; Fechar parenteses
(show-paren-mode 1)			; Mostra o parenteses par

(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g h" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-s i" . consult-imenu)
         ("M-g i" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history))
  :init (setq consult-preview-key "M-.")
  :ensure t)

(defun rom-lsp ()
  (setq lsp-keymap-prefix "C-M-<return>"
        lsp-idle-delay 0.5
        lsp-prefer-capf t)
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(flex))))
(use-package lsp-mode
  :ensure t
  :custom   (lsp-completion-provider :none) ;; we use Corfu!
  :commands (lsp lsp-deferred)
  :init (rom-lsp)
  :config
  (define-key lsp-mode-map (kbd "C-M-<return>") lsp-command-map)
  :hook ((java-mode . lsp-deferred)
         (lsp-completion-mode . my/lsp-mode-setup-completion)
         (lsp-mode . lsp-enable-which-key-integration)))

(use-package lsp-java
  :ensure t)

(use-package dap-mode
  :ensure t)

(use-package dired
  :init (setq dired-listing-switches "-agho --group-directories-first")
  :custom (setq dired-omit-files
                (rx (or (seq bol (? ".") "#")     ;; emacs autosave files
                        (seq bol "." (not (any "."))) ;; dot-files
                        (seq "~" eol)                 ;; backup-files
                        (seq bol "CVS" eol)           ;; CVS dirs
                        ))))
(put 'dired-find-alternate-file 'diasbled nil)

(use-package embark
  :ensure t

  :bind
  (("C-ç" . embark-act)         ;; pick some comfortable binding
   ("M-ç" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package emmet-mode
  :ensure t
  :init (setq emmet-move-cursor-between-quotes t)
  :hook ((web-mode . emmet-mode)
         (scss-mode . emmet-mode)))

(use-package eshell
  :bind ("<f7>" . eshell))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package web-mode
  :ensure t
  :mode "\\.html\\'")

(use-package magit
  :ensure t)

(use-package marginalia
  :ensure t
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init (marginalia-mode))

(use-package corfu
  :ensure t
  :custom ((corfu-auto t)
           (corfu-separator)) 
  :init (global-corfu-mode)
  (setq completion-styles '(orderless basic)
      completion-category-defaults nil
      completion-category-overrides '((file (styles . (partial-completion))))))        

(use-package emacs
  :init
  (setq completion-cycle-threshold 3)
  (setq tab-always-indent 'complete))

(use-package markdown-mode
  :bind ("C-c RET" . markdown-toggle-gfm-checkbox))

(defun prot-orderless-literal-dispatcher (pattern _index _total)
  "Literal style dispatcher using the equals sign as a suffix.
It matches PATTERN _INDEX and _TOTAL according to how Orderless
parses its input."
  (when (string-suffix-p "=" pattern)
    `(orderless-literal . ,(substring pattern 0 -1))))

(use-package orderless
  :ensure t
  :init (setq completion-styles '(orderless basic)
              completion-category-defaults nil
              completion-category-overrides '((file (styles partial-completion))))
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion prot-orderless-literal-dispatcher)))))

(use-package org
  :ensure t
  :custom ((setq org-return-follows-link  t)
           (setq org-default-notes-file (concat org-directory "/notes.org"))
           (setq org-src-tab-acts-natively t)
           (setq org-latex-toc-command "\\tableofcontents \\clearpage") )
  :hook ((org-mode . (lambda() (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))
         (org-mode . org-indent-mode))
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)))
(use-package org-protocol
  :demand
  :config
  (setq org-capture-templates
        `(
          ("p" "Protocol" entry (file+headline ,(concat org-directory "/notes.org") "Navegador")
           "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
          ("L" "Protocol Link" entry (file+headline ,(concat org-directory "/notes.org") "Navegador")
           "* %? [[%:link][%:description]] \nCaptured On: %U")
          ("t" "TODO" entry (file+headline ,(concat org-directory "/todo.org") "Tarefas")
           "* TODO %?\n  %i\n  %a")
          ("n" "Notas" entry (file+headline ,(concat org-directory "/notes.org") "Notas")
           "* %?\n  %i\n  %a"))))

(use-package org-bullets
  :ensure t
  :hook ((org-mode) . org-bullets-mode))

(defun efs/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/dotfiles/.emacs.d/Emacs.org"))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle)
      (load "~/dotfiles/.emacs.d/init.el"))))

(use-package pdf-tools
  :ensure t
  :mode "\\.pdf\\'")

(defun tree-sitter-mark-bigger-node ()
  (interactive)
  (let* ((p (point))
         (m (or (mark) p))
         (beg (min p m))
         (end (max p m))
         (root (ts-root-node tree-sitter-tree))
         (node (ts-get-descendant-for-position-range root beg end))
         (node-beg (ts-node-start-position node))
         (node-end (ts-node-end-position node)))
    ;; Node fits the region exactly. Try its parent node instead.
    (when (and (= beg node-beg) (= end node-end))
      (when-let ((node (ts-get-parent node)))
        (setq node-beg (ts-node-start-position node)
              node-end (ts-node-end-position node))))
    (set-mark node-end)
    (goto-char node-beg)))

(use-package tree-sitter
  :ensure t
  :config (global-tree-sitter-mode 1)
  :custom (setq er/try-expand-list (append er/try-expand-list
                                           '(tree-sitter-mark-bigger-node))))
(use-package tree-sitter-langs
  :ensure t)

(use-package which-key
  :ensure t
  :config (which-key-mode 1)
  :init (setq which-key-idle-delay 0.3))

(global-set-key (kbd "C-c <left>") 'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>") 'windmove-up)
(global-set-key (kbd "C-c <down>") 'windmove-down)

(use-package yaml-mode
  :ensure t)

(defun rom-elisp ()
  (if (locate-library "ediff")
      (progn
        (autoload 'ediff-files "ediff")
        (autoload 'ediff-buffers "ediff")

        (eval-after-load "ediff" '(progn
                                    (message "doing ediff customisation")
                                    (setq diff-switches               "-u"
                                          ediff-custom-diff-options   "-U3"
                                          ediff-split-window-function 'split-window-horizontally
                                          ediff-window-setup-function 'ediff-setup-windows-plain)

                                    (add-hook 'ediff-startup-hook 'ediff-toggle-wide-display)
                                    (add-hook 'ediff-cleanup-hook 'ediff-toggle-wide-display)
                                    (add-hook 'ediff-suspend-hook 'ediff-toggle-wide-display))))))
(use-package ediff
  :config (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  :init (rom-elisp))

(use-package selectrum
  :config (selectrum-mode 1)
  :ensure t)
