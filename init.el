(setq gc-cons-threshold most-positive-fixnum)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa" . "http://melpa.org/packages/")))
(require 'package)

(package-initialize)
(setq my-packages
      '( company
	 counsel
	 counsel-projectile
	 dap-mode
	 ivy
	 lsp-ui
	 lsp
	 npm-mode
	 projectile
	 scss-mode
	 smart-mode-line
	 which-key
	 org
	 emmet-mode
	 flymake
	 imenu-anywhere
	 ))

(dolist (pkg my-packages)
  (unless (package-installed-p pkg)
    (package-install pkg)))



(setq read-process-output-max (* 1024 1024))
(setq next-line-add-newlines t)
(setq inhibit-startup-screen t)
(setq backup-directory-alist '(("" . "~/.emacs.d/backup")))
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(add-hook 'after-init-hook 'global-company-mode)

(global-set-key [f5] 'custom-kill-buffer-fn)
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)
(global-set-key "\C-x2" (lambda () (interactive)(split-window-vertically) (other-window 1)))
(global-set-key "\C-x3" (lambda () (interactive)(split-window-horizontally) (other-window 1)))
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c f") 'counsel-fzf)
(global-set-key (kbd "C-c r") 'counsel-rg)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
(define-key global-map [remap forward-word] 'forward-to-word)
(define-key global-map [remap isearch-forward] 'swiper)

(with-eval-after-load 'ido
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  (setq ido-use-filename-at-point 'guess))

(with-eval-after-load 'company
  (setq company-minimum-prefix-length 1))

(with-eval-after-load 'typescript-mode
  (setq-default typescript-indent-level 2)
  (setq-default tab-width 2)
  (add-hook 'typescript-mode-hook (lambda() (lsp-deferred))))

(with-eval-after-load 'js-mode
  (setq-default js-indent-level 2)
  (setq tab-width 2)

  (add-hook 'js-mode-hook (lambda() (lsp-deferred))))

(with-eval-after-load 'scss-mode
  (add-hook 'scss-mode-hook (lambda() (lsp-deferred))))

(with-eval-after-load 'lsp
  (setq lsp-keymap-prefix "C-l")
  (setq lsp-idle-delay 0.500)
  (setq lsp-lens-enable t)
  (setq lsp-signature-auto-activate nil)
  (define-key lsp-mode-map (kbd "C-l") 'lsp-command-map)
  (add-hook 'lsp-deferred-hook 'lsp-ui)
  (add-hook 'lsp-deferred-hook 'lsp-enable-which-key-integration)
  (add-hook 'lsp-deferred-hook 'dap-mode))

(with-eval-after-load 'projectile
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (define-key counsel-mode-map (kbd "<f6>") 'counsel-projectile-find-file)
  (define-key counsel-mode-map (kbd "<f7>") 'counsel-projectile-rg)
  (add-hook 'projectile-hook 'lsp-dired-mode))

(with-eval-after-load lsp-ui
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-sideline-show-symbol nil)
  (lsp-ui-doc-position 'bottom))

(with-eval-after-load 'lsp-treemacs
  (local-set-key (kbd "C-l T v") 'lsp-treemacs-symbols))

(with-eval-after-load 'which-key
  (setq which-key-idle-delay 0.3))

(with-eval-after-load 'term
  (setq explicit-shell-file-name "bash")
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

(with-eval-after-load 'org
  (add-hook 'org-mode-hook 'dw/org-mode-setup)
  (setq org-ellipsis " ▾"
	org-hide-emphasis-markers t))

(with-eval-after-load 'emmet-mode
  (setq emmet-move-cursor-between-quotes t))

(with-eval-after-load 'mhtml-mode
  (add-hook 'mhtml-mode-hook 'emmet-mode))

(with-eval-after-load 'flymake
  (flymake-show-diagnostic))
;; ===== Functions =====

(defun dw/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq evil-auto-indent nil))

(defun npm-start()
  (interactive)
  (npm-mode--exec-process "npm start"))

(defun custom-kill-buffer-fn (&optional arg)
  (interactive "P")
  (cond
   ((and (consp arg) (equal arg '(4)))
    (mapc
     (lambda (x)
       (let ((name (buffer-name x)))
	 (unless (eq ?\s (aref name 0))
	   (kill-buffer x)
	   (if (> (length (mapcar #'window-buffer (window-list))) 1)
	       (delete-window)))))
     (buffer-list)))
   (t
    (kill-buffer (current-buffer))
    (if (> (length (mapcar #'window-buffer (window-list))) 1)
	(delete-window)))))
(put 'dired-find-alternate-file 'disabled nil)


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
 '(package-selected-packages
   (quote
    (which-key))))
(custom-set-faces )

(projectile-mode 1)
(lsp-treemacs-sync-mode 1)
(doom-modeline-mode 1)
					;(global-visual-line-mode t)							
(show-paren-mode t)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(visual-line-mode 1)
(electric-pair-mode 1)
(ivy-mode 1)
(counsel-mode 1)
(ido-mode 1)
(which-key-mode)
(global-linum-mode)
