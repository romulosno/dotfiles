(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(setq inhibit-startup-screen t)
(setq backup-directory-alist '(("" . "~/.emacs.d/backup")))
(setq enable-recursive-minibuffers t)
(defalias 'yes-or-no-p 'y-or-n-p)

(setq package-list
      '(company
	which-key
	lsp-mode
	lsp-ui
	eglot
	smex))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-c <left>") 'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>") 'windmove-up)
(global-set-key (kbd "C-c <down>") 'windmove-down)
(global-set-key (kbd "<f5>") 'kill-buffer-and-window)
(global-set-key "\C-x2" (lambda () (interactive)(split-window-vertically) (other-window 1)))
(global-set-key "\C-x3" (lambda () (interactive)(split-window-horizontally) (other-window 1)))

(defun efs/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
		      (expand-file-name "~/.emacs.d/Emacs.org"))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(electric-pair-mode 1)
(ido-mode 1)
(show-paren-mode 1)
(global-company-mode 1)
(visual-line-mode 1)
(electric-pair-mode 1)
(global-linum-mode 1)
(which-key-mode 1)
(projectile-mode 1)
(show-paren-mode t)
(yas-global-mode)

(tool-bar-mode -1)
(menu-bar-mode -1)
(toggle-scroll-bar -1)

(with-eval-after-load 'which-key
  (setq which-key-idle-delay 0.3))

(add-hook 'org-mode-hook (lambda() (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))

(defun add-pcomplete-to-capf ()
  (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t))

(setq org-src-tab-acts-natively t)

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-ignore-extensions t)

(add-hook 'dired-load-hook
	  (function (lambda () (load "dired-x"))))
(setq dired-omit-files
      (rx (or (seq bol (? ".") "#")     ;; emacs autosave files
	      (seq bol "." (not (any "."))) ;; dot-files
	      (seq "~" eol)                 ;; backup-files
	      (seq bol "CVS" eol)           ;; CVS dirs
	      )))
(put 'dired-find-alternate-file 'disabled nil)

(with-eval-after-load 'typescript-mode
  (setq-default typescript-indent-level 2)
  (setq-default tab-width 2))

(with-eval-after-load 'js-mode
  (setq-default js-indent-level 2)
  (setq-default tab-width 2))

(with-eval-after-load 'company
  (setq company-minimum-prefix-length 1))

(with-eval-after-load 'company
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
	(mapcar #'mars/company-backend-with-yas company-backends)))

(add-hook 'after-init-hook #'global-company-mode)

(with-eval-after-load 'markdown-mode
  (global-set-key (kbd "C-c RET") 'markdown-toggle-gfm-checkbox))

(add-hook 'mhtml-mode-hook 'emmet-mode)
(with-eval-after-load 'emmet-mode
  (setq emmet-move-cursor-between-quotes t))

(with-eval-after-load 'projectile
  (define-key projectile-mode-map (kbd "C-c p") projectile-command-map))
(add-hook 'projectile-hook 'lsp-dired-mode)
(add-hook 'projectile-hook 'counsel-projectile-mode)

(require 'yasnippet)
(yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)
