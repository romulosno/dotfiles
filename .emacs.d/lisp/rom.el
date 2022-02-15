;;; rom.el --- Funções custom

;;; Commentary:
;; Minhas funções em Lisp

;;; Code:

(setq-default my-packages
	      '( counsel
		 counsel-projectile
		 dap-mode
		 emmet-mode
		 flycheck
		 imenu-anywhere
		 ivy
		 lsp-mode
		 lsp-ui
		 npm-mode
		 org
		 projectile
		 scss-mode
		 smart-mode-line
		 typescript-mode
		 which-key
		 company
		 yasnippet
		 yasnippet-snippets
		 ))


(defun rom-setup ()
  "Realiza o setup inicial do Emacs."
  (interactive)
  (when (not package-archive-contents)
    (package-refresh-contents))
  (dolist (pkg my-packages)
    (unless (package-installed-p pkg)
      (package-install pkg))))


(defun rom-select-line ()
  "Select the current line."
  (interactive)
  (end-of-line)
  (set-mark (line-beginning-position)))


(defun rom-kill-close (&optional ARG)
  "Kill buffer and close window.
ARG interactive."
  (interactive "P")
  (cond
   ((and (consp ARG) (equal ARG '(4)))
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

(defun rom-indent-file ()
  "Indenta todo arquivo e se mantem a mesma linha."
  (interactive)
  (indent-region (point-min) (point-max)))

(provide 'rom)
;;; rom.el ends here
