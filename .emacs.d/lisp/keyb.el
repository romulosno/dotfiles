;;; keyb.el --- Adiciona os keybindings

;;; Commentary:
;; Adiciona os keybindings gerais, principalmente para usar counsel

;;; Code:

(global-set-key [f5] 'rom-kill-close)
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)
(global-set-key "\C-x2" (lambda () (interactive)(split-window-vertically) (other-window 1)))
(global-set-key "\C-x3" (lambda () (interactive)(split-window-horizontally) (other-window 1)))
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c f") 'counsel-fzf)
(global-set-key (kbd "C-c r") 'counsel-rg)
(global-set-key (kbd "C-M-S-l") 'rom-indent-file)
(global-set-key (kbd "C-c SPC") 'rom-select-line)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
(define-key global-map [remap forward-word] 'forward-to-word)
(define-key global-map [remap isearch-forward] 'swiper)

(provide 'keyb)

;;; keyb.el ends here
