;; -*- Mode: Emacs-Lisp -*-
(setq inhibit-startup-message t)
(setq frame-title-format "%b")

(global-set-key [(control x) (control b)] 'electric-buffer-list)
(global-set-key [(insert)] nil)
(global-set-key [(f11)] 'grep)
(global-set-key [(f12)] 'compile)
(global-set-key [(control z)] nil)
(global-set-key [(f1)] 'next-error)
(global-set-key "\M-," (lambda nil (interactive) (find-tag "" t)))
(global-set-key [(f6)] 'query-replace)
(global-set-key "\C-c\C-r" 'comment-region)
(global-set-key "\C-xvt" 'clearcase-gui-vtree-browser-current-buffer)
(global-set-key "\M-g" 'goto-line)
(global-set-key "\M-a" 'align)

(add-hook 'c-mode-hook 'imenu-add-menubar-index)
(add-hook 'c++-mode-hook 'imenu-add-menubar-index)
(add-hook 'java-mode-hook 'imenu-add-menubar-index)
(add-hook 'python-mode-hook 'imenu-add-menubar-index)
(add-hook 'perl-mode-hook 'imenu-add-menubar-index)
(add-hook 'shell-script-mode-hook 'imenu-add-menubar-index)

;; Verify file creation
(add-hook 'find-file-not-found-functions
          '(lambda nil (or (yes-or-no-p 
                            (format "File %s not found. Create? " 
                                    buffer-file-name)) (keyboard-quit)) nil) t)

(add-to-list 'c-mode-hook (lambda nil (abbrev-mode -1)))

(defun toggle-fullscreen (&optional f)
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
                         (if (equal 'fullboth current-value)
                             (if (boundp 'old-fullscreen) old-fullscreen nil)
                           (progn (setq old-fullscreen current-value)
                                  'fullboth)))))
(global-set-key [f9] 'toggle-fullscreen)


(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(abbrev-mode nil)
 '(auto-save-default nil)
 '(blink-cursor-mode nil)
 '(case-fold-search t)
 '(clearcase-suppress-checkout-comments t)
 '(column-number-mode t)
 '(compilation-scroll-output t)
 '(compile-command "make ")
 '(current-language-environment "UTF-8")
 '(default-input-method "rfc1345")
 '(delete-auto-save-files nil)
 '(delete-selection-mode nil nil (delsel))
 '(font-lock-maximum-decoration t)
 '(font-lock-mode t t (font-lock))
 '(global-font-lock-mode t nil (font-lock))
 '(global-hl-line-mode nil)
 '(grep-command "grep -nH -i  ")
 '(grep-scroll-output t)
 '(gutter-buffers-tab-visible-p nil)
 '(imenu-auto-rescan t)
 '(imenu-sort-function (quote imenu--sort-by-name))
 '(indicate-buffer-boundaries (quote right))
 '(menu-bar-mode t)
 '(mode-line-in-non-selected-windows t)
 '(mode-line-inverse-video t)
 '(paren-mode (quote sexp) t (paren))
 '(require-final-newline t)
 '(scroll-bar-mode (quote right))
 '(show-paren-mode t nil (paren))
 '(tool-bar-mode nil nil (tool-bar))
 '(toolbar-visible-p nil)
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify))
 '(which-function-mode t))

(pending-delete-mode)

;; Confirm exit
(setq confirm-kill-emacs 'yes-or-no-p)
;;(setq show-paren-style 'expression)
(setq make-backup-files nil)


(add-to-list 'load-path "~/elisp")
(require 'clearcase)
(setq clearcase-use-normal-diff 1)
(add-to-list 'clearcase-normal-diff-arguments "-b")




;;(set-default-font "-*-Inconsolata-medium-r-normal-*-14-100-96-96-*-*-*-1")

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
;; '(default ((t (:slant normal :weight normal :height 87 :width normal :family "Liberation Mono"))))
 '(default ((t (:slant normal :weight normal :height 80 :width normal :family "Monaco" :background "#FFFFFF"))))
)

(add-to-list 'load-path "~/elisp/color-theme-6.6.0")
(require 'color-theme)


(load "~/elisp/color-theme-gnome-3-adwaita.el")
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
;;     (color-theme-gnome-3-adwaita)
))

(load "~/elisp/pink-bliss.el")
(pink-bliss)
