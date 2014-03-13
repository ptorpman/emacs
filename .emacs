(setq inhibit-startup-message t)
(setq frame-title-format "%b")

(defun set-frame-size-according-to-resolution ()
  (interactive)
  (if window-system
  (progn
    ;; use 120 char wide window for largeish displays
    ;; and smaller 80 column windows for smaller displays
    ;; pick whatever numbers make sense for you
    (if (> (x-display-pixel-width) 1280)
           (add-to-list 'default-frame-alist (cons 'width 120))
           (add-to-list 'default-frame-alist (cons 'width 80)))
    ;; for the height, subtract a couple hundred pixels
    ;; from the screen height (for panels, menubars and
    ;; whatnot), then divide by the height of a char to
    ;; get the height we want
    (add-to-list 'default-frame-alist 
         (cons 'height (/ (- (x-display-pixel-height) 100)
                             (frame-char-height)))))))

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
(global-set-key [(meta control left)] 'backward-sexp)
(global-set-key [(meta control right)] 'forward-sexp)

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






;;(load-theme 'torpman)
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(abbrev-mode nil)
 '(ansi-color-names-vector ["#FFFFDD" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#000000"])
 '(auto-save-default nil)
 '(blink-cursor-mode nil)
 '(case-fold-search t)
 '(clearcase-suppress-checkout-comments t)
 '(column-number-mode t)
 '(compilation-scroll-output t)
 '(compile-command "make ")
 '(cua-delete-selection nil)
 '(cua-enable-cua-keys nil)
 '(cua-remap-control-v nil)
 '(cua-remap-control-z nil)
 '(current-language-environment "UTF-8")
 '(custom-safe-themes (quote ("d96768f6fb4ccf7f443f0c1f95cf710fd0fafb1b5b042670f83078b516ab1f1e" "c712d616ea5a9ef4e513681846eb908728bbb087c2d251ded8374ee9faafa199" default)))
 '(custom-theme-directory "/home/epetorp/elisp")
 '(default-frame-alist (quote ((menu-bar-lines . 1) (witdh . 130))))
 '(default-input-method "rfc1345")
 '(delete-auto-save-files nil)
 '(delete-selection-mode t nil (delsel))
 '(fci-rule-color "#383838")
 '(font-lock-maximum-decoration t)
 '(font-lock-mode t t (font-lock))
 '(global-font-lock-mode t nil (font-lock))
 '(global-hl-line-mode nil)
 '(grep-command "grep -nH -i  ")
 '(grep-scroll-output t)
 '(gutter-buffers-tab-visible-p nil)
 '(imenu-auto-rescan t)
 '(imenu-sort-function (quote imenu--sort-by-name))
 '(indent-tabs-mode nil)
 '(indicate-buffer-boundaries (quote right))
 '(mode-line-in-non-selected-windows t)
 '(mode-line-inverse-video t)
 '(paren-mode (quote sexp) t (paren))
 '(python-indent-guess-indent-offset t)
 '(python-indent-offset 4)
 '(require-final-newline t)
 '(scroll-bar-mode (quote right))
 '(show-paren-mode t nil (paren))
 '(tool-bar-mode nil nil (tool-bar))
 '(toolbar-visible-p nil)
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map (quote ((20 . "#BC8383") (40 . "#CC9393") (60 . "#DFAF8F") (80 . "#D0BF8F") (100 . "#E0CF9F") (120 . "#F0DFAF") (140 . "#5F7F5F") (160 . "#7F9F7F") (180 . "#8FB28F") (200 . "#9FC59F") (220 . "#AFD8AF") (240 . "#BFEBBF") (260 . "#93E0E3") (280 . "#6CA0A3") (300 . "#7CB8BB") (320 . "#8CD0D3") (340 . "#94BFF3") (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3")
 '(which-function-mode t))


(add-to-list 'load-path "/home/epetorp/elisp")
(require 'clearcase)
(setq clearcase-use-normal-diff 1)
(add-to-list 'clearcase-normal-diff-arguments "-b")



(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(cperl-array-face ((t (:foreground "cyan" :weight bold))) t)
 '(cperl-hash-face ((t (:foreground "Red" :slant italic :weight bold))) t)
 '(cperl-nonoverridable-face ((t (:foreground "lightgreen"))) t))


;;(pending-delete-mode)

;; Confirm exit
(setq confirm-kill-emacs 'yes-or-no-p)
(setq show-paren-style 'expression)
(setq make-backup-files nil)

(setq cperl-indent-level 4
      cperl-close-paren-offset -4
      cperl-continued-statement-offset 4
      cperl-indent-parens-as-block t
      cperl-tab-always-indent t)

(setq visible-bell t)



;; Save all tempfiles in $TMPDIR/emacs$UID
(defconst emacs-tmp-dir (format "%s/%s%s/" temporary-file-directory "emacs" (user-uid)))
(setq backup-directory-alist
      `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix
      emacs-tmp-dir)


;; (add-to-list 'load-path "~/elisp/git-modes")
;; (add-to-list 'load-path "~/elisp/magit")
;; (require 'magit)
;; (autoload 'magit-status "magit" nil t)


(setq custom-theme-load-path '())
(add-to-list 'custom-theme-load-path "/home/epetorp/elisp")

;;(load "~/elisp/pink-bliss2.el")
;;(pink-bliss2)

(load-theme 'blue-mood)
;;(load-theme 'xemacs)

;;(set-face-font 'default "-adobe-courier-medium-r-normal--12-*-*-*-*-*-*-*")
;;(set-face-font 'default "-misc-fixed-medium-r-semicondensed-*-*-150-*-*-*-*-*-*")
;;(set-face-font 'default "-*-terminus-medium-r-*-*-12-*-*-*-*-*-iso8859-1")
;;(set-face-attribute 'default nil :family "Anonymous Pro" :height 110)
(set-face-attribute 'default nil :family "Fantasque Sans Mono" :height 110)
;;(set-face-attribute 'default nil :family "Ubuntu Mono" :height 120 :background "#1f1f1f1f1f1f" :foreground "#dcdcdcdccccc")
;;(set-face-attribute 'default nil :family "Fantasque Sans Mono" :height 120 :background "#1f1f1f1f1f1f" :foreground "#dcdcdcdccccc")
;;(set-face-attribute 'default nil :family "Ubuntu Mono" :height 120 )


(set-frame-size-according-to-resolution)


