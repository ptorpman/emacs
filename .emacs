;;; package --- Summary:
;;; Commentary:
;;; Code:

(setq inhibit-startup-message t)
(setq frame-title-format "%b")

;;----------------------------------------------------------
;; Functions and Utilities
;;----------------------------------------------------------

(defun set-frame-size-according-to-resolution ()
  (interactive)
  (if window-system
  (progn
    (if (> (x-display-pixel-width) 1280)
           (add-to-list 'default-frame-alist (cons 'width 120))
           (add-to-list 'default-frame-alist (cons 'width 80)))
    (add-to-list 'default-frame-alist 
         (cons 'height (/ (- (x-display-pixel-height) 80)
                             (frame-char-height)))))))

;; Verify file creation
(add-hook 'find-file-not-found-functions
          '(lambda nil (or (yes-or-no-p 
                            (format "File %s not found. Create? " 
                                    buffer-file-name)) (keyboard-quit)) nil) t)

;;----------------------------------------------------------
;; Key bindings
;;----------------------------------------------------------
(global-set-key [(control x) (control b)] 'electric-buffer-list)
(global-set-key [(insert)] nil)
(global-set-key [(f1)] 'next-error)
(global-set-key [(f6)] 'query-replace)
(global-set-key [(f11)] 'grep)
(global-set-key [(f12)] 'compile)
(global-set-key [(control z)] nil)
(global-set-key "\M-," (lambda nil (interactive) (find-tag "" t)))
(global-set-key "\C-c\C-r" 'comment-region)
;;(global-set-key "\C-xvt" 'clearcase-gui-vtree-browser-current-buffer)
(global-set-key "\M-g" 'goto-line)
(global-set-key "\M-a" 'align)
(global-set-key [(meta control left)] 'backward-sexp)
(global-set-key [(meta control right)] 'forward-sexp)

;;----------------------------------------------------------
;; Imenu
;;----------------------------------------------------------
(add-hook 'c-mode-hook 'imenu-add-menubar-index)
(add-hook 'c++-mode-hook 'imenu-add-menubar-index)
(add-hook 'java-mode-hook 'imenu-add-menubar-index)
(add-hook 'python-mode-hook 'imenu-add-menubar-index)
(add-hook 'perl-mode-hook 'imenu-add-menubar-index)
(add-hook 'shell-script-mode-hook 'imenu-add-menubar-index)

(add-to-list 'c-mode-hook (lambda nil (abbrev-mode -1)))
(add-to-list 'load-path "~/emacs")
(add-to-list 'load-path "~/emacs/groovy")

(autoload 'groovy-mode "groovy-mode" "Major mode for editing Groovy code." t)
(add-to-list 'auto-mode-alist '("\\.groovy\\'" . groovy-mode))

(autoload 'multi-mode "multi-mode" "Major mode for handling multiple modes." t)
(autoload 'php-mode "php-mode" "Major mode for editing PHP code." t)
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))


;;----------------------------------------------------------
;; MELPA Package Management
;;----------------------------------------------------------
(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line


;;----------------------------------------------------------
;; JS Hint
;;----------------------------------------------------------
(add-to-list 'load-path "/home/peter/projects/emacs/jshint-mode")
(add-to-list 'load-path "/home/peter/projects/emacs/flycheck")

(require 'flycheck)
(add-hook 'java-script-mode-mode-hook
          (lambda () (flycheck-mode t)))

(add-hook 'after-init-hook #'global-flycheck-mode)

;;----------------------------------------------------------
;; Mode Specific: Python
;----------------------------------------------------------
(add-hook 'python-mode-hook
  (lambda ()
    (setq indent-tabs-mode nil)
    (setq python-indent 4)
    (setq tab-width 4)))


;;----------------------------------------------------------
;; ClearCase
;;----------------------------------------------------------
;; (require 'clearcase)
;; (setq clearcase-use-normal-diff 1)
;; (add-to-list 'clearcase-normal-diff-arguments "-b")

;;----------------------------------------------------------
;; Perl
;;----------------------------------------------------------

;;----------------------------------------------------------
;; Temporary files
;;----------------------------------------------------------
;; Save all tempfiles in $TMPDIR/emacs$UID
(defconst emacs-tmp-dir (format "%s/%s%s/" temporary-file-directory "emacs" (user-uid)))
(setq backup-directory-alist
      `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix
      emacs-tmp-dir)

;;----------------------------------------------------------
;; Appearance
;;----------------------------------------------------------
(load "~/emacs/themes/pink-bliss.el")
(pink-bliss)
;;(load-file "~/emacs/themes/color-theme-mac-classic.el")
;;(color-theme-mac-classic)

(set-frame-font "Inconsolata:pixelsize=14")

;;(set-frame-size-according-to-resolution)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(abbrev-mode nil t)
 '(auto-save-default nil)
 '(blink-cursor-mode nil)
 '(c-basic-offset 4)
 '(case-fold-search t)
 '(column-number-mode t)
 '(compilation-scroll-output t)
 '(compile-command "make ")
 '(confirm-kill-emacs (quote yes-or-no-p))
 '(cperl-indent-level 4)
 '(cua-delete-selection nil)
 '(cua-enable-cua-keys nil)
 '(cua-remap-control-v nil)
 '(cua-remap-control-z nil)
 '(current-language-environment "UTF-8")
 '(custom-safe-themes
   (quote
    ("cde4592248d2269f335110b935c3910bbbc5b9ee7f7bcbafb5df2b47dcbe78d4" "86cf4472f6a472dec2d7910a61ae2c127b8804f6397c7a5366af4a8ce4c1c160" "4f5bb895d88b6fe6a983e63429f154b8d939b4a8c581956493783b2515e22d6d" "ad950f1b1bf65682e390f3547d479fd35d8c66cafa2b8aa28179d78122faa947" default)))
 '(custom-theme-directory "/home/epetorp/elisp")
 '(custom-theme-load-path (quote ("~/emacs/themes/")))
 '(default-input-method "rfc1345")
 '(delete-auto-save-files nil)
 '(delete-selection-mode t nil (delsel))
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
 '(jshint-mode-jshintrc "/home/peter/emacs/.jshint")
 '(make-backup-files nil)
 '(mode-line-in-non-selected-windows t)
 '(mode-line-inverse-video t)
 '(paren-mode (quote sexp) t (paren))
 '(python-indent 4)
 '(python-indent-offset 4)
 '(require-final-newline t)
 '(scroll-bar-mode (quote right))
 '(show-paren-mode t nil (paren))
 '(show-paren-style (quote expression))
 '(tab-width 4)
 '(tool-bar-mode nil nil (tool-bar))
 '(toolbar-visible-p nil)
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify))
 '(visible-bell t)
 '(which-function-mode t))


;;----------------------------------------------------------
;; MELPA - Package installation
;;----------------------------------------------------------
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;; M-x package-install RET flycheck
;; M-x package-install RET flymake-easy
;; M-x package-install RET flymake-jslint
;; M-x package-install RET flymake-jshint


;;----------------------------------------------------------
;; JS Hint
;;----------------------------------------------------------
(require 'flymake-jshint)
(add-hook 'js-mode-hook 'flycheck-mode)

;;----------------------------------------------------------
;; Guess Style
;;----------------------------------------------------------
;; (load "guess-style")
;; (autoload 'guess-style-set-variable "guess-style" nil t)
;; (autoload 'guess-style-guess-variable "guess-style")
;; (autoload 'guess-style-guess-all "guess-style" nil t)

;;----------------------------------------------------------
;; Smart Tabs
;;----------------------------------------------------------
;; (load "smart-tabs-mode")

;; (smart-tabs-advice python-indent-line-1 python-indent)
;; (add-hook 'python-mode-hook
;; 		  (lambda ()
;; 			(setq indent-tabs-mode nil)
;; 			(setq tab-width (default-value 'tab-width))))
;; (smart-tabs-advice py-indent-line py-indent-offset)
;; (smart-tabs-advice py-newline-and-indent py-indent-offset)
;; (smart-tabs-advice py-indent-region py-indent-offset)


(add-hook 'python-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq python-indent 4)
            (setq tab-width 4)))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )



