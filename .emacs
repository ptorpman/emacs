(setq inhibit-startup-message t)
(setq frame-title-format "%b")

(defun set-frame-size-according-to-resolution ()
  (interactive)
  (if window-system
  (progn
    (if (> (x-display-pixel-width) 1280)
           (add-to-list 'default-frame-alist (cons 'width 120))
           (add-to-list 'default-frame-alist (cons 'width 80)))
    (add-to-list 'default-frame-alist 
         (cons 'height (/ (- (x-display-pixel-height) 100)
                             (frame-char-height)))))))


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

;; Verify file creation
(add-hook 'find-file-not-found-functions
          '(lambda nil (or (yes-or-no-p 
                            (format "File %s not found. Create? " 
                                    buffer-file-name)) (keyboard-quit)) nil) t)

(add-to-list 'c-mode-hook (lambda nil (abbrev-mode -1)))
(add-to-list 'load-path "~/emacs")
(add-to-list 'load-path "~/emacs/groovy")

(autoload 'groovy-mode "groovy-mode" "Major mode for editing Groovy code." t)
(add-to-list 'auto-mode-alist '("\\.groovy\\'" . groovy-mode))

(autoload 'multi-mode "multi-mode" "Major mode for handling multiple modes." t)
(autoload 'php-mode "php-mode" "Major mode for editing PHP code." t)
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))



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
;; Variable Customization
;;----------------------------------------------------------
;; Confirm exit
(setq confirm-kill-emacs 'yes-or-no-p)
(setq show-paren-style 'expression)
(setq make-backup-files nil)
(setq visible-bell t)
(setq custom-theme-load-path '())
(add-to-list 'custom-theme-load-path "~/emacs/themes/")


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
;;(load "~/emacs/themes/pink-bliss.el")
;;(pink-bliss)

;;(load-theme 'xemacs)
;;(load-theme 'tomorrow-night-blue)
;;(load-theme 'solarized-dark)
;;(load-theme 'ample-flat)

;; (require 'color-theme)
;; (color-theme-initialize)
(load-file "~/emacs/themes/color-theme-mac-classic.el")
(color-theme-mac-classic)

;;(set-frame-font "Inconsolata:pixelsize=14")
;;(set-frame-font "Anonymous Pro:pixelsize=14")
(set-frame-font "Code New Roman:pixelsize=14")


(set-frame-size-according-to-resolution)

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
 '(cperl-indent-level 4)
 '(cua-delete-selection nil)
 '(cua-enable-cua-keys nil)
 '(cua-remap-control-v nil)
 '(cua-remap-control-z nil)
 '(current-language-environment "UTF-8")
 '(custom-safe-themes
   (quote
    ("55e5171af9aa2a85eb5a9da9dacee372bd4e747ae1a857fba0242f63ae1fcc55" "cde4592248d2269f335110b935c3910bbbc5b9ee7f7bcbafb5df2b47dcbe78d4" "86cf4472f6a472dec2d7910a61ae2c127b8804f6397c7a5366af4a8ce4c1c160" "12b4427ae6e0eef8b870b450e59e75122d5080016a9061c9696959e50d578057" "ad950f1b1bf65682e390f3547d479fd35d8c66cafa2b8aa28179d78122faa947" "4f5bb895d88b6fe6a983e63429f154b8d939b4a8c581956493783b2515e22d6d" "52588047a0fe3727e3cd8a90e76d7f078c9bd62c0b246324e557dfa5112e0d0c" "bf14d1e09123b72d2929be172918ac27f84ac39798ad26a5d697ba22381b20e7" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "a3821772b5051fa49cf567af79cc4dabfcfd37a1b9236492ae4724a77f42d70d" "b42cf9ee9e59c3aec585fff1ce35acf50259d8b59f3047e57df0fa38516aa335" "0ae977e603e99d89c80d679377bfed4a904317968bd885ee063455cee01728d3" "d96768f6fb4ccf7f443f0c1f95cf710fd0fafb1b5b042670f83078b516ab1f1e" "8016855a07f289a6b2deb248e192633dca0165f07ee5d51f9ba982ec2c36797d" "e4a37a67a646afe50263ed5a36c74e6dec615139065bbffe80beb2fb1b582047" default)))
 '(custom-theme-directory "/home/epetorp/elisp")
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
 '(jshint-mode-jshintrc "/home/peter/nmt/intyg/.jshintrc")
 '(mode-line-in-non-selected-windows t)
 '(mode-line-inverse-video t)
 '(paren-mode (quote sexp) t (paren))
 '(python-indent 4)
 '(python-indent-offset 4)
 '(require-final-newline t)
 '(scroll-bar-mode (quote right))
 '(show-paren-mode t nil (paren))
 '(tab-width 4)
 '(tool-bar-mode nil nil (tool-bar))
 '(toolbar-visible-p nil)
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify))
 '(which-function-mode t))




;;----------------------------------------------------------
;; JS Hint
;;----------------------------------------------------------
(add-to-list 'load-path "/home/peter/projects/emacs/jshint-mode")
(require 'flymake-jshint)
(add-hook 'javascript-mode-hook
    (lambda () (flymake-mode t)))

(add-hook 'find-file-hook 'flymake-find-file-hook)

(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
(setq exec-path
      '(
    "/usr/local/bin"
    "/usr/bin"
    ))


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



