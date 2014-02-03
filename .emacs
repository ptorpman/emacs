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
 '(current-language-environment "UTF-8")
 '(custom-safe-themes (quote ("234249a92c2cf7b61223d9f83e1d9eefcd80fcf6b7a5e9ca03dc9d3f1b122ae2" "beeb4fbb490f1a420ea5acc6f589b72c6f0c31dd55943859fc9b60b0c1091468" "7ec6a9707c69e7a4ea1a8761b3f28f8dc55c6c5cacd597718c994b1561e435f3" "87818a78deaefd55594bb4fef802fb4948989996c12f8e0e609c46c6bd038edf" "cfd79d66fe6b142b570048ed9a28cd2c71876f824d76e1d9f2da0f3353062f3f" "0ca71d3462db28ebdef0529995c2d0fdb90650c8e31631e92b9f02bd1bfc5f36" "fc89666d6de5e1d75e6fe4210bd20be560a68982da7f352bd19c1033fb7583ba" "2c50bf38069a99a18404275e8d139a8a1019a629dab4be9b92b8d5d9c43bbb92" "e008d9149dd39b249d4f8a9b5c1362d8f85bd11e9c08454e5728fbf0fcc11690" "60e97fc4cdb64c43cab637cd0027e09cf27939fe799a1889a30cfedd6f2e7f8e" "57d7e8b7b7e0a22dc07357f0c30d18b33ffcbb7bcd9013ab2c9f70748cfa4838" "b42cf9ee9e59c3aec585fff1ce35acf50259d8b59f3047e57df0fa38516aa335" "a1493957ee779057acdc4c337133f217dd7b2edfdeeffed903ba2f16246f665a" "80ee5b0e403162518b90236ba7c31c4f29192c451ad124097f31166c038f2523" "2588175e0f3591583582a72c465e6d38bd8c99b36daee949ab08f1e758052117" "caa9a86ff9b85f733b424f520ec6ecff3499a36f20eb8d40e3096dbbe1884069" "f211f8db2328fb031908c9496582e7de2ae8abd5f59a27b4c1218720a7d11803" "64c60102b3f704d8ecf38205380f6b1b83e200561abb32f787d4937f788fc328" "55573f69249d1cfdd795dacf1680e56c31fdaab4c0ed334b28de96c20eec01a3" "0c5204945ca5cdf119390fe7f0b375e8d921e92076b416f6615bbe1bd5d80c88" "008775b6f17cba84b22da8c820d9c6778fac161291f1a9cc252a7e735714bc56" "f2355ec455645cd4a4b8f8ac8bcb96c50bc8f383634e59307d8bc651143f6be4" "0ae977e603e99d89c80d679377bfed4a904317968bd885ee063455cee01728d3" "a405a0c2ec845e34ecb32a83f477ca36d1858b976f028694e0ee7ff4af33e400" "6394ba6170fd0bc9f24794d555fa84676d2bd5e3cfd50b3e270183223f8a6535" "89127a6e23df1b1120aa61bd7984f1d5f2747cad1e700614a68bdb7df77189ba" "8016855a07f289a6b2deb248e192633dca0165f07ee5d51f9ba982ec2c36797d" "6981a905808c6137dc3a3b089b9393406d2cbddde1d9336bb9d372cbc204d592" "eb399cbd3ea4c93d9ab15b513fd6638e801600e13c8a70b56f38e609397a5eca" "c712d616ea5a9ef4e513681846eb908728bbb087c2d251ded8374ee9faafa199" "6e03b7f86fcca5ce4e63cda5cd0da592973e30b5c5edf198eddf51db7a12b832" "2c73700ef9c2c3aacaf4b65a7751b8627b95a1fd8cebed8aa199f2afb089a85f" "aa95b9a243de8c18230ed97315c737ceba2c8ebda8cff997d35b4c2fab5ba007" "978bd4603630ecb1f01793af60beb52cb44734fc14b95c62e7b1a05f89b6c811" "29ed40288147808c39b1e69d60a2f21a471154e751d90ed1ff06f2517c0c66bc" "ccf95502d17c1aa4d1815f67af350b6f5e04dd3d9b193242c076db3bcb01665f" "a82a6dd47c295da29b2579303ae5e3227ca45a3e70a2a4907ae5826499e444d8" "d0ff5ea54497471567ed15eb7279c37aef3465713fb97a50d46d95fe11ab4739" "f220c05492910a305f5d26414ad82bf25a321c35aa05b1565be12f253579dec6" "ee965a30e9d83e7f21f98fef4f75c15fc35c1d4cd4b11d6a700d4dabb07d9c22" "d293542c9d4be8a9e9ec8afd6938c7304ac3d0d39110344908706614ed5861c9" "61d1a82d5eaafffbdd3cab1ac843da873304d1f05f66ab5a981f833a3aec3fc0" "179a38957de9e28bcaae35cff42b138b34502a9ec0d4217e0d349c04c0e7301e" "6065e2561d75b42102aeca504335910e989a8377344c5063ba4d41e4a33d0184" "3bbf583352389226c8624248b01c25a04b78d9b7c6d62fed5434ddfde34cb283" "9ff2a70634dde4588f69778bee5b0abc2a803d0097ec52a0136749e8c348af43" "908f2cfe8f11253ba5e09ddedd49a635dfcef4f71a71610c14e304622a081e27" "d96768f6fb4ccf7f443f0c1f95cf710fd0fafb1b5b042670f83078b516ab1f1e" "7a80197da149e5584b8f975e9c55f6da29c7aaa684f7c4267b6180973b4c00d0" default)))
 '(custom-theme-directory "~/elisp")
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


(add-to-list 'load-path "~/elisp")
(require 'clearcase)
(setq clearcase-use-normal-diff 1)
(add-to-list 'clearcase-normal-diff-arguments "-b")



(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(cperl-array-face ((t (:foreground "Blue" :weight bold))) t)
 '(cperl-hash-face ((t (:foreground "Red" :slant italic :weight bold))) t)
 '(cperl-nonoverridable-face ((t (:foreground "blue2"))) t)
 '(region ((t (:background "blue" :foreground "yellow"))))
 '(show-paren-match ((t (:background "#3B5998" :foreground "yellow" :weight bold))))
 '(show-paren-mismatch ((t (:background "red" :foreground "yellow" :weight bold)))))



(pending-delete-mode)

;; Confirm exit
(setq confirm-kill-emacs 'yes-or-no-p)
;;(setq show-paren-style 'expression)
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


;;(set-face-font 'default "-adobe-courier-medium-r-normal--12-*-*-*-*-*-*-*")

;;(set-face-font 'default "-misc-fixed-medium-r-semicondensed-*-*-120-*-*-*-*-*-*")
(load "~/elisp/pink-bliss.el")
(pink-bliss)
;;(load-theme 'zenburn)
(set-face-attribute 'default nil :family "Inconsolata" :height 120) 
