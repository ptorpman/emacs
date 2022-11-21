;;; package --- Summary:
;;; Commentary:
;;; Code:

(setq frame-inhibit-implied-resize t) ;; prevent resize window on startup
(setq default-frame-alist '((width . 120) (height . 70)))
(defun x/disable-scroll-bars (frame)
  (modify-frame-parameters frame '((horizontal-scroll-bars . nil)
                                   (vertical-scroll-bars . nil))))
(if (display-graphic-p)
    (progn
      (scroll-bar-mode -1)
      (tool-bar-mode -1)
      (fringe-mode '(8 . 0))
      (add-hook 'after-make-frame-functions 'x/disable-scroll-bars))
  (progn
    (menu-bar-mode -1)
    (setq-default
     left-margin-width 1
     right-margin-width 0)))

(setq inhibit-startup-message t)
(setq frame-title-format "%b")



;;----------------------------------------------------------
;; Functions and Utilities
;;----------------------------------------------------------


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
(global-set-key "\M-," 'xref-pop-marker-stack)
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
;;(add-to-list 'load-path "/home/peter/projects/emacs/jshint-mode")
;;(add-to-list 'load-path "/home/peter/projects/emacs/flycheck")

;; (require 'flycheck)
;; (add-hook 'java-script-mode-mode-hook
;;           (lambda () (flycheck-mode t)))

;; (add-hook 'after-init-hook #'global-flycheck-mode)

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(abbrev-mode nil t)
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#2e3436" "#a40000" "#4e9a06" "#c4a000" "#204a87" "#5c3566" "#729fcf" "#eeeeec"])
 '(auto-save-default nil)
 '(blink-cursor-mode nil)
 '(c-basic-offset 2)
 '(case-fold-search t)
 '(column-number-mode t)
 '(compilation-message-face 'default)
 '(compilation-scroll-output t)
 '(compile-command "make ")
 '(confirm-kill-emacs 'yes-or-no-p)
 '(cperl-indent-level 4)
 '(cua-delete-selection nil)
 '(cua-enable-cua-keys nil)
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(cua-remap-control-v nil)
 '(cua-remap-control-z nil)
 '(current-language-environment "UTF-8")
 '(custom-enabled-themes '(emacs-21))
 '(custom-safe-themes
   '("780c67d3b58b524aa485a146ad9e837051918b722fd32fd1b7e50ec36d413e70" "a455366c5cdacebd8adaa99d50e37430b0170326e7640a688e9d9ad406e2edfd" "0598c6a29e13e7112cfbc2f523e31927ab7dce56ebb2016b567e1eff6dc1fd4f" "72c530c9c8f3561b5ab3bf5cda948cd917de23f48d9825b7a781fe1c0d737f2f" "b8c5adfc0230bd8e8d73450c2cd4044ad7ba1d24458e37b6dec65607fc392980" "d91ef4e714f05fff2070da7ca452980999f5361209e679ee988e3c432df24347" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "df85955fd38ee2dae7476a5fa93e58e594df96132871c10ecaf4de95bdae932a" "b71da830ae97a9b70d14348781494b6c1099dbbb9b1f51494c3dfa5097729736" "f5b6be56c9de9fd8bdd42e0c05fecb002dedb8f48a5f00e769370e4517dde0e8" "3e200d49451ec4b8baa068c989e7fba2a97646091fd555eca0ee5a1386d56077" "d9c957b0e8d2d7f1bbb781fc729e06598017ade2d0c18611e5abbdde0f65d981" "57d7e8b7b7e0a22dc07357f0c30d18b33ffcbb7bcd9013ab2c9f70748cfa4838" "be0efbaebc85494f3c1c06e320fd13a24abf485d5f221a90fe811cea9a39ed85" "e624f013e266f41148aa2e445a4b8681b0afb346a9126993e345309c9a829535" "51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773" "57a29645c35ae5ce1660d5987d3da5869b048477a7801ce7ab57bfb25ce12d3e" "69ad4071c7b2d91543fddd9030816404ff22e46f7207549319ce484e23082dee" "06e0662b31a2ae8da5c6b5e9a05b25fabd1dc8dd3c3661ac194201131cafb080" "46f6f73fb47a2a19b6ee1a49781f835fd73a185674268d4e048acf6feac9c55d" "830877f4aab227556548dc0a28bf395d0abe0e3a0ab95455731c9ea5ab5fe4e1" "285d1bf306091644fb49993341e0ad8bafe57130d9981b680c1dbd974475c5c7" "efcecf09905ff85a7c80025551c657299a4d18c5fcfedd3b2f2b6287e4edd659" "833ddce3314a4e28411edf3c6efde468f6f2616fc31e17a62587d6a9255f4633" "4c56af497ddf0e30f65a7232a8ee21b3d62a8c332c6b268c81e9ea99b11da0d3" "1378b60d701d80b44563d0764c2b257871c636538afccd098cd129f5e2b3227c" "e9f642ee0dbd5638e40390b8b8eded9743f1426ad1390e7b2e5d3fa04efa2969" "32706490cdcc393ed0d3498021a840ee3d6f7d59ae385cc145231eacb0040164" "beb47ba28dab4497001d5dfe29112e7ebb813512c58110d098c4e21a1377a0e2" "d89e15a34261019eec9072575d8a924185c27d3da64899905f8548cbd9491a36" "cde4592248d2269f335110b935c3910bbbc5b9ee7f7bcbafb5df2b47dcbe78d4" "86cf4472f6a472dec2d7910a61ae2c127b8804f6397c7a5366af4a8ce4c1c160" "4f5bb895d88b6fe6a983e63429f154b8d939b4a8c581956493783b2515e22d6d" "ad950f1b1bf65682e390f3547d479fd35d8c66cafa2b8aa28179d78122faa947" default))
 '(default-input-method "rfc1345")
 '(delete-auto-save-files nil)
 '(delete-selection-mode t nil (delsel))
 '(fci-rule-color "#eee8d5")
 '(font-lock-maximum-decoration t)
 '(font-lock-mode t t (font-lock))
 '(global-font-lock-mode t nil (font-lock))
 '(global-hl-line-mode nil)
 '(grep-command "grep -nH -i  ")
 '(grep-scroll-output t)
 '(gutter-buffers-tab-visible-p nil)
 '(highlight-changes-colors '("#d33682" "#6c71c4"))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#fdf6e3" 0.25)
    '("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2")))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   '(("#eee8d5" . 0)
     ("#B4C342" . 20)
     ("#69CABF" . 30)
     ("#69B7F0" . 50)
     ("#DEB542" . 60)
     ("#F2804F" . 70)
     ("#F771AC" . 85)
     ("#eee8d5" . 100)))
 '(hl-bg-colors
   '("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342"))
 '(hl-fg-colors
   '("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3"))
 '(hl-paren-colors '("#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900"))
 '(imenu-auto-rescan t)
 '(imenu-sort-function 'imenu--sort-by-name)
 '(indent-tabs-mode nil)
 '(indicate-buffer-boundaries 'right)
 '(ispell-dictionary nil)
 '(jshint-mode-jshintrc "/home/ptorpman/emacs/.jshint")
 '(make-backup-files nil)
 '(mode-line-in-non-selected-windows t)
 '(mode-line-inverse-video t)
 '(nrepl-message-colors
   '("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4"))
 '(package-selected-packages
   '(clang-format ## bliss-theme color-theme-modern color-theme-sanityinc-solarized solarized-theme color-theme))
 '(paren-mode 'sexp t (paren))
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(python-indent 4)
 '(python-indent-offset 4)
 '(require-final-newline t)
 '(scroll-bar-mode 'right)
 '(show-paren-mode t)
 '(show-paren-style 'expression)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(tab-width 2)
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(tool-bar-mode nil nil (tool-bar))
 '(toolbar-visible-p nil)
 '(uniquify-buffer-name-style 'post-forward-angle-brackets nil (uniquify))
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   '((20 . "#dc322f")
     (40 . "#c8805d801780")
     (60 . "#bec073400bc0")
     (80 . "#b58900")
     (100 . "#a5008e550000")
     (120 . "#9d0091000000")
     (140 . "#950093aa0000")
     (160 . "#8d0096550000")
     (180 . "#859900")
     (200 . "#66aa9baa32aa")
     (220 . "#57809d004c00")
     (240 . "#48559e556555")
     (260 . "#392a9faa7eaa")
     (280 . "#2aa198")
     (300 . "#28669833af33")
     (320 . "#279993ccbacc")
     (340 . "#26cc8f66c666")
     (360 . "#268bd2")))
 '(vc-annotate-very-old-color nil)
 '(visible-bell t)
 '(warning-suppress-log-types
   '(((package reinitialization))
     ((package reinitialization))))
 '(warning-suppress-types '(((package reinitialization))))
 '(weechat-color-list
   '(unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496"))
 '(which-function-mode t)
 '(xterm-color-names
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"])
 '(xterm-color-names-bright
   ["#fdf6e3" "#cb4b16" "#93a1a1" "#839496" "#657b83" "#6c71c4" "#586e75" "#002b36"]))


;; M-x package-install RET flycheck
;; M-x package-install RET flymake-easy
;; M-x package-install RET flymake-jslint
;; M-x package-install RET flymake-jshint


;;----------------------------------------------------------
;; JS Hint
;;----------------------------------------------------------
;; (require 'flymake-jshint)
;; (add-hook 'js-mode-hook 'flycheck-mode)

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

(defun display-line-numbers-equalize ()
  "Equalize The width"
  (setq display-line-numbers-width (length (number-to-string (line-number-at-pos (point-max))))))
(add-hook 'find-file-hook 'display-line-numbers-equalize)

(add-hook 'python-mode-hook 'display-line-numbers-mode)
(add-hook 'c-mode-hook 'display-line-numbers-mode)
(add-hook 'cc-mode-hook 'display-line-numbers-mode)


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Cascadia Code PL" :foundry "SAJA" :slant normal :weight normal :height 110 :width normal)))))




;; C/C++ Style
(require 'mozilla-c-style)
(add-hook 'c-mode-common-hook 'mozilla-c-mode-style-hook)
(setq c-basic-offset 2)

;; package-install clang-format
(require 'clang-format)
(setq clang-format-style "~/.clang-format")
(setq clang-format-executable "/usr/bin/clang-format")
(global-set-key (kbd "C-c i") 'clang-format-region)
(global-set-key (kbd "C-c u") 'clang-format-buffer)


