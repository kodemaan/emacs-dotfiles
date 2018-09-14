(setq delete-old-versions -1 )		; delete excess backup versions silently
(setq version-control t )		; use version control
(setq vc-make-backup-files t )		; make backups file even when in version controlled dir
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")) ) ; which directory to put backups file
(setq vc-follow-symlinks t )				       ; don't ask for confirmation when opening symlinked file
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)) ) ;transform backups file name
(setq inhibit-startup-screen t )	; inhibit useless and old-school startup screen
(setq ring-bell-function 'ignore )	; silent bell when you make a mistake
(setq coding-system-for-read 'utf-8 )	; use utf-8 by default
(setq coding-system-for-write 'utf-8 )
(setq sentence-end-double-space nil)	; sentence SHOULD end with only a point.
(setq default-fill-column 80)		; toggle wrapping text at the 80th character
(setq initial-scratch-message "Welcome in Emacs") ; print a default message in the empty scratch buffer opened at startup
;; Minimal UI
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)
(add-to-list 'default-frame-alist '(font . "Fira Code"))
(add-to-list 'default-frame-alist '(height . 24))
(add-to-list 'default-frame-alist '(width . 80))

;; Ensure usr local bin is set
(add-to-list 'exec-path "/usr/local/bin/")


;; Package configs
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("org"   . "http://orgmode.org/elpa/")
                         ("gnu"   . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Vim mode
(use-package evil
  :ensure t
  :config
  (evil-mode 1)
  (setq-default evil-escape-delay 0.2)
  :init
  (evil-escape-mode)
)

;; Evil Escape, map lk to escape
(use-package evil-escape
  :ensure t
  :init
  (setq-default evil-escape-key-sequence "lk")
)

;; Theme
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t))

;; Helm
(use-package helm
  :ensure t
  :init
  (setq helm-M-x-fuzzy-match t
  helm-mode-fuzzy-match t
  helm-buffers-fuzzy-matching t
  helm-recentf-fuzzy-match t
  helm-locate-fuzzy-match t
  helm-semantic-fuzzy-match t
  helm-imenu-fuzzy-match t
  helm-completion-in-region-fuzzy-match t
  helm-candidate-number-list 150
  helm-split-window-in-side-p t
  helm-move-to-line-cycle-in-source t
  helm-echo-input-in-header-line t
  helm-autoresize-max-height 0
  helm-autoresize-min-height 20)
  :config
  (helm-mode 1))

;; Which Key
(use-package which-key
  :ensure t
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode 1))

;; Counsel for various file searching things
(use-package counsel :ensure t)
(use-package helm-projectile
  :ensure t
  :init
  (helm-projectile-on)
)

(use-package helm-swoop
  :ensure t
  :config
  (setq helm-swoop-pre-input-function (lambda () ""))
)
(use-package helm-ag :ensure t)

(use-package rjsx-mode :ensure t)
(add-to-list 'auto-mode-alist '(".js'" . rjsx-mode))

;; Projectile
(use-package projectile
  :ensure t
  :init
  (setq projectile-require-project-root nil)
  :config
  (projectile-mode 1))

(use-package evil-magit :ensure t)
;; Custom keybinding
(use-package general
  :ensure t
  :config (general-define-key
  :states '(normal visual insert emacs)
  :prefix "SPC"
  :non-normal-prefix "M-SPC"
  ;; Global commands
  "SPC" '(helm-M-x :which-key "M-x")
  "/"   '(helm-swoop :which-key "find in the current buffer")

  ;; Files
  "f"   '(:ignore t :which-key "File")
  "ff"  '(helm-find-files :which-key "find files")

  ;; Project
  "p"   '(:ignore t :which-key "Project")
  "pf"  '(helm-projectile-find-file :which-key "find file in project")
  "pp"  '(helm-projectile-switch-project :which-key "switch project")
  "pt"  '(helm-projectile-ag :which-key "find text in project")

  ;; Git
  "g"   '(:ignore t :which-key "Git")
  "gs"  '(magit-status :which-key "magit status")

  ;; Buffers
  "b"   '(:ignore t :which-key "Buffers")
  "bb"  '(helm-buffers-list :which-key "buffers list")
  "bk"  '(kill-current-buffer :which-key "kill current buffer")
  "bs"  '(helm-multi-swoop-all :which-key "text search all buffers")

  ;; Window
  "w"   '(:ignore t :which-key "Windows")
  "wc"  '(delete-window :which-key "remove current window")
  "wh"  '(evil-window-left :which-key "move to window left")
  "wj"  '(evil-window-down :which-key "move to window down")
  "wl"  '(evil-window-right :which-key "move to window right")
  "wk"  '(evil-window-up :which-key "move to window up")
  "w/"  '((split-window-right evil-window-right) :which-key "split window right")
  "w-"  '((split-window-below evil-window-down) :which-key "split window down")
))

;; Fancy titlebar for MacOS
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(setq ns-use-proxy-icon  nil)
(setq frame-title-format nil)

;; Show matching parens
(setq show-paren-delay 0)
(show-paren-mode 1)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (helm-ag general projectile counsel which-key helm doom-themes evil-escape evil use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
