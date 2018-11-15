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
(setq use-package-always-ensure t) ; Always ensure listed packages, why would i not want this? :D

;; Set constants
(defconst emacs-d
  (file-name-directory
   (file-chase-links load-file-name)))

;; Minimal UI
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)
(setq-default indent-tabs-mode nil)
(add-to-list 'default-frame-alist '(font . "Fira Code"))
(add-to-list 'default-frame-alist '(height . 24))
(add-to-list 'default-frame-alist '(width . 80))

;; Package configs
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("org"   . "http://orgmode.org/elpa/")
                         ("gnu"   . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; start enable ligatures for emacs
(when (window-system)
  (set-frame-font "Fira Code"))
(let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
               (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
               (36 . ".\\(?:>\\)")
               (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
               (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
               (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
               (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
               (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
               (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
               (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
               (48 . ".\\(?:x[a-zA-Z]\\)")
               (58 . ".\\(?:::\\|[:=]\\)")
               (59 . ".\\(?:;;\\|;\\)")
               (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
               (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
               (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
               (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
               (91 . ".\\(?:]\\)")
               (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
               (94 . ".\\(?:=\\)")
               (119 . ".\\(?:ww\\)")
               (123 . ".\\(?:-\\)")
               (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
               (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
               )
             ))
  (dolist (char-regexp alist)
    (set-char-table-range composition-function-table (car char-regexp)
                          `([,(cdr char-regexp) 0 font-shape-gstring]))))

;; end enable ligatures for emacs
;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(setq use-package-compute-statistics t)
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
)

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
    (setq exec-path (append exec-path '("/usr/local/bin")))

(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

(use-package nlinum-relative
  :config
  (nlinum-relative-setup-evil)
  (add-hook 'prog-mode-hook 'nlinum-relative-mode)
  (setq nlinum-relative-current-symbol "")
)

;; Vim mode
(use-package evil
  :config
  (evil-mode 1)
  (setq-default evil-escape-delay 0.2)
  :init
  (evil-escape-mode)
)

(use-package powerline
  :config
  (powerline-default-theme)
)
;; Evil Escape, map lk to escape
(use-package evil-escape
  :init
  (setq-default evil-escape-key-sequence "lk")
)

(use-package ob-http)
(use-package org
  :config
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  (setq org-log-done t)
  (transient-mark-mode 1)
  (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (lisp . t)
       (shell . t)
       (http . t)
       (sql . t))))

(use-package flycheck
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (flycheck-add-mode 'javascript-eslint 'rjsx-mode)
)

(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))
(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

;; Helm
(use-package helm
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
  (helm-mode 1)
)

;; Which Key
(use-package which-key
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode 1)
)

;; Counsel for various file searching things
(use-package counsel)
(use-package helm-projectile
  :init
  (helm-projectile-on)
)

(use-package helm-swoop
  :config
  (setq helm-swoop-pre-input-function (lambda () ""))
)

(use-package symon
  :config
  (symon-mode)
)
(use-package helm-ag :ensure t)

(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode)
)
(use-package company-tern)

(add-to-list 'company-backends 'company-tern)
(add-hook 'rjsx-mode-hook (lambda () (tern-mode) (company-mode)))

(use-package rjsx-mode
  :config
  (define-key rjsx-mode-map "<" nil)
  (define-key rjsx-mode-map (kbd "C-d") nil)
  (define-key rjsx-mode-map ">" nil)
)
(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))

(use-package php-mode)
(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))


;; Projectile
(use-package projectile
  :init
  (setq projectile-require-project-root nil)
  :config
  (projectile-mode 1)
)

(use-package eslint-fix)
(use-package evil-magit)

;; Custom functions
(defun pdc/find-initfile ()
  "Edit init.el in the current window."
  (interactive)
  (find-file-existing (expand-file-name "init.el" emacs-d)))
(defun pdc/reload-initfile ()
  "Reload init-real.el, in the current window."
  (interactive)
  (load-file (concat user-emacs-directory "init.el")))
;; Custom keybinding
(use-package nimbus-theme)
(use-package general
  :config (general-define-key
  :states '(normal visual insert emacs)
  :prefix "SPC"
  :non-normal-prefix "M-SPC"
  ;; Global commands
  "SPC" '(helm-M-x :which-key "M-x")
  "/"   '(helm-swoop :which-key "find in the current buffer")

  ;; Errors
  "e"   '(:ignore t :which-key "Linting errors")
  "el"  '(flycheck-list-errors :which-key "List errors")
  "ev"  '(flycheck-verify-setup :which-key "Verify setup")
  "ef"  '(eslint-fix :which-key "Fix errors in current file")

  ;; Files
  "f"   '(:ignore t :which-key "File")
  "ff"  '(helm-find-files :which-key "find files")
  "fed" '(pdc/find-initfile :which-key "Open Init File")
  "fer" '(pdc/reload-initfile :which-key "Reload File")

  ;; org-mode commands
  "o"   '(:ignore t :which-key "Org mode")
  "oe"  '(org-ctrl-c-ctrl-c :which-key "execute")
  "ot"  '(org-todo :which-key "org-todo")
  "o RET"'(org-meta-return :which-key "org-meta-return")

  ;; Project
  "p"   '(:ignore t :which-key "Project")
  "pf"  '(helm-projectile :which-key "find file in project")
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
  "w/"  '(split-window-right :which-key "split window right")
  "w-"  '(split-window-below :which-key "split window down")

  ;; restclient
  "rs"  '(restclient-http-send-current :which-key "send request")
  "rh"  '(browse-url-of-buffer :which-key "browse buffer as html")
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
 '(js-indent-level 2)
 '(package-selected-packages
   (quote
    (ob-http pdf-tools restclient magit-p4 magit-gitflow helm-ag general projectile counsel which-key helm doom-themes evil-escape evil use-package)))
 '(safe-local-variable-values (quote ((org-confirm-babel-evaluate)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
