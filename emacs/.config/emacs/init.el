;; 2023-07-03 jbgreer init.el  -*- lexical-binding: t; -*-

(setq debug-on-error t)

;; dump generated custom settings in a separate file
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file :no-error-if-file-is-missing)


;; PACKAGE : builtin package manager
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)


;; USE-PACKAGE
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-verbose t
        use-package-always-ensure t))


;; EXEC-PATH-FROM-SHELL: when using emacs from a GUI or via a daemon, extend path with shell setting
(use-package  exec-path-from-shell
  :if (or (memq window-system '(mac ns x)) (daemonp))
  :config (exec-path-from-shell-initialize))


;; disable nativing compilation warnings
(setq native-comp-async-report-warnings-errors nil)

;; utf-8 encoding
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; No tabs, but set indent to normal tab width
(setq-default indent-tabs-mode nil)
(setq-default tab-width 8)

;; YYYY-MM-DD Calendar
(setq calendar-date-style 'iso)

;;
(use-package elec-pair
  :ensure nil
  :config
  (electric-pair-mode +1))

;; DELSEL : delete selection if you insert
(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))


;; Do-What-I-Mean behaviour for a general `keyboard-quit'.  
(defun jg/keyboard-quit-dwim ()
" - When the region is active, disable it.
  - When a minibuffer is open, but not focused, close the minibuffer.
  - When the Completions buffer is selected, close it.
  - In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p) (keyboard-quit))
   ((derived-mode-p 'completion-list-mode) (delete-completion-window))
   ((> (minibuffer-depth) 0) (abort-recursive-edit))
   (t (keyboard-quit))))
(define-key global-map (kbd "C-g") #'jg/keyboard-quit-dwim)


;; quit emacslient vs emacs
(defun jg/emacsclient-c-x-c-c (&optional arg)
  (interactive "P")
  (if (or arg (not (frame-parameter nil 'client)))
      (save-buffers-kill-emacs)
    (save-buffers-kill-terminal)))

(if (daemonp)
    (global-set-key (kbd "C-x C-c") #'jg/emacsclient-c-x-c-c))


;;; UI STUFF

;;  CATPPUCCIN theme
(use-package catppuccin-theme
  :config (load-theme 'catppuccin :no-confirm))


;; ICONS and FONTS

;; ICONS : Remember to do M-x and run `nerd-icons-install-fonts' to get the
;; font files.  Then restart Emacs to see the effect.
(use-package nerd-icons)

(use-package nerd-icons-completion
  :after marginalia
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-corfu
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

;; FONTS AND FRAME SIZE
(set-face-attribute 'default nil
                    :font "JetBrains Mono"
                    :height 140
                    :weight 'medium)
(set-face-attribute 'variable-pitch nil
                    :font "Ubuntu"
                    :height 140
                    :weight 'medium)
(set-face-attribute 'fixed-pitch nil
                    :font "JetBrains Mono"
                    :height 140
                    :weight 'medium)
;; Makes commented text and keywords italics. Font must have italic face available.
(set-face-attribute 'font-lock-comment-face nil
                    :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
		                :slant 'italic)

;; Set Frame width/heighth and default font on graphical frames
(setq default-frame-alist
      '((font . "JetBrains Mono-14")
      (top . 25) (left . 275) (width . 140) (height . 60)))

;; KEY BINDINGS AND MOUSE WHEEL for zooming in/out
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

;; Turn off startup message, set visual bell
(setq inhibit-startup-message t)
(setq visible-bell t)

;; Disable Menubar, Toolbars Tooltips, and Scrollbars, and set fringe
(menu-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)
(set-fringe-mode 10)

;; Display Line Numbers and Truncated Lines
(global-display-line-numbers-mode t)
(global-visual-line-mode t)

;; disable bidirectional text scanning
(setq-default bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;; skip fontification during input
(setq redisplay-skip-fontification-on-input t)

;; increase process output buffer for LSP
(setq read-process-output-max (* 1 1024 1024))

;; don't render cursors in non-focused windows
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; save clipboard before killing
(setq save-interprogram-paste-before-kill t)

;; don't duplicate entries in kill ring
(setq kill-do-not-save-duplicates t)

;; change all yes/no questions to y/n
(setq use-short-answers t)


;; DIMINISH : hide minor modes from the mode line
(use-package diminish)


;; WHICH-KEY : a minor mode that displays available keybindings
(use-package which-key
  :ensure nil
  :init
  (which-key-mode 1)
  :config
  (setq which-key-side-window-max-height 0.25
	which-key-sort-order #'which-key-key-order-alpha
	which-key-sort-uppercase-first nil
	which-key-add-column-padding 1
	which-key-max-display-columns nil
	which-key-min-display-lines 6
	which-key-side-window-slot -10
	which-key-idle-delay 0.8
	which-key-max-description-length 25
	which-key-allow-imprecise-window-fit t
	which-key-separator " → " ))


;; VERTICO, MARGINALIA, ORDERLESS, SAVEHIST, CORFU

;; VERTICO : VERTical Interactive COmpletion
;; <TAB> : vertico-insert
;; <RETURN> : vertico-exit
;; M-<RETURN> : vertico-exit-input
(use-package vertico
  :hook (after-init . vertico-mode))

;; MARGINALIA : Marginalia (marks or annotations) in the minibuffer
(use-package marginalia
  :hook (after-init . marginalia-mode))

;; ORDERLESS : completion style dividing pattern into space-separated components
(use-package orderless
  :custom
    (completion-styles '(orderless basic))
    (completion-category-defaults nil)
    (completion-category-overrides
     '((file (styles partial-completion))
       (eglot (styles orderless))
       (eglot-capf (styles orderless)))))

;; SAVEHIST: save minibuffer history
(use-package savehist
  :ensure nil ; it is built-in
  :hook (after-init . savehist-mode)
  :config
  (add-to-list 'savehist-additional-variables 'corfu-history))

;; CORFU : COmpletion in Region FUnction - enhances in-buffer completion with a popupo
(use-package corfu
  :hook (after-init . global-corfu-mode)
  :bind (:map corfu-map ("<tab>" . corfu-complete))
  :config
  (setq tab-always-indent 'complete)
  (setq corfu-preview-current nil)
  (setq corfu-min-width 20)
  (setq corfu-popupinfo-delay '(1.25 . 0.5))
  (corfu-popupinfo-mode 1)
  (corfu-history-mode 1))


;;; DIRED, DIRED-SUBTREEE, TRASHEED

;; DIRED : create a buffer containing a listing of a directory
(use-package dired
  :ensure nil
  :commands (dired)
  :hook
  ((dired-mode . dired-hide-details-mode)
   (dired-mode . hl-line-mode))
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-dwim-target t))

;; DIRED-SUBTREE : insert subdirectories in a tree-like fashion
(use-package dired-subtree
  :after dired
  :bind
  (:map dired-mode-map
     ("TAB" . dired-subtree-toggle)
     ("S-TAB" . dired-subtree-remove))
  :config
  (setq dired-subtree-use-backgrounds nil))


;; TRASHED : open, view, browse, restore, permanently delete files in the trash
(use-package trashed
  :commands (trashed)
  :config
  (setq trashed-action-confirmer 'y-or-n-p)
  (setq trashed-use-header-line t)
  (setq trashed-sort-key '("Date deleted" . t))
  (setq trashed-date-format "%Y-%m-%d %H:%M:%S"))


;; DEVELOPMENT PACKAGES


;; MAGIT : a git porcelain inside emacs
(use-package magit
  :bind ("C-c g" . magit-status))


;; RAINBOW-DELIMITERS : different colored parens based on nesting
(use-package rainbow-delimiters)


;; paredit - structural editing for s-expressions
(use-package paredit
  :config
  ;; paredit steals RET for auto-newline-and-indent, which is annoying
  (define-key paredit-mode-map (kbd "RET") nil)
   (define-key paredit-mode-map (kbd "M-s") search-map)
  (define-key paredit-mode-map (kbd "M-D") #'paredit-splice-sexp)
  ;; preserve M-? for xref-find-references
  (define-key paredit-mode-map (kbd "M-?") nil)
  (add-hook 'paredit-mode-hook (lambda () (electric-pair-local-mode -1)))
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  ;; enable in the *scratch* buffer
  (add-hook 'lisp-interaction-mode-hook #'paredit-mode)
  (add-hook 'ielm-mode-hook #'paredit-mode)
  (add-hook 'lisp-mode-hook #'paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode)
  (diminish 'paredit-mode "()"))

;; CLOJURE MODE
(use-package clojure-mode
  :config
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'clojure-mode-hook #'paredit-mode)
  (add-hook 'clojure-mode-hook #'subword-mode)
  (add-hook 'clojure-mode-hook #'eglot-ensure))

;; EGLOT : LSP integration
(use-package eglot
  :ensure nil   ; built-in
  :config
  (setq eglot-autoshutdown t)
  ;; Clojure-lsp is usually auto-detected, but you can be explicit:
  (add-to-list 'eglot-server-programs
               '((clojure-mode clojurec-mode clojurescript-mode)
                 . ("clojure-lsp")))
  ;; Optional: performance tuning
  (setq eglot-events-buffer-config '(:size 102400 :format full))
  (setq eglot-send-changes-idle-time 0.5)
  
  ;; Tell corfu/capf to use eglot's completion
  ;;(setq completion-category-overrides '((eglot (styles orderless))
  ;;(eglot-capf (styles orderless))))
  :bind (:map eglot-mode-map
              ("C-c e r" . eglot-rename)
              ("C-c e f" . eglot-format)
              ("C-c e a" . eglot-code-actions)
              ("C-c e d" . eldoc)
              ("M-."     . xref-find-definitions)
              ("M-,"     . xref-pop-marker-stack)
              ("M-?"     . xref-find-references)))

;; CIDER: NREPL interaction for CLOJURE
(use-package cider
  :config
  (setq nrepl-log-messages t)
  (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  ;; hide the *nrepl-connection* and *nrepl-server* buffers
  (setq nrepl-hide-special-buffers t))

