;; 2023-07-03 jbgreer init.el  -*- lexical-binding: t; -*-

;; Emit backtraces on error — useful during development, noisy in daily use.
(setq debug-on-error t)


;;; CUSTOM FILE — keep auto-generated settings out of init.el

(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file :no-error-if-file-is-missing)


;;; PACKAGE MANAGER

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)


;;; USE-PACKAGE — built-in declarative package configuration macro (Emacs 30+)

(eval-and-compile
  (setq use-package-verbose t
        ;; Automatically :ensure every package unless told otherwise.
        use-package-always-ensure t))


;;; ============================================================
;;; GENERAL SETTINGS
;;; ============================================================

;; ENCODING — prefer UTF-8 throughout
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; INDENTATION — spaces only, but display tab stops at width 8
(setq-default indent-tabs-mode nil)
(setq-default tab-width 8)

;; CALENDAR — ISO 8601 date format (YYYY-MM-DD)
(setq calendar-date-style 'iso)

;; RESPONSES — accept y/n instead of yes/no everywhere
(setq use-short-answers t)

;; KILL RING — preserve clipboard contents before overwriting;
;; suppress duplicate entries
(setq save-interprogram-paste-before-kill t
      kill-do-not-save-duplicates        t)

;; STARTUP — suppress the splash screen; use a visible bell instead of beeping
(setq inhibit-startup-message t
      visible-bell             t)

;; NATIVE COMPILATION — suppress async warning buffers
(setq native-comp-async-report-warnings-errors nil)


;;; PERFORMANCE TUNING

;; Raise the subprocess read buffer to 1 MB — the default (4 KB) is a
;; bottleneck for LSP servers that emit large JSON payloads.
(setq read-process-output-max (* 1 1024 1024))

;; Skip re-fontifying the buffer while the user is actively typing.
(setq redisplay-skip-fontification-on-input t)

;; Disable bidirectional text scanning — a significant speedup for
;; buffers that contain no right-to-left text.
(setq-default bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;; Don't draw cursors or highlight selections in windows that don't
;; have focus — reduces unnecessary redisplay work.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)


;;; ============================================================
;;; UI SETTINGS
;;; ============================================================

;; CHROME — strip down the GUI to essentials
(menu-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)
(set-fringe-mode 10)

;; LINE NUMBERS — show in all buffers
(global-display-line-numbers-mode t)

;; VISUAL LINE MODE — wrap long lines at word boundaries rather than
;; hard-truncating.  Applied globally here; scope to text-mode if
;; unwanted in code buffers.
(global-visual-line-mode t)

;; FONTS — JetBrains Mono for fixed-pitch, Ubuntu for variable-pitch.
;; Height is in units of 1/10 pt, so 140 = 14 pt.
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

;; Italicise comments and keywords — requires a font with a true italic face.
(set-face-attribute 'font-lock-comment-face nil :slant 'italic)
(set-face-attribute 'font-lock-keyword-face  nil :slant 'italic)

;; FRAME GEOMETRY — applied to every new graphical frame
(setq default-frame-alist
      '((font   . "JetBrains Mono-14")
        (top    . 25)
        (left   . 275)
        (width  . 140)
        (height . 60)))

;; ZOOM — Ctrl+scroll to scale text; note that macOS may intercept
;; C-scroll for system zoom unless disabled in System Settings.
(global-set-key (kbd "<C-wheel-up>")   'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)


;;; ============================================================
;;; BUILT-IN BEHAVIOUR PACKAGES
;;; ============================================================

;; ELEC-PAIR — auto-insert matching delimiters (parens, brackets, quotes)
(use-package elec-pair
  :ensure nil
  :config (electric-pair-mode +1))


;; DELSEL — typing replaces the active region (standard selection behaviour)
(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))


;;; ============================================================
;;; KEYBINDINGS
;;; ============================================================

;; KEYBOARD-QUIT DWIM — context-sensitive C-g:
;;   active region        → deactivate mark
;;   unfocused minibuffer → close it
;;   Completions buffer   → close it
;;   otherwise            → standard keyboard-quit
(defun jg/keyboard-quit-dwim ()
  (interactive)
  (cond
   ((region-active-p)                      (keyboard-quit))
   ((derived-mode-p 'completion-list-mode) (delete-completion-window))
   ((> (minibuffer-depth) 0)               (abort-recursive-edit))
   (t                                      (keyboard-quit))))
(define-key global-map (kbd "C-g") #'jg/keyboard-quit-dwim)


;; EMACSCLIENT EXIT — when running as a daemon, C-x C-c closes only the
;; current frame rather than killing the whole Emacs process.
;; C-u C-x C-c still kills Emacs entirely.
(defun jg/emacsclient-c-x-c-c (&optional arg)
  (interactive "P")
  (if (or arg (not (frame-parameter nil 'client)))
      (save-buffers-kill-emacs)
    (save-buffers-kill-terminal)))
(when (daemonp)
  (global-set-key (kbd "C-x C-c") #'jg/emacsclient-c-x-c-c))


;;; ============================================================
;;; UI PACKAGES
;;; ============================================================

;; EXEC-PATH-FROM-SHELL — import shell $PATH into Emacs when running
;; as a GUI app or daemon (both of which bypass the normal shell login)
(use-package exec-path-from-shell
  :if (or (memq window-system '(mac ns x)) (daemonp))
  :config (exec-path-from-shell-initialize))


;; CATPPUCCIN-THEME — pastel color theme
(use-package catppuccin-theme
  :config (load-theme 'catppuccin :no-confirm))


;; DIMINISH — hide minor-mode lighters from the mode line
(use-package diminish)


;; WHICH-KEY — display available keybindings in a popup after a prefix
(use-package which-key
  :ensure nil
  :hook (after-init . which-key-mode)
  :config
  (setq which-key-side-window-max-height     0.25
        which-key-sort-order                 #'which-key-key-order-alpha
        which-key-sort-uppercase-first       nil
        which-key-add-column-padding         1
        which-key-max-display-columns        nil
        which-key-min-display-lines          6
        which-key-side-window-slot           -10
        which-key-idle-delay                 0.8
        which-key-max-description-length     25
        which-key-allow-imprecise-window-fit t
        which-key-separator                  " → "))


;; NERD-ICONS — icon font used by the nerd-icons-* integration packages.
;; Run M-x nerd-icons-install-fonts once after a fresh install.
(use-package nerd-icons)


;; NERD-ICONS-COMPLETION — icons in the marginalia-annotated minibuffer
(use-package nerd-icons-completion
  :after marginalia
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup))


;; NERD-ICONS-CORFU — icons in the corfu completion popup
(use-package nerd-icons-corfu
  :after corfu
  :config (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))


;; NERD-ICONS-DIRED — icons in dired buffers
(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))


;;; ============================================================
;;; COMPLETION PACKAGES
;;; ============================================================

;; VERTICO — vertical interactive minibuffer completion UI
;;   TAB   → vertico-insert
;;   RET   → vertico-exit
;;   M-RET → vertico-exit-input
(use-package vertico
  :hook (after-init . vertico-mode))


;; MARGINALIA — annotations (type, docstring, key) alongside minibuffer candidates
(use-package marginalia
  :hook (after-init . marginalia-mode))


;; ORDERLESS — completion style that matches space-separated tokens in any order
(use-package orderless
  :custom
  (completion-styles             '(orderless basic))
  (completion-category-defaults  nil)
  (completion-category-overrides
   '((file       (styles partial-completion))
     (eglot      (styles orderless))
     (eglot-capf (styles orderless)))))


;; SAVEHIST — persist minibuffer history across sessions;
;; also saves corfu-history so completion candidates are ranked by past use
(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :config
  (add-to-list 'savehist-additional-variables 'corfu-history))


;; CORFU — in-buffer completion popup (replaces the default *Completions* buffer)
(use-package corfu
  :hook (after-init . global-corfu-mode)
  :bind (:map corfu-map
              ("<tab>" . corfu-complete))
  :config
  (setq tab-always-indent     'complete
        corfu-preview-current nil
        corfu-min-width       20
        ;; Show documentation popup after (first-display . subsequent) seconds.
        corfu-popupinfo-delay '(1.25 . 0.5))
  ;; Show documentation in a secondary popup after corfu-popupinfo-delay.
  (corfu-popupinfo-mode 1)
  ;; Rank candidates by input history (requires savehist integration above).
  (corfu-history-mode 1))


;;; ============================================================
;;; FILE MANAGEMENT PACKAGES
;;; ============================================================

;; DIRED — built-in directory browser
(use-package dired
  :ensure nil
  :commands (dired)
  :hook ((dired-mode . dired-hide-details-mode)
         (dired-mode . hl-line-mode))
  :config
  (setq dired-recursive-copies    'always
        dired-recursive-deletes   'always
        ;; Move deleted files to the OS trash rather than deleting immediately.
        delete-by-moving-to-trash t
        ;; When two dired windows are open, default copy/move target to the other.
        dired-dwim-target         t))


;; DIRED-SUBTREE — expand subdirectories inline as a tree
;;   TAB   → toggle subtree
;;   S-TAB → remove subtree
(use-package dired-subtree
  :after dired
  :bind (:map dired-mode-map
              ("TAB"   . dired-subtree-toggle)
              ("S-TAB" . dired-subtree-remove))
  :config (setq dired-subtree-use-backgrounds nil))


;; TRASHED — browse and manage the OS trash from within Emacs
(use-package trashed
  :commands (trashed)
  :config
  (setq trashed-action-confirmer 'y-or-n-p
        trashed-use-header-line  t
        trashed-sort-key         '("Date deleted" . t)
        trashed-date-format      "%Y-%m-%d %H:%M:%S"))


;;; ============================================================
;;; DEVELOPMENT PACKAGES
;;; ============================================================

;; MAGIT — full-featured Git interface
(use-package magit
  :bind ("C-c g" . magit-status))


;; RAINBOW-DELIMITERS — colorize nested delimiters by depth
(use-package rainbow-delimiters)


;; PAREDIT — structural editing for s-expressions (slurp, barf, splice, etc.)
;; Enabled for all Lisp-family modes including the scratch buffer and IELM.
(use-package paredit
  :hook ((emacs-lisp-mode                  . paredit-mode)
         (lisp-interaction-mode            . paredit-mode)
         (ielm-mode                        . paredit-mode)
         (lisp-mode                        . paredit-mode)
         (eval-expression-minibuffer-setup . paredit-mode)
         ;; elec-pair and paredit both insert closing delimiters — disable
         ;; elec-pair locally wherever paredit is active to avoid double-insertion.
         (paredit-mode . (lambda () (electric-pair-local-mode -1))))
  :config
  ;; paredit binds RET to newline-and-indent; restore default RET behaviour.
  (define-key paredit-mode-map (kbd "RET") nil)
  ;; paredit binds M-s to splice-sexp, shadowing the Emacs search prefix map.
  ;; Remap splice to M-D and restore M-s as the search prefix.
  (define-key paredit-mode-map (kbd "M-s") search-map)
  (define-key paredit-mode-map (kbd "M-D") #'paredit-splice-sexp)
  ;; Restore M-? for xref-find-references (paredit binds it to describe-symbol).
  (define-key paredit-mode-map (kbd "M-?") nil)
  (diminish 'paredit-mode "()"))


;; CLOJURE-MODE — syntax, indentation, and font-lock for Clojure/ClojureScript
(use-package clojure-mode
  :hook ((clojure-mode . rainbow-delimiters-mode)
         (clojure-mode . paredit-mode)
         (clojure-mode . subword-mode)
         (clojure-mode . eglot-ensure)))


;; EGLOT — built-in LSP client
(use-package eglot
  :ensure nil
  :bind (:map eglot-mode-map
              ("C-c e r" . eglot-rename)
              ("C-c e f" . eglot-format)
              ("C-c e a" . eglot-code-actions)
              ("C-c e d" . eldoc)
              ("M-."     . xref-find-definitions)
              ("M-,"     . xref-pop-marker-stack)
              ("M-?"     . xref-find-references))
  :config
  (setq eglot-autoshutdown         t
        ;; Cap the event log at 100 KB — the default is unlimited and can grow large.
        eglot-events-buffer-config '(:size 102400 :format full)
        ;; Wait 0.5 s of idle time before sending incremental changes to the server.
        eglot-send-changes-idle-time 0.5)
  ;; clojure-lsp is usually auto-detected via PATH; listed here explicitly
  ;; in case detection fails.
  (add-to-list 'eglot-server-programs
               '((clojure-mode clojurec-mode clojurescript-mode)
                 . ("clojure-lsp"))))


;; CIDER — Clojure interactive development environment (nREPL client)
(use-package cider
  :hook ((cider-repl-mode . rainbow-delimiters-mode)
         (cider-repl-mode . paredit-mode))
  :config
  (setq ;; Log all nREPL messages to *nrepl-messages* — useful for debugging
        ;; CIDER itself or inspecting wire-level protocol traffic.
        nrepl-log-messages        t
        nrepl-hide-special-buffers t))
