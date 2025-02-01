;; 2023-07-03 jbgreer init.el
;; 2025-01-26 jbgreer removed pre-29 cruft
;; 2025-01-30 jbgreer prot simple +


(setq debug-on-error t)

;; dump generated custom settings in a separate file
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file :no-error-if-file-is-missing)


;; PACKAGE
(require 'package)
(package-initialize)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("org-elpa" . "https://orgmode.org/elpa/"))

(when (< emacs-major-version 29)
  (unless (package-installed-p 'use-package)
    (unless package-archive-contents
      (package-refresh-contents))
    (package-install 'use-package)))

(setq use-package-verbose t)


;; DELSEL
(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))


(defun jg/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

(define-key global-map (kbd "C-g") #'jg/keyboard-quit-dwim)



;; EVIL, EVIL-COLLECTION, EVIL-TUTOR- vi emulation
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t) ;; default setting
  (setq evil-want-keybinding nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (evil-mode 1))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (setq evil-collection-mode-list '(dashboard dired ibuffer))
  :init (evil-collection-init))

;;(use-package evil-surround
  ;;:ensure t
  ;;:after evil)

(use-package evil-smartparens
             :ensure t
             :after evil)

(use-package evil-tutor
  :ensure t
  :after evil)



;; GENERAL - keybindings
(use-package general
  :ensure t
  :config
  (general-evil-setup)

  ;; set up 'SPC' as the global leader key
  (general-create-definer jg/leader-keys
			  :states '(normal insert visual emacs)
			  :keymaps 'override
			  :prefix "SPC" ;; set leader
			  :global-prefix "M-SPC") ;; access leader in insert mode

  ;; find
  (jg/leader-keys
    "f f" '(find-file :wk "Find file")
    "f c" '((lambda () (interactive) (find-file (concat user-emacs-directory "init.el"))) :wk "Edit emacs config")
    "f r" '(counsel-recentf :wk "Find recent files")
    "TAB TAB" '(comment-line :wk "Comment lines"))

  ;; buffer
  (jg/leader-keys
     "b" '(:ignore t :wk "buffer")
     "b b" '(switch-to-buffer :wk "Switch buffer")
     "b i" '(ibuffer :wk "Ibuffer")
     "b k" '(kill-this-buffer :wk "Kill this buffer")
     "b n" '(next-buffer :wk "Next buffer")
     "b p" '(previous-buffer :wk "Previous buffer")
     "b r" '(revert-buffer :wk "Reload buffer"))

  ;; evaluate
  (jg/leader-keys
     "e" '(:ignore t :wk "Eshell/Evaluate")
     "e b" '(eval-buffer :wk "Evaluate elisp in buffer")
     "e d" '(eval-defun :wk "Evaluate defun containing or after point")
     "e e" '(eval-expression :wk "Evaluate an elisp expression")
     "e h" '(counsel-esh-history :which-key "Eshell history")
     "e l" '(eval-last-sexp :wk "Evaluate elisp expression before point")
     "e r" '(eval-region :wk "Evaluate elisp in region")
     "e s" '(eshell :which-key "Eshell"))

  ;; help
  (jg/leader-keys
     "h" '(:ignore t :wk "Help")
     "h f" '(describe-function :wk "Describe function")
     "h z" '(describe-variable :wk "Describe variable")
     "h r r" '((lambda () (interactive) (load-file (concat user-emacs-directory "init.el")) :wk "Reload emacs config")))
     ;; "h r r" '(reload-init-file :wk "Reload emacs config"))

  ;; toggle
  (jg/leader-keys
   "t" '(:ignore t :wk "Toggle")
   "t l" '(display-line-numbers-mode :wk "Toggle line numbers")
   "t t" '(visual-line-mode :wk "Toggle truncated lines")
   "t v" '(vterm-toggle :wk "Toggle vterm"))

  ;; windows
  (jg/leader-keys
    "w" '(:ignore t :wk "Windows")
    ;; Window splits
    "w c" '(evil-window-delete :wk "Close window")
    "w n" '(evil-window-new :wk "New window")
    "w s" '(evil-window-split :wk "Horizontal split window")
    "w v" '(evil-window-vsplit :wk "Vertical split window")
    ;; Window motions
    "w h" '(evil-window-left :wk "Window left")
    "w j" '(evil-window-down :wk "Window down")
    "w k" '(evil-window-up :wk "Window up")
    "w l" '(evil-window-right :wk "Window right")
    "w w" '(evil-window-next :wk "Goto next window")
    ;; Move Windows
    "w H" '(buf-move-left :wk "Buffer move left")
    "w J" '(buf-move-down :wk "Buffer move down")
    "w K" '(buf-move-up :wk "Buffer move up")
    "w L" '(buf-move-right :wk "Buffer move right"))
)


;;; UI STUFF

;;  CATPPUCCIN theme
(load-theme 'catppuccin :no-confirm)


;; ICONS and FONTS

;; Remember to do M-x and run `nerd-icons-install-fonts' to get the
;; font files.  Then restart Emacs to see the effect.
(use-package nerd-icons
  :ensure t)

(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :config
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-dired
  :ensure t
  :hook (dired-mode . nerd-icons-dired-mode))

;; Set the Font Face
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
;; This is working in emacsclient but not emacs.
(set-face-attribute 'font-lock-comment-face nil
      :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
		    :slant 'italic)

;; Set default font on all graphical frames created after restarting Emacs.
;; Does the same thing as 'set-face-attribute default' above, but emacsclient fonts
;; are not right unless I also add this method of setting the default font.
(add-to-list 'default-frame-alist '(font . "JetBrains Mono-14"))

;; Key bindings and mouse whell for zooming in/out
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

;; Turn off startup message, set visual bell
(setq inhibit-startup-message t)
(setq visible-bell t)

;; Set Frame width/heighth
(setq default-frame-alist
  '((top . 25) (left . 275) (width . 140) (height . 60)))

;; Disable Menubar, Toolbars Tooltips, and Scrollbars, and set fringe
(menu-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)
(set-fringe-mode 10)

;; Display Line Numbers and Truncated Lines
(global-display-line-numbers-mode 1)
(global-visual-line-mode t)

;; change all yes/no questions to y/n
(fset 'yes-or-no-p 'y-or-n-p)



;; WHICH-KEY : a minor mode that displays available keybindings
(use-package which-key
  :ensure t
  :init
  (which-key-mode 1)
  :config
  (setq which-key-popup-type 'minibuffer
	which-key-side-window-location 'bottom
	which-key-side-window-max-height 0.25
	which-key-sort-order #'which-key-key-order-alpha
	which-key-sort-uppercase-first nil
	which-key-add-column-padding 1
	which-key-max-display-columns nil
	which-key-min-display-lines 6
	which-key-side-window-slot -10
	which-key-side-window-max-height 0.25
	which-key-idle-delay 0.8
	which-key-max-description-length 25
	which-key-allow-imprecise-window-fit t
	which-key-separator " → " ))



;; VERTICO, MARGINALIA, ORDERLESS, SAVEHIST, CORFU
(use-package vertico
  :ensure t
  :hook (after-init . vertico-mode))

(use-package marginalia
  :ensure t
  :hook (after-init . marginalia-mode))

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic))
  (setq completion-category-defaults nil)
  (setq completion-category-overrides nil))

(use-package savehist
  :ensure nil ; it is built-in
  :hook (after-init . savehist-mode))

(use-package corfu
  :ensure t
  :hook (after-init . global-corfu-mode)
  :bind (:map corfu-map ("<tab>" . corfu-complete))
  :config
  (setq tab-always-indent 'complete)
  (setq corfu-preview-current nil)
  (setq corfu-min-width 20)

  (setq corfu-popupinfo-delay '(1.25 . 0.5))
  (corfu-popupinfo-mode 1) ; shows documentation after `corfu-popupinfo-delay'

  ;; Sort by input history (no need to modify `corfu-sort-function').
  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history)))



;;; DIRED, DIRED-SUBTREEE, TRASHEED

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

(use-package dired-subtree
  :ensure t
  :after dired
  :bind
  ( :map dired-mode-map
    ("<tab>" . dired-subtree-toggle)
    ("TAB" . dired-subtree-toggle)
    ("<backtab>" . dired-subtree-remove)
    ("S-TAB" . dired-subtree-remove))
  :config
  (setq dired-subtree-use-backgrounds nil))

(use-package trashed
  :ensure t
  :commands (trashed)
  :config
  (setq trashed-action-confirmer 'y-or-n-p)
  (setq trashed-use-header-line t)
  (setq trashed-sort-key '("Date deleted" . t))
  (setq trashed-date-format "%Y-%m-%d %H:%M:%S"))



;; MAGIT : a git porcelain inside emacs
(use-package transient
  :ensure t)
(use-package magit
  :after transient
  :ensure t)



;; ORG MODE

;; These are the defaults we want to change.  We do so in the
;; following `use-package' declaration.
(setq org-M-RET-may-split-line '((default . t)))
(setq org-insert-heading-respect-content nil)
(setq org-log-done nil)
(setq org-log-into-drawer nil)

(use-package org
  :ensure nil ; built-in
  :config
  (setq org-M-RET-may-split-line '((default . nil)))
  (setq org-insert-heading-respect-content t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-directory "/Org")
  (setq org-agenda-files (list org-directory))

  ;; Learn about the ! and more by reading the relevant section of the
  ;; Org manual.  Evaluate: (info "(org) Tracking TODO state changes")
  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAIT(w!)" "|" "CANCEL(c!)" "DONE(d!)"))))



;; DEVELOPMENT PACKAGES

;; RAINBOW-DELIMITERS : different colored parens based on nesting
(use-package rainbow-delimiters
  :ensure t)

;; SMARTPARENS :
(use-package smartparens
             :ensure t)

;; RACKET-MODE :
(use-package racket-mode
             :ensure t)
(add-hook 'racket-mode-hook #'rainbow-delimiters-mode)
(add-hook 'racket-mode-hook #'smartparens-mode)
(add-hook 'racket-repl-mode-hook #'rainbow-delimiters-mode)
(add-hook 'racket-repl-mode-hook #'smartparens-mode)

;; SCHEME MODE
(add-hook 'scheme-mode-hook #'rainbow-delimiters-mode)
(add-hook 'scheme-mode-hook #'smartparens-mode)

;; GEISER-RACKET
(use-package geiser-racket
             :ensure t)

;; GEISER-CHEZ
(use-package geiser-chez
             :ensure t)

(add-hook 'geiser-repl-mode-hook #'rainbow-delimiters-mode)
(add-hook 'geiser-repl-mode-hook #'smartparens-mode)
(setq geiser-active-implementations '(racket chez))
