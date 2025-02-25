;; 2023-07-03 jbgreer init.el
;; 2025-01-26 jbgreer removed pre-29 cruft
;; 2025-01-30 jbgreer prot simple +
;; 2025-02-09 jbgreer remove evil


(setq debug-on-error t)

;; dump generated custom settings in a separate file
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file :no-error-if-file-is-missing)


;; PACKAGE
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("org-elpa" . "https://orgmode.org/elpa/"))
(package-initialize)
(setq use-package-verbose t)

;; disable nativing compilation warnings
(setq native-comp-async-report-warnings-errors nil)


;; Indent with 2 spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)


;; YYYY-MM-DD Calendar
(setq calendar-date-style 'iso)


;; set 'exec-path' to match shell PATH automatically


;; DELSEL : delete selection if you insert
(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))


;; 
(defun jg/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.
  - When the region is active, disable it.
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
    "If running in emacsclient, make C-x C-c exit frame, and C-u C-x C-c exit Emacs."
    (interactive "P") ; prefix arg in raw form
    (if arg
        (save-buffers-kill-emacs)
    (save-buffers-kill-terminal)))

(if (daemonp)
    (global-set-key (kbd "C-x C-c") #'jg/emacsclient-c-x-c-c))


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
(set-face-attribute 'font-lock-comment-face nil
                    :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
		                :slant 'italic)

;; Set default font on all graphical frames created after restarting Emacs.
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

;; VERTICO : VERTical Interactive COmpletion
;; <TAB> : vertico-insert
;; <RETURN> : vertico-exit
;; M-<RETURN> : vertico-exit-input
(use-package vertico
  :ensure t
  :hook (after-init . vertico-mode))

;; MARGINALIA : Marginalia (marks or annotations) in the minibuffer
(use-package marginalia
  :ensure t
  :hook (after-init . marginalia-mode))

;; ORDERLESS : completion style dividing pattery into space-separated components
(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic))
  (setq completion-category-defaults nil)
  (setq completion-category-overrides nil))

;; SAVEHIST: save minibuffer history
(use-package savehist
  :ensure nil ; it is built-in
  :hook (after-init . savehist-mode))

;; CORFU : COmpletion in Region FUnction - enhances in-buffer completion with a popupo
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
;; TAB : subtree-toggle
;; BACKTAB / S-TAB : subtree-remove
(use-package dired-subtree
  :ensure t
  :after dired
  :bind
  (:map dired-mode-map
    ("<tab>" . dired-subtree-toggle)
    ("TAB" . dired-subtree-toggle)
    ("<backtab>" . dired-subtree-remove)
    ("S-TAB" . dired-subtree-remove))
  :config
  (setq dired-subtree-use-backgrounds nil))

;; TRASHED : open, view, browse, restore, permanently delete files in the trash
(use-package trashed
  :ensure t
  :commands (trashed)
  :config
  (setq trashed-action-confirmer 'y-or-n-p)
  (setq trashed-use-header-line t)
  (setq trashed-sort-key '("Date deleted" . t))
  (setq trashed-date-format "%Y-%m-%d %H:%M:%S"))


;; MU4E
(add-to-list 'load-path "/opt/homebrew/Cellar/mu/1.12.8/share/emacs/site-list/mu/mu4e")
(use-package mu4e
  :ensure nil 
  :config
  (setq
   mu4e-mu-binary "/opt/homebrew/bin/mu"
   mu4e-headers-skip-duplicates  t
   mu4e-view-show-images t
   mu4e-view-show-addresses t
   mu4e-compose-format-flowed nil
   mu4e-date-format "%y/%m/%d"
   mu4e-headers-date-format "%Y/%m/%d"
   mu4e-change-filenames-when-moving t
   mu4e-attachments-dir "~/Downloads"
   mu4e-maildir       "~/Maildir"
   mu4e-update-interval (* 15 60)
   ;; the paths must start w/ / and are relative to maildir root
   mu4e-refile-folder "/Archive"
   mu4e-sent-folder   "/Sent"
   mu4e-drafts-folder "/Drafts"
   mu4e-trash-folder  "/Trash")

  ;; press U to re-sync and re-index mail
  (setq mu4e-get-mail-command  "mbsync -a"))

;; move email to trash rather than destroy it
(fset 'my-move-to-trash "mTrash")
(define-key mu4e-headers-mode-map (kbd "d") 'my-move-to-trash)
(define-key mu4e-view-mode-map (kbd "d") 'my-move-to-trash)

;; sending email
(setq mail-user-agent 'mu4e-user-agent
      send-mail-function 'message-send-mail-with-sendmail
      message-send-mail-function 'message-send-mail-with-sendmail)
(setq sendmail-program (executable-find "msmtp"))
(setq messsage-sendmail-envelope-from 'header)
(setq mu4e-user-mail-address-list '("jbgreer@grimjeer.com"))
(setq user-mail-address "jbgreer@grimjeer.com")

;; TRANSIENT and MAGIS

;; TRANSIENT : implements keyboard-driven menus in magit
(use-package transient
  :ensure t)

;; MAGIT : a git porcelain inside emacs
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
  :ensure nil                           ; built-in
  :config
  (setq org-M-RET-may-split-line '((default . nil)))
  (setq org-insert-heading-respect-content t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-directory "~/Org")
  (setq org-agenda-files (list org-directory))

  ;; Learn about the ! and more by reading the relevant section of the
  ;; Org manual.  Evaluate: (info "(org) Tracking TODO state changes")
  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAIT(w!)" "|" "CANCEL(c!)" "DONE(d!)"))))

;; recommended keybindings
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

;; JOURNALING
(setq journal-file (concat org-directory "/journal.org"))
(setq tasks-file (concat org-directory "/tasks.org"))
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline tasks-file "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("j" "Journal Entry" entry (file+datetree journal-file)
         "* %?" :empty-lines 1)))



;; EASY-HUGO - blogging
(use-package easy-hugo
  :ensure t
  :init
  (setq easy-hugo-bin "/opt/homebrew/bin/hugo")
  (setq easy-hugo-basedir "~/Blog/")
  (setq easy-hugo-url "https://jbgreer.github.io")
  :bind ("C-c C-k" . easy-hugo-menu)
  :config
  (easy-hugo-enable-menu))



;; DEVELOPMENT PACKAGES

;; RAINBOW-DELIMITERS : different colored parens based on nesting
(use-package rainbow-delimiters
  :ensure t)

;; SMARTPARENS : minor mode for working with pairs
(use-package smartparens
  :ensure t
  :config
  (require 'smartparens-config))


;; RACKET-MODE : Racket programming language mode
(use-package racket-mode
  :ensure t)
(add-hook 'racket-mode-hook #'rainbow-delimiters-mode)
(add-hook 'racket-mode-hook #'smartparens-mode)
(add-hook 'racket-repl-mode-hook #'rainbow-delimiters-mode)
(add-hook 'racket-repl-mode-hook #'smartparens-mode)

;; SCHEME MODE : Scheme programming language mode
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
(setq geiser-racket-binary "/opt/homebrew/bin/racket")
(setq geiser-chez-binary "/opt/homebrew/bin/chez")
