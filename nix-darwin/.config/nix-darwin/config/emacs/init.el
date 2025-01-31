;; 2023-07-03 jbgreer init.el
;; 2025-01-26 jbgreer removed pre-29 cruft


(setq debug-on-error t)

;; dump generated custom settings in a separate file
(setq custom-file "~/.config/emacs/custom.el")
(load custom-file 'noerror)


;; PACKAGE
(require 'package)

;; Nice macro for updating lists in place.
(defmacro append-to-list (target suffix)
  "Append SUFFIX to TARGET in place."
  `(setq ,target (append ,target ,suffix)))

;; Set up emacs package archives with 'package
(append-to-list package-archives
                '(("melpa" . "http://melpa.org/packages/")
                  ("melpa-stable" . "http://stable.melpa.org/packages/")
                  ("org-elpa" . "https://orgmode.org/elpa/")))
(package-initialize)

;; Ensure use-package is present. From here on out, all packages are loaded
;; with use-package, a macro for importing and installing packages.
;; Also, refresh the package archive on load so we can pull the latest packages.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; allow use-package
(require 'use-package)
(setq
 use-package-always-ensure t ;; download new packages if they aren't already downloaded
 use-package-verbose t) ;; Package installation logging.

;; Slurp environment variables from the shell.
;; a.k.a. The Most Asked Question On r/emacs
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))



;; EVIL, EVIL-COLLECTION, EVIL-TUTOR- vi emulation
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
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

(use-package evil-surround
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


;; ALL-THE-ICONS, ALL-THE-ICONS-DIRED : Icons for dired, etc.  Install the latest fonts with M-x all-the-icons-install-fonts
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . (lambda () (all-the-icons-dired-mode t))))

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



;; IVY, IVY-RICH, COUNSEL

;; COUNSEL, a collection of Ivy-enhanced versions of common Emacs commands.
(use-package counsel
  :ensure t
  :after ivy
  :config (counsel-mode))

;; IVY, a generic completion mechanism for Emacs.
(use-package ivy
  :ensure t
  :bind
  ;; ivy-resume resumes the last Ivy-based completion.
  (("C-c C-r" . ivy-resume)
   ("C-x B" . ivy-switch-buffer-other-window))
  :custom
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq enable-recursive-minibuffers t)
  :config
  (ivy-mode 1))

;; ALL-THE-ICONS-IVY-RICH
(use-package all-the-icons-ivy-rich
  :ensure t
  :init (all-the-icons-ivy-rich-mode 1))

;; IVY-RICH allows us to add descriptions alongside the commands in M-x.
(use-package ivy-rich
  :after ivy
  :ensure t
  :init (ivy-rich-mode 1) ;; this gets us descriptions in M-x.
  :custom
  (ivy-rich-ivy-path-style 'abbrev
			   ivy-virtual-abbreviate 'full
			   ivy-rich-switch-buffer-align-virtual-buffer t))

;; SWIPER, an enhanced alternative to isearch
(use-package swiper
  :after ivy
  :ensure t
  :bind ("C-s" . swiper))

;; ivy-based interface to standard commands
;;(global-set-key (kbd "C-s") 'swiper-isearch)
;;(global-set-key (kbd "M-x") 'counsel-M-x)
;;(global-set-key (kbd "C-x C-f") 'counsel-find-file)
;;(global-set-key (kbd "M-y") 'counsel-yank-pop)
;;(global-set-key (kbd "<f1> f") 'counsel-describe-function)
;;(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
;;(global-set-key (kbd "<f1> l") 'counsel-describe-library)
;;(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
;;(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
;;(global-set-key (kbd "<f2> j") 'counsel-set-variable)
;;(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
;;(global-set-key (kbd "C-c v") 'ivy-push-view)
;;(global-set-key (kbd "C-c V") 'ivy-pop-view)

;; ivy-based interfaces to shell and other tools
;;(global-set-key (kbd "C-c c") 'counsel-compile)
;;(global-set-key (kbd "C-c g") 'counsel-git)
;;(global-set-key (kbd "C-c j") 'counsel-git-grep)
;;(global-set-key (kbd "C-c L") 'counsel-git-log)
;;(global-set-key (kbd "C-c k") 'counsel-rg)
;;(global-set-key (kbd "C-c m") 'counsel-linux-app)
;;(global-set-key (kbd "C-c n") 'counsel-fzf)
;;(global-set-key (kbd "C-x l") 'counsel-locate)
;;(global-set-key (kbd "C-c J") 'counsel-file-jump)
;;(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
;;(global-set-key (kbd "C-c w") 'counsel-wmctrl)

;; ivy-resume and other commands
;;(global-set-key (kbd "C-c C-r") 'ivy-resume)
;;(global-set-key (kbd "C-c b") 'counsel-bookmark)
;;(global-set-key (kbd "C-c d") 'counsel-descbinds)
;;(global-set-key (kbd "C-c g") 'counsel-git)
;;(global-set-key (kbd "C-c o") 'counsel-outline)
;;(global-set-key (kbd "C-c t") 'counsel-load-theme)
;;(global-set-key (kbd "C-c F") 'counsel-org-file)



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
  :ensure nil ; do not try to install it as it is built-in
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

;; PAREDIT : Parentheses matching and colorization for lisps
(use-package paredit
  :ensure t)
;;(add-hook 'prog-mode-hook #'enable-paredit-mode)



;; RAINBOW-DELIMITERS : different colored parens based on nesting
(use-package rainbow-delimiters
  :ensure t)
;;(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; RACKET-MODE : Racket development
(use-package racket-mode
  :ensure t)
(add-hook 'racket-mode-hook #'enable-paredit-mode)
(add-hook 'racket-mode-hook #'rainbow-delimiters-mode)
(add-hook 'racket-mode-hook #'turn-on-surround-mode)
(add-hook 'racket-repl-mode-hook #'enable-paredit-mode)
(add-hook 'racket-repl-hook #'rainbow-delimiters-mode)
(add-hook 'racket-repl-mode-hook #'turn-on-surround-mode)

;; GEISER-RACKET : Racket development
(use-package geiser-racket
  :ensure t)
;; GEISER-CHEZ : Chez Scheme development
(use-package geiser-chez
             :ensure t)
(add-hook 'scheme-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook #'rainbow-delimiters-mode)
(add-hook 'scheme-mode-hook #'turn-on-surround-mode)

(setq geiser-active-implementations '(racket chez))



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("d77d6ba33442dd3121b44e20af28f1fae8eeda413b2c3d3b9f1315fbda021992" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
)
