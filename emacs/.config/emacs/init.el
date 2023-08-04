;; 2023-07-03 jbgreer init.el

;; always process early-init.el first
(when (version< emacs-version "27")
  (load (concat user-emacs-directory "early-init.el")))

(setq debug-on-error t)
;;(setq user-emacs-directory "~/.config/emacs/")



;; ELPACA: The Elisp Package Manager  https://github.com/progfolio/elpaca
(defvar elpaca-installer-version 0.5)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))



;; ELPACA-USE-PACKAGE : use-package for succint package inclusion
(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :elpaca t unless otherwise specified.
  (setq elpaca-use-package-by-default t))
;; Block until current queue processed.
(elpaca-wait)



;; EVIL, EVIL-COLLECTION, EVIL-TUTOR- vi emulation
(use-package evil
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (evil-mode))

(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-mode-list '(dashboard dired ibuffer))
  (evil-collection-init))

(use-package evil-tutor)



;; GENERAL - keybindings
(use-package general
  :config
  (general-evil-setup)

  ;; set up 'SPC' as the global leader key
  (general-create-definer jg/leader-keys
			  :states '(normal insert visual emacs)
			  :keymaps 'override
			  :prefix "SPC" ;; set leader
			  :global-prefix "M-SPC") ;; access leader in insert mode

 
  
;; org-mode 
(setq org-log-done 'time)



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
   "e e" '(eval-expression :wk "Evaluate and elisp expression")
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
  
  ;; org-mode
  (jg/leader-keys
    "o" '(:ignore t :wk "Org")
    "o a" '(org-agenda :wk "Org Agenda")
    "o e" '(org-export-dispatch :wk "Org Export Dispatch")
    "o i" '(org-toggle-item :wk "Org Item Toggle")
    "o t" '(org-todo :wk "Org Todo")
    "o B" '(org-babel-tangle :wk "Org Babel Tangle")
    "o T" '(org-todo-list :wk "Org Todo List")
    "o d" '(org-time-stamp :wk "Org Date/timestamp"))

  ;; projectile
  (jg/leader-keys
    "p" '(projectile-command-map :wk "Projectile"))

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



;; UI STUFF

;; DOOM-THEMES : UI Theme
(use-package doom-themes
;;	    :ensure t
	    :config
	    (setq doom-themes-enable-bold t
		  doom-themes-enable-italic t)
	    (load-theme 'doom-dracula t)
	    (doom-themes-visual-bell-config))
;;	    (doom-themes-org-config))

;; ALL-THE-ICONS, ALL-THE-ICONS-DIRED : Icons for dired, etc.  Install the latest fonts with M-x all-the-icons-install-fonts
(use-package all-the-icons
;;  :ensure t
    :if (display-graphic-p))
(use-package all-the-icons-dired
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



;; PROJECTILE : a project interaction library
(use-package projectile
  :defer 1
  :commands
  (projectile-find-file projectile-switch-project)
  :config
  (projectile-global-mode)
  (setq projectile-completion-system 'ivy)
  (setq projectile-enable-caching t)
  (setq projectile-switch-project-action #'magit-status)
  :bind-keymap
  ("C-c p" . projectile-command-map))
;;  :bind (:map projectile-mode-map
;;	      ("C-c p") . projectile-command-map)))



;; COUNSEL, IVY, ALL-THE-ICONS-IVY-RICH, IVY-RICH, SWIPER
;; Ivy, a generic completion mechanism for Emacs.
;; Counsel, a collection of Ivy-enhanced versions of common Emacs commands.
;; Ivy-rich allows us to add descriptions alongside the commands in M-x.
;; Swiper, an enhanced alternative to isearch
(use-package counsel
  :after ivy
  :config (counsel-mode))
(use-package ivy
  :bind
  ;; ivy-resume resumes the last Ivy-based completion.
  (("C-c C-r" . ivy-resume)
   ("C-x B" . ivy-switch-buffer-other-window))
  :custom
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq enable-recursive-minibuffers t)
  :config
  (ivy-mode))
(use-package all-the-icons-ivy-rich
;; :ensure t
  :init (all-the-icons-ivy-rich-mode 1))
(use-package ivy-rich
  :after ivy
;;  :ensure t
  :init (ivy-rich-mode 1) ;; this gets us descriptions in M-x.
  :custom
  (ivy-rich-ivy-path-style 'abbrev
			   ivy-virtual-abbreviate 'full
			   ivy-rich-switch-buffer-align-virtual-buffer t))
(use-package swiper
  :after ivy
  :bind ("C-s" . swiper))
;; ivy, counsel, swiper global keybindings
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



;; MAGIG : a git porcelion inside emacs
(use-package magit)
;;  :ensure t)




;; PAREDIT : Parentheses matching and colorization for lisps
(use-package paredit)
;;(add-hook 'prog-mode-hook #'enable-paredit-mode)
(use-package rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)



;; CLOJURE-MODE, CIDER : clojure development
(use-package clojure-mode)
(use-package cider)
(add-hook 'cider-repl-mode-hook #'enable-paredit-mode)
(add-hook 'cider-mode-hook #'enable-paredit-mode)
(add-hook 'clojure-repl-mode-hook #'enable-paredit-mode)
;;(add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)
;;(add-hook 'cider-mode-hook #'rainbow-delimiters-mode)
;;(add-hook 'clojure-repl-mode-hook #'rainbow-delimiters-mode)



;; GEISER-CHEZZ : chez scheme development
(use-package geiser-chez)
(add-to-list 'auto-mode-alist
	     '("\\.sls\\'" . scheme-mode)
	     '("\\.sc\\'" . scheme-mode))
;;(add-hook 'scheme-mode #'rainbow-delimiters-mode)



;; RACKET-MODE : racket development
(use-package racket-mode)
;;(add-hook 'racket-mode #'rainbow-delimiters-mode)


;; company - complete anything
;;(use-package company)

