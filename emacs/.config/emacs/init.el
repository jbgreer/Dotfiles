; 2023-07-03 jbgreer init.el



(setq debug-on-error t)
(setq user-emacs-directory "~/.config/emacs/")



;; elpaca: The Elisp Package Manager  https://github.com/progfolio/elpaca
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



;; Install use-package for succint package inclusion
(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :elpaca t unless otherwise specified.
  (setq elpaca-use-package-by-default t))
;; Block until current queue processed.
(elpaca-wait)



;; Set UI Theme
(load-theme 'zenburn t)
;; Icons for dired, etc.  Install the latest fonts with M-x all-the-icons-install-fonts
(use-package all-the-icons
  :ensure t
    :if (display-graphic-p))
(use-package all-the-icons-dired
  :hook (dired-mode . (lambda () (all-the-icons-dired-mode t))))

;; Set the Font Face
(set-face-attribute 'default nil
      :font "JetBrains Mono"
      :height 110
      :weight 'medium)
(set-face-attribute 'variable-pitch nil
      :font "Ubuntu"
      :height 120
      :weight 'medium)
(set-face-attribute 'fixed-pitch nil
      :font "JetBrains Mono"
      :height 110
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
(add-to-list 'default-frame-alist '(font . "JetBrains Mono-11"))

;; Uncomment the following line if line spacing needs adjusting.
(setq-default line-spacing 0.12)

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
  '((top . 25) (left . 275) (width . 140) (height . 70)))

;; Disable Menubar, Toolbars Tooltips, and Scrollbars, and set fringe
(menu-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)
(set-fringe-mode 10)

;; Display Line Numbers and Truncated Lines
(global-display-line-numbers-mode 1)
(global-visual-line-mode t)



;; Install which-key, a minor mode that displays available keybindings
(use-package which-key
  :init
  (which-key-mode 1)
  :config
  (setq which-key-side-window-location 'bottom
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



;; projectile - a project interaction library
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



;; ivy, counsel, swiper
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







;; magit, a git porcelion inside emacs
(use-package magit)
;;  :ensure t)



;; Parentheses matching and colorization for lisps
(use-package paredit)
;;(add-hook 'prog-mode-hook #'enable-paredit-mode)
(use-package rainbow-delimiters)
;;(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)



;; clojure development
(use-package clojure-mode)
(use-package cider)
(add-hook 'cider-repl-mode-hook #'enable-paredit-mode)
(add-hook 'cider-mode-hook #'enable-paredit-mode)
(add-hook 'clojure-repl-mode-hook #'enable-paredit-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
(add-hook 'cider-mode-hook 'rainbow-delimiters-mode)
(add-hook 'clojure-repl-mode-hook 'rainbow-delimiters-mode)



;; company - complete anything
;;(use-package company)


