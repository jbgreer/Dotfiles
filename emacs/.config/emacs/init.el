; 2023-06-18 jbgreer
(setq inhibit-startup-message t)    ; Disable startup splash screen

(scroll-bar-mode -1)                ; Disable visible scrollbar
(tool-bar-mode -1)                  ; Disable toolbar
(tooltip-mode -1)                   ; Disable tooltips
(set-fringe-mode 10)

(org-babel-load-file
 (expand-file-name
  "config.org"
  user-emacs-directory))
(org-babel-load-file
 (expand-file-name
  "config.org"
  user-emacs-directory))
