;; 2023-07-03 jbgreer turn off package.el
(setq package-enable-at-startup nil)

;; default gc cons threshold
(defvar default-gc-cons-threshold 16777216 ; 16mb
  "desired value of `gc-cons-threshold'during normal emacs operations.")

;; make garbage collector less invasive during startup
(setq gc-cons-threshold  most-positive-fixnum
      gc-cons-percentage 0.6)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(setq default-file-name-handler-alist file-name-handler-alist
      file-name-handler-alist nil)

;; reset gc threshold
(add-hook 'emacs-startup-hook
          (lambda (&rest _)
            (setq gc-cons-threshold default-gc-cons-threshold
                  gc-cons-percentage 0.1
                  file-name-handler-alist default-file-name-handler-alist)

            ;; delete no longer necessary startup variable
            (makunbound 'default-file-name-handler-alist)))

