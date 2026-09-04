;; MACOS DIRED -*- lexical-binding: t; -*-

;; MacOS ls does not implement --dired option
(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil))

