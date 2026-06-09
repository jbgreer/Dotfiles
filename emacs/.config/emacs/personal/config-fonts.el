;; FONTS AND FRAME SIZE

;; these fonts must be installed separately
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

