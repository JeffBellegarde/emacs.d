(deftheme My initial theme
  "Created 2012-11-29.")

(custom-theme-set-variables
 'My initial theme
 )

(custom-theme-set-faces
 'My initial theme
 '(flymake-errline ((t (:background "red"))))
 '(trailing-whitespace ((((class color) (background light)) (:background "orange"))))
 '(font-lock-comment-face ((t (:foreground "dim gray"))))
 '(default ((t (:inherit nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 121 :width normal :foundry "unknown" :family "DejaVu Sans Mono")))))

(provide-theme 'My initial theme)
