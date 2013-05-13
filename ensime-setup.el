;; load the ensime lisp code...
(add-to-list 'load-path "~/src/ensime_2.10.0-RC3-0.9.8.2/elisp/")
(require 'ensime)

;; This step causes the ensime-mode to be started whenever
;; scala-mode is started for a buffer. You may have to customize this step
;; if you're not using the standard scala mode.
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
