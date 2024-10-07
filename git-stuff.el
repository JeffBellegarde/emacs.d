;; * Git related stuff

;; ** Magit
(use-package with-editor)

;;(use-package magit-section)

(use-package git-commit)

(use-package magit-popup)

;; magit-gh-pulls is throwing errors.
;; (use-package gh)

;; (use-package magit-gh-pulls
;;   :commands (turn-on-magit-gh-pulls))

(use-package magit
  :straight t
  :bind ("C-c i" . magit-status)
  :defines (magit-emacsclient-executable)
  :custom
  (magit-repository-directories `(("~/src" . 1)))
  :config
  (if (eq system-type "darwin")
      (setq magit-emacsclient-executable "/Applications/Emacs.app/Contents/MacOS/bin/emacsclient"))
  ;; (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)
  )

(use-package magithub
  :disabled t
  :after magit
  :config (magithub-feature-autoinject t))

;(use-package ghub
;  :after magit)

;; *** Gist
(use-package gist
  :commands (gist-list)
  :init
  (bind-key "l" #'gist-list launcher-map))

;; *** Git messenger
(use-package git-messenger
  :straight t
  :bind ("C-x v p" . git-messenger:popup-message)
  :custom
  (git-messenger:show-detail t)
  ;;;;Where does magit-commit-mode come from?
  ;;(add-hook 'git-messenger:popup-buffer-hook 'magit-commit-mode)
  )
