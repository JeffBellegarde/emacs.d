(setq jmb-emacs-init-file load-file-name)
(setq jmb-emacs-config-dir
      (file-name-directory jmb-emacs-init-file))

;; Set up 'custom' system
(setq custom-file (expand-file-name "emacs-customizations.el" jmb-emacs-config-dir))
(load custom-file)
