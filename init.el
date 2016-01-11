(iconify-frame)
(setq debug-on-error t)

;;(defvar jmb-emacs-init-file load-file-name)
;; (defvar jmb-emacs-config-dir
;;   (file-name-directory jmb-emacs-init-file))
(defvar user-emacs-directory "~/.emacs.d")
(defvar jmb-lisp-dir (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path jmb-lisp-dir)

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("sunrise-commander" . "http://joseito.republika.pl/sunrise-commander/") t)
(package-initialize)
(defvar jmb-required-packages
      (list
       'fish-mode
       'flx-ido
;;       'git-gutter-fringe
       'gitconfig-mode
       'gitignore-mode
       'gist
       'go-eldoc
       'go-errcheck
;;       'ibuffer-vc
       'ido
       'ido-vertical-mode
       'paradox
       'projectile
       'rfringe
       'ruby-end
       'use-package
       'yasnippet
       'zenburn-theme
      ))

(dolist (package jmb-required-packages)
  (when (not (package-installed-p package))
    (package-refresh-contents)
    (package-install package)))
(eval-when-compile ;;Load use-package only when needed
  (require 'use-package))
(setq use-package-verbose t)
(setq use-package-minimum-reported-time 0.01)
(setq use-package-always-ensure t)
(require 'diminish)  ;;use-package dependencies
(require 'bind-key)


(load-file "~/.emacs.d/init2.el")
(org-babel-load-file "~/.emacs.d/setup.org")

(setq debug-on-error nil)
(make-frame-visible)
