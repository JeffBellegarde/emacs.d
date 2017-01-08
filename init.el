;; * Hide during load
(iconify-frame)
(setq debug-on-error t)

;; * Setup personal lisp directory.
;; This is where I put non package lisp code.
(defconst user-emacs-directory (if load-file-name
                                   (file-name-directory load-file-name)
                                 "~/.emacs.d"))
(defvar jmb-lisp-dir (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path jmb-lisp-dir)


;; * Init the package system
(require 'package)
;; Use https to access packages. (Your Editor is Malware)[https://glyph.twistedmatrix.com/2015/11/editor-malware.html]
(setq package-archives nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(unless (getenv "CI")
  (require 'tls)
  (setq tls-checktrust t)
  ;; Uses python's certi installed with 'python -m pip install --user certifi'
  (let ((trustfile
         (replace-regexp-in-string
          "\\\\" "/"
          (replace-regexp-in-string
           "\n" ""
           (shell-command-to-string "python -m certifi")))))
    (setq tls-program
          (list
           (format "gnutls-cli%s --x509cafile %s -p %%p %%h"
                   (if (eq window-system 'w32) ".exe" "") trustfile)))
    (setq gnutls-verify-error t)
    (setq gnutls-trustfiles (list trustfile))))

(package-initialize)

;; ** Load a list of packages
;; I want to remove things from this list and use use-package instead.
;; Also inits use-package.
(defvar jmb-required-packages
  (list
   'fish-mode
   'flx-ido
   ;;       'git-gutter-fringe
   'gitconfig-mode
   'gitignore-mode
   'go-eldoc
   'go-errcheck
   ;;       'ibuffer-vc
   'ido
   'ido-vertical-mode
   'rfringe
   'ruby-end
   'use-package
   'yasnippet
   'zenburn-theme
   'general ;;provides general-chord
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
(require 'diminish) ;;use-package dependencies
(require 'bind-key)
(require 'general)

;; ** Customization
(setq custom-file (expand-file-name "emacs-customizations.el" user-emacs-directory))
(load custom-file)

;; * Privates
;; Load private vars. Not to be checked in.
(when (file-exists-p "~/private.el")
  (load "~/private.el"))

;; * Theme
;; Use after-init-hook to avoid loading until after the config is loaded.

(add-hook 'after-init-hook (lambda () (load-theme 'zenburn t)))
;; * Configuration

;; ** Low level stuff
(setq tab-always-indent 'complete)
(prefer-coding-system 'utf-8)
;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; *** Rename current buffer file
;; From: http://whattheemacsd.com/file-defuns.el-01.html
(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))
(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)

;; ** Base Keymaps
;; My plan is to setup a small number of base keymaps to hook other functioanlity to. Some commands
;; may still be attached to top level commands, but I want everything to be accessible using the base keymaps.
(defvar jmb-base-keys-map (make-sparse-keymap))
(defvar jmb-base-keys-work-at-point-map (make-sparse-keymap)
  "Keymap for point based manipulation.
If a command works on or around the point, it goes here.
Often commands that belong are also mapped to a top-level key for speed.
Some commands that only use the thing at point as a default might be put in other places.
If the the point of the command is the point, is should probably be here.
")
(defvar jmb-base-keys-point-jump-map (make-sparse-keymap))
(define-key jmb-base-keys-work-at-point-map "j" jmb-base-keys-point-jump-map)
(define-key jmb-base-keys-work-at-point-map ";" 'comment-line)

(defvar jmb-base-keys-buffer-map (make-sparse-keymap)
  "Keymap for buffer manipulation.
This also handles frames, and windows. If it rearranges what is shown this is a good place for it.
")
(define-key jmb-base-keys-buffer-map (kbd "<SPC>") #'other-window)

(define-minor-mode jmb-base-keys-mode
  "docs"
  :lighter " Base"
  :global t
  :keymap jmb-base-keys-map)

(jmb-base-keys-mode 1)

;; ** Link-modes
(defmacro my-link-modes (leader-mode follower-mode)
  (let* ((funcname (intern (concat "my-link-modes/"
                                   (symbol-name leader-mode)
                                   "--"
                                   (symbol-name follower-mode))))
         (leader-hook (intern (concat (symbol-name leader-mode) "-hook")))
         (docstring (concat "Hook to enable "
                            (symbol-name follower-mode)
                            " whenever " (symbol-name leader-mode)
                            " is active.")))
    `(progn
       (defun ,funcname ()
         ,docstring
         (if (memq ',leader-mode minor-mode-alist)
             (,follower-mode (symbol-value ',leader-mode))
           (,follower-mode 1)))
       (add-hook ',leader-hook #',funcname))))

;; ** Disable show trailing whitespace.
;; Utility function whow trailing-whitespace. Add to the appropriate mode hookds.

(defun jmb/turn-on-show-trailing-whitespace ()
  (setq show-trailing-whitespace t))

;; ** Spinner
(use-package spinner
  :ensure t)

;; ** Prog mode
;; It's pretty rare to have a programming mode that wants whitespace at the end of a line.
(use-package prog-mode
  :ensure nil
  :config
  (add-hook 'prog-mode-hook #'jmb/turn-on-show-trailing-whitespace)
  ;; (defun my/linum-mode ()
  ;;   (linum-mode t))
  ;; (remove-hook 'prog-mode-hook #'my/linum-mode)
  (my-link-modes prog-mode linum-mode))

;; ** Projectile
(use-package projectile
  :bind ("C-c p" . projectile-command-map)
  :config
  (setq projectile-switch-project-action
        #'projectile-commander)
  (setq projectile-create-missing-test-files t)
  (def-projectile-commander-method ?s
    "Open a *shell* buffer for the project."
    (shell (get-buffer-create
            (format "*shell %s*"
                    (projectile-project-name)))))

  (def-projectile-commander-method ?c
    "Run `compile' in the project."
    (call-interactively #'compile)))

;; ** Chords for use-package
;; Enables key chords
(use-package use-package-chords
  :ensure t
  :config (key-chord-mode 1))

(bind-chord "jk" jmb-base-keys-work-at-point-map jmb-base-keys-map)
;; (general-define-key :keymaps 'jmb-base-keys-map
;;                     (general-chord "jk") jmb-base-keys-work-at-point-map
;;                     (general-chord "kj") jmb-base-keys-work-at-point-map)
(bind-chord "fd" jmb-base-keys-buffer-map jmb-base-keys-map)

;; ** Aggressive indent mode
(use-package aggressive-indent)

;; ** restclient
(use-package restclient
  :commands (restclient-http-parse-current-and-do))


;; ** Org
(use-package org
  :bind (
         ("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c b" . org-iswitchb)
         ("C-c r" . org-capture))
  :defines 'org-plantuml-jar-path
  :config
  (org-clock-persistence-insinuate)
  (setq org-plantuml-jar-path "/usr/local/Cellar/plantuml/8018/plantuml.8018.jar")
  :ensure t)

;; *** ob-restclient
;; Alows restclient in org-mode.
;; This is intialized by putting 'restclient' in the variable `org-babel-load-languages' configured through customize.
(use-package ob-restclient)

;; *** Org protocol
(require 'org-protocol)

;; ** orgstruct
(use-package orgstruct-mode
  :ensure nil
  :commands (orgstruct-mode)
  :defines (orgstruct-heading-prefix-regexp)
  :preface
  (defun my/list-mode-hook ()
    (setq orgstruct-heading-prefix-regexp  ";; "))
  :init
  (add-hook 'emacs-lisp-mode-hook #'my/list-mode-hook)
  (add-hook 'emacs-lisp-mode-hook 'orgstruct-mode))

;; From http://doc.norang.ca/org-mode.html
(defun bh/display-inline-images ()
  (condition-case nil
      (org-display-inline-images)
    (error nil)))
(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images 'append)

;; ** Deft
(use-package deft
  :ensure t
  :bind ("C-c d" . deft))

;; ** Helm
(use-package helm
  :ensure t
  :defines (helm-M-x-fuzzy-match helm-grep-default-command helm-grep-default-recurse-command)
  :bind (
         ("C-c h" . helm-command-prefix)
         ("M-x" . helm-M-x)
         ("C-x b" . helm-mini)
         ("C-h SPC" . helm-all-mark-rings)
         ("C-x C-f" . helm-find-files)
         ("M-y" . helm-show-kill-ring))
  :config
  (require 'helm-config)
  (helm-mode 1)
  (global-unset-key (kbd "C-x c"))
  ;;rebind the keys
  (bind-key "C-c h g" 'helm-google-suggest)
  (bind-key "C-c h o" 'helm-occur)
  (bind-key "C-c h x" 'helm-register)
  (bind-key "C-c h M-:" 'helm-eval-expression-with-eldoc)
  (setq helm-M-x-fuzzy-match t)
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
  (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
  (define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)
  (when (executable-find "ack")
    (setq helm-grep-default-command "ack -H --no-group --no-color %p %f"
          helm-grep-default-recurse-command "ack -Hn --no-group --no-color %p %f"))

  ;; *** Descbinds
  (use-package helm-descbinds
    :ensure t
    :defer t
    :bind ("C-h b" . helm-descbinds)
    :config
    (progn
      (helm-descbinds-mode))))

;; *** Describe modes
(use-package helm-describe-modes
  :bind (("C-h m" . helm-describe-modes)))

;; *** swoop
(use-package helm-swoop
  :ensure t
  :init
  (defun my-helm-swoop-not-at-point ()
    (interactive)
    (let ((helm-swoop-pre-input-function (lambda () "")))
      (helm-swoop)))
  :bind
  (("M-i" . helm-swoop)
   ("M-I" . helm-swoop-back-to-last-point)
   ("C-c M-i" . helm-multi-swoop)
   ("C-x M-i" . helm-multi-swoop-all))
  :general
  (:keymaps 'jmb-base-keys-buffer-map "i"  'my-helm-swoop-not-at-point)
  (:keymaps 'jmb-base-keys-work-at-point-map "i"  'helm-swoop))

;; *** ag
;; Funcationality enabled but not bound to anything yet.
(use-package helm-ag
  :ensure t
  :commands (helm-ag helm-do-agg))

;; ** sr-speedbar
;; I also customize speedbar itself here.
(use-package sr-speedbar
  :commands (sr-speedbar-toggle)
  :ensure t
  :config
  (setq speedbar-show-unknown-files t))

;; ** dired
(use-package dired
  :commands (dired)
  :ensure nil
  :config
  (defun my/dired-mode-hook ()
    (hl-line-mode t)
    (toggle-truncate-lines 1))
  (add-hook 'dired-mode-hook #'my/dired-mode-hook)

  (use-package dired+
    :ensure t
    :config
    (add-hook 'dired-mode-hook #'my/dired-mode-hook)))

;; ** Select and expand
(use-package region-command-mode
  :ensure nil
  :load-path "~/src/region-command-mode"
  :config
  (region-command-mode t))

;; ** expand-region
(use-package expand-region
  :ensure t
  :commands (er/expand-region er/contract-region er--expand-region-1 er/clear-history)
  :init
  (defun my-er/clear-history ()
    (unless region-command-active-mode
      (er/clear-history)))
  (defun my-er/expand-region ()
    (interactive)
    (let ((expand-region-fast-keys-enabled nil))
      (er/expand-region 1)))
  (defun my-er/contract-region ()
    (interactive)
    (let ((expand-region-fast-keys-enabled nil))
      (er/contract-region 1)))
  (define-key region-command-mode-keymap "," #'my-er/expand-region)
  (define-key region-command-mode-keymap "." #'my-er/contract-region)
  (add-hook 'region-command-active-mode-hook #'my-er/clear-history)
  (define-key jmb-base-keys-work-at-point-map " " #'my-er/expand-region))
;; (require 'expand-region)
;; (global-set-key (kbd "C-@") 'er/expand-region)
;; (global-set-key (kbd "C-#") 'er/contract-region)
;; (global-set-key (kbd "C-&") 'shrink-window-horizontally)
;; (global-set-key (kbd "C-*") 'enlarge-window-horizontally)
;; (global-set-key (kbd "C-(") 'shrink-window)
;; (global-set-key (kbd "C-)") 'enlarge-window)

;; ** ensime
(use-package ensime
  :commands (ensime-scala-mode-hook)
  :ensure t
  :init (add-hook 'scala-mode-hook 'ensime-scala-mode-hook))

;; ** gomode
(setenv "GOPATH" "/Users/Bellegarde/go_src")
(setq exec-path (append exec-path '("/Users/Bellegarde/go_src/bin")))
(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :config
  (progn
    (defun jmb/go-mode-hook ()
      (setq gofmt-command "goimports")
      (add-hook 'before-save-hook 'gofmt-before-save)
      ;; Customize compile command to run go build
      (local-set-key (kbd "M-.") 'godef-jump)
      (local-set-key (kbd "C-c C-i") 'go-goto-imports)
      (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
      (if (not (string-match "go" compile-command))
          (set (make-local-variable 'compile-command)
               "go build -v; and go test -v -coverprofile coverage.out; and go vet"))
      (go-oracle-mode))
    (add-hook 'go-mode-hook 'go-eldoc-setup)
    ;;(remove-hook 'before-save-hook 'jmb/gofmt-before-save)
    (add-hook 'go-mode-hook 'jmb/go-mode-hook)
    ;;(add-to-list 'load-path "~/go_src/src/github.com/dougm/goflymake")
    ;;(require 'go-flymake)
    ;;(require 'go-flycheck)
    (add-to-list 'load-path "~/go_src/src/github.com/nsf/gocode")
    (add-to-list 'load-path (concat (getenv "GOPATH")  "/src/github.com/golang/lint/misc/emacs"))
    (when (file-exists-p "$GOPATH/src/code.google.com/p/go.tools/cmd/oracle/oracle.el")
      (load-file "$GOPATH/src/code.google.com/p/go.tools/cmd/oracle/oracle.el"))))

;; ** Unorganized stuff

;;(require 'golint)
;; Conflicts with company.
;; Best to find a way to shift to the correct one for each mode.
(use-package auto-complete
  :ensure t
  :disabled t
  :config
  (global-auto-complete-mode t))

;;(require 'auto-complete-config) ;don't seem to need this

;;What can I use to defer this?
(use-package go-autocomplete
  :ensure t)

;; ** git-gutter
;; git-gutter-fringe avoids conflicts with linum mode.
;; Error: fringe-helper-modification-func: Invalid search bound (wrong side of point)
;; It happens often and the package does not apper to be maintained.
;; Moving to diff-hl.
(when (functionp 'define-fringe-bitmap)
  (use-package git-gutter-fringe
    :disabled t
    :config
    (setq git-gutter:update-interval 2)))


;; ** diff-hl
(use-package diff-hl
  :config
  (setq diff-hl-draw-borders nil)
  (global-diff-hl-mode))

;; ** guide-key
;; currently using which-key instead.
(use-package guide-key
  :ensure t
  :disabled t
  :defer 2
  :config
  (progn
    (setq guide-key/guide-key-sequence t)
    (guide-key-mode 1)))

;; ** Idle-highlight
(use-package idle-highlight-mode
  :ensure t
  :commands (idle-highlight-mode))

;; ** Pry
(when (file-exists-p "emacs-pry-setup")
  (load "emacs-pry-setup"))

;;smex
;; (global-set-key (kbd "M-x") 'smex)
;; (global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
;; (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; ** key-chord -- Automaticall brought in by use-package-chords
(use-package key-chord
  :ensure t
  :disabled t
  :config (key-chord-mode 1))
;;(require 'key-chord)
;;(key-chord-mode 1)
;;(key-chord-define-global "q["     "\C-u5\C-x{")
;;(key-chord-define-global "q]"     "\C-u5\C-x}")

;; (require 'ibuffer-vc)
(require 'ruby-end)

(use-package exec-path-from-shell
  :commands (exec-path-from-shell-initialize)
  :ensure t)

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH"))

;; (add-hook 'ibuffer-hook
;;      (lambda ()
;;        (ibuffer-vc-set-filter-groups-by-vc-root)
;;        (ibuffer-do-sort-by-alphabetic)))
;; ido
;;(require 'ido)
;;(require 'ido-vertical-mode)
;;(ido-vertical-mode 1)
;;(flx-ido-mode 1)

(use-package hungry-delete
  :ensure t
  :config
  (global-hungry-delete-mode))

(global-auto-revert-mode t)

;; ** EPA for file encryotion
;; Add -*- mode: org -*- -*- epa-file-encrypt-to: ("bellegar@gmail.com") -*-
;; to automatically encrypt/decrypt.
(require 'epa)
(epa-file-enable)

;; * Launcher
;; The launcher map is defined at the top so other things can add to it.

(defun my-switch-to-gnus-group-buffer ()
  "Switch to gnus group buffer if it exists, otherwise start gnus"
  (interactive)
  (if (or (not (fboundp 'gnus-alive-p))
          (not (gnus-alive-p)))
      (gnus)
    (switch-to-buffer "*Group*")))
(bind-keys :prefix "C-x l" :prefix-map launcher-map :prefix-docstring "Key map for launching \"applications\"."
           ("c" . calc)
           ("d" . ediff-buffers)
           ;;("f" . find-dired)
           ("a" . ack)
           ("e" . elfeed)
           ("h" . man) ; Help
           ("i" . package-install-from-buffer)
           ("g" . my-switch-to-gnus-group-buffer)
           ;;("n" . nethack)
           ;; ("l" . count-lines-page)
           ("p" . paradox-list-packages)
           ("s" . shell)
           ("t" . proced)) ; top
(global-set-key (kbd "s-l") 'launcher-map)

;; ** Paradox
;; package management
;; TODO: Should add to the key to the launcher map here.
(use-package paradox
  :commands (paradox-list-packages))

;; ** Reddit
(use-package nnredit
  :ensure nil
  :load-path "nnreddit")

;; ** Cider
;; I don't use cider itself but use it's overlay functionality.
(use-package cider
  :mode "\\.clj\\'")

;; ** Typeit
;; A typing speed test for emacs
(use-package typit
  :commands (typit-basic-test typit-advanced-test))

;; ** Inline overlays
;; From http://endlessparentheses.com/eval-result-overlays-in-emacs-lisp.html
(use-package my/inline-overlay
  :ensure nil
  :init
  (autoload 'cider--make-result-overlay "cider-overlays")

  (defun endless/eval-overlay (value point)
    (cider--make-result-overlay (format "%S" value)
      :where point
      :duration 'command)
    ;; Preserve the return value.
    value)

  (advice-add 'eval-region :around
              (lambda (f beg end &rest r)
                (endless/eval-overlay
                 (apply f beg end r)
                 end)))

  (advice-add 'eval-last-sexp :filter-return
              (lambda (r)
                (endless/eval-overlay r (point))))

  (advice-add 'eval-defun :filter-return
              (lambda (r)
                (endless/eval-overlay
                 r
                 (save-excursion
                   (end-of-defun)
                   (point))))))

;; ** Magit
;; magit-gh-pulls is throwing errors.
(use-package gh)

(use-package magit-gh-pulls
  :commands (turn-on-magit-gh-pulls))

(use-package magit
  :ensure t
  :bind ("C-c i" . magit-status)
  :defines (magit-emacsclient-executable)
  :config
  (if (eq system-type "darwin")
      (setq magit-emacsclient-executable "/Applications/Emacs.app/Contents/MacOS/bin/emacsclient"))
  (setq magit-repository-directories `("~/src"))
  (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls))

;; *** Gist
(use-package gist
  :commands (gist-list)
  :init
  (bind-key "l" #'gist-list launcher-map))

;; *** Git messenger
(use-package git-messenger
  :ensure t
  :bind ("C-x v p" . git-messenger:popup-message)
  :config
  (setq git-messenger:show-detail t)
  ;;;;Where does magit-commit-mode come from?
  ;;(add-hook 'git-messenger:popup-buffer-hook 'magit-commit-mode)
  )

;;(require 'rfringe)
;;(require 'flymake-cursor)
;;(require 'flymake-ruby)
;; ** Linum
(require 'linum)
;; (global-linum-mode nil)
;;(window-numbering-mode)
;;(require 'imenu+)
;;(require 'ack)

;; ** Ack
;; does not appear to be available in melpa.
;; Formally abandoned. Need to fina a replacement.
(use-package ack-and-a-half
  :ensure nil
  :commands (ack-and-a-half ack-and-a-half-same ack-and-a-half-find-file ack-and-a-half-find-file-same)
  :init
  (progn
    (defalias 'ack 'ack-and-a-half)
    (defalias 'ack-same 'ack-and-a-half-same)
    (defalias 'ack-find-file 'ack-and-a-half-find-file)
    (defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)))

;; ** Undo Tree
(use-package undo-tree
  :config
  ;; Keep region when undoing in region
  ;; From: http://whattheemacsd.com/my-misc.el-02.html
  (defadvice undo-tree-undo (around keep-region activate)
    (if (use-region-p)
        (let ((m (set-marker (make-marker) (mark)))
              (p (set-marker (make-marker) (point))))
          ad-do-it
          (goto-char p)
          (set-mark m)
          (set-marker p nil)
          (set-marker m nil))
      ad-do-it))
  (global-undo-tree-mode))

(prefer-coding-system 'utf-8)

;; ** Company
(use-package company
  ;;  :commands (global-company-mode)
  :demand t
  :bind (("C-/" . company-complete))
  :config
  (add-hook 'after-init-hook 'global-company-mode))

;; Uses C-/ within company complete to shift to helm.
(use-package helm-company
  :commands (helm-company)
  :config
  (with-eval-after-load 'company
    (define-key company-active-map (kbd "C-/") 'helm-company)))

(use-package company-quickhelp
  :if (display-graphic-p)
  :defer t
  :init (add-hook 'company-mode-hook 'company-quickhelp-mode)
  :config
  (setq company-quickhelp-delay 1
        company-quickhelp-max-lines 10))

;; ** Desktop
(use-package desktop
  :functions 'desktop-owner
  :config
  (desktop-save-mode 1)
  (defun jmb-desktop-save ()
    (interactive)
    ;; Don't call desktop-save-in-desktop-dir, as it prints a message.
    (if (eq (desktop-owner) (emacs-pid))
        (desktop-save desktop-dirname)))
  (add-hook 'auto-save-hook 'jmb-desktop-save))


;; ** Ruby stuff
;;Locally installed. Not on melpa. Deal with next time i do ruby.
;;(use-package rcov-overlay)
(use-package yari
  :commands (yari))
;;(require 'yari)
(defun ri-bind-key ()
  (local-set-key [f1] 'yari))

;;(autoload 'ruby-mode "~/,emacs.d/ruby/ruby-mode" "Major mode for ruby files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
;;(add-hook 'ruby-mode-hook 'turn-on-show-trailing-whitespace)

(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.thor$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Thorfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Berksfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("buildfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))

(add-hook 'ruby-mode-hook 'idle-highlight-mode)
(add-hook 'ruby-mode-hook 'ri-bind-key)

;; ** Crystal
(use-package crystal-mode
  :ensure nil
  :mode "\\.cr$"
  :interpreter "crystal"
  :load-path "emacs-crystal-mode")

;;;(add-hook 'ruby-mode-hook 'minimap-create)

;;(require `fish-mode)

;;(setq exec-path (append exec-path (list (expand-file-name "/usr/local/Cellar/go/1.2/libexec/bin"))))
;;(setq exec-path (append exec-path (list (expand-file-name "~/go_src/bin"))))

;; ** Environment variables from fish
(defun jmb/empty-string (str)
  (string= "" str))
(defun jmb/update-env-vars-from-fish ()
  (let ((lines (split-string (shell-command-to-string "fish -c \"set -xUL\"") "\n")))
    (dolist (line lines)
      (let* ((parts (split-string line " "))
             (name (elt parts 0))
             (value (mapconcat 'identity (cl-remove-if 'jmb/empty-string (reverse (butlast (reverse parts)  1))) ":")))
        (if (not (string= "" name)) (setenv name value))))))


;; ** indent whole buffer
(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

;; ** random unused stuff
;;(require 'rubydb)
;;(require 'one-key-macro)

;;(global-set-key  "\C-c\C-a" 'autotest-switch)


;;(defun try-to-add-imenu ()
;;  (condition-case nil (imenu-add-defs-to-menubar) (error nil)))
;; (add-hook 'font-lock-mode-hook 'try-to-add-imenu)

;; ** visual-regexp
(use-package visual-regexp
  :ensure t
  :bind (("C-c C-q" . vr/replace)
         ("C-c q" . vr/query-replace)
         ("C-c m" . vr/mc-mark)))

;; ** Shell mode stuff
(use-package ansi-color
  :commands (ansi-color-for-comint-mode-on)
  :preface (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on))

(defun my-standard-comint-mode-hooks ()
  (add-hook 'comint-output-filter-functions 'comint-truncate-buffer)
  (add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)
  (add-hook 'comint-output-filter-functions 'ansi-color-process-output)
  (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)
  (local-set-key [home]        ; move to beginning of line, after prompt
                 'comint-bol)
  (local-set-key [up]          ; cycle backward through command history
                 '(lambda () (interactive)
                    (if (comint-after-pmark-p)
                        (comint-previous-input 1)
                      (previous-line 1))))
  (local-set-key [down]        ; cycle forward through command history
                 '(lambda () (interactive)
                    (if (comint-after-pmark-p)
                        (comint-next-input 1)
                      (forward-line 1)))))

(add-hook 'shell-mode-hook 'my-standard-comint-mode-hooks)

                                        ;(require 'rdebug)
                                        ;(add-hook 'shell-mode-hook 'turn-on-rdebug-track-mode)
                                        ;(remove-hook 'shell-mode-hook 'turn-on-rdebug-track-mode)

;;Allow the mouse to work even under an xterm window
;;(require 'mouse)
;;(xterm-mouse-mode t)
;;(defun track-mouse (e))

;;(load 'ensime-setup.e')

(add-hook 'gud-mode-hook 'my-standard-comint-mode-hooks)
;;     '(lambda ()
;;              (local-set-key [home]        ; move to beginning of line, after prompt
;;                            'comint-bol)
;;        (local-set-key [up]          ; cycle backward through command history
;;                            '(lambda () (interactive)
;;                               (if (comint-after-pmark-p)
;;                                   (comint-previous-input 1)
;;                                 (previous-line 1))))
;;        (local-set-key [down]        ; cycle forward through command history
;;                            '(lambda () (interactive)
;;                               (if (comint-after-pmark-p)
;;                                   (comint-next-input 1)
;;                                (forward-line 1))))
;;             ))

;;Disable flymake in rspec
;;(defun flymake-rspec-init ()
;;  ())

;;(push '(".+_spec\\.rb$" flymake-rspec-init) flymake-allowed-file-name-masks)

;;;Guard notifications
;;(defun guard-notification (type title message image)
;;  (message type))

;; ** Hydra
(use-package hydra
  :ensure t
  :bind ( ("C-M-o" . hydra-window/body)
          ("<f2>" . hydra-zoom/body)
          ("C-x SPC" . hydra-rectangle/body))
  :chords ( ;; ("jk" . hydra-window/body)
           ("jl" . hydra-navigate/body))
  :commands (defhydra)
  :config
  (hydra-add-font-lock)
  ;; *** Zoom
  (defhydra hydra-zoom (global-map "<f2>")
    "zoom"
    ("g" text-scale-increase "in")
    ("l" text-scale-decrease "out")
    ("0" (text-scale-set 0) "reset"))
  
  ;; *** Window manipulation
  (defun hydra-move-splitter-left (arg)
    "Move window splitter left."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'right))
        (shrink-window-horizontally arg)
      (enlarge-window-horizontally arg)))

  (defun hydra-move-splitter-right (arg)
    "Move window splitter right."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'right))
        (enlarge-window-horizontally arg)
      (shrink-window-horizontally arg)))

  (defun hydra-move-splitter-up (arg)
    "Move window splitter up."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'up))
        (enlarge-window arg)
      (shrink-window arg)))

  (defun hydra-move-splitter-down (arg)
    "Move window splitter down."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'up))
        (shrink-window arg)
      (enlarge-window arg)))

  (defhydra hydra-window (:color amaranth)
    "window"
    ("h" windmove-left)
    ("j" windmove-down)
    ("k" windmove-up)
    ("l" windmove-right)
    ("H" hydra-move-splitter-left)
    ("J" hydra-move-splitter-down)
    ("K" hydra-move-splitter-up)
    ("L" hydra-move-splitter-right)

    ("3" (lambda ()
           (interactive)
           (split-window-right)
           (windmove-right))
     "vert")
    ("2" (lambda ()
           (interactive)
           (split-window-below)
           (windmove-down))
     "horz")
    ("t" transpose-frame "'")
    ("1" delete-other-windows "one" :color blue)
    ("a" ace-window "ace")
    ("s" ace-swap-window "swap")
    ("d" ace-delete-window "del")
    ("i" ace-maximize-window "ace-one" :color blue)
    ("b" helm-mini "buf")
    ;;("m" headlong-bookmark-jump "bmk")
    ("q" nil "cancel"))
  (global-set-key (kbd "C-M-o") 'hydra-window/body)
  ;; (key-chord-define-global "jk" 'hydra-window/body)

  ;; *** Navigation
  (defhydra hydra-navigate (:color amaranth)
    "navigate"
    ("k" beginning-of-defun "beginning-of-defun")
    ("j" end-of-defun "end-of-defun")
    ("h" er/expand-region "expand-region")
    ("l" er/contract-region "contract-region")
    ("a" move-beginning-of-line "line start")
    ("e" move-end-of-line "line end")
    ("SPC" set-mark-command :color red)
    ("n" narrow-to-region "narrow")
    ("q" nil "cancel"))
  ;;  (key-chord-define-global "jl" 'hydra-navigate/body)

  ;; Rectangles
  (defhydra hydra-rectangle (:body-pre (rectangle-mark-mode 1)
                                       :color pink
                                       :post (deactivate-mark))
    "
      _k_   ^^_d_elete    _s_tring
    _h_ _l_   _o_k        _w_kill
      _j_   ^^_n_ew-copy  _r_eset
  ^^        ^^_e_xchange  _u_ndo
  ^^^^      ^^            _y_ank
    "
    ("h" backward-char nil)
    ("l" forward-char nil)
    ("k" previous-line nil)
    ("j" next-line nil)
    ("<left>" backward-char nil)
    ("<right>" forward-char nil)
    ("<up>" previous-line nil)
    ("<down>" next-line nil)
    ("e" exchange-point-and-mark nil)
    ("n" copy-rectangle-as-kill nil)
    ("d" delete-rectangle nil)
    ("r" (if (region-active-p)
             (deactivate-mark)
           (rectangle-mark-mode 1)) nil)
    ("y" yank-rectangle nil)
    ("u" undo nil)
    ("s" string-rectangle nil)
    ("w" kill-rectangle nil)
    ("o" nil nil)
    ("q" nil nil)
    )
  ;; (global-set-key (kbd "C-x SPC") 'hydra-rectangle/body)
  )

;; ** Avy
;; Jumping around.
(use-package avy
  :commands (avy-goto-word-0)
  :init
  (define-key jmb-base-keys-buffer-map "j" #'avy-goto-word-0))

;; ** ace-window
;; Still not in the habit of using it.
(use-package ace-window
  :ensure t
  :commands (ace-window)
  :init
  (define-key jmb-base-keys-buffer-map " " #'ace-window)
  :config
  (setq aw-keys   '(?a ?s ?d ?f ?j ?k ?l)
        ace-window-display-mode t
        ;; aw-dispatch-always t
        aw-dispatch-alist
        '((?x aw-delete-window     "Ace - Delete Window")
          (?c aw-swap-window       "Ace - Swap Window")
          (?n aw-flip-window)
          (?v aw-split-window-vert "Ace - Split Vert Window")
          (?h aw-split-window-horz "Ace - Split Horz Window")
          (?m delete-other-windows "Ace - Maximize Window")
          (?g delete-other-windows)
          (?b balance-windows)
          (?u winner-undo)
          (?r winner-redo)))
  (when (package-installed-p 'hydra)
    (defhydra hydra-window-size (:color red)
      "Windows size"
      ("h" shrink-window-horizontally "shrink horizontal")
      ("j" shrink-window "shrink vertical")
      ("k" enlarge-window "enlarge vertical")
      ("l" enlarge-window-horizontally "enlarge horizontal"))
    (defhydra hydra-window-frame (:color red)
      "Frame"
      ("f" make-frame "new frame")
      ("x" delete-frame "delete frame"))
    (defhydra hydra-window-scroll (:color red)
      "Scroll other window"
      ("n" scroll-other-window "scroll")
      ("p" scroll-other-window-down "scroll down"))
    (add-to-list 'aw-dispatch-alist '(?w hydra-window-size/body) t)
    (add-to-list 'aw-dispatch-alist '(?o hydra-window-scroll/body) t)
    (add-to-list 'aw-dispatch-alist '(?\; hydra-window-frame/body) t))
  (ace-window-display-mode t))

;; ** define-word
(use-package define-word
  :commands (define-word)
  :ensure t)

(use-package request
  :commands (request))

;; ** Eww basic browser.
;; Occasionally useful.
(use-package eww
  :commands (eww)
  :config
  (defadvice eww-tag-title (after rrix/eww-rename-buffer-ad (cont))
    "Update EWW buffer title with new page load."
    (message eww-current-title)
    (rename-buffer (format "*eww : %s *" eww-current-title) t))
  :ensure t)

;; ** flycheck
(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode 1)
  (setq flycheck-emacs-lisp-load-path  'inherit))
(add-to-list 'flycheck-disabled-checkers 'emacs-lisp-checkdoc)

;; don't know wht this is disabled? Maybe conflict with org?
(use-package flycheck-tip
  :ensure t
  :disabled t
  :bind ("C-c C-n" . flycheck-tip-cycle)
  :config (setq flycheck-tip-avoid-show-func nil))

(use-package flycheck-cask
  :commands (flycheck-cask-setup)
  :init (add-hook 'flycheck-mode-hook #'flycheck-cask-setup))

;; ** Swift -- for editing .swift
;; *** TODO Autocompletion https://github.com/nathankot/company-sourcekit
(use-package swift-mode
  :mode "\\.swift\\'"
  :config
  (add-to-list flycheck-checkers 'swift)
  :ensure t)

;; ** Wanderlust -- email
;; Stores read state locally isntead of in gmail. Not Used.
(use-package wanderlust
  :ensure t
  :disabled t
  :init
  (setq elmo-maildir-folder-path "~/Maildir"
        wl-stay-folder-window t                       ;; show the folder pane (left)
        wl-folder-window-width 25                     ;; toggle on/off with 'i') 
        wl-default-folder "[Gmail].All Mail"           ;; my main inbox 
        wl-draft-folder "[Gmail].Drafts"            ;; store drafts in 'postponed'
        wl-trash-folder "[Gmail].Trash"             ;; put trash in 'trash'
        wl-spam-folder ".Gmail].Trash"              ;; ...spam as well
        wl-queue-folder ".queue")             ;; we don't use this

  ;; IMAP
  ;; (setq elmo-imap4-default-server "imap.gmail.com")
  ;; (setq elmo-imap4-default-user "bellegar@gmail.com")
  ;; (setq elmo-imap4-default-authenticate-type 'clear)
  ;; (setq elmo-imap4-default-port '993)
  ;; (setq elmo-imap4-default-stream-type 'ssl)

  ;; (setq elmo-imap4-use-modified-utf7 t)

  ;; SMTP

  (setq wl-smtp-connection-type 'starttls)
  (setq wl-smtp-posting-port 587)
  (setq wl-smtp-authenticate-type "plain")
  (setq wl-smtp-posting-user "bellegar")
  (setq wl-smtp-posting-server "smtp.gmail.com")
  (setq wl-local-domain "gmail.com")

  (setq wl-default-spec "%")
  (setq wl-folder-check-async t)

  ;;(setq elmo-imap4-use-modified-utf7 t)

  (autoload 'wl-user-agent-compose "wl-draft" nil t)
  (if (boundp 'mail-user-agent)
      (setq mail-user-agent 'wl-user-agent))
  (if (fboundp 'define-mail-user-agent)
      (define-mail-user-agent
        'wl-user-agent
        'wl-user-agent-compose
        'wl-draft-send
        'wl-draft-kill
        'mail-send-hook))
  :commands (wl wl-other-frame wl-draft))


(use-package keychain-environment)


;; ** docker
(use-package docker)

(use-package dockerfile-mode
  :ensure t)

(use-package docker-tramp)

;;(use-package uniquify)

;; ** applescript
(use-package apples-mode
  :commands (apples-do-applescript))

(defun yank-chrome-url ()
  "Yank current URL from Chrome"
  (interactive)
  (require 'apples-mode)
  (apples-do-applescript "tell application \"Google Chrome\"
 get URL of active tab of first window
end tell"
                         #'(lambda (url status script)
                             ;; comes back with quotes which we strip off
                             (insert (cl-subseq url 1 (1- (length url)))))))

(remove-hook 'text-mode-hook #'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

(global-set-key (kbd "<f9>") 'compile)

(use-package impatient-mode
  :ensure t
  :commands (impatient-mode))

(use-package popwin)

(use-package command-log-mode
  :commands command-log-mode
  :ensure t)

;; ** Lisp stuff
;; *** paredit
;; I prefer lispy these days
(use-package paredit
  :disabled t
  :commands (enable-paredit-mode)
  :ensure t
  :init
  (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook           #'enable-paredit-mode))

(defun jmb--elevate-keymap (keymap-name)
  (add-to-list 'minor-mode-overriding-map-alist
               (assoc keymap-name minor-mode-map-alist)))

(use-package lispy
  :commands lispy-mode
  :init
  (defun jmb-lispy/activate-lispy-mode ()
    (lispy-mode 1))
  (add-hook 'emacs-lisp-mode-hook #'jmb-lispy/activate-lispy-mode)
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
  ;;Lispy rebinds M-i so put it back.
  (defun jmb-lispy/rebind-to-helm-swoop ()
    (bind-key "M-i" 'helm-swoop lispy-mode-map-lispy))
  (defun jmb-lispy/elevate-command-region-keys ()
    (jmb--elevate-keymap 'region-command-active-mode))
  (add-hook 'lispy-mode-hook #'jmb-lispy/rebind-to-helm-swoop)
  (add-hook 'lispy-mode-hook #'jmb-lispy/elevate-command-region-keys))

(eldoc-mode)

;; ** StackExchange
(use-package sx
  :ensure t
  :commands (sx-tab-all-questions))

;; ** elm
(use-package elm-mode
  :mode "\\.elm\\'")
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

;; ** Mu4e -- email
;; I don't like the view system.
(use-package mu4e
  :ensure nil
  :disabled t
  :commands (mu4e)
  :config
  (setq mu4e-drafts-folder "/[Gmail].Drafts")
  (setq mu4e-sent-folder   "/[Gmail].Sent Mail")
  (setq mu4e-trash-folder  "/[Gmail].Trash")
  (setq mu4e-update-interval 300)
  (setq mu4e-view-show-images t)
  (setq mu4e-view-prefer-html t)

  ;;  don't save message to Sent Messages, Gmail/IMAP takes care of this
  (setq mu4e-sent-messages-behavior 'delete)

  ;; (See the documentation for `mu4e-sent-messages-behavior' if you have
  ;; additional non-Gmail addresses and want assign them different
  ;; behavior.)

  ;; setup some handy shortcuts
  ;; you can quickly switch to your Inbox -- press ``ji''
  ;; then, when you want archive some messages, move them to
  ;; the 'All Mail' folder by pressing ``ma''.

  (setq mu4e-maildir-shortcuts
        '( ("/INBOX"               . ?i)
           ("/[Gmail].Sent Mail"   . ?s)
           ("/[Gmail].Trash"       . ?t)
           ("/[Gmail].All Mail"    . ?a)))

  ;;  allow for updating mail using 'U' in the main view:
  (setq mu4e-get-mail-command "offlineimap")

  ;; something about ourselves

  (setq
   user-mail-address "bellegar@gmail.com"
   user-full-name  "Jeff Bellegarde"
   mu4e-compose-signature
   (concat
    "Jeff Bellegarde\n"))
  (require 'mu4e-contrib)
  (setq mu4e-html2text-command 'mu4e-shr2text))


(use-package smtpmail
  :ensure nil
  :config
  ;; (setq message-send-mail-function 'smtpmail-send-it
  ;;       starttls-use-gnutls t
  ;;       smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
  ;;       smtpmail-auth-credentials '(("smtp.gmail.com" 587 "bellegar@gmail.com" nil))
  ;;       smtpmail-default-smtp-server "smtp.gmail.com"
  ;;       smtpmail-smtp-server "smtp.gmail.com"
  ;;       smtpmail-smtp-service 587)

  ;;Alternatively, for emacs-24 you can use:
  (setq message-send-mail-function 'smtpmail-send-it
        smtpmail-stream-type 'starttls
        smtpmail-default-smtp-server "smtp.gmail.com"
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587)

  ;; don't keep message buffers around

  (setq message-kill-buffer-on-exit t))

;; ** github-notifier
(use-package github-notifier
  :defer 10
  :config
  (setq github-notifier-mode-line
        '(:eval
          (let (unread-text help-text)
            (cond
             ((null github-notifier-unread-count)
              (setq unread-text "GH-?" help-text "The Github notifications number is unknown."))
             ((zerop github-notifier-unread-count)
              (setq unread-text "" help-text "Good job, you don't have unread notification."))
             (t
              (setq unread-text
                    (format "GH-%d" github-notifier-unread-count)
                    help-text
                    (if
                        (= github-notifier-unread-count 1)
                        "You have 1 unread notification.\nmouse-1 Read it on Github."
                      (format "You have %d unread notifications.\nmouse-1 Read them on Github." github-notifier-unread-count)))))
            (propertize
             (concat " " unread-text)
             'help-echo help-text 'local-map github-notifier-mode-line-map 'mouse-face 'mode-line-highlight))))
  (github-notifier-mode))

;; ** Esup Startup profiler.
;; Only shows numbers up to initialize-packages. Not useful yet.
(use-package esup
  :commands (esup))

;; ** Eclim -- Java Editing

(use-package eclim
  :disabled t
  :ensure emacs-eclim
  :config
  (require 'eclimd)
  (setq eclim-executable "/opt/eclipse/eclim"
        eclimd-default-workspace "~/Documents/workspace"
        eclim-problems-show-pos t)
  (add-hook 'eclim-mode-hook 'company-mode))

;; ** youtube-dl
;; download youtube videos.
(defun youtube-dl ()
  (interactive)
  (let* ((str (current-kill 0))
         (default-directory "~/Downloads")
         (proc (get-buffer-process (ansi-term "/bin/bash"))))
    (term-send-string
     proc
     (concat "cd ~/Downloads && youtube-dl " str "\n"))))

;; ** pop-to-mark
;; When popping the mark, continue popping until the cursor
;; actually moves.
;; from [http://endlessparentheses.com/faster-pop-to-mark-command.html?source=rss]
(defadvice pop-to-mark-command (around ensure-new-position activate)
  (let ((p (point)))
    (dotimes (i 10)
      (when (= p (point)) ad-do-it))))

;; repeat pop-to-mark on C-SPC
(setq set-mark-command-repeat-pop t)

;; ** popup-imenu
;; SHould probably enable this only where imenu is used.
(use-package popup-imenu
  :bind ("C-<tab>" . popup-imenu)
  :ensure t
  :config
  (setq popup-imenu-position 'point)
  (define-key popup-isearch-keymap (kbd "C-<tab>") 'popup-isearch-cancel))

;; ** super-save
;; Auto save buffers
(use-package super-save
  :ensure t
  :functions 'super-save-initialize
  :config (super-save-initialize))

;; **el-mock
;; used for testing
(use-package el-mock)

;; **engine-mode
;; External search engines.

(use-package engine-mode
  :config
  (engine-mode)
  (define-key region-command-mode-keymap "/" engine-mode-prefixed-map))

(defengine amazon
  "http://www.amazon.com/s/ref=nb_sb_noss?url=search-alias%3Daps&field-keywords=%s"
  :keybinding "a")

(defengine duckduckgo
  "https://duckduckgo.com/?q=%s"
  :keybinding "d")

(defengine github
  "https://github.com/search?ref=simplesearch&q=%s"
  :keybinding "g")

(defengine google
  "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
  :keybinding "o")

(defengine google-images
  "http://www.google.com/images?hl=en&source=hp&biw=1440&bih=795&gbv=2&aq=f&aqi=&aql=&oq=&q=%s")

(defengine google-maps
  "http://maps.google.com/maps?q=%s"
  :docstring "Mappin' it up.")

(defengine project-gutenberg
  "http://www.gutenberg.org/ebooks/search/?query=%s")

(defengine rfcs
  "http://pretty-rfc.herokuapp.com/search?q=%s")

(defengine stack-overflow
  "https://stackoverflow.com/search?q=%s")

(defengine start-page
  "https://startpage.com/do/dsearch?query=%s"
  :keybinding "/")

(defengine twitter
  "https://twitter.com/search?q=%s")

(defengine wikipedia
  "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
  :keybinding "w"
  :docstring "Searchin' the wikis.")

(defengine wiktionary
  "https://www.wikipedia.org/search-redirect.php?family=wiktionary&language=en&go=Go&search=%s")

(defengine wolfram-alpha
  "http://www.wolframalpha.com/input/?i=%s")

(defengine youtube
  "http://www.youtube.com/results?aq=f&oq=&search_query=%s"
  :keybinding "y")

;; ** Multiple-cursors
(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-prev-like-this)
         ("C-c <" . multiple-cursors-hydra/body))
  :config
  (defhydra multiple-cursors-hydra (:hint nil)
    "
     ^Up^            ^Down^        ^Other^
----------------------------------------------
[_p_]   Next    [_n_]   Next    [_l_] Edit lines
[_P_]   Skip    [_N_]   Skip    [_a_] Mark all
[_M-p_] Unmark  [_M-n_] Unmark  [_r_] Mark by regexp
^ ^             ^ ^             [_q_] Quit
"
    ("l" mc/edit-lines :exit t)
    ("a" mc/mark-all-like-this :exit t)
    ("n" mc/mark-next-like-this)
    ("N" mc/skip-to-next-like-this)
    ("M-n" mc/unmark-next-like-this)
    ("p" mc/mark-previous-like-this)
    ("P" mc/skip-to-previous-like-this)
    ("M-p" mc/unmark-previous-like-this)
    ("r" mc/mark-all-in-region-regexp :exit t)
    ("q" nil))
  (add-to-list 'mc/unsupported-minor-modes 'region-command-mode)
  (add-to-list 'mc/unsupported-minor-modes 'lispy-mode)
  (define-key region-command-mode-keymap "m" #'mc/mark-next-like-this))


;; ** travis
;; requires access token to be set.
(use-package travis
  :commands travis-show-projects)

;; **slack

(use-package alert
  :commands (alert)
  :config
  (setq alert-default-style 'notifier))

(use-package circe
  :commands (circe)
  :config
  (setq circe-network-options
        '(("eloldreader.irc.slack.com"
           :tls t
           :port 6667
           :nick "bellegar"
           :sasl-username "bellegar"
           :pass circe-eloldreader-pass
           :channels ("#general")
           ))))

(use-package slack
  :commands (slack-start)
  :defines (slack-user-name)
  :config
  (setq slack-buffer-emojify t) ;; if you want to enable emoji, default nil
  (setq slack-room-subscription '(general slackbot))
  (setq slack-client-id "18435938982.18441556390")
  (setq slack-user-name "bellegar"))


;; * Emacs Basics
;; ** Install emacs
;; Emacs needs to be installed before this can even run. Still provides a nice place to track the command line args. The --with-xwidgets are does not seem to have an effect. Not sure why.
;; #+begin_src sh
;; brew install emacs --with-cocoa --with-imagemagick --with-gnutls --with-xwidgets
;; #+end_src

;; ** Personal information
;; Who I am.
(setq user-full-name "Jeff Bellegarde"
      user-mail-address "bellegar@gmail.com")

;; ** Basic config
(require 'use-package)
(column-number-mode)
(tool-bar-mode -1)
(line-number-mode 1)
(add-hook 'after-init-hook 'server-start)
(add-hook 'edit-server-start-hook 'ns-raise-emacs-with-frame)

;;mode-compile
(autoload 'mode-compile "mode-compile"
  "Command to compile current buffer file based on the major mode" t)
(global-set-key "\C-cc" 'mode-compile)
(autoload 'mode-compile-kill "mode-compile"
  "Command to kill a compilation launched by `mode-compile'" t)
(global-set-key "\C-ck" 'mode-compile-kill)
(global-set-key "\C-z" 'undo)
(autoload 'git-mergetool-emacsclient-ediff "git-ediff" "Run Ediff for git" t)
(auto-compression-mode 1)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq use-dialog-box nil)

(setq ring-bell-function (lambda () (message "*beep*")))
(transient-mark-mode 1)

(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; ** Startup test
;; This will verify that emacs startup is working correctly.
;; From http://oremacs.com/2015/03/05/testing-init-sanity/

(defun ora-test-emacs ()
  (interactive)
  (require 'async)
  (async-start
   (lambda () (shell-command-to-string
               "emacs --batch --eval \"
(condition-case e
    (progn
      (load \\\"~/.emacs.d/init.el\\\")
      (message \\\"-OK-\\\"))
  (error
   (message \\\"ERROR!\\\")
   (signal (car e) (cdr e))))\""))
   `(lambda (output)
      (if (string-match "-OK-" output)
          (when ,(called-interactively-p 'any)
            (message "All is well"))
        (switch-to-buffer-other-window "*startup error*")
        (delete-region (point-min) (point-max))
        (insert output)
        (search-backward "ERROR!")))))

;; ** Personal key bindings
(bind-key "C-h K" 'describe-personal-keybindings)

;; * Global Minor Modes

;; ** Key frequency
(use-package keyfreq
  :defer 30
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

;; ** Winner (window layouts)
;; Binds C-c <left> and C-c <right>
(when (fboundp 'winner-mode)
  (winner-mode 1)
  (defhydra hydra-winner (:timeout 10)
    ("d" (progn
           (winner-undo)
           (setq this-command 'winner-undo)) "Undo")
    ("<SPC>" nil "Quit")
    ("q" #'winner-redo "Cancel" :exit t))
  (defun my-winner/start-winner ()
    (interactive)
    (winner-undo)
    (setq this-command 'winner-undo)
    (hydra-winner/body))
  (define-key jmb-base-keys-buffer-map (kbd "d") #'my-winner/start-winner))


;; ** KeyChord
;; I try to hit the keys at the same time so I want a really short delay.
(setq key-chord-two-keys-delay 0.05)


;; ** Smart Mode line


(use-package smart-mode-line
  :defer 2
  :ensure t
  :config
  (sml/setup))



;; ** ISpell
(use-package ispell
  :commands (ispell-word)
  :init
  (define-key jmb-base-keys-work-at-point-map (kbd "s") #'ispell-word))

;; ** Auto complete ISpell
(use-package ac-ispell
  :ensure t
  :commands (ac-ispell-ac-setup)
  :init (add-hook 'text-mode-hook 'ac-ispell-ac-setup)
  :config (ac-ispell-setup))


;; ** Dash

;; Api docs for os x. Open a seperate app. Not sure if I like it yet.
(use-package dash-at-point
  :ensure t
  :bind (("s-D"     . dash-at-point)
         ("C-c e"   . dash-at-point-with-docset)))

;; ** which-key

;; An improved version of guide-key
(use-package which-key
  :ensure t
  :defer 5
  :diminish ""
  :config
  (which-key-mode)
  (setq which-key-use-C-h-commands t
        which-key-idle-delay 0.5)
  (which-key-setup-side-window-right-bottom))


;; ** Browse kill ring
(use-package browse-kill-ring
  :disabled t
  :bind ("M-y" . browse-kill-ring)
  :ensure t)



;; ** Help-at

(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 0.1)
(help-at-pt-set-timer)


;; ** Beacon

(use-package beacon
  :diminish ""
  :config (beacon-mode))

;; ** God Mode

(use-package god-mode
  :bind ("<escape>" . god-mode-all)
  :disabled t
  :config
  (add-to-list 'god-exempt-major-modes 'Custom-mode)
  (add-to-list 'god-exempt-major-modes 'Info-mode)
  (define-key god-local-mode-map (kbd "i") 'god-local-mode)
  (defun my-update-cursor ()
    (setq cursor-type (if (or god-local-mode buffer-read-only)
                          'box
                        'bar)))
  (add-hook 'god-mode-enabled-hook 'my-update-cursor)
  (add-hook 'god-mode-disabled-hook 'my-update-cursor)
  (defun c/god-mode-update-cursor ()
    (let ((limited-colors-p (> 257 (length (defined-colors)))))
      (cond (god-local-mode (progn
                              (set-face-background 'mode-line (if limited-colors-p "white" "#e9e2cb"))
                              (set-face-background 'mode-line-inactive (if limited-colors-p "white" "#e9e2cb"))))
            (t (progn
                 (set-face-background 'mode-line (if limited-colors-p "black" "#0a2832"))
                 (set-face-background 'mode-line-inactive (if limited-colors-p "black" "#0a2832")))))))
  (add-hook 'god-mode-enabled-hook 'c/god-mode-update-cursor)
  (add-hook 'god-mode-disabled-hook 'c/god-mode-update-cursor))


;; ** Bookmarks

(use-package bm
  :general
  (:keymaps 'jmb-base-keys-buffer-map "t"  'bm-previous "g" 'bm-next)
  (:keymaps 'jmb-base-keys-work-at-point-map "b"  'bm-toggle))

(use-package helm-bm
  :general
  (:keymaps 'jmb-base-keys-buffer-map "b"  'helm-bm))
;; ** ace-link
(use-package ace-link
  :config
  (ace-link-setup-default))

;; * Major modes

;; ** IBuffer


;;(require 'vc)
(use-package ibuffer-vc
  :ensure t
  :commands (ibuffer-vc-set-filter-groups-by-vc-root))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :config
  (require 'ibuf-ext)
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-vc-set-filter-groups-by-vc-root)
              (ibuffer-do-sort-by-alphabetic))))


;; ** Elfeed (Rss)
(defvar jmb-elfeed-auto-update-timer)
(defvar jmb-elfeed-auto-update-min-delay (* 60 60))
(defvar jmb-elfeed-auto-update-idle-delay (* 10 60))
(defun jmb-elfeed-update ()
  (let ((idle-time (current-idle-time)))
    (when (and idle-time
               (> (float-time idle-time) jmb-elfeed-auto-update-idle-delay)
               (> (- (float-time) (elfeed-db-last-update)) jmb-elfeed-auto-update-min-delay))
      (message "Starting elfeed update")
      (elfeed-update))))
(defun jmb-elfeed-start-auto-update ()
  (interactive)
  (setq jmb-elfeed-auto-update-timer (run-at-time 0 600 #'jmb-elfeed-update))
  (add-hook 'kill-buffer-hook 'jmb-elfeed-stop-auto-update nil t))
(defun jmb-elfeed-stop-auto-update ()
  (interactive)
  (when (timerp jmb-elfeed-auto-update-timer)
    (cancel-timer jmb-elfeed-auto-update-timer)
    (setq jmb-elfeed-auto-update-timer nil)))
(use-package elfeed
  :commands (elfeed)
  :disabled t
  :ensure t
  :config
  (progn
    ;;    (add-hood 'elfeed-search-mode-hook 'jmb-elfeed-start-auto-update)
    (elfeed-org)))
(use-package elfeed-org
  :disabled t
  :commands (elfeed-org)
  :ensure t)

;; ** Ediff
;; Restore window layout after an ediff session.
;; From https://emacs.stackexchange.com/questions/7482/restoring-windows-and-layout-after-an-ediff-session
(defvar my-ediff-last-windows nil)

(defun my-store-pre-ediff-winconfig ()
  (setq my-ediff-last-windows (current-window-configuration)))

(defun my-restore-pre-ediff-winconfig ()
  (set-window-configuration my-ediff-last-windows))

(add-hook 'ediff-before-setup-hook #'my-store-pre-ediff-winconfig)
(add-hook 'ediff-quit-hook #'my-restore-pre-ediff-winconfig)


;; ** Overseer
(use-package overseer)
;; * Edit Server
(use-package edit-server
  :ensure t
  :defer 5
  :config (edit-server-start))

;; * Custom code

;; ** reverse-string
(defun my/reverse-string ()
  (interactive)
  (let* ((input (buffer-substring (mark) (point)))
         (reversed (concat (reverse (append input nil)))))
    (delete-region (mark)
                   (point))
    (insert reversed)))
;; ** Allow the narrow-to-region command.
(put 'narrow-to-region 'disabled nil)

;; * Finish loading
(setq debug-on-error nil)
(make-frame-visible)


