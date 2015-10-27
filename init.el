
(setq debug-on-error t)

(defvar jmb-emacs-init-file load-file-name)
(defvar jmb-emacs-config-dir
      (file-name-directory jmb-emacs-init-file))
(defvar user-emacs-directory jmb-emacs-config-dir)
(defvar jmb-disabled-whitespace-mode-hooks
      (list 'magit-mode-hook 'yari-mode-hook 'gud-mode-hook 'shell-mode-hook 'pry 'info-mode))
(defun jmb-disable-show-trailing-whitespace ()
  (interactive)
  (setq show-trailing-whitespace nil))

(dolist (hook jmb-disabled-whitespace-mode-hooks)
  (add-hook hook 'jmb-disable-show-trailing-whitespace))

(defvar jmb-lisp-dir (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path jmb-lisp-dir)
;;;(add-to-list 'load-path "~/.emacs.d/ruby")
;;(add-to-list 'load-path "~/src/emacs-Flymake")
;;(add-to-list 'load-path "~/src/autotest.el")
;;(add-to-list 'load-path "~/src/markdown-mode")

;;(add-to-list 'load-path "~/src/perspective-el")
;;(require 'perspective)
;;(persp-mode)

;;(add-to-list 'load-path "~/src/eproject")
;;(require 'eproject)
;;(require 'eproject-extras)

;(define-project-type gem (generic)
;  (look-for "Gemfile")
;  :relevant-files ("\.rb$"
;                   "Gemfile$"
;        "Rakefile"
;                   "\.ru$"
;        "bin/.*"
;                   "README\..*$")
;  :irrelevant-files ("bundle/.*"
;          "coverage/.*"
;          ".git/.*"
;          "pkg/.*"
;                     ".bin/.*")
;  :main-file "Gemfile")


(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("sunrise-commander" . "http://joseito.republika.pl/sunrise-commander/") t)
(package-initialize)
(defvar jmb-required-packages
      (list
       'fish-mode
       'flx-ido
       'git-gutter-fringe
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
(require 'diminish)  ;;use-package dependencies
(require 'bind-key)

;;(require 'use-package)

;;(defvar jmb-emacs-src-dir (expand-file-name "src" user-emacs-directory))
;;(setq custom-file (expand-file-name "emacs-customizations.el" jmb-emacs-config-dir))
;;(load custom-file)

;; ;;mode-compile
;; (autoload 'mode-compile "mode-compile"
;;   "Command to compile current buffer file based on the major mode" t)
;; (global-set-key "\C-cc" 'mode-compile)
;; (autoload 'mode-compile-kill "mode-compile"
;;   "Command to kill a compilation launched by `mode-compile'" t)
;; (global-set-key "\C-ck" 'mode-compile-kill)
;; (global-set-key "\C-z" 'undo)
;; (autoload 'git-mergetool-emacsclient-ediff "git-ediff" "Run Ediff for git" t)
;; (auto-compression-mode 1)

;;(add-to-list 'load-path (expand-file-name "yaml-mode" jmb-emacs-src-dir))
;;(require 'yaml-mode)
;;(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))


;;text scaling Use f2 instead
;;(global-set-key [67108907] 'text-scale-increase)
;;(global-set-key [67108909] 'text-scale-decrease)

(use-package use-package-chords
  :ensure t
  :config (key-chord-mode 1))

 (use-package org
   :bind (
          ("C-c l" . org-store-link)
          ("C-c a" . org-agenda)
          ("C-c b" . org-iswitchb)
          ("C-c r" . org-capture))
   :config (org-clock-persistence-insinuate)
   :ensure t)

(use-package deft
  :ensure t
  :bind ("C-c d" . deft))

;;;Requires async-shell-command which is only in emacs 23.2.
;;;(add-to-list 'load-path "~/src/bundler.el")
;;;(require 'bundler)

;; (require 'ibuf-ext)
;; (global-set-key (kbd "C-x C-b") 'ibuffer)
;; (add-hook 'ibuffer-mode-hook
;;      '(lambda ()
;;         (ibuffer-auto-mode 1)
;;         (ibuffer-switch-to-saved-filter-groups "home")))

;; (setq ibuffer-saved-filter-groups
;;       '(("home"
;;     ("emacs-config" (or (filename . ".emacs.d")
;;             (filename . "emacs")))
;;     ("magit" (name . "\*magit")))))
(use-package sr-speedbar
  :commands (sr-speedbar-toggle)
  :ensure t)

(defun my/dired-mode-hook ()
  (hl-line-mode t)
  (toggle-truncate-lines 1))

(use-package dired+
  :ensure t
  :config
  (add-hook 'dired-mode-hook #'my/dired-mode-hook))

;;expand-region
(use-package expand-region
  :ensure t
  :commands (er/expand-region er/contract-region))
;; (require 'expand-region)
;; (global-set-key (kbd "C-@") 'er/expand-region)
;; (global-set-key (kbd "C-#") 'er/contract-region)
;; (global-set-key (kbd "C-&") 'shrink-window-horizontally)
;; (global-set-key (kbd "C-*") 'enlarge-window-horizontally)
;; (global-set-key (kbd "C-(") 'shrink-window)
;; (global-set-key (kbd "C-)") 'enlarge-window)

;;ensime
(use-package ensime
  :commands (ensime-scala-mode-hook)
  :ensure t
  :init (add-hook 'scala-mode-hook 'ensime-scala-mode-hook))

;; gomode
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
    (load-file "$GOPATH/src/code.google.com/p/go.tools/cmd/oracle/oracle.el")
    ))

;;(require 'golint)

(use-package auto-complete
  :ensure t)

;;(require 'auto-complete-config) ;don't seem to need this
(use-package go-autocomplete
  :ensure t)

;;git-gutter
(when (functionp 'define-fringe-bitmap)
  (require 'git-gutter-fringe))

;; currently using which-key instead.
(use-package guide-key
  :ensure t
  :disabled t
  :defer 2
  :config
  (progn
    (setq guide-key/guide-key-sequence t)
    (guide-key-mode 1)))

;;(load "idle-highlight-setup")
(use-package idle-highlight-mode
  :ensure t
  :commands (idle-highlight-mode))

(load "emacs-pry-setup")

;;smex
;(global-set-key (kbd "M-x") 'smex)
;(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
;(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; key-chord
(use-package key-chord
  :ensure t
  :config (key-chord-mode 1))
;;(require 'key-chord)
;;(key-chord-mode 1)
;;(key-chord-define-global "q["     "\C-u5\C-x{")
;;(key-chord-define-global "q]"     "\C-u5\C-x}")

;(require 'ibuffer-vc)
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
;ido
;;(require 'ido)
;;(require 'ido-vertical-mode)
;;(ido-vertical-mode 1)
;;(flx-ido-mode 1)


(use-package hungry-delete
  :ensure t
  :config
  (global-hungry-delete-mode))

(global-auto-revert-mode t)
(use-package magit
  :ensure t
  :bind ("C-c i" . magit-status))
;(require 'magit)
;(global-set-key "\C-ci" 'magit-status)

;(require 'rfringe)
;;(require 'flymake-cursor)
;;(require 'flymake-ruby)
(require 'linum)
(global-linum-mode)
;;(window-numbering-mode)
;;(require 'imenu+)
;;(require 'ack)
(use-package ack-and-a-half
  :ensure t
  :commands (ack-and-a-half ack-and-a-half-same ack-and-a-half-find-file ack-and-a-half-find-file-same)
  :init
  (progn
    (defalias 'ack 'ack-and-a-half)
    (defalias 'ack-same 'ack-and-a-half-same)
    (defalias 'ack-find-file 'ack-and-a-half-find-file)
    (defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)))

(use-package undo-tree
  :ensure t
  :config
  (progn
    (add-hook 'undo-tree-visualizer-mode-hook 'jmb-disable-show-trailing-whitespace)
    (global-undo-tree-mode)))

(use-package auto-complete
  :ensure t
  :config
  (global-auto-complete-mode t))

;;(require 'desktop-recover)
(prefer-coding-system 'utf-8)

(use-package desktop
  :ensure t
  :init
  (progn
    (desktop-save-mode 1)
    (defun jmb-desktop-save ()
      (interactive)
      ;; Don't call desktop-save-in-desktop-dir, as it prints a message.
      (if (eq (desktop-owner) (emacs-pid))
          (desktop-save desktop-dirname)))
    (add-hook 'auto-save-hook 'jmb-desktop-save)))

(use-package rcov-overlay)
(use-package yari)
;;(require 'yari)
(defun ri-bind-key ()
  (local-set-key [f1] 'yari))

;;(defun turn-on-show-trailing-whitespace ()
;;  (setq show-trailing-whitespace t))

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
;;;(add-hook 'ruby-mode-hook 'minimap-create)

;;(require `fish-mode)

;;(setq exec-path (append exec-path (list (expand-file-name "/usr/local/Cellar/go/1.2/libexec/bin"))))
;;(setq exec-path (append exec-path (list (expand-file-name "~/go_src/bin"))))

(defun jmb/empty-string (str)
  (string= "" str))
(defun jmb/update-env-vars-from-fish ()
   (let ((lines (split-string (shell-command-to-string "fish -c \"set -xUL\"") "\n")))
     (dolist (line lines)
       (let* ((parts (split-string line " "))
              (name (elt parts 0))
              (value (mapconcat 'identity (cl-remove-if 'jmb/empty-string (reverse (butlast (reverse parts)  1))) ":")))
         (if (not (string= "" name)) (setenv name value))))))

;;(require 'rubydb)
;;(require 'one-key-macro)

(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

;;(global-set-key  "\C-c\C-a" 'autotest-switch)


;;(defun try-to-add-imenu ()
;;  (condition-case nil (imenu-add-defs-to-menubar) (error nil)))
;; (add-hook 'font-lock-mode-hook 'try-to-add-imenu)

;;visual-regexp
(use-package visual-regexp
  :ensure t
  :bind (("C-c C-q" . vr/replace)
         ("C-c q" . vr/query-replace)
         ("C-c m" . vr/mc-mark)))

(use-package ansi-color
  :config (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on))

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
;     '(lambda ()
;             (local-set-key [home]        ; move to beginning of line, after prompt
;                            'comint-bol)
;        (local-set-key [up]          ; cycle backward through command history
;                            '(lambda () (interactive)
;                               (if (comint-after-pmark-p)
;                                   (comint-previous-input 1)
;                                 (previous-line 1))))
;        (local-set-key [down]        ; cycle forward through command history
;                            '(lambda () (interactive)
;                               (if (comint-after-pmark-p)
;                                   (comint-next-input 1)
;                                 (forward-line 1))))
;             ))

;;Disable flymake in rspec
;(defun flymake-rspec-init ()
;  ())

;(push '(".+_spec\\.rb$" flymake-rspec-init) flymake-allowed-file-name-masks)

;;;Guard notifications
;(defun guard-notification (type title message image)
;  (message type))



(use-package keychain-environment)

(if (eq system-type "darwin")
    (setq magit-emacsclient-executable "/Applications/Emacs.app/Contents/MacOS/bin/emacsclient"))

;;docker
(use-package dockerfile-mode
  :ensure t)

(use-package uniquify)

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
(use-package flycheck
  :ensure t
  :config (global-flycheck-mode 1))
(use-package flycheck-tip
  :ensure t
  :disabled t
  :bind ("C-c C-n" . flycheck-tip-cycle)
  :config (setq flycheck-tip-avoid-show-func nil))

(use-package popwin
  :defer 3)

(use-package command-log-mode
  :commands command-log-mode
  :ensure t)

(use-package paredit
  :commands (enable-paredit-mode)
  :ensure t
  :init
    (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
    (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
    (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
    (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
    (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
    (add-hook 'scheme-mode-hook           #'enable-paredit-mode))

(eldoc-mode)

(use-package sx
  :ensure t
  :commands (sx-tab-all-questions)
  :config
  (add-hook 'sx-question-mode-hook 'jmb-disable-show-trailing-whitespace))

(org-babel-load-file "~/.emacs.d/setup.org")
(require 'org-protocol)

(setq org-plantuml-jar-path "/usr/local/Cellar/plantuml/8018/plantuml.8018.jar")

;; From http://doc.norang.ca/org-mode.html
(defun bh/display-inline-images ()
  (condition-case nil
      (org-display-inline-images)
    (error nil)))
(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images 'append)

(use-package ace-window
  :ensure t
  :commands (ace-window)
  :config
  (setq aw-keys   '(?a ?s ?d ?f ?j ?k ?l)
        ace-window-display-mode t
        aw-dispatch-always t
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
      ("p" scroll-other-window-dowqn "scroll down"))
    (add-to-list 'aw-dispatch-alist '(?w hydra-window-size/body) t)
    (add-to-list 'aw-dispatch-alist '(?o hydra-window-scroll/body) t)
    (add-to-list 'aw-dispatch-alist '(?\; hydra-window-frame/body) t))
  (ace-window-display-mode t))

(use-package define-word
  :commands (define-word)
  :ensure t)

(use-package restclient
  :commands (restclient)
  :ensure t)

(use-package eww
  :commands (eww)
  :config
  (add-hook 'eww-mode-hook 'jmb-disable-show-trailing-whitespace)
  (defadvice eww-tag-title (after rrix/eww-rename-buffer-ad (cont))
    "Update EWW buffer title with new page load."
    (message eww-current-title)
    (rename-buffer (format "*eww : %s *" eww-current-title) t))
  :ensure t)

(use-package swift-mode
  :mode "\\.swift\\'"
  :config
  (add-to-list 'flycheck-checkers 'swift)
  :ensure t)

(use-package wanderlust
  :disabled t
  :ensure t
  :init
  ;; IMAP
  (setq elmo-imap4-default-server "imap.gmail.com")
  (setq elmo-imap4-default-user "bellegar@gmail.com")
  (setq elmo-imap4-default-authenticate-type 'clear)
  (setq elmo-imap4-default-port '993)
  (setq elmo-imap4-default-stream-type 'ssl)

  (setq elmo-imap4-use-modified-utf7 t)

  ;; SMTP
  (setq wl-smtp-connection-type 'starttls)
  (setq wl-smtp-posting-port 587)
  (setq wl-smtp-authenticate-type "plain")
  (setq wl-smtp-posting-user "bellegar")
  (setq wl-smtp-posting-server "smtp.gmail.com")
  (setq wl-local-domain "gmail.com")

  (setq wl-default-folder "%inbox")
  (setq wl-default-spec "%")
  (setq wl-draft-folder "%[Gmail]/Drafts") ; Gmail IMAP
  (setq wl-trash-folder "%[Gmail]/Trash")

  (setq wl-folder-check-async t)

  (setq elmo-imap4-use-modified-utf7 t)

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
  :commands (wl wl-other-frame wl-draft)
  )


(setq debug-on-error nil)

