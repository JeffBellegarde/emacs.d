;; * Setup
;; We need the use-package macro to be defined to avoid a bunch of errors from flycheck.

(eval-when-compile ;;Load use-package only when needed
  (require 'use-package))


;; Load private vars. Not to be checked in.
(load "~/private.el")
;; * Top heading
;; ** Disable show trailing whitespace.
;; I show whitespace by default but need to turn it off in some modes.
;; Orignally i kept a list in but now I use
;;   (add-hook <hook-name> jmb-disable-show-trailing-whitespace)
;; in appropriate use-package statements.

(defvar jmb-disabled-whitespace-mode-hooks
      (list 'magit-mode-hook 'yari-mode-hook 'gud-mode-hook 'shell-mode-hook 'pry 'info-mode))

(defun jmb-disable-show-trailing-whitespace ()
  (interactive)
  (setq show-trailing-whitespace nil))

(dolist (hook jmb-disabled-whitespace-mode-hooks)
  (add-hook hook 'jmb-disable-show-trailing-whitespace))


;; ** Chords for use-package
;; Enables key chords
(use-package use-package-chords
  :ensure t
  :config (key-chord-mode 1))

;; ** restclient
(use-package restclient
  :commands (restclient))


;; ** org
(use-package org
   :bind (
          ("C-c l" . org-store-link)
          ("C-c a" . org-agenda)
          ("C-c b" . org-iswitchb)
          ("C-c r" . org-capture))
   :config (org-clock-persistence-insinuate)
   :ensure t)

;; *** ob-restclient
;; Alows restclient in org-mode.
;; Use my local copy until [[https://github.com/alf/ob-restclient.el/pull/5]] is closed.
(use-package ob-restclient
  :load-path "~/src/ob-restclient.el"
  :ensure t)

;; ** Deft
(use-package deft
  :ensure t
  :bind ("C-c d" . deft))

;; ** sr-speedbar
(use-package sr-speedbar
  :commands (sr-speedbar-toggle)
  :ensure t)

(use-package dired
  :commands (dired)
  :defer t
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





;; ** expand-region
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
    (load-file "$GOPATH/src/code.google.com/p/go.tools/cmd/oracle/oracle.el")
    ))

;; ** Unorganized stuff

;;(require 'golint)
(use-package auto-complete
  :ensure t
  :config
  (global-auto-complete-mode t))

;;(require 'auto-complete-config) ;don't seem to need this

;;What can I use to defer this?
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

;; key-chord -- Automaticall brought in by use-package-chords
(use-package key-chord
  :ensure t
  :disabled t
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

;; ** Magit
(use-package magit
  :ensure t
  :bind ("C-c i" . magit-status)
  :defines (magit-emacsclient-executable)
  :config (if (eq system-type "darwin")
              (setq magit-emacsclient-executable "/Applications/Emacs.app/Contents/MacOS/bin/emacsclient")))

;; ** Git messenger
(use-package git-messenger
  :ensure t
  :bind ("C-x v p" . git-messenger:popup-message)
  :config
	  (setq git-messenger:show-detail t)
  ;;;;Where does magit-commit-mode come from?
  ;;(add-hook 'git-messenger:popup-buffer-hook 'magit-commit-mode)
    )

;(require 'rfringe)
;;(require 'flymake-cursor)
;;(require 'flymake-ruby)
;; ** Linum
(require 'linum)
(global-linum-mode)
;;(window-numbering-mode)
;;(require 'imenu+)
;;(require 'ack)

;; ** Ack
(use-package ack-and-a-half
  :ensure t
  :commands (ack-and-a-half ack-and-a-half-same ack-and-a-half-find-file ack-and-a-half-find-file-same)
  :init
  (progn
    (defalias 'ack 'ack-and-a-half)
    (defalias 'ack-same 'ack-and-a-half-same)
    (defalias 'ack-find-file 'ack-and-a-half-find-file)
    (defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)))

;; ** Undo Tree
(use-package undo-tree
  :ensure t
  :config
  (add-hook 'undo-tree-visualizer-mode-hook 'jmb-disable-show-trailing-whitespace)
  (global-undo-tree-mode))

(prefer-coding-system 'utf-8)

;; ** Desktop
(use-package desktop
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

;; ** ace-window
;; Still not in the habit of using it.
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
  (add-hook 'eww-mode-hook 'jmb-disable-show-trailing-whitespace)
  (defadvice eww-tag-title (after rrix/eww-rename-buffer-ad (cont))
    "Update EWW buffer title with new page load."
    (message eww-current-title)
    (rename-buffer (format "*eww : %s *" eww-current-title) t))
  :ensure t)

;; ** Swift -- for editing .swift
;; *** TODO Autocompletion https://github.com/nathankot/company-sourcekit
(use-package swift-mode
  :mode "\\.swift\\'"
  :config
  (add-to-list 'flycheck-checkers 'swift)
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


;;docker
(use-package docker)

(use-package dockerfile-mode
  :ensure t)

(use-package docker-tramp)

;;(use-package uniquify)

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
  :config
  (global-flycheck-mode 1)
  (setq flycheck-emacs-lisp-load-path  'inherit))
(add-to-list 'flycheck-disabled-checkers 'emacs-lisp-checkdoc)

(use-package flycheck-tip
  :ensure t
  :disabled t
  :bind ("C-c C-n" . flycheck-tip-cycle)
  :config (setq flycheck-tip-avoid-show-func nil))

(use-package popwin)

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


(require 'org-protocol)

(setq org-plantuml-jar-path "/usr/local/Cellar/plantuml/8018/plantuml.8018.jar")

(use-package orgstruct-mode
  :ensure nil
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
  (setq mu4e-html2text-command 'mu4e-shr2text)
  (add-hook 'mu4e-headers-mode-hook 'jmb-disable-show-trailing-whitespace))
(add-hook 'mu4e-view-mode-hook 'jmb-disable-show-trailing-whitespace)

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


(defun youtube-dl ()
  (interactive)
  (let* ((str (current-kill 0))
         (default-directory "~/Downloads")
         (proc (get-buffer-process (ansi-term "/bin/bash"))))
    (term-send-string
     proc
     (concat "cd ~/Downloads && youtube-dl " str "\n"))))

