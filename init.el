(defvar jmb-emacs-init-file load-file-name)
(defvar jmb-emacs-config-dir
      (file-name-directory jmb-emacs-init-file))
(defvar user-emacs-directory jmb-emacs-config-dir)
(defvar jmb-disabled-whitespace-mode-hooks
      (list 'magit-mode-hook 'yari-mode-hook 'gud-mode-hook 'shell-mode-hook 'pry))
(defun jmb-disable-show-trailing-whitespace ()
  (setq show-trailing-whitespace nil))

(dolist (hook jmb-disabled-whitespace-mode-hooks)
  (add-hook hook 'jmb-disable-show-trailing-whitespace))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
;;;(add-to-list 'load-path "~/.emacs.d/ruby")
;;(add-to-list 'load-path "~/src/emacs-Flymake")
;(add-to-list 'load-path "~/src/autotest.el")
;(add-to-list 'load-path "~/src/markdown-mode")

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
(setq package-archives
      '(("melpa" . "http://melpa.milkbox.net/packages/")
        ("gnu" . "http://elpa.gnu.org/packages/")
        ("sunrise-commander" . "http://joseito.republika.pl/sunrise-commander/")))
(package-initialize)
(defvar jmb-required-packages
      (list
       'ace-window
       'ack-and-a-half
       'apples-mode
       'auto-complete
       'dired+
       'dired-details+
;;       'deft
;;       'dockerfile-mode
       'ensime
       'exec-path-from-shell
;;       'expand-region
       'fish-mode
       'flx-ido
       'flycheck
       'git-gutter-fringe
       'gitconfig-mode
       'gitignore-mode
       'gist
       'go-autocomplete
       'go-eldoc
       'go-errcheck
       'go-mode
       'guide-key
       'hungry-delete
;;       'hydra
       'ibuffer-vc
       'ido
       'ido-vertical-mode
       'idle-highlight-mode
       'impatient-mode
       'key-chord
;;       'magit
;;       'markdown-mode
;;       'minimap
       ;;       'mode-compile
       'ox-reveal
       'paradox
       'projectile
       'rfringe
       'ruby-end
;;       'smart-mode-line
       'smex
;;       'undo-tree
       'use-package
;;       'visual-regexp
       'yasnippet
       'zenburn-theme
      ))
(dolist (package jmb-required-packages)
  (when (not (package-installed-p package))
    (package-refresh-contents)
    (package-install package)))
(require 'use-package)

(defvar jmb-emacs-src-dir (expand-file-name "src" user-emacs-directory))
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


;;org-mode
(require 'org)
;;
;; Standard key bindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cr" 'org-capture)

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

;;dired
(require 'dired+)
(require 'dired-details+)

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
(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

;; gomode
(setenv "GOPATH" "/Users/Bellegarde/go_src")
(setq exec-path (append exec-path '("/Users/Bellegarde/go_src/bin")))
(require 'go-mode)
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

;;(require 'golint)

(require 'auto-complete-config)
(require 'go-autocomplete)

;;git-gutter
(require 'git-gutter-fringe)

;; guide-key
(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-c p" "C-c C-o" "C-c h"))
(guide-key-mode 1)  ; Enable guide-key-mode

(load "idle-highlight-setup")
(load "emacs-pry-setup")

;;smex
;(global-set-key (kbd "M-x") 'smex)
;(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
;(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; key-chord
(require 'key-chord)
(key-chord-mode 1)
;;(key-chord-define-global "q["     "\C-u5\C-x{")
;;(key-chord-define-global "q]"     "\C-u5\C-x}")

(require 'ibuffer-vc)
(require 'ruby-end)

(require 'exec-path-from-shell)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH"))

(add-hook 'ibuffer-hook
     (lambda ()
       (ibuffer-vc-set-filter-groups-by-vc-root)
       (ibuffer-do-sort-by-alphabetic)))
;ido
;;(require 'ido)
;;(require 'ido-vertical-mode)
;;(ido-vertical-mode 1)
;;(flx-ido-mode 1)


(require 'hungry-delete)
(global-hungry-delete-mode)

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
(require 'ack-and-a-half)
(defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)
(defalias 'ack-find-file 'ack-and-a-half-find-file)
(defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)

(use-package undo-tree
;  :bind (("C-x u" . undo-tree-vizualize)
;         ("C-z" . undo-tree-undo))
  :init (global-undo-tree-mode)
  :config (add-hook 'undo-tree-visualizer-mode-hook 'jmb-disable-show-trailing-whitespace))


(require 'auto-complete)
(global-auto-complete-mode t)

;;(require 'desktop-recover)
(prefer-coding-system 'utf-8)
;;(require 'auto-save-desktop)
;;(require 'unit-test)
;;(require 'autotest)
;;(require 'rcov-overlay)
;;(require 'yari)
(defun ri-bind-key ()
  (local-set-key [f1] 'yari))

(defun turn-on-show-trailing-whitespace ()
  (setq show-trailing-whitespace t))

;;(autoload 'ruby-mode "~/,emacs.d/ruby/ruby-mode" "Major mode for ruby files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
(add-hook 'ruby-mode-hook 'turn-on-show-trailing-whitespace)

(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.thor$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Thorfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Berksfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("buildfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))

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

(require 'ansi-color)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

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



;;(require 'keychain-environment)

(if (eq system-type "darwin")
    (setq magit-emacsclient-executable "/Applications/Emacs.app/Contents/MacOS/bin/emacsclient"))

;;docker
(use-package dockerfile-mode
  :ensure t)

(require 'uniquify)


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

(org-babel-load-file "~/.emacs.d/setup.org")
