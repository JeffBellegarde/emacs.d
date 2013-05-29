(setq jmb-emacs-init-file load-file-name)
(setq jmb-emacs-config-dir
      (file-name-directory jmb-emacs-init-file))
(setq user-emacs-directory jmb-emacs-config-dir)

(setq backup-directory-alist
      (list (cons "." (expand-file-name "backup" user-emacs-directory))))

(add-to-list 'load-path user-emacs-directory)
;;;(add-to-list 'load-path "~/.emacs.d/ruby")
(add-to-list 'load-path "~/src/emacs-flymake")
(add-to-list 'load-path "~/src/autotest.el")
(add-to-list 'load-path "~/src/markdown-mode")

(tool-bar-mode -1)
(require 'mode-compile)
(line-number-mode 1)
(add-hook 'after-init-hook 'server-start)

(setq jmb-emacs-src-dir (expand-file-name "src" user-emacs-directory))
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

(add-to-list 'load-path (expand-file-name "yaml-mode" jmb-emacs-src-dir))
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;;org-mode

(require 'org)
;;
;; Standard key bindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cr" 'org-capture)
(global-set-key "\C-cd" 'deft)

;;;Requires async-shell-command which is only in emacs 23.2.
;;;(add-to-list 'load-path "~/src/bundler.el")
;;;(require 'bundler)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(add-hook 'ibuffer-mode-hook
	  '(lambda ()
	     (ibuffer-auto-mode 1)
	     (ibuffer-switch-to-saved-filter-groups "home")))

(setq ibuffer-saved-filter-groups
      '(("home"
	 ("emacs-config" (or (filename . ".emacs.d")
			     (filename . "emacs")))
	 ("magit" (name . "\*magit")))))

;;(add-to-list 'load-path "~/src/perspective-el")
;;(require 'perspective)
;;(persp-mode)

(add-to-list 'load-path "~/src/eproject")
;;(require 'eproject)
;;(require 'eproject-extras)

;(define-project-type gem (generic)
;  (look-for "Gemfile")
;  :relevant-files ("\.rb$"
;                   "Gemfile$"
;		   "Rakefile"
;                   "\.ru$"
;		   "bin/.*"
;                   "README\..*$")
;  :irrelevant-files ("bundle/.*"
;		     "coverage/.*"
;		     ".git/.*"
;		     "pkg/.*"
;                     ".bin/.*")
;  :main-file "Gemfile")


;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
;(when
;    (load
;     (expand-file-name "~/.emacs.d/elpa/package.el"))
;  (package-initialize))

;;; Use Marmalizde instead of ELPA
(setq package-archives
      '(("melpa" . "http://melpa.milkbox.net/packages/")
	("marmalade" . "http://marmalade-repo.org/packages/")
	("gnu" . "http://elpa.gnu.org/packages/")
	("sunrise-commander" . "http://joseito.republika.pl/sunrise-commander/")))
(require 'package)
(package-initialize)
(setq jmb-required-packages
      (list
       'ack-and-a-half
       'auto-complete
       'dired+
       'dired-details+
       'exec-path-from-shell
       'expand-region
       'git-gutter-fringe
       'ibuffer-vc
       'ido
       'ioccur
       'key-chord
       'loccur
       'magit
       'rfringe
       'ruby-end
       'undo-tree
	    ))
(dolist (package jmb-required-packages)
  (when (not (package-installed-p package))
    (package-refresh-contents)
    (package-install package)))

;;dired
(require 'dired+)
(require 'dired-details+)

;;expand-region
(require 'expand-region)
(global-set-key (kbd "C-@") 'er/expand-region)
(global-set-key (kbd "C-#") 'er/contract-region)
(global-set-key (kbd "C-&") 'shrink-window-horizontally)
(global-set-key (kbd "C-*") 'enlarge-window-horizontally)
(global-set-key (kbd "C-(") 'shrink-window)
(global-set-key (kbd "C-)") 'enlarge-window)

;;git-gutter
(require 'git-gutter-fringe)

;;ace-jump-mode
(global-set-key (kbd "C-0") 'ace-jump-mode)

;; key-chord
(require 'key-chord)
(key-chord-mode 1)
;;(key-chord-define-global "q["     "\C-u5\C-x{")
;;(key-chord-define-global "q]"     "\C-u5\C-x}")

(require 'ibuffer-vc)
(require 'ruby-end)

(require 'exec-path-from-shell)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(add-hook 'ibuffer-hook
     (lambda ()
       (ibuffer-vc-set-filter-groups-by-vc-root)
       (ibuffer-do-sort-by-alphabetic)))
(require 'ido)
(global-auto-revert-mode t)
(require 'magit)
(global-set-key "\C-ci" 'magit-status)
(require 'rfringe)
(require 'flymake-cursor)
(require 'flymake-ruby)
(require 'linum)
(global-linum-mode)
(require 'window-numbering)
(require 'imenu+)
;;(require 'ack)
(require 'ack-and-a-half)
(defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)
(defalias 'ack-find-file 'ack-and-a-half-find-file)
(defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)

(load "undo-tree-setup")

(require 'auto-complete)
(global-auto-complete-mode t)

;;(require 'desktop-recover)
(prefer-coding-system 'utf-8)
(require 'auto-save-desktop)
(require 'unit-test)
;;(require 'autotest)
(require 'rcov-overlay)
(require 'yari)
(defun ri-bind-key ()
  (local-set-key [f1] 'yari))

;;(autoload 'ruby-mode "~/,emacs.d/ruby/ruby-mode" "Major mode for ruby files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))

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


(defun minimap-toggle ()
  "Toggle the minimap."
  (interactive)
  (if (and minimap-window
           (window-live-p minimap-window))
      (minimap-kill)
    (minimap-create)))

(global-set-key "\C-cm" 'minimap-toggle)

(require 'rubydb)
(require 'one-key-macro)

(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

;;(global-set-key  "\C-c\C-a" 'autotest-switch)

(require 'ioccur)
(require 'loccur)
;; defines shortcut for loccur of the current word
(define-key global-map [(control o)] 'loccur-current)
;; defines shortcut for the interactive loccur command
;;(define-key global-map [(control meta o)] 'loccur)
;; defines shortcut for the loccur of the previously found word
(define-key global-map [(control shift o)] 'loccur-previous-match)

(defun rspec-outline-occur ()
  (interactive)
  (loccur "^[[:space:]]*it[[:space:]].*do\\|{$\\|^[[:space:]]*describe[[:space:]].*do$\\|^[[:space:]]*context.*do$"))

(defcustom outline-occur-by-mode nil
  ""
  :group 'outline-loccur
  :type
  '(repeat
    (cons :tag "Outline rule" (symbol :tag "Major mode") (string :tag "Regexp")
    )))

(defun outline-occur ()
  (interactive)
  (loccur (cdr (assoc 'ruby-mode outline-occur-by-mode))))

(define-key global-map [(control meta o)] 'rspec-outline-occur)
;(define-key global-map [(control meta o)] 'outline-occur)

(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
(setq auto-mode-alist (cons '("\\.text" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.md" . gfm-mode) auto-mode-alist))

;;(defun try-to-add-imenu ()
;;  (condition-case nil (imenu-add-defs-to-menubar) (error nil)))
;; (add-hook 'font-lock-mode-hook 'try-to-add-imenu)

(if (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file))

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
;	  '(lambda ()
;             (local-set-key [home]        ; move to beginning of line, after prompt
;                            'comint-bol)
;	     (local-set-key [up]          ; cycle backward through command history
;                            '(lambda () (interactive)
;                               (if (comint-after-pmark-p)
;                                   (comint-previous-input 1)
;                                 (previous-line 1))))
;	     (local-set-key [down]        ; cycle forward through command history
;                            '(lambda () (interactive)
;                               (if (comint-after-pmark-p)
;                                   (comint-next-input 1)
;                                 (forward-line 1))))
;             ))

;;Disable flymake in rspec
(defun flymake-rspec-init ()
  ())

(push '(".+_spec\\.rb$" flymake-rspec-init) flymake-allowed-file-name-masks)

;;;Guard notifications
(defun guard-notification (type title message image)
  (message type))

(setq jmb-disabled-whitespace-mode-hooks
      (list 'magit-mode-hook 'undo-tree-visualizer-mode-hook 'yari-mode-hook 'gud-mode-hook 'shell-mode-hook))

(defun jmb-disable-show-trailing-whitespace ()
  (setq show-trailing-whitespace nil))

(dolist (hook jmb-disabled-whitespace-mode-hooks)
  (add-hook hook 'jmb-disable-show-trailing-whitespace))

(require 'keychain-environment)
;; Set up 'custom' system
(setq custom-file (expand-file-name "emacs-customizations.el" jmb-emacs-config-dir))
(load custom-file)


