(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(abbrev-mode t t)
 '(ack-and-a-half-arguments (quote ("--ignore-dir bundle" "--ignore-dir vendor")))
 '(ack-and-a-half-executable "ack")
 '(ack-and-a-half-prompt-for-directory t)
 '(ansi-color-for-comint-mode t)
 '(auth-source-save-behavior t)
 '(auth-sources
   (quote
    ("~/.authinfo.gpg" macos-keychain-internet "~/.netrc")))
 '(auto-revert-verbose nil)
 '(autotest-use-ui t)
 '(beacon-blink-when-focused t)
 '(beacon-blink-when-window-scrolls nil)
 '(bm-buffer-persistence t)
 '(bm-cycle-all-buffers t)
 '(bm-in-lifo-order t)
 '(browse-kill-ring-highlight-current-entry t)
 '(browse-kill-ring-highlight-inserted-item (quote solid))
 '(browse-url-new-window-flag t)
 '(canlock-password "0d77a95b65d4b5720f2886989a80711c9dd00fb0")
 '(circe-server-killed-confirmation nil)
 '(circe-server-max-reconnect-attempts 1)
 '(comint-prompt-read-only t)
 '(comint-scroll-to-bottom-on-input t)
 '(command-log-mode-is-global t)
 '(company-quickhelp-delay 1)
 '(company-quickhelp-max-lines 10)
 '(cua-enable-modeline-indications t)
 '(custom-enabled-themes nil)
 '(custom-safe-themes t)
 '(custom-unlispify-tag-names nil)
 '(deft-extension "org")
 '(deft-text-mode (quote org-mode))
 '(desktop-load-locked-desktop t)
 '(desktop-restore-eager 10)
 '(dired-auto-revert-buffer t)
 '(dired-details-hide-link-targets nil)
 '(dired-use-ls-dired nil)
 '(ediff-keep-variants nil)
 '(ensime-graphical-tooltips t)
 '(eproject-completing-read-function (quote eproject--ido-completing-read))
 '(exec-path
   (quote
    ("/usr/bin" "/bin" "/usr/sbin" "/sbin" "/Applications/Emacs.app/Contents/MacOS/libexec" "/Applications/Emacs.app/Contents/MacOS/bin" "/usr/local/bin" "/usr/local/Cellar/go/1.2/libexec/bin" "/Users/Bellegarde/go_src/bin")))
 '(flycheck-completion-system (quote ido))
 '(flycheck-disabled-checkers (quote (emacs-lisp-checkdoc)))
 '(flycheck-python-flake8-executable "python3")
 '(flycheck-python-pycompile-executable "python3")
 '(flymake-gui-warnings-enabled nil)
 '(flymake-run-in-place nil)
 '(flymake-start-on-flymake-mode nil)
 '(flymake-start-syntax-check-on-find-file nil)
 '(flymake-start-syntax-check-on-newline nil)
 '(fringe-mode (quote (nil . 0)) nil (fringe))
 '(gh-profile-alist
   (quote
    (("github" :url "https://api.github.com" :remote-regexp "^\\(?:git@github\\.com:\\|\\(?:git\\|https?\\|ssh\\)://.*@?github\\.com/\\)\\(.*\\)/\\(.*\\)\\(?:\\.git\\)?"))))
 '(git-gutter:verbosity 2)
 '(git-messenger:show-detail t t)
 '(global-auto-revert-mode t)
 '(global-auto-revert-non-file-buffers t)
 '(global-git-gutter-mode t)
 '(global-whitespace-newline-mode nil)
 '(gnus-always-force-window-configuration t)
 '(gnus-always-read-dribble-file t)
 '(gnus-asynchronous t)
 '(gnus-group-line-format "%M%S%p%P%5y:%B%(%G%)
")
 '(gnus-group-mode-line-format "Gnus: %%b")
 '(gnus-inhibit-startup-message t)
 '(gnus-large-newsgroup 50)
 '(gnus-save-duplicate-list t)
 '(gnus-summary-ignore-duplicates t)
 '(gnus-summary-line-format "%U%R%z%I%(%[%4L: %-23,23f%]%) %4d %s
")
 '(gnus-suppress-duplicates t)
 '(gnus-topic-display-empty-topics nil)
 '(gnus-treat-from-gravatar (quote head))
 '(gnus-treat-mail-gravatar (quote head))
 '(gnus-use-adaptive-scoring (quote (word line)))
 '(gnus-use-cache t)
 '(gnus-use-header-prefetch t)
 '(go-oracle-command "/Users/Bellegarde/go_src/bin/oracle")
 '(helm-semantic-fuzzy-match t)
 '(helm-time-zone-home-location "Seattle")
 '(ibuffer-expert t)
 '(ibuffer-filter-format-alist nil)
 '(ibuffer-formats
   (quote
    ((mark modified read-only vc-status-mini " "
           (name 18 18 :left :elide)
           " "
           (size 9 -1 :right)
           " "
           (mode 16 16 :left :elide)
           " " filename-and-process)
     (mark " "
           (name 16 -1)
           " " filename))))
 '(ibuffer-saved-filter-groups
   (quote
    (("home"
      ("emacs-config"
       (or
        (filename . ".emacs.d")
        (filename . "emacs")))
      ("magit"
       (name . "*magit"))))))
 '(ibuffer-saved-filters
   (quote
    (("gnus"
      ((or
        (mode . message-mode)
        (mode . mail-mode)
        (mode . gnus-group-mode)
        (mode . gnus-summary-mode)
        (mode . gnus-article-mode))))
     ("programming"
      ((or
        (mode . emacs-lisp-mode)
        (mode . cperl-mode)
        (mode . c-mode)
        (mode . java-mode)
        (mode . idl-mode)
        (mode . ruby-mode)
        (mode . lisp-mode)))))))
 '(ibuffer-show-empty-filter-groups nil)
 '(ido-read-file-name-non-ido (quote (find-dired)))
 '(imenu-auto-rescan t)
 '(imenu-sort-function nil)
 '(indent-tabs-mode nil)
 '(indicate-buffer-boundaries nil)
 '(indicate-empty-lines nil)
 '(inhibit-startup-echo-area-message "Bellegarde")
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(kill-whole-line t)
 '(load-prefer-newer t)
 '(magit-default-tracking-name-function (quote magit-default-tracking-name-branch-only))
 '(magit-diff-refine-hunk t)
 '(magit-expand-staged-on-commit (quote full))
 '(magit-log-arguments (quote ("--graph" "--color" "--decorate")))
 '(magit-process-connection-type nil)
 '(magit-process-popup-time 5)
 '(magit-remote-ref-format (quote remote-slash-branch))
 '(magit-repo-dirs
   (quote
    ("/Users/Bellegarde/src" "/Users/Bellegarde/.emacs.d")))
 '(magit-repository-directories (quote ("~/src")) t)
 '(magit-revert-item-confirm nil)
 '(magit-save-some-buffers (quote dontask))
 '(magit-set-upstream-on-push (quote dontask))
 '(magit-sha1-abbrev-length 5)
 '(magit-stage-all-confirm nil)
 '(make-backup-files nil)
 '(minimap-window-location (quote right))
 '(ns-tool-bar-display-mode (quote both) t)
 '(ns-tool-bar-size-mode nil t)
 '(org-agenda-dim-blocked-tasks t)
 '(org-agenda-files
   (quote
    ("~/.deft/work.org" "~/.deft/ent.org" "~/.deft/notes.org")))
 '(org-agenda-todo-list-sublevels nil)
 '(org-babel-load-languages
   (quote
    ((emacs-lisp . t)
     (dot . t)
     (plantuml . t)
     (restclient . t))))
 '(org-capture-templates
   (quote
    (("w" "Default template" entry
      (file+headline "~/.deft/notes.org" "Urls")
      "** TODO %u %? %c
%i
" :empty-lines 1))))
 '(org-clock-clocked-in-display (quote frame-title))
 '(org-clock-idle-time 10)
 '(org-clock-out-remove-zero-time-clocks t)
 '(org-clock-persist t)
 '(org-confirm-babel-evaluate nil)
 '(org-default-notes-file "~/.deft/notes.org")
 '(org-directory "~/.deft")
 '(org-fontify-done-headline t)
 '(org-log-into-drawer t)
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-docview org-gnus org-info org-irc org-mhe org-rmail org-w3m org-mouse)))
 '(org-pretty-entities t)
 '(org-pretty-entities-include-sub-superscripts nil)
 '(org-refile-targets (quote ((org-agenda-files :level . 1))))
 '(org-replace-disputed-keys t)
 '(org-return-follows-link t)
 '(org-src-fontify-natively t)
 '(org-src-window-setup (quote current-window))
 '(org-startup-with-inline-images t)
 '(org-use-speed-commands t)
 '(outline-occur-by-mode
   (quote
    ((ruby-mode . "^[[:space:]]*it[[:space:]].*do\\\\|{$\\\\|^[[:space:]]*describe[[:space:]].*do$\\\\|^[[:space:]]*context.*do$\\\\|^[[:space:]]*module\\\\|^[[:space:]]*class\\\\|^[[:space:]]*def"))))
 '(package-selected-packages
   (quote
    (solarized-theme color-theme-solarized company-jedi racer rust-mode cargo company-lsp pipenv pyvenv pyenv lsp-python lsp-ui lsp-mode ghub magithub better-shell highlight-indent-guides schrute atomic-chrome overseer ace-link smart-mode-line circe alert popwin flycheck request ace-window avy gh magit helm-bm bm general zenburn-theme yari which-key wanderlust visual-regexp use-package-chords undo-tree typit travis tle sx swift-mode super-save sr-speedbar smart-mode-line-powerline-theme slack sicp scala-mode2 ruby-end rfringe projectile popup-imenu paredit paradox ob-restclient multiple-cursors mode-compile minimap magit-gh-pulls loccur lispy keyfreq keychain-environment ioccur impatient-mode ido-vertical-mode idle-highlight-mode ibuffer-vc hungry-delete helm-swoop helm-describe-modes helm-descbinds helm-company helm-ag guide-key god-mode go-errcheck go-eldoc go-autocomplete gitignore-mode github-notifier gitconfig-mode git-messenger gist fuzzy fringe-helper fold-this flycheck-tip flycheck-cask fish-mode facemenu+ eyedropper expand-region exec-path-from-shell esup ensime engine-mode emacs-eclim elm-mode elfeed-org el-mock edit-server dockerfile-mode docker dired-details+ dired+ diff-hl deft define-word dash-at-point dark-souls company-quickhelp command-log-mode cider browse-kill-ring beacon apples-mode aggressive-indent ack-and-a-half ace-jump-mode ac-ispell 2048-game)))
 '(paradox-automatically-star nil)
 '(paradox-execute-asynchronously t)
 '(paradox-github-token t)
 '(pipenv-executable "/Users/Bellegarde/Library/Python/3.6/bin/pipenv")
 '(pomodoro-work-start-message "Back to work!")
 '(projectile-completion-system (quote helm))
 '(projectile-global-mode t)
 '(projectile-mode-line nil)
 '(python-shell-interpreter "python3")
 '(racer-cmd "/Users/Bellegarde/.cargo/bin/racer" t)
 '(racer-rust-src-path
   "/Users/Bellegarde/.rustup/toolchains/stable-x86_64-apple-darwin/lib/rustlib/src/rust/src" t)
 '(rdebug-short-key-mode t)
 '(rdebug-track-do-tracking-p t)
 '(remember-annotation-functions (quote (org-remember-annotation)))
 '(remember-data-file "~/.deft/notes.org")
 '(remember-handler-functions (quote (org-remember-handler)))
 '(remember-mode-hook
   (quote
    (flyspell-mode turn-on-auto-fill org-remember-apply-template)))
 '(rm-blacklist
   (quote
    (" hl-p" " Undo-Tree" " Helm" " Guide" " Abbrev" " MRev" " GitGutter" " Projectile" " AC")))
 '(rspec-spec-command "rspec")
 '(rspec-use-rake-flag nil)
 '(safe-local-variable-values
   (quote
    ((org-use-property-inheritance . t)
     (orgstruct-heading-prefix-regexp . "^;;; +")
     (encoding . utf-8)
     (ruby-compilation-executable . "ruby")
     (ruby-compilation-executable . "ruby1.8")
     (ruby-compilation-executable . "ruby1.9")
     (ruby-compilation-executable . "rbx")
     (ruby-compilation-executable . "jruby"))))
 '(scroll-bar-mode (quote right))
 '(send-mail-function (quote smtpmail-send-it))
 '(shell-file-name "/usr/local/bin/fish")
 '(show-paren-mode t)
 '(show-paren-style (quote mixed))
 '(show-trailing-whitespace nil)
 '(shr-table-corner 43)
 '(sml/replacer-regexp-list
   (quote
    (("^~/org" ":Org:")
     ("^/sudo:.*:" ":SU:")
     ("^~/Documents/" ":Doc:")
     ("^~/Dropbox/" ":DB:")
     ("^:\\([^:]*\\):Documento?s/" ":\\1/Doc:")
     ("^~/[Gg]it/" ":Git:")
     ("^~/[Gg]it[Hh]ub/" ":Git:")
     ("^~/[Gg]it\\([Hh]ub\\|\\)-?[Pp]rojects/" ":Git:"))))
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 587)
 '(solarized-high-contrast-mode-line nil)
 '(speedbar-default-position (quote left))
 '(speedbar-use-images t)
 '(sr-speedbar-right-side nil)
 '(tab-width 2)
 '(tool-bar-mode nil)
 '(tramp-auto-save-directory "~/.tramp_tmp_dir" nil (tramp))
 '(undo-tree-visualizer-diff nil)
 '(undo-tree-visualizer-relative-timestamps t)
 '(undo-tree-visualizer-timestamps nil)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(uniquify-ignore-buffers-re "^\\\\*")
 '(uniquify-separator "/")
 '(use-package-verbose t)
 '(visual-line-mode nil t))
'(tool-bar-mode nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Monaco"))))
 '(aw-mode-line-face ((t (:foreground "red"))))
 '(bm-persistent-face ((t (:underline "#5F7F5F"))))
 '(cider-result-overlay-face ((t (:foreground "dark gray"))))
 '(diff-hl-change ((t (:background "#6CA0A3" :foreground "#6CA0A3"))))
 '(diff-hl-delete ((t (:background "#DCA3A3" :foreground "#DCA3A3"))))
 '(diff-hl-insert ((t (:background "#8FB28F" :foreground "#8FB28F"))))
 '(diredp-dir-priv ((t (:foreground "#7474FFFFFFFF"))))
 '(hydra-face-amaranth ((t (:foreground "magenta1" :weight bold))))
 '(hydra-face-red ((t (:foreground "brown3" :weight bold))))
 '(ido-first-match ((t (:foreground "cyan" :weight bold))))
 '(message-header-subject ((t (:foreground "#DFAF8F" :weight bold :height 2.0))))
 '(region ((t (:background "DarkOrchid4")))))
