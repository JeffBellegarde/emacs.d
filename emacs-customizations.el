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
 '(auth-source-save-behavior nil)
 '(autotest-use-ui t)
 '(beacon-blink-when-focused t)
 '(beacon-blink-when-window-scrolls nil)
 '(browse-kill-ring-highlight-current-entry t)
 '(browse-kill-ring-highlight-inserted-item (quote solid))
 '(browse-url-new-window-flag t)
 '(circe-server-killed-confirmation nil)
 '(circe-server-max-reconnect-attempts 1)
 '(comint-prompt-read-only t)
 '(comint-scroll-to-bottom-on-input t)
 '(command-log-mode-is-global t)
 '(cua-enable-modeline-indications t)
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes
   (quote
    ("68d36308fc6e7395f7e6355f92c1dd9029c7a672cbecf8048e2933a053cf27e6" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "85c59044bd46f4a0deedc8315ffe23aa46d2a967a81750360fb8600b53519b8a" "f5eb916f6bd4e743206913e6f28051249de8ccfd070eae47b5bde31ee813d55f" "11636897679ca534f0dec6f5e3cb12f28bf217a527755f6b9e744bd240ed47e1" "a444b2e10bedc64e4c7f312a737271f9a2f2542c67caa13b04d525196562bf38" "2e5705ad7ee6cfd6ab5ce81e711c526ac22abed90b852ffaf0b316aa7864b11f" "3dafeadb813a33031848dfebfa0928e37e7a3c18efefa10f3e9f48d1993598d3" "6a9606327ecca6e772fba6ef46137d129e6d1888dcfc65d0b9b27a7a00a4af20" "e80932ca56b0f109f8545576531d3fc79487ca35a9a9693b62bf30d6d08c9aaf" "7c89d1df5a1dd624983f6d107aced89a4b3d787b20997e5c6cff30cc1ba1b55d" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "e56f1b1c1daec5dbddc50abd00fcd00f6ce4079f4a7f66052cf16d96412a09a9" "cbef37d6304f12fb789f5d80c2b75ea01465e41073c30341dc84c6c0d1eb611d" "c5a044ba03d43a725bd79700087dea813abcb6beb6be08c7eb3303ed90782482" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" "9dae95cdbed1505d45322ef8b5aa90ccb6cb59e0ff26fef0b8f411dfc416c552" "146d24de1bb61ddfa64062c29b5ff57065552a7c4019bee5d869e938782dfc2a" "9f443833deb3412a34d2d2c912247349d4bd1b09e0f5eaba11a3ea7872892000" "4dacec7215677e4a258e4529fac06e0231f7cdd54e981d013d0d0ae0af63b0c8" "36a309985a0f9ed1a0c3a69625802f87dee940767c9e200b89cdebdb737e5b29" "2c728316cf424224af84ecfaacaa70cf8532cf15ed9d31c915ac65913cd7df83" "bf7ed640479049f1d74319ed004a9821072c1d9331bc1147e01d22748c18ebdf" "71b172ea4aad108801421cc5251edb6c792f3adbaecfa1c52e94e3d99634dee7" "14bb0cc73f3ddd0509a7c0bb610b057aef0c0eedb098100f1049e4e5ea108150" default)))
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
 '(explicit-shell-file-name "/bin/bash")
 '(flycheck-completion-system (quote ido))
 '(flycheck-disabled-checkers (quote (emacs-lisp-checkdoc)))
 '(flymake-gui-warnings-enabled nil)
 '(flymake-run-in-place nil)
 '(flymake-start-syntax-check-on-find-file nil)
 '(flymake-start-syntax-check-on-newline nil)
 '(fringe-mode (quote (nil . 0)) nil (fringe))
 '(gh-profile-alist
   (quote
    (("github" :url "https://api.github.com" :remote-regexp "^\\(?:git@github\\.com:\\|\\(?:git\\|https?\\|ssh\\)://.*@?github\\.com/\\)\\(.*\\)/\\(.*\\)\\(?:\\.git\\)?"))))
 '(gh-profile-default-profile "wp-github")
 '(git-gutter:verbosity 2)
 '(global-auto-revert-mode t)
 '(global-git-gutter-mode t)
 '(global-linum-mode t)
 '(global-whitespace-newline-mode nil)
 '(gnus-always-force-window-configuration t)
 '(gnus-asynchronous t)
 '(gnus-inhibit-startup-message t)
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
 '(ido-everywhere t)
 '(ido-mode (quote both) nil (ido))
 '(ido-read-file-name-non-ido (quote (find-dired)))
 '(ido-rotate-file-list-default t)
 '(ido-show-dot-for-dired t)
 '(ido-use-filename-at-point (quote guess))
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
 '(magit-log-arguments
   (quote
    ("--graph" "--color" "--decorate" "--simplify-by-decoration")))
 '(magit-process-connection-type nil)
 '(magit-process-popup-time 5)
 '(magit-remote-ref-format (quote remote-slash-branch))
 '(magit-repo-dirs
   (quote
    ("/Users/Bellegarde/src" "/Users/Bellegarde/.emacs.d")))
 '(magit-revert-item-confirm nil)
 '(magit-save-some-buffers (quote dontask))
 '(magit-set-upstream-on-push (quote dontask))
 '(magit-sha1-abbrev-length 5)
 '(magit-stage-all-confirm nil)
 '(make-backup-files nil)
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
     (sh . t)
     (dot . t)
     (plantuml . t)
     (restclient . t))))
 '(org-capture-templates
   (quote
    (("w" "Default template" entry
      (file+headline "~/.deft/notes.org" "Urls")
      "** TODO %?%c
%u
%i" :empty-lines 1))))
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
 '(paradox-execute-asynchronously t)
 '(paradox-github-token t)
 '(pomodoro-work-start-message "Back to work!")
 '(projectile-completion-system (quote helm))
 '(projectile-global-mode t)
 '(projectile-mode-line nil)
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
 '(show-trailing-whitespace t)
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
 '(speedbar-use-images nil)
 '(sr-speedbar-right-side nil)
 '(tab-width 2)
 '(tramp-auto-save-directory "~/.tramp_tmp_dir")
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
 '(diredp-dir-priv ((t (:foreground "#7474FFFFFFFF"))))
 '(hydra-face-amaranth ((t (:foreground "magenta1" :weight bold))))
 '(hydra-face-red ((t (:foreground "brown3" :weight bold))))
 '(ido-first-match ((t (:foreground "cyan" :weight bold))))
 '(message-header-subject ((t (:foreground "#DFAF8F" :weight bold :height 2.0)))))
