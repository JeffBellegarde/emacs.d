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
 '(autotest-use-ui t)
 '(comint-prompt-read-only t)
 '(comint-scroll-to-bottom-on-input t)
 '(cua-enable-cua-keys t)
 '(cua-enable-modeline-indications t)
 '(cua-mode t nil (cua-base))
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes
   (quote
    ("e56f1b1c1daec5dbddc50abd00fcd00f6ce4079f4a7f66052cf16d96412a09a9" "cbef37d6304f12fb789f5d80c2b75ea01465e41073c30341dc84c6c0d1eb611d" "c5a044ba03d43a725bd79700087dea813abcb6beb6be08c7eb3303ed90782482" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" "9dae95cdbed1505d45322ef8b5aa90ccb6cb59e0ff26fef0b8f411dfc416c552" "146d24de1bb61ddfa64062c29b5ff57065552a7c4019bee5d869e938782dfc2a" "9f443833deb3412a34d2d2c912247349d4bd1b09e0f5eaba11a3ea7872892000" "4dacec7215677e4a258e4529fac06e0231f7cdd54e981d013d0d0ae0af63b0c8" "36a309985a0f9ed1a0c3a69625802f87dee940767c9e200b89cdebdb737e5b29" "2c728316cf424224af84ecfaacaa70cf8532cf15ed9d31c915ac65913cd7df83" "bf7ed640479049f1d74319ed004a9821072c1d9331bc1147e01d22748c18ebdf" "71b172ea4aad108801421cc5251edb6c792f3adbaecfa1c52e94e3d99634dee7" "14bb0cc73f3ddd0509a7c0bb610b057aef0c0eedb098100f1049e4e5ea108150" default)))
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
 '(flycheck-disabled-checkers (quote (emacs-lisp-checkdoc)))
 '(flymake-gui-warnings-enabled nil)
 '(flymake-run-in-place nil)
 '(flymake-start-syntax-check-on-find-file nil)
 '(flymake-start-syntax-check-on-newline nil)
 '(fringe-mode (quote (nil . 0)) nil (fringe))
 '(gh-profile-alist
   (quote
    (("github" :url "https://api.github.com" :remote-regexp "^\\(?:git@github\\.com:\\|\\(?:git\\|https?\\|ssh\\)://.*@?github\\.com/\\)\\(.*\\)/\\(.*\\)\\(?:\\.git\\)?")
     ("wp-github" :url "https://api.github.dev.pages" :username "jbellegarde" :token "3cf9ddc0f84d8170cf3b8be1dda5d6ab2a5df950" :remote-regexp "^\\(?:git@github\\.dev\\.pages:\\|\\(?:git\\|https?\\|ssh\\)://.*@?github\\.dev\\.pages/\\)\\(.*\\)/\\(.*\\)\\(?:\\.git\\)?"))))
 '(gh-profile-default-profile "wp-github")
 '(git-gutter:verbosity 2)
 '(global-auto-revert-mode t)
 '(global-git-gutter-mode t)
 '(global-linum-mode t)
 '(global-whitespace-newline-mode nil)
 '(go-oracle-command "/Users/Bellegarde/go_src/bin/oracle")
 '(helm-time-zone-home-location "Seattle")
 '(ibuffer-expert t)
 '(ibuffer-filter-format-alist nil)
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
 '(kill-whole-line t)
 '(magit-default-tracking-name-function (quote magit-default-tracking-name-branch-only))
 '(magit-diff-refine-hunk t)
 '(magit-expand-staged-on-commit (quote full))
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
    ("~/.deft/tasks.org" "~/.deft/cleanup.org" "~/.deft/maintenance.org" "~/.deft/support.org" "~/.deft/notes.org")))
 '(org-agenda-todo-list-sublevels nil)
 '(org-babel-load-languages (quote ((emacs-lisp . t) (sh . t))))
 '(org-capture-templates
   (quote
    (("s" "Support" entry
      (file+headline "~/.deft/support.org" "Support")
      "** TODO %?
%U %a
%i
")
     ("c" "Clean up" entry
      (file+headline "~/.deft/cleanup.org" "Clean up")
      "** TODO %?
%U %a
%i")
     ("m" "Maintenance" entry
      (file+headline "~/.deft/maintenance.org" "Maintenance")
      "** TODO %?
%U %a
%i")
     ("t" "Tasks" entry
      (file+headline "~/.deft/tasks.org" "Tasks")
      "** TODO %?
%U %a
%i"))))
 '(org-confirm-babel-evaluate nil)
 '(org-default-notes-file "~/.deft/notes.org")
 '(org-fontify-done-headline t)
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-docview org-gnus org-info org-irc org-mhe org-rmail org-w3m org-mouse)))
 '(org-pretty-entities t)
 '(org-pretty-entities-include-sub-superscripts nil)
 '(org-replace-disputed-keys t)
 '(org-src-fontify-natively t)
 '(org-src-window-setup (quote current-window))
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
    ((orgstruct-heading-prefix-regexp . "^;;; +")
     (encoding . utf-8)
     (ruby-compilation-executable . "ruby")
     (ruby-compilation-executable . "ruby1.8")
     (ruby-compilation-executable . "ruby1.9")
     (ruby-compilation-executable . "rbx")
     (ruby-compilation-executable . "jruby"))))
 '(scroll-bar-mode (quote right))
 '(shell-file-name "/usr/local/bin/fish")
 '(show-paren-mode t)
 '(show-paren-style (quote mixed))
 '(show-trailing-whitespace t)
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
 '(diredp-dir-priv ((t (:foreground "#7474FFFFFFFF"))))
 '(hydra-face-amaranth ((t (:foreground "magenta1" :weight bold))))
 '(hydra-face-red ((t (:foreground "brown3" :weight bold))))
 '(ido-first-match ((t (:foreground "cyan" :weight bold)))))
