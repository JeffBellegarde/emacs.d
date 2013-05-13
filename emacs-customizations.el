(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(abbrev-mode t t)
 '(ack-and-a-half-arguments (quote ("--ignore-dir bundle" "--ignore-dir vendor")))
 '(ack-and-a-half-prompt-for-directory t)
 '(ansi-color-for-comint-mode t)
 '(autotest-use-ui t)
 '(comint-prompt-read-only t)
 '(comint-scroll-to-bottom-on-input t)
 '(cua-enable-cua-keys t)
 '(cua-enable-modeline-indications nil)
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes (quote ("4dacec7215677e4a258e4529fac06e0231f7cdd54e981d013d0d0ae0af63b0c8" "36a309985a0f9ed1a0c3a69625802f87dee940767c9e200b89cdebdb737e5b29" "2c728316cf424224af84ecfaacaa70cf8532cf15ed9d31c915ac65913cd7df83" "bf7ed640479049f1d74319ed004a9821072c1d9331bc1147e01d22748c18ebdf" "71b172ea4aad108801421cc5251edb6c792f3adbaecfa1c52e94e3d99634dee7" "14bb0cc73f3ddd0509a7c0bb610b057aef0c0eedb098100f1049e4e5ea108150" default)))
 '(deft-extension "org")
 '(deft-text-mode (quote org-mode))
 '(desktop-load-locked-desktop t)
 '(desktop-restore-eager 10)
 '(dired-details-hide-link-targets nil)
 '(ediff-keep-variants nil)
 '(eproject-completing-read-function (quote eproject--ido-completing-read))
 '(exec-path (quote ("/usr/bin" "/bin" "/usr/sbin" "/sbin" "/Applications/Emacs.app/Contents/MacOS/libexec" "/Applications/Emacs.app/Contents/MacOS/bin" "/usr/local/bin")))
 '(flymake-run-in-place nil)
 '(fringe-mode (quote (nil . 0)) nil (fringe))
 '(global-auto-revert-mode t)
 '(global-git-gutter-mode t)
 '(global-whitespace-mode nil)
 '(global-whitespace-newline-mode nil)
 '(ibuffer-expert t)
 '(ibuffer-filter-format-alist nil)
 '(ibuffer-saved-filter-groups (quote (("home" ("emacs-config" (or (filename . ".emacs.d") (filename . "emacs"))) ("magit" (name . "*magit"))))))
 '(ibuffer-saved-filters (quote (("gnus" ((or (mode . message-mode) (mode . mail-mode) (mode . gnus-group-mode) (mode . gnus-summary-mode) (mode . gnus-article-mode)))) ("programming" ((or (mode . emacs-lisp-mode) (mode . cperl-mode) (mode . c-mode) (mode . java-mode) (mode . idl-mode) (mode . ruby-mode) (mode . lisp-mode)))))))
 '(ibuffer-show-empty-filter-groups nil)
 '(ido-everywhere t)
 '(ido-mode (quote both) nil (ido))
 '(ido-read-file-name-non-ido (quote (find-dired)))
 '(ido-rotate-file-list-default t)
 '(ido-show-dot-for-dired t)
 '(ido-use-filename-at-point (quote guess))
 '(imenu-auto-rescan t)
 '(imenu-sort-function nil)
 '(kill-whole-line t)
 '(magit-diff-refine-hunk t)
 '(magit-process-connection-type nil)
 '(magit-process-popup-time 5)
 '(magit-remote-ref-format (quote remote-slash-branch))
 '(magit-repo-dirs (quote ("/autohome/jbellegarde/src")))
 '(magit-revert-item-confirm nil)
 '(magit-save-some-buffers (quote dontask))
 '(magit-set-upstream-on-push (quote dontask))
 '(magit-sha1-abbrev-length 5)
 '(make-backup-files nil)
 '(minimap-dedicated-window t)
 '(org-agenda-files (quote ("~/.deft/tasks.org" "~/.deft/cleanup.org" "~/.deft/maintenance.org" "~/.deft/support.org" "~/.deft/notes.org")))
 '(org-capture-templates (quote (("s" "Support" entry (file+headline "~/.deft/support.org" "Support") "** TODO %?
%U %a
%i
") ("c" "Clean up" entry (file+headline "~/.deft/cleanup.org" "Clean up") "** TODO %?
%U %a
%i") ("m" "Maintenance" entry (file+headline "~/.deft/maintenance.org" "Maintenance") "** TODO %?
%U %a
%i") ("t" "Tasks" entry (file+headline "~/.deft/tasks.org" "Tasks") "** TODO %?
%U %a
%i"))))
 '(org-default-notes-file "~/.deft/notes.org")
 '(org-fontify-done-headline t)
 '(org-modules (quote (org-bbdb org-bibtex org-docview org-gnus org-info org-jsinfo org-irc org-mew org-mhe org-rmail org-vm org-wl org-w3m org-mouse)))
 '(org-pretty-entities t)
 '(org-pretty-entities-include-sub-superscripts nil)
 '(org-replace-disputed-keys t)
 '(org-use-speed-commands t)
 '(outline-occur-by-mode (quote ((ruby-mode . "^[[:space:]]*it[[:space:]].*do\\\\|{$\\\\|^[[:space:]]*describe[[:space:]].*do$\\\\|^[[:space:]]*context.*do$\\\\|^[[:space:]]*module\\\\|^[[:space:]]*class\\\\|^[[:space:]]*def"))))
 '(pomodoro-work-start-message "Back to work!")
 '(rdebug-short-key-mode t)
 '(rdebug-track-do-tracking-p t)
 '(remember-annotation-functions (quote (org-remember-annotation)))
 '(remember-data-file "~/.deft/notes.org")
 '(remember-handler-functions (quote (org-remember-handler)))
 '(remember-mode-hook (quote (flyspell-mode turn-on-auto-fill org-remember-apply-template)))
 '(rspec-spec-command "rspec")
 '(rspec-use-rake-flag nil)
 '(safe-local-variable-values (quote ((encoding . utf-8) (ruby-compilation-executable . "ruby") (ruby-compilation-executable . "ruby1.8") (ruby-compilation-executable . "ruby1.9") (ruby-compilation-executable . "rbx") (ruby-compilation-executable . "jruby"))))
 '(show-paren-mode t)
 '(show-paren-style (quote mixed))
 '(show-trailing-whitespace t)
 '(undo-tree-visualizer-diff t)
 '(undo-tree-visualizer-timestamps t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Monaco"))))
 '(diredp-dir-priv ((t (:foreground "#7474FFFFFFFF"))))
 '(ido-first-match ((t (:foreground "cyan" :weight bold)))))
