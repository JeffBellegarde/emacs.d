;; * Wanderlust -- email
;; Stores read state locally isntead of in gmail. Not Used.
(use-package wanderlust
  :straight t
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

;; * Mu4e -- email
;; I don't like the view system.
(use-package mu4e
  :straight nil
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


;; * SMTP
;; I'm not using emacs for mail so disabled.
(use-package smtpmail
  :disabled t
  :straight nil
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

