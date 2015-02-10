(require 'idle-highlight-mode)
(defun jmb-idle-highlight-hook ()
  (idle-highlight-mode t))

(add-hook 'ruby-mode 'idle-highlight-mode)
