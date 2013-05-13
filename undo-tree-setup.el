(require 'undo-tree)
(global-undo-tree-mode)

(setq jmb-disabled-whitespace-mode-hooks
      (list 'magit-mode-hook))

(defun jmb-disable-show-trailing-whitespace ()
  (setq show-trailing-whitespace nil))

(dolist (hook jmb-disabled-whitespace-mode-hooks)
  (add-hook hook 'jmb-disable-show-trailing-whitespace))


