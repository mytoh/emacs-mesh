;;; mesh-run-new -*- lexical-binding: t; coding: utf-8; -*-

;;; Code:

(add-to-list 'load-path
             (expand-file-name
              "~/huone/ateljee/emacs-mesh"))
(add-to-list 'load-path
             (expand-file-name
              "~/huone/ateljee/emacs-glof"))

(require 'mesh)

(define-prefix-command 'mesh-prefix)

(global-set-key (kbd "C-z") #'mesh-prefix)
(global-set-key (kbd "C-z n") #'mesh:next-tab)
(global-set-key (kbd "C-z c") #'mesh:new-tab)
(global-set-key (kbd "C-z x") #'mesh:kill-pane)
(global-set-key (kbd "C-z [TAB]") #'mesh:next-pane)
(global-set-key (kbd "C-z C-z") #'mesh:switch)

;;; mesh-run-new.el ends here
