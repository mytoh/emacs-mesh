;;; mesh-mode-line -*- lexical-binding: t; coding: utf-8; -*-

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'glof)

(require 'mesh-core "lib/mesh-core")

(defcustom mesh:mode-line-format
  `((:eval
     (mesh:mode-line)))
  "mode line for mesh")

(cl-defun mesh:mode-line ()
  (cl-letf* ((current-session (mesh:current-session))
             (current-tab (glof:get current-session :current-tab))
             (current-tab-name (glof:get current-tab :name))
             (current-tab-index (glof:get current-tab :index))
             (current-buffer-name (buffer-name (current-buffer)))
             (current-pane-index (thread-first
                                     (seq-find
                                      (lambda (pane) (cl-equalp current-buffer-name
                                                           (buffer-name (glof:get pane :buffer))))
                                      (glof:get current-tab :panes))
                                   (glof:get :index)))
             (last-command-name eshell-last-command-name)
             (directory-name (abbreviate-file-name (eshell/pwd))))
    (string-join
     (list
      (seq-concatenate 'string
                       (propertize
                        current-tab-name
                        'face 'font-lock-doc-face)
                       "."
                       (propertize
                        (number-to-string current-tab-index)
                        'face 'font-lock-doc-face))
      (propertize
       (number-to-string current-pane-index)
       'face 'font-lock-keyword-face)
      (propertize
       last-command-name
       'face 'font-lock-function-name-face)
      (propertize
       directory-name
       'face 'font-lock-variable-name-face))
     " ")))

(provide 'mesh-mode-line)

;;; mesh-mode-line.el ends here
