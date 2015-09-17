;;; mesh-mode-line -*- lexical-binding: t; coding: utf-8; -*-

;;; Code:

(require 'cl-lib)
(require 'eieio)

(require 'mesh-core "lib/mesh-core")

(defcustom mesh:mode-line-format
  `((:eval
     (mesh:mode-line)))
  "mode line for mesh")

(cl-defun mesh:mode-line ()
  (cl-letf* ((current-session (mesh:current-session))
             (current-tab (mesh:get-current-tab current-session))
             (current-tab-name (mesh:get-name current-tab))
             (current-tab-index (mesh:get-index current-tab))
             (current-buffer-name (buffer-name (current-buffer)))
             (current-pane-index (thread-first
                                     (seq-find
                                      (lambda (pane) (cl-equalp current-buffer-name
                                                           (buffer-name (glof:get pane :buffer))))
                                      (mesh:get-panes current-tab))
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
