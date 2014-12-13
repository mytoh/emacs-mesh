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
             (current-session-name (mesh:get-name current-session))
             (current-tab (mesh:get-current-tab current-session))
             (current-tab-name (mesh:get-name current-tab))
             (current-tab-index (mesh:get-index current-tab))
             (current-pane-index (mesh:get-index
                                  (mesh:get-current-pane current-tab)))
             (last-command-name eshell-last-command-name))
    (string-join
     (list
      (propertize
       current-session-name
       'face 'font-lock-type-face)
      (concat
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
       'face 'font-lock-function-name-face))
     " ")))

(provide 'mesh-mode-line)

;;; mesh-mode-line.el ends here
