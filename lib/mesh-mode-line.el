;;; mesh-mode-line -*- lexical-binding: t; coding: utf-8; -*-

;;; Code:

(require 'cl-lib)
(require 'glof)

(require 'mesh-core "lib/mesh-core")

(defcustom mesh:mode-line-format
  `((:eval
     (mesh:mode-line mesh:*state*)))
  "mode line for mesh")

(cl-defun mesh:mode-line (state)
  (cl-letf* ((cursession (glof:get state :current-session))
             (curtab (glof:get cursession :current-tab))
             (curtabname (glof:get curtab :name))
             (curtabindex (glof:get curtab :index))
             (curbufname (buffer-name (current-buffer)))
             (curpaneindex (glof:get (seq-find
                                      (lambda (pane) (cl-equalp curbufname
                                                           (buffer-name (glof:get pane :buffer))))
                                      (glof:get curtab :panes))
                                     :index))
             (last-command-name eshell-last-command-name)
             (directory-name (abbreviate-file-name (eshell/pwd))))
    (string-join
     (list
      (seq-concatenate 'string
                       (propertize
                        curtabname
                        'face 'font-lock-doc-face)
                       "."
                       (propertize
                        (number-to-string curtabindex)
                        'face 'font-lock-doc-face))
      (propertize
       (number-to-string curpaneindex)
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
