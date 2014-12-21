;;; mesh-header-line -*- lexical-binding: t; coding: utf-8; -*-

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'seq)

(require 'mesh-core "lib/mesh-core")

(defcustom mesh:header-line-format
  `((:eval
     (mesh:header-line)))
  "header line for mesh")

(cl-defun mesh:header-line ()
  (cl-letf* ((current-session (mesh:current-session))
             (current-session-list (mesh:session-list)))
    (string-join
     (seq-map
      (lambda (session)
        (if (cl-equalp session current-session)
            (propertize
             (mesh:get-name session)
             'face 'font-lock-type-face)
          (propertize
           (mesh:get-name session)
           'face 'font-lock-comment-face)))
      current-session-list)
     " ")))

(provide 'mesh-header-line)

;;; mesh-header-line.el ends here
