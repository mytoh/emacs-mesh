;;; mesh-command-session -*- lexical-binding: t; coding: utf-8; -*-

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(require 'mesh-core "lib/mesh-core")
(require 'mesh-session "lib/mesh-session")

;;;###autoload
(cl-defun mesh:new-session (new-session-name)
  (interactive "sSession name: ")
  (mesh:session--command-create new-session-name))

(cl-defun mesh:kill-session ())

;;;###autoload
(cl-defun mesh:next-session ()
  (interactive)
  (mesh:session--command-next))

(cl-defun mesh:prev-session ())

(provide 'mesh-command-session)

;;; mesh-command-session.el ends here
