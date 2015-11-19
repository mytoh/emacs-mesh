;;; mesh-command-session -*- lexical-binding: t; coding: utf-8; -*-

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'glof)

(require 'mesh-core "lib/mesh-core")
(require 'mesh-session "lib/mesh-session")

;;;###autoload
(cl-defun mesh:new-session (new-session-name)
  (interactive "sSession name: ")
  (mesh::handle-command #'mesh:session--command-create
                     mesh:*state* new-session-name))

;;;###autoload
(cl-defun mesh:next-session ()
  (interactive)
  (mesh::handle-command #'mesh:session--command-next
                     mesh:*state*))

;;;###autoload
(cl-defun mesh:prev-session ()
  (interactive)
  (mesh::handle-command #'mesh:session--command-prev
                     mesh:*state*))

(cl-defun mesh:kill-session ()
  (interactive)
  (mesh::handle-command #'mesh:session--command-kill
                     mesh:*state*))

(provide 'mesh-command-session)

;;; mesh-command-session.el ends here
