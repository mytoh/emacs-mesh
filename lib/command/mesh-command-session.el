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
  (mesh::update #'mesh:session--command-create
             new-session-name))

;;;###autoload
(cl-defun mesh:next-session ()
  (interactive)
  (mesh::update #'mesh:session--command-next))

;;;###autoload
(cl-defun mesh:prev-session ()
  (interactive)
  (mesh::update #'mesh:session--command-prev))

(cl-defun mesh:kill-session ()
  (interactive)
  (mesh::update #'mesh:session--command-kill))

(provide 'mesh-command-session)

;;; mesh-command-session.el ends here
