;;; mesh-command-pane -*- lexical-binding: t; coding: utf-8; -*-

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(require 'mesh-core "lib/mesh-core")
(require 'mesh-session "lib/mesh-session")
(require 'mesh-tab "lib/mesh-tab")
(require 'mesh-pane "lib/mesh-pane")

;;;###autoload
(mesh:defcommand mesh:kill-pane
  (mesh:pane--command-kill
   (mesh:current-session)))

;;;###autoload
(mesh:defcommand mesh:next-pane
  (mesh:pane--command-next
   (mesh:current-session)))

(cl-defun mesh:prev-pane ())


(provide 'mesh-command-pane)

;;; mesh-command-pane.el ends here
