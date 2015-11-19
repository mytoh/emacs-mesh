;;; mesh-command-tab -*- lexical-binding: t; coding: utf-8; -*-

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'glof)

(require 'mesh-core "lib/mesh-core")
(require 'mesh-session "lib/mesh-session")
(require 'mesh-tab "lib/mesh-tab")
(require 'mesh-pane "lib/mesh-pane")

;;;###autoload
(mesh:defcommand mesh:split-tab
  (mesh::handle-command #'mesh:tab--command-split
                     mesh:*state*))

;;;###autoload
(mesh:defcommand mesh:new-tab
  (mesh::handle-command #'mesh:tab--command-new mesh:*state*))

;;;###autoload
(mesh:defcommand mesh:vsplit-tab
  (mesh::handle-command #'mesh:tab--command-vsplit mesh:*state*))

;;;###autoload
(mesh:defcommand mesh:next-tab
  (mesh::handle-command #'mesh:tab--command-next mesh:*state*))

;;;###autoload
(mesh:defcommand mesh:prev-tab
  (mesh::handle-command #'mesh:tab--command-prev mesh:*state*))

;;;###autoload
(mesh:defcommand mesh:kill-tab
  (mesh::handle-command #'mesh:tab--command-kill mesh:*state*))

(provide 'mesh-command-tab)

;;; mesh-command-tab.el ends here
