;;; mesh-command-tab -*- lexical-binding: t; coding: utf-8; -*-

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(require 'mesh-core "lib/mesh-core")
(require 'mesh-session "lib/mesh-session")
(require 'mesh-tab "lib/mesh-tab")
(require 'mesh-pane "lib/mesh-pane")

;;;###autoload
(mesh:defcommand mesh:split-tab
  (mesh:tab--command-split))

;;;###autoload
(mesh:defcommand mesh:new-tab
  (mesh:tab--command-new))

;;;###autoload
(mesh:defcommand mesh:vsplit-tab
  (mesh:tab--command-vsplit))

;;;###autoload
(mesh:defcommand mesh:next-tab
  (mesh:tab--command-next))

;;;###autoload
(mesh:defcommand mesh:prev-tab
  (mesh:tab--command-prev))

;;;###autoload
(mesh:defcommand mesh:kill-tab
  (mesh:tab--command-kill))

(provide 'mesh-command-tab)

;;; mesh-command-tab.el ends here
