;;; mesh-command-tab -*- lexical-binding: t; coding: utf-8; -*-

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(require 'mesh-core "lib/mesh-core")
(require 'mesh-session "lib/mesh-session")
(require 'mesh-tab "lib/mesh-tab")
(require 'mesh-pane "lib/mesh-pane")

(mesh:defcommand mesh:split-tab
  (mesh:tab--command-split))

(mesh:defcommand mesh:new-tab
  (mesh:tab--command-new))

(mesh:defcommand mesh:vsplit-tab
  (mesh:tab--command-vsplit))

(mesh:defcommand mesh:next-tab
  (mesh:tab--command-next))

(mesh:defcommand mesh:prev-tab
  (mesh:tab--command-prev))

(mesh:defcommand mesh:kill-tab
  (mesh:tab--command-kill))

(provide 'mesh-command-tab)

;;; mesh-command-tab.el ends here
