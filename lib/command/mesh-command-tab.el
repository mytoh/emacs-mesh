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
  (setq mesh:*state*
        (mesh:tab--command-split mesh:*state*)))

;;;###autoload
(mesh:defcommand mesh:new-tab
  (setq mesh:*state*
        (mesh:tab--command-new mesh:*state*)))

;;;###autoload
(mesh:defcommand mesh:vsplit-tab
  (setq mesh:*state*
        (mesh:tab--command-vsplit mesh:*state*)))

;;;###autoload
(mesh:defcommand mesh:next-tab
  (setq mesh:*state*
        (mesh:tab--command-next mesh:*state*)))

;;;###autoload
(mesh:defcommand mesh:prev-tab
  (setq mesh:*state*
        (mesh:tab--command-prev mesh:*state*)))

;;;###autoload
(mesh:defcommand mesh:kill-tab
  (setq mesh:*state*
        (mesh:tab--command-kill mesh:*state*)))

(provide 'mesh-command-tab)

;;; mesh-command-tab.el ends here
