;;; mesh-command-switch -*- lexical-binding: t; coding: utf-8; -*-

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(require 'mesh-core "lib/mesh-core")
(require 'mesh-session "lib/mesh-session")
(require 'mesh-tab "lib/mesh-tab")
(require 'mesh-pane "lib/mesh-pane")

;;;###autoload
(cl-defun mesh:switch ()
  (interactive)
  (if (mesh:inside-session-p)
      (mesh:command--switch-inside)
    (mesh:command--switch-outside)))

(cl-defun mesh:command--switch-inside ()
  (cl-letf* ((old-session (mesh:current-session))
             (tabs (mesh:get-tabs old-session))
             (old-tab (mesh:get-current-tab old-session)))
    (cl-letf* ((new-session old-session)
               (new-tab old-tab))
      (setf (mesh:get-conf new-tab) (current-window-configuration))
      (setf (mesh:get-tabs new-session)
            (cl-subst new-tab old-tab tabs))
      (setq mesh:*session-list*
            (cl-subst new-session old-session
                      (mesh:session-list)))))
  (jump-to-register mesh:window-configuration-name)
  (mesh:unset-inside-session))

(cl-defun mesh:command--switch-outside ()
  (window-configuration-to-register mesh:window-configuration-name)
  (if (mesh:session-list)
      (cl-letf* ((session (mesh:current-session))
                 (conf (thread-first session
                         mesh:get-current-tab
                         mesh:get-conf)))
        (set-window-configuration conf)
        (mesh:set-current-session session)
        (mesh:set-inside-session))
    (cl-letf* ((new-session (mesh:session--new
                             mesh:default-session-name
                             (mesh:session-list)))
               (tab (car (mesh:get-tabs new-session)))
               (conf (mesh:get-conf tab)))
      (cond
        (conf
         (set-window-configuration conf))
        (t
         (switch-to-buffer
          (thread-first tab
            mesh:get-panes
            car
            mesh:get-buffer))
         (delete-other-windows)))
      (mesh:set-current-session new-session)
      (setq mesh:*session-list* (list new-session))
      (mesh:set-inside-session))))

(provide 'mesh-command-switch)

;;; mesh-command-switch.el ends here
