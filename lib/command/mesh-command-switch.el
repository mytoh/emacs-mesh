;;; mesh-command-switch -*- lexical-binding: t; coding: utf-8; -*-

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'glof)

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
    (cl-letf* ((new-session old-session))
      ;; (setf (mesh:get-conf new-tab) (current-window-configuration))
      ;; (setf (mesh:get-tabs new-session)
      ;;       (cl-subst new-tab old-tab tabs))
      (cl-letf* ((new-tab (thread-first old-tab
                            (glof:assoc :conf (current-window-configuration))))
                 (new-tabs (cl-substitute-if new-tab
                                             (lambda (tab) (eq (glof:get tab :index)
                                                          (glof:get old-tab :index)))
                                             tabs))))
      (setf ((mesh:get-current-tab new-session) new-tab))
      (setf (mesh:get-tabs new-session) new-tabs)
      ;; (setq mesh:*session-list*
      ;;       (cl-subst new-session old-session
      ;;                 (mesh:session-list)))
      (mesh:tab--subst-session-list
       new-session old-session)
      ))
  (jump-to-register mesh:*window-configuration-name*)
  (mesh:unset-inside-session))

(cl-defun mesh:command--switch-outside ()
  (window-configuration-to-register mesh:*window-configuration-name*)
  (if (mesh:session-list)
      (cl-letf* ((session (mesh:current-session))
                 (conf (thread-first session
                         mesh:get-current-tab
                         (glof:get :conf))))
        (set-window-configuration conf)
        (mesh:set-current-session session)
        (mesh:set-inside-session))
    (cl-letf* ((new-session (mesh:session--new
                             mesh:default-session-name
                             (mesh:session-list)))
               (tab (cl-first (mesh:get-tabs new-session)))
               (conf (glof:get tab :conf)))
      (cond
        (conf
         (set-window-configuration conf)
         (mesh:set-current-session new-session)
         (setq mesh:*session-list* (list new-session))
         (mesh:set-inside-session))
        (t
         (switch-to-buffer
          (thread-first tab
            (glof:get :panes)
            cl-first
            (glof:get :buffer)))
         (delete-other-windows)
         (setf (mesh:get-tabs new-session)
               (thread-first tab
                 (glof:assoc :conf (current-window-configuration))
                 (cl-substitute-if (lambda (tb) (eq (glof:get tb :index)
                                               (glof:get tab :index)))
                                   (mesh:get-tabs new-session))))
         (mesh:set-current-session new-session)
         (setq mesh:*session-list* (list new-session))
         (mesh:set-inside-session))))))

(provide 'mesh-command-switch)

;;; mesh-command-switch.el ends here
