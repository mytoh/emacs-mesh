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
  (mesh:command--switch-toggle))

(cl-defmethod mesh:command--switch-toggle (&context
                                           ((mesh:inside-session-p) (eql t)))
  (cl-letf* ((old-session (mesh:current-session))
             (tabs (glof:get old-session :tabs))
             (old-tab (glof:get old-session :current-tab)))
    (cl-letf* ((new-session old-session))
      (cl-letf* ((new-tab (thread-first old-tab
                            (glof:assoc :conf (current-window-configuration))))
                 (new-tabs (mesh:substitute-if-v new-tab
                                                 (lambda (tab) (eq (glof:get tab :index)
                                                              (glof:get old-tab :index)))
                                                 tabs)))
        (mesh:tab--subst-session
         (thread-first new-session
           (glof:assoc :current-tab new-tab
                       :tabs new-tabs))
         old-session))))
  (jump-to-register mesh:*window-configuration-name*)
  (mesh:unset-inside-session))

(cl-defmethod mesh:command--switch-toggle (&context
                                           ((mesh:inside-session-p) (eql nil)))
  (window-configuration-to-register mesh:*window-configuration-name*)
  (if (not (seq-empty-p (mesh:sessions)))
      (cl-letf* ((session (mesh:current-session))
                 (conf (thread-first session
                         (glof:get :current-tab)
                         (glof:get :conf))))
        (set-window-configuration conf)
        (mesh:set-current-session session)
        (mesh:set-inside-session))
    (cl-letf* ((new-session (mesh:session--new
                             mesh:default-session-name
                             (mesh:sessions)))
               (tab (mesh:first (glof:get new-session :tabs)))
               (conf (glof:get tab :conf)))
      (cond
        (conf
         (set-window-configuration conf)
         (mesh:set-current-session new-session)
         (setq mesh:*sessions* (mesh:conj new-session mesh:*sessions*))
         (mesh:set-inside-session))
        (t
         (switch-to-buffer
          (thread-first tab
            (glof:get :panes)
            mesh:first
            (glof:get :buffer)))
         (delete-other-windows)
         (cl-letf* ((new-tab (thread-first tab
                               (glof:assoc :conf (current-window-configuration))))
                    (new-tabs (mesh:substitute-if-v new-tab
                                                    (lambda (tb) (eq (glof:get tb :index)
                                                                (glof:get tab :index)))
                                                    (glof:get new-session :tabs)))
                    (new-session (thread-first new-session
                                   (glof:assoc
                                    :current-tab new-tab
                                    :tabs new-tabs))))
           (mesh:set-current-session new-session)
           (setq mesh:*sessions* (mesh:conj new-session mesh:*sessions*))
           (mesh:set-inside-session)))))))

(provide 'mesh-command-switch)

;;; mesh-command-switch.el ends here
