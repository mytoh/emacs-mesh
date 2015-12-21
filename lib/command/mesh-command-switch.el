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
  (cl-letf ((state (if mesh:*state* mesh:*state*
                     (mesh:initial-state))))
    (if (glof:get state :inside-session-p)
        (setq mesh:*state* (mesh:command--switch-inside state))
      (setq mesh:*state* (mesh:command--switch-outside state)))))

(cl-defun mesh:command--switch-inside (state)
  (cl-letf* ((old-session (glof:get state :current-session))
             (tabs (glof:get old-session :tabs))
             (old-tab (glof:get old-session :current-tab)))
    (cl-letf* ((new-session old-session))
      (cl-letf* ((new-tab (thread-first old-tab
                            (glof:assoc :conf (current-window-configuration))))
                 (new-tabs (mesh:substitute-if-v new-tab
                                              (lambda (tab) (eq (glof:get tab :index)
                                                           (glof:get old-tab :index)))
                                              tabs)))
        (jump-to-register mesh:*window-configuration-name*)
        (glof:assoc state
                    :sessions
                    (mesh:tab--subst-session
                     state
                     (thread-first new-session
                       (glof:assoc :current-tab new-tab
                                   :tabs new-tabs))
                     old-session)
                    :inside-session-p nil)))))

(cl-defun mesh:command--switch-outside (state)
  (window-configuration-to-register mesh:*window-configuration-name*)
  (if (not (seq-empty-p (glof:get state :sessions)))
      (cl-letf* ((session (glof:get state :current-session))
                 (conf (glof:get-in session
                                    [:current-tab :conf])))
        (set-window-configuration conf)
        (glof:assoc state
                    :inside-session-p t))
    (cl-letf* ((new-session
                (thread-last (glof:get state :sessions)
                  (mesh:session--new mesh:default-session-name)))
               (tab (mesh:first (glof:get new-session :tabs)))
               (conf (glof:get tab :conf)))
      (cond
        (conf
         (set-window-configuration conf)
         (glof:assoc state
                     :current-session new-session
                     :sessions (mesh:conj new-session
                                       (glof:get state :sessions))
                     :inside-session-p t))
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
           (glof:assoc state
                       :current-session new-session
                       :sessions (mesh:conj new-session
                                         (glof:get state :sessions))
                       :inside-session-p t)))))))

(provide 'mesh-command-switch)

;;; mesh-command-switch.el ends here
