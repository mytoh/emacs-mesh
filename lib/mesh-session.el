;;; mesh-session -*- lexical-binding: t; coding: utf-8; -*-

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'seq)
(require 'glof)

(require 'mesh-core "lib/mesh-core")
(require 'mesh-class "lib/mesh-class")
(require 'mesh-tab "lib/mesh-tab")


(cl-defun mesh:session--new (session-name sessions)
  (if-let ((found (seq-find (lambda (s) (cl-equalp (glof:get s :name)
                                              session-name))
                            sessions)))
      found
    (cl-letf ((new-tab (mesh:tab--new
                        mesh:default-tab-name
                        session-name)))
      (glof:plist
       :name session-name
       :current-tab new-tab
       :tabs (list new-tab)))))

(cl-defun mesh:session--command-create (new-session-name)
  (cl-letf ((new-session-name
             (if (seq-find
                  (lambda (session) (cl-equalp new-session-name
                                          (glof:get session :name)))
                  (mesh:sessions))
                 (seq-concatenate 'string new-session-name "*")
               new-session-name)))
    (cl-letf* ((current-session (mesh:current-session))
               (current-session-tabs (glof:get current-session :tabs))
               (current-tab (glof:get current-session :current-tab)))
      (cl-letf* ((new-session current-session))
        (cl-letf* ((new-tab (thread-first current-tab
                              (glof:assoc :conf (current-window-configuration))))
                   (new-tabs (mesh:substitute-if new-tab
                                                 (lambda (tab)
                                                   (eq (glof:get current-tab :index)
                                                       (glof:get tab :index)))
                                                 current-session-tabs)))
          (cl-letf ((new-session (thread-first new-session
                                   (glof:assoc :current-tab new-tab
                                               :tabs new-tabs))))
            (mesh:tab--subst-session
             new-session current-session)))))
    (cl-letf* ((new-session (mesh:session--new
                             new-session-name
                             (mesh:sessions)))
               (tab (mesh:first (glof:get new-session :tabs)))
               (conf (glof:get tab :conf)))
      (switch-to-buffer
       (thread-first tab
         (glof:get :panes)
         mesh:first
         (glof:get :buffer)))
      (delete-other-windows)
      (mesh:set-current-session new-session)
      (setq mesh:*sessions*
            (mesh:cons new-session
                       (mesh:sessions))))))

(cl-defun mesh:session--command-next (session)
  (cl-letf* ((current-session session)
             (next-session
              (mesh:find-next `[:session ,current-session ]
                              (mesh:sessions))))
    (when next-session
      (cl-letf* ((current-session-tabs (glof:get current-session :tabs))
                 (current-session-tab (glof:get current-session :current-tab)))
        (cl-letf ((new-session current-session))
          (cl-letf* ((new-tab (thread-first current-session-tab
                                (glof:assoc :conf (current-window-configuration))))
                     (new-tabs (mesh:substitute-if new-tab
                                                   (lambda (tab)
                                                     (eq (glof:get tab :index)
                                                         (glof:get current-session-tab
                                                                   :index)))
                                                   current-session-tabs)))
            (cl-letf ((new-session (thread-first new-session
                                     (glof:assoc :current-tab new-tab
                                                 :tabs new-tabs))))
              (mesh:tab--subst-session
               new-session current-session)))))
      (cl-letf* ((next-session-conf (thread-first next-session
                                      (glof:get :current-tab)
                                      (glof:get :conf))))
        (set-window-configuration next-session-conf)
        (mesh:set-current-session next-session)))))

(cl-defun mesh:session--command-prev (session)
  (cl-letf* ((current-session session)
             (prev-session
              (mesh:find-prev `[:session ,current-session ]
                              (mesh:sessions))))
    (when prev-session
      (cl-letf* ((current-session-tabs (glof:get current-session :tabs))
                 (current-session-tab (glof:get current-session :current-tab)))
        (cl-letf ((new-session current-session)
                  (new-tab current-session-tab))
          (cl-letf* ((new-tab (thread-first current-session-tab
                                (glof:assoc :conf (current-window-configuration))))
                     (new-tabs (mesh:substitute-if new-tab
                                                   (lambda (tab)
                                                     (eq (glof:get tab :index)
                                                         (glof:get current-session-tab :index)))
                                                   current-session-tabs))
                     (new-session (thread-first new-session
                                    (glof:assoc :current-tab new-tab
                                                :tabs new-tabs))))
            (mesh:tab--subst-session new-session current-session))))
      (cl-letf* ((prev-session-conf (thread-first prev-session
                                      (glof:get :current-tab)
                                      (glof:get :conf))))
        (set-window-configuration prev-session-conf)
        (mesh:set-current-session prev-session)))))

(cl-defun mesh:session--command-kill (session)
  (cl-letf* ((current-session session)
             (current-session-list (mesh:sessions)))
    (pcase (seq-length current-session-list)
      (1
       (cl-letf ((current-tab (glof:get current-session :current-tab))
                 (current-tabs (glof:get current-session :tabs)))
         (mesh:unset-sessions)
         (jump-to-register mesh:*window-configuration-name*)
         (mesh:unset-inside-session)
         (mesh:unset-current-session)
         (seq-each
          #'mesh:tab--kill-panes
          current-tabs)))
      (_
       (cl-letf ((current-tab (glof:get current-session :current-tab))
                 (current-tabs (glof:get current-session :tabs))
                 (next-session (mesh:find-next `[:session ,current-session] current-session-list)))
         (setq mesh:*sessions*
               (cl-remove current-session (mesh:sessions)))
         (cl-letf* ((next-session-conf (thread-first next-session
                                         (glof:get :current-tab)
                                         (glof:get :conf))))
           (set-window-configuration next-session-conf)
           (mesh:set-current-session next-session))
         (seq-each
          #'mesh:tab--kill-panes
          current-tabs))))))


(provide 'mesh-session)

;;; mesh-session.el ends here
