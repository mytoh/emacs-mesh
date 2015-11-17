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
  (let ((found (seq-find (lambda (s) (cl-equalp (glof:get s :name)
                                                session-name))
                         sessions)))
    (if (not (seq-empty-p found))
        found
      (cl-letf ((new-tab (mesh:tab--new
                          mesh:default-tab-name
                          session-name)))
        (glof:plist
         :name session-name
         :current-tab new-tab
         :tabs (vector new-tab))))))

(cl-defun mesh:session--command-create (state new-session-name)
  (cl-letf ((new-session-name
             (if (not (seq-empty-p
                       (seq-find
                        (lambda (session) (cl-equalp new-session-name
                                                     (glof:get session :name)))
                        (glof:get state :sessions))))
                 (seq-concatenate 'string new-session-name "*")
               new-session-name)))
    (cl-letf* ((current-session (glof:get state :current-session))
               (current-session-tabs (glof:get current-session :tabs))
               (current-tab (glof:get current-session :current-tab)))
      (cl-letf* ((new-tab (thread-first current-tab
                            (glof:assoc :conf (current-window-configuration))))
                 (new-tabs (mesh:substitute-if-v new-tab
                                              (lambda (tab)
                                                (eq (glof:get current-tab :index)
                                                    (glof:get tab :index)))
                                              current-session-tabs)))
        (cl-letf* ((new-state (glof:assoc state
                                          :sessions
                                          (mesh:tab--subst-session state
                                                                (thread-first current-session
                                                                  (glof:assoc :current-tab new-tab
                                                                              :tabs new-tabs))
                                                                current-session)))
                   (new-session (mesh:session--new
                                 new-session-name
                                 (glof:get new-state :sessions))))
          (switch-to-buffer
           (thread-first (mesh:first (glof:get new-session :tabs))
             (glof:get :panes)
             mesh:first
             (glof:get :buffer)))
          (delete-other-windows)
          (glof:assoc new-state
                      :current-session new-session
                      :sessions (mesh:conj new-session
                                        (glof:get new-state :sessions))))))))

(cl-defun mesh:session--command-next (state)
  (cl-letf* ((current-session (glof:get state :current-session))
             (next-session
              (mesh:find-next `[:session ,current-session ]
                           (glof:get state :sessions))))
    (when next-session
      (cl-letf* ((current-session-tabs (glof:get current-session :tabs))
                 (current-session-tab (glof:get current-session :current-tab)))
        (cl-letf ((new-session current-session))
          (cl-letf* ((new-tab (thread-first current-session-tab
                                (glof:assoc :conf (current-window-configuration))))
                     (new-tabs (mesh:substitute-if-v new-tab
                                                  (lambda (tab)
                                                    (eq (glof:get tab :index)
                                                        (glof:get current-session-tab
                                                                  :index)))
                                                  current-session-tabs)))
            (cl-letf* ((new-state (glof:assoc state
                                              :sessions
                                              (mesh:tab--subst-session
                                               state
                                               (thread-first current-session
                                                 (glof:assoc :current-tab new-tab
                                                             :tabs new-tabs))
                                               current-session)))
                       (next-session-conf (thread-first next-session
                                            (glof:get :current-tab)
                                            (glof:get :conf))))
              (set-window-configuration next-session-conf)
              (glof:assoc new-state
                          :current-session next-session))))))))

(cl-defun mesh:session--command-prev (state)
  (cl-letf* ((current-session (glof:get state :current-session))
             (prev-session
              (mesh:find-prev `[:session ,current-session]
                           (glof:get state :sessions))))
    (when prev-session
      (cl-letf* ((current-session-tabs (glof:get current-session :tabs))
                 (current-session-tab (glof:get current-session :current-tab)))
        (cl-letf* ((new-tab (thread-first current-session-tab
                              (glof:assoc :conf (current-window-configuration))))
                   (new-tabs (mesh:substitute-if-v new-tab
                                                (lambda (tab)
                                                  (eq (glof:get tab :index)
                                                      (glof:get current-session-tab :index)))
                                                current-session-tabs)))
          (cl-letf* ((new-state (glof:assoc state
                                            :sessions
                                            (mesh:tab--subst-session state
                                                                  (thread-first current-session
                                                                    (glof:assoc :current-tab new-tab
                                                                                :tabs new-tabs))
                                                                  current-session)))
                     (prev-session-conf (thread-first prev-session
                                          (glof:get :current-tab)
                                          (glof:get :conf))))
            (set-window-configuration prev-session-conf)
            (glof:assoc new-state
                        :current-session prev-session)))))))

(cl-defun mesh:session--command-kill (state)
  (cl-letf* ((current-session (glof:get state :current-session))
             (current-session-list (glof:get state :sessions)))
    (pcase (seq-length current-session-list)
      (1
       (cl-letf ((current-tab (glof:get current-session :current-tab))
                 (current-tabs (glof:get current-session :tabs)))
         (jump-to-register mesh:*window-configuration-name*)
         (seq-each
          #'mesh:tab--kill-panes
          current-tabs)
         (glof:assoc state
                     :session []
                     :inside-session-p nil
                     :current-session nil)))
      (_
       (cl-letf ((current-tab (glof:get current-session :current-tab))
                 (current-tabs (glof:get current-session :tabs))
                 (next-session (mesh:find-next `[:session ,current-session] current-session-list)))
         (set-window-configuration
          (thread-first next-session
            (glof:get :current-tab)
            (glof:get :conf)))
         (seq-each
          #'mesh:tab--kill-panes
          current-tabs)
         (glof:assoc state
                     :sessions (mesh:removev current-session current-session-list)
                     :current-session next-session))))))


(provide 'mesh-session)

;;; mesh-session.el ends here
