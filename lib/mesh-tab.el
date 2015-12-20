;;; mesh-tab -*- lexical-binding: t; coding: utf-8; -*-

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'glof)

(require 'mesh-class "lib/mesh-class")
(require 'mesh-core "lib/mesh-core")
(require 'mesh-pane "lib/mesh-pane")

(cl-defun mesh:tab--new (tab-name session-name &optional (index 0))
  (cl-letf* ((new-pane (mesh:pane--new session-name
                                    tab-name
                                    index)))
    (glof:assoc mesh:<tab>
                :name tab-name
                :conf nil
                :index index
                :current-pane new-pane
                :session-name session-name
                :panes `[,new-pane])))

(cl-defun mesh:tab--command-new (state)
  (cl-letf* ((current-session (glof:get state :current-session))
             (current-session-name (glof:get current-session :name)))
    (cl-letf* ((new-tab-name mesh:default-tab-name)
               (last-tab-index
                (mesh:find-last (glof:get current-session :tabs)))
               (missing-tab-indices
                (mesh:find-missing-index (lambda (tab) (glof:get tab :index))
                                      (glof:get current-session :tabs)))
               (new-tab-index
                (if missing-tab-indices
                    (mesh:first missing-tab-indices)
                  (1+  last-tab-index))))
      (cl-letf* ((new-tab (mesh:tab--new new-tab-name current-session-name
                                      new-tab-index))
                 (current-tab (glof:get current-session :current-tab))
                 (current-tabs (glof:get current-session :tabs)))
        (cl-letf* ((new-current-tab (glof:assoc  current-tab
                                                 :conf (current-window-configuration)))
                   (new-current-tabs (mesh:substitute-if-v new-current-tab
                                                        (lambda (tab)
                                                          (eq (glof:get tab :index)
                                                              (glof:get current-tab :index)))
                                                        current-tabs))
                   (new-current-session (thread-first current-session
                                          (glof:assoc :current-tab new-current-tab
                                                      :tabs new-current-tabs))))
          (delete-other-windows)
          (switch-to-buffer
           (thread-first new-tab
             (glof:get :panes)
             mesh:first
             (glof:get :buffer)))
          (cl-letf* ((new-tab (thread-first new-tab
                                (glof:assoc :conf
                                            (current-window-configuration))))
                     (new-session
                      (thread-first new-current-session
                        (glof:assoc :tabs (mesh:conj new-tab
                                                  (glof:get new-current-session :tabs))
                                    :current-tab new-tab))))
            (glof:assoc state
                        :current-session new-session
                        :sessions
                        (mesh:tab--subst-session state
                                              new-session current-session))))))))

(cl-defun mesh:tab--command-split (state)
  (cl-letf* ((current-session (glof:get state :current-session))
             (current-tab (thread-first current-session
                            (glof:get :current-tab)))
             (pane (glof:get current-tab :current-pane))
             (pane-last-index
              (seq-max (seq-map
                        (lambda (p) (glof:get p :index))
                        (glof:get current-tab :panes))))
             (pane-missing-indices
              (mesh:find-missing-index (lambda (p) (glof:get p :index))
                                    (glof:get current-tab :panes))))
    (cl-letf* ((new-pane (mesh:pane--create
                          state
                          (if pane-missing-indices
                              (mesh:first pane-missing-indices)
                            (1+  pane-last-index)))))
      (cl-letf ((new-window (split-window nil nil 'below)))
        (set-window-buffer new-window (glof:get new-pane :buffer))
        (select-window new-window))
      (cl-letf* ((new-tab (thread-first current-tab
                            (glof:assoc
                             :current-pane new-pane
                             :panes (mesh:conj
                                     new-pane
                                     (glof:get current-tab :panes))
                             :conf (current-window-configuration))))
                 (new-tabs (mesh:substitute-if-v
                            new-tab
                            (lambda (tab)
                              (eq (glof:get tab :index)
                                  (glof:get current-tab :index)))
                            (glof:get current-session :tabs))))
        (cl-letf ((new-session
                   (thread-first current-session
                     (glof:assoc :tabs new-tabs
                                 :current-tab new-tab))))
          (glof:assoc state
                      :current-session new-session
                      :sessions
                      (mesh:tab--subst-session state
                                            new-session current-session)))))))

(cl-defun mesh:tab--command-vsplit (state)
  (cl-letf* ((current-session (glof:get state :current-session))
             (current-tab (thread-first current-session
                            (glof:get :current-tab)))
             (pane (glof:get current-tab :current-pane))
             (pane-last-index
              (seq-max (seq-map
                        (lambda (p) (glof:get p :index))
                        (glof:get current-tab :panes))))
             (pane-missing-indices
              (mesh:find-missing-index (lambda (p) (glof:get p :index))
                                    (glof:get current-tab :panes))))
    (cl-letf* ((new-pane (mesh:pane--create
                          state
                          (if pane-missing-indices
                              (mesh:first pane-missing-indices)
                            (1+  pane-last-index)))))
      (cl-letf ((new-window (split-window nil nil 'right)))
        (set-window-buffer new-window (glof:get new-pane :buffer))
        (select-window new-window))
      (cl-letf* ((new-tab (thread-first current-tab
                            (glof:assoc :current-pane new-pane
                                        :panes
                                        (mesh:conj new-pane
                                                (glof:get current-tab :panes))
                                        :conf
                                        (current-window-configuration))))
                 (new-tabs (mesh:substitute-if-v new-tab
                                              (lambda (tab)
                                                (eq (glof:get tab :index)
                                                    (glof:get current-tab :index)))
                                              (glof:get current-session :tabs)))
                 (new-session (thread-first current-session
                                (glof:assoc :tabs new-tabs
                                            :current-tab new-tab))))

        (glof:assoc state
                    :current-session new-session
                    :sessions
                    (mesh:tab--subst-session state
                                          new-session current-session))))))

(cl-defun mesh:tab--subst-session (state new-session old-session)
  (mesh:substitute-if-v
   new-session
   (lambda (session)
     (cl-equalp (glof:get old-session :name)
                (glof:get session :name)))
   (glof:get state :sessions)))

(cl-defun mesh:tab--command-next (state)
  (cl-letf* ((current-session (glof:get state :current-session))
             (current-tab (glof:get current-session :current-tab))
             (current-tabs (glof:get current-session :tabs))
             (next-tab (mesh:find-next `[:tab ,current-tab] current-tabs)))
    (when next-tab
      (cl-letf* ((new-current-session current-session))
        (cl-letf* ((new-tab
                    (thread-first current-tab
                      (glof:assoc :conf (current-window-configuration))))
                   (new-tabs (mesh:substitute-if-v new-tab
                                                (lambda (tab)
                                                  (eq (glof:get tab :index)
                                                      (glof:get current-tab :index)))
                                                (glof:get current-session :tabs)))
                   (new-current-session
                    (thread-first current-session
                      (glof:assoc :tabs new-tabs
                                  :current-tab next-tab))))
          (set-window-configuration (glof:get next-tab :conf))
          (glof:assoc state
                      :current-session new-current-session
                      :sessions
                      (mesh:tab--subst-session state
                                            new-current-session current-session)))))))

(cl-defun mesh:tab--command-prev (state)
  (cl-letf* ((current-session (glof:get state :current-session))
             (current-tab (glof:get current-session :current-tab))
             (current-tabs (glof:get current-session :tabs))
             (prev-tab (mesh:find-prev `[:tab ,current-tab] current-tabs)))
    (when prev-tab
      (cl-letf* ((new-current-tab (thread-first current-tab
                                    (glof:assoc :conf (current-window-configuration))))
                 (new-current-tabs (mesh:substitute-if-v new-current-tab
                                                      (lambda (tab)
                                                        (eq (glof:get tab :index)
                                                            (glof:get current-tab :index)))
                                                      (glof:get current-session :tabs)))
                 (new-current-session
                  (thread-first current-session
                    (glof:assoc :tabs new-current-tabs
                                :current-tab prev-tab))))
        (set-window-configuration (glof:get prev-tab :conf))
        (glof:assoc state
                    :current-session new-current-session
                    :sessions
                    (mesh:tab--subst-session state
                                          new-current-session current-session))))))

(cl-defun mesh:tab--command-kill (state)
  (cl-letf ((current-session (glof:get state :current-session)))
    (cl-letf ((current-tab (glof:get current-session :current-tab))
              (current-tabs (glof:get current-session :tabs)))
      (pcase (seq-length current-tabs)
        (1 (mesh:session--command-kill current-session))
        (_ (cl-letf* ((next-tab (mesh:find-next `[:tab ,current-tab] current-tabs))
                      (new-current-session current-session))
             (mesh:tab--kill-panes current-tab)
             (cl-letf* ((new-current-tabs (mesh:removev
                                           (lambda (tab)
                                             (eq (glof:get tab :index)
                                                 (glof:get current-tab :index)))
                                           current-tabs))
                        (new-current-session (thread-first current-session
                                               (glof:assoc :tabs new-current-tabs
                                                           :current-tab next-tab))))
               (set-window-configuration (glof:get next-tab :conf))
               (glof:assoc state
                           :current-session new-current-session
                           :sessions (mesh:tab--subst-session state
                                                           new-current-session current-session)))))))))

(cl-defun mesh:tab--kill-panes (tab)
  (seq-each
   #'kill-buffer
   (seq-map
    (lambda (p) (glof:get p :buffer))
    (glof:get tab :panes))))

(provide 'mesh-tab)

;;; mesh-tab.el ends here
