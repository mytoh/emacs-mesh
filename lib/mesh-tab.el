;;; mesh-tab -*- lexical-binding: t; coding: utf-8; -*-

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'seq)

(require 'mesh-class "lib/mesh-class")
(require 'mesh-core "lib/mesh-core")
(require 'mesh-pane "lib/mesh-pane")

(cl-defun mesh:tab--new (tab-name session-name &optional (index 0))
  (cl-letf* ((new-pane (mesh:pane--new session-name
                                       tab-name
                                       index)))
    (make-instance 'mesh:tab
                   :name tab-name
                   :conf nil
                   :index index
                   :current-pane new-pane
                   :session session-name
                   :panes (list new-pane))))

(cl-defun mesh:tab--command-new ()
  (cl-letf* ((current-session (mesh:current-session))
             (current-session-name (mesh:get-name current-session)))
    (cl-letf* ((new-tab-name mesh:default-tab-name)
               (last-tab-index
                (mesh:find-last (mesh:get-tabs current-session)))
               (last-tab
                (cl-find-if
                 (lambda (tab)
                   (eq last-tab-index (mesh:get-index tab)))
                 (mesh:get-tabs current-session)))
               (missing-tab-indices
                (mesh:find-missing-index #'mesh:get-index
                                         (mesh:get-tabs current-session)))
               (new-tab-index
                (if missing-tab-indices
                    (car missing-tab-indices)
                  (cl-incf  last-tab-index))))
      (cl-letf* ((new-tab (mesh:tab--new new-tab-name current-session-name
                                         new-tab-index))
                 (current-tab (mesh:get-current-tab current-session))
                 (new-current-tab current-tab)
                 (current-tabs (mesh:get-tabs current-session))
                 (new-session current-session))

        (setf (mesh:get-conf new-current-tab) (current-window-configuration))
        (setf (mesh:get-tabs new-session)
              (cl-subst new-current-tab current-tab current-tabs))

        (cl-letf ((new-pane-buffer
                   (thread-first new-tab
                     mesh:get-panes
                     car
                     mesh:get-buffer)))
          (delete-other-windows)
          (switch-to-buffer new-pane-buffer))
        (setf (mesh:get-conf new-tab) (current-window-configuration))
        (setf (mesh:get-tabs new-session)
              (append (mesh:get-tabs new-session)
                      (list new-tab)))
        (setf (mesh:get-current-tab new-session) new-tab)
        (mesh:set-current-session new-session)
        (setq mesh:*session-list*
              (cl-subst new-session current-session
                        mesh:*session-list*))))))

(cl-defun mesh:tab--command-split ()
  (cl-letf* ((current-session (mesh:current-session))
             (current-tab (thread-first current-session
                            mesh:get-current-tab))
             (pane (mesh:get-current-pane current-tab))
             (pane-last-index
              (apply #'max (seq-map
                            #'mesh:get-index
                            (mesh:get-panes current-tab))))
             (pane-missing-indices
              (mesh:find-missing-index #'mesh:get-index (mesh:get-panes current-tab))))
    (cl-letf* ((new-session current-session)
               (new-tab current-tab)
               (new-pane (mesh:pane--create
                          current-tab
                          (mesh:get-name current-session)
                          (if pane-missing-indices
                              (car pane-missing-indices)
                            (cl-incf  pane-last-index)))))
      (cl-letf ((new-window (split-window nil nil 'below)))
        (set-window-buffer new-window (mesh:get-buffer new-pane))
        (select-window new-window))
      (setf (mesh:get-current-pane new-tab) new-pane)
      (setf (mesh:get-panes new-tab)
            (append (mesh:get-panes current-tab)
                    (list new-pane)))
      (setf (mesh:get-conf new-tab) (current-window-configuration))
      (setf (mesh:get-tabs new-session)
            (cl-subst new-tab current-tab
                      (mesh:get-tabs current-session)))
      (mesh:set-current-session new-session)
      (mesh:tab--subst-session-list
       new-session current-session))))

(cl-defun mesh:tab--command-vsplit ()
  (cl-letf* ((current-session (mesh:current-session))
             (current-tab (thread-first current-session
                            mesh:get-current-tab))
             (pane (mesh:get-current-pane current-tab))
             (pane-last-index
              (apply #'max (seq-map
                            #'mesh:get-index
                            (mesh:get-panes current-tab))))
             (pane-missing-indices
              (mesh:find-missing-index #'mesh:get-index (mesh:get-panes current-tab))))
    (cl-letf* ((new-session current-session)
               (new-tab current-tab)
               (new-pane (mesh:pane--create
                          current-tab
                          (mesh:get-name current-session)
                          (if pane-missing-indices
                              (car pane-missing-indices)
                            (cl-incf  pane-last-index)))))
      (cl-letf ((new-window (split-window nil nil 'right)))
        (set-window-buffer new-window (mesh:get-buffer new-pane))
        (select-window new-window))
      (mesh:set-slots new-tab
        'current-pane new-pane
        'panes
        (append (mesh:get-panes current-tab)
                (list new-pane))
        'conf (current-window-configuration))
      (setf (mesh:get-tabs new-session)
            (cl-subst new-tab current-tab
                      (mesh:get-tabs current-session)))
      (mesh:set-current-session new-session)
      (mesh:tab--subst-session-list
       new-session current-session))))

(cl-defun mesh:tab--subst-session-list (new-session old-session)
  (setq mesh:*session-list*
        (cl-subst new-session old-session
                  mesh:*session-list*)))

(cl-defun mesh:tab--command-next ()
  (cl-letf* ((current-session (mesh:current-session))
             (current-tab (mesh:get-current-tab current-session))
             (current-tabs (mesh:get-tabs current-session))
             (next-tab (mesh:find-next current-tab current-tabs)))
    (when next-tab
      (cl-letf* ((new-current-tab current-tab)
                 (new-current-session current-session))
        (setf (mesh:get-conf new-current-tab) (current-window-configuration))
        (setf (mesh:get-tabs new-current-session)
              (cl-subst new-current-tab current-tab (mesh:get-tabs current-session)))
        (setf (mesh:get-current-tab new-current-session) next-tab)
        (mesh:set-current-session new-current-session)
        (setq mesh:*session-list*
              (cl-subst new-current-session current-session mesh:*session-list*))
        (set-window-configuration (mesh:get-conf next-tab))))))

(cl-defun mesh:tab--command-prev ()
  (cl-letf* ((current-session (mesh:current-session))
             (current-tab (mesh:get-current-tab current-session))
             (current-tabs (mesh:get-tabs current-session))
             (prev-tab (mesh:find-prev current-tab current-tabs)))
    (when prev-tab
      (cl-letf* ((new-current-tab current-tab)
                 (new-current-session current-session))
        (setf (mesh:get-conf new-current-tab) (current-window-configuration))
        (setf (mesh:get-tabs new-current-session)
              (cl-subst new-current-tab current-tab (mesh:get-tabs current-session)))
        (setf (mesh:get-current-tab new-current-session) prev-tab)
        (mesh:set-current-session new-current-session)
        (setq mesh:*session-list*
              (cl-subst new-current-session current-session mesh:*session-list*))
        (set-window-configuration (mesh:get-conf prev-tab))))))

(cl-defun mesh:tab--command-kill ()
  (cl-letf ((current-session (mesh:current-session)))
    (with-slots ((current-tab current-tab)
                 (current-tabs tabs))
        current-session
      (pcase (seq-length current-tabs)
        (1 (mesh:session--command-kill))
        (_ (cl-letf* ((next-tab (mesh:find-next current-tab current-tabs))
                      (new-current-session current-session))
             (mesh:tab--kill-panes current-tab)
             (mesh:set-slots new-current-session
               'tabs (cl-remove current-tab current-tabs)
               'current-tab next-tab)
             (mesh:set-current-session new-current-session)
             (mesh:tab--subst-session-list new-current-session current-session)
             (set-window-configuration (mesh:get-conf next-tab))))))))

(cl-defmethod mesh:tab--kill-panes ((tab mesh:tab))
  (seq-each
   #'kill-buffer
   (seq-map
    #'mesh:get-buffer
    (mesh:get-panes tab))))

(provide 'mesh-tab)

;;; mesh-tab.el ends here
