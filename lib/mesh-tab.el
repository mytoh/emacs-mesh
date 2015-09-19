;;; mesh-tab -*- lexical-binding: t; coding: utf-8; -*-

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'seq)
(require 'glof)

(require 'mesh-class "lib/mesh-class")
(require 'mesh-core "lib/mesh-core")
(require 'mesh-pane "lib/mesh-pane")

(cl-defun mesh:tab--new (tab-name session-name &optional (index 0))
  (cl-letf* ((new-pane (mesh:pane--new session-name
                                       tab-name
                                       index)))
    (glof:assoc mesh:<tab>-template
                :name tab-name
                :conf nil
                :index index
                :current-pane new-pane
                :session session-name
                :panes (list new-pane))
    ;; (make-instance 'mesh:<tab>
    ;;                :name tab-name
    ;;                :conf nil
    ;;                :index index
    ;;                :current-pane new-pane
    ;;                :session session-name
    ;;                :panes (list new-pane))
    ))

(cl-defun mesh:tab--command-new ()
  (cl-letf* ((current-session (mesh:current-session))
             (current-session-name (mesh:get-name current-session)))
    (cl-letf* ((new-tab-name mesh:default-tab-name)
               (last-tab-index
                (mesh:find-last (mesh:get-tabs current-session)))
               (last-tab
                (seq-find
                 (lambda (tab)
                   (eq last-tab-index (glof:get tab :index)))
                 (mesh:get-tabs current-session)))
               (missing-tab-indices
                (mesh:find-missing-index (lambda (tab) (glof:get tab :index))
                                         (mesh:get-tabs current-session)))
               (new-tab-index
                (if missing-tab-indices
                    (cl-first missing-tab-indices)
                  (1+  last-tab-index))))
      (cl-letf* ((new-tab (mesh:tab--new new-tab-name current-session-name
                                         new-tab-index))
                 (current-tab (mesh:get-current-tab current-session))
                 (new-current-tab current-tab)
                 (current-tabs (mesh:get-tabs current-session))
                 (new-session current-session))

        ;; (setf (mesh:get-conf new-current-tab) (current-window-configuration))
        ;; (setf (mesh:get-tabs new-session)
        ;;       (cl-subst new-current-tab current-tab current-tabs))
        (setf (mesh:get-tabs new-session)
              (thread-first new-current-tab
                (glof:assoc :conf (current-window-configuration))
                (cl-substitute-if
                 (lambda (tab)
                   (eq (glof:get tab :index)
                       (glof:get current-tab :index)))
                 current-tabs)))

        (cl-letf ((new-pane-buffer
                   (thread-first new-tab
                     (glof:get :panes)
                     cl-first
                     (glof:get :buffer))))
          (delete-other-windows)
          (switch-to-buffer new-pane-buffer))
        ;; (setf (mesh:get-conf new-tab) (current-window-configuration))
        ;; (setf (mesh:get-tabs new-session)
        ;;       (append (mesh:get-tabs new-session)
        ;;               (list new-tab)))
        (setf (mesh:get-tabs new-session)
              (cons
               (thread-first new-tab
                 (glof:assoc :conf (current-window-configuration)))
               (mesh:get-tabs new-session)))
        
        (setf (mesh:get-current-tab new-session) new-tab)
        (mesh:set-current-session new-session)
        ;; (setq mesh:*session-list*
        ;;       (cl-subst new-session current-session
        ;;                 mesh:*session-list*))
        (mesh:tab--subst-session
         new-session current-session)
        ))))

(cl-defun mesh:tab--command-split ()
  (cl-letf* ((current-session (mesh:current-session))
             (current-tab (thread-first current-session
                            mesh:get-current-tab))
             (pane (glof:get current-tab :current-pane))
             (pane-last-index
              (seq-max (seq-map
                        (lambda (p) (glof:get p :index))
                        (glof:get current-tab :panes))))
             (pane-missing-indices
              (mesh:find-missing-index (lambda (p) (glof:get p :index))
                                       (glof:get current-tab :panes))))
    (message pane-missing-indices)
    (cl-letf* ((new-session current-session)
               (new-tab current-tab)
               (new-pane (mesh:pane--create
                          current-tab
                          (mesh:get-name current-session)
                          (if pane-missing-indices
                              (cl-first pane-missing-indices)
                            (1+  pane-last-index)))))
      (cl-letf ((new-window (split-window nil nil 'below)))
        (set-window-buffer new-window (glof:get new-pane :buffer))
        (select-window new-window))
      ;; (setf (glof:get new-tab :current-pane) new-pane)
      ;; (setf (mesh:get-panes new-tab)
      ;;       (append (mesh:get-panes current-tab)
      ;;               (list new-pane)))
      ;; (setf (mesh:get-conf new-tab) (current-window-configuration))

      ;; (setf (mesh:get-tabs new-session)
      ;;       (cl-subst new-tab current-tab
      ;;                 (mesh:get-tabs current-session)))

      (cl-letf* ((new-tab (thread-first new-tab
                            (glof:assoc :current-pane new-pane)
                            (glof:assoc :panes (cons
                                                new-pane
                                                (glof:get current-tab :panes)))
                            (glof:assoc :conf (current-window-configuration))))
                 (new-tabs (cl-substitute-if
                            new-tab
                            (lambda (tab)
                              (eq (glof:get tab :index)
                                  (glof:get current-tab :index)))
                            (mesh:get-tabs current-session))))
        (setf (mesh:get-tabs new-session) new-tabs)
        (setf (mesh:get-current-tab new-session) new-tab)
        (mesh:set-current-session new-session)
        (mesh:tab--subst-session
         new-session current-session)))))

(cl-defun mesh:tab--command-vsplit ()
  (cl-letf* ((current-session (mesh:current-session))
             (current-tab (thread-first current-session
                            mesh:get-current-tab))
             (pane (glof:get current-tab :current-pane))
             (pane-last-index
              (seq-max (seq-map
                        (lambda (p) (glof:get p :index))
                        (glof:get current-tab :panes))))
             (pane-missing-indices
              (mesh:find-missing-index (lambda (p) (glof:get p :index)) (glof:get current-tab :panes))))
    (cl-letf* ((new-session current-session)
               (new-tab current-tab)
               (new-pane (mesh:pane--create
                          current-tab
                          (mesh:get-name current-session)
                          (if pane-missing-indices
                              (cl-first pane-missing-indices)
                            (1+  pane-last-index)))))
      (cl-letf ((new-window (split-window nil nil 'right)))
        (set-window-buffer new-window (glof:get new-pane :buffer))
        (select-window new-window))
      ;; (mesh:set-slots new-tab
      ;;   'current-pane new-pane
      ;;   'panes
      ;;   (append (glof:get current-tab :panes)
      ;;           (list new-pane))
      ;;   'conf (current-window-configuration))
      ;; (setf (mesh:get-tabs new-session)
      ;;             (cl-subst new-tab current-tab
      ;;                       (mesh:get-tabs current-session)))
      (setf (mesh:get-tabs new-session)
            (thread-first new-tab
              (glof:assoc :current-pane new-pane)
              (glof:assoc :panes
                          (cons new-pane
                                (glof:get current-tab :panes)))
              (glof:assoc :conf
                          (current-window-configuration))
              (cl-substitute-if
               (lambda (tab)
                 (eq (glof:get tab :index)
                     (glof:get current-tab :index)))
               (mesh:get-tabs current-session))))
      
      (mesh:set-current-session new-session)
      (mesh:tab--subst-session
       new-session current-session))))

(cl-defun mesh:tab--subst-session (new-session old-session)
  (setq mesh:*sessions*
        (cl-substitute-if
         new-session
         (lambda (session)
           (cl-equalp (mesh:get-name old-session)
                      (mesh:get-name session)))
         mesh:*sessions*)))

(cl-defun mesh:tab--command-next ()
  (cl-letf* ((current-session (mesh:current-session))
             (current-tab (mesh:get-current-tab current-session))
             (current-tabs (mesh:get-tabs current-session))
             (next-tab (mesh:find-next `[:tab ,current-tab] current-tabs)))
    (when next-tab
      (cl-letf* ((new-current-tab current-tab)
                 (new-current-session current-session))
        ;; (setf (mesh:get-conf new-current-tab) (current-window-configuration))
        ;; (setf (mesh:get-tabs new-current-session)
        ;;       (cl-subst new-current-tab current-tab (mesh:get-tabs current-session)))
        (setf (mesh:get-tabs new-current-session)
              (thread-first new-current-tab
                (glof:assoc :conf (current-window-configuration))
                (cl-substitute-if
                 (lambda (tab)
                   (eq (glof:get tab :index)
                       (glof:get current-tab :index)))
                 (mesh:get-tabs current-session))))
        (setf (mesh:get-current-tab new-current-session) next-tab)
        (mesh:set-current-session new-current-session)
        (mesh:tab--subst-session new-current-session current-session)
        ;; (setq mesh:*session-list*
        ;;       (cl-subst new-current-session current-session mesh:*session-list*))
        (set-window-configuration (glof:get next-tab :conf))))))

(cl-defun mesh:tab--command-prev ()
  (cl-letf* ((current-session (mesh:current-session))
             (current-tab (mesh:get-current-tab current-session))
             (current-tabs (mesh:get-tabs current-session))
             (prev-tab (mesh:find-prev `[:tab ,current-tab] current-tabs)))
    (when prev-tab
      (cl-letf* ((new-current-tab current-tab)
                 (new-current-session current-session))
        ;; (setf (mesh:get-conf new-current-tab) (current-window-configuration))
        ;; (setf (mesh:get-tabs new-current-session)
        ;; (cl-subst new-current-tab current-tab (mesh:get-tabs current-session)))
        (setf (mesh:get-tabs new-current-session)
              (thread-first new-current-tab
                (glof:assoc :conf (current-window-configuration))
                (cl-substitute-if
                 (lambda (tab)
                   (eq (glof:get tab :index)
                       (glof:get current-tab :index)))
                 (mesh:get-tabs current-session))))
        (setf (mesh:get-current-tab new-current-session) prev-tab)
        (mesh:set-current-session new-current-session)
        (mesh:tab--subst-session new-current-session current-session)
        ;; (setq mesh:*session-list*
        ;;       (cl-subst new-current-session current-session mesh:*session-list*))
        (set-window-configuration (glof:get prev-tab :conf))))))

(cl-defun mesh:tab--command-kill ()
  (cl-letf ((current-session (mesh:current-session)))
    (with-slots ((current-tab current-tab)
                 (current-tabs tabs))
        current-session
      (pcase (seq-length current-tabs)
        (1 (mesh:session--command-kill))
        (_ (cl-letf* ((next-tab (mesh:find-next `[:tab ,current-tab] current-tabs))
                      (new-current-session current-session))
             (mesh:tab--kill-panes current-tab)
             ;; (mesh:set-slots new-current-session
             ;;   'tabs (cl-remove current-tab current-tabs)
             ;;   'current-tab next-tab)
             (setf (mesh:get-tabs new-current-session)
                   (seq-remove
                    (lambda (tab)
                      (eq (glof:get tab :index)
                          (glof:get current-tab :index)))
                    current-tabs))
             (setf (mesh:get-current-tab new-current-session)
                   next-tab)
             (mesh:set-current-session new-current-session)
             (mesh:tab--subst-session new-current-session current-session)
             (set-window-configuration (glof:get next-tab :conf))))))))

(cl-defmethod mesh:tab--kill-panes (tab)
  (seq-each
   #'kill-buffer
   (seq-map
    (lambda (p) (glof:get p :buffer))
    (glof:get tab :panes))))

(provide 'mesh-tab)

;;; mesh-tab.el ends here
