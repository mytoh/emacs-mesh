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
  (if-let ((found (seq-find (lambda (s) (cl-equalp (mesh:get-name s)
                                              session-name))
                            sessions)))
      found
    (cl-letf ((new-tab (mesh:tab--new
                        mesh:default-tab-name
                        session-name)))
      (make-instance 'mesh:<session>
                     :name session-name
                     :current-tab new-tab
                     :tabs (list new-tab)))))

(cl-defun mesh:session--command-create (new-session-name)
  (cl-letf ((new-session-name
             (if (seq-find
                  (lambda (session) (cl-equalp new-session-name
                                          (mesh:get-name session)))
                  (mesh:sessions))
                 (seq-concatenate 'string new-session-name "*")
               new-session-name)))
    (cl-letf* ((current-session (mesh:current-session))
               (current-session-tabs (mesh:get-tabs current-session))
               (current-tab (mesh:get-current-tab current-session)))
      (cl-letf* ((new-session current-session)
                 (new-tab current-tab))
        ;; (setf (mesh:get-conf new-tab) (current-window-configuration))
        ;; (setf (mesh:get-tabs new-session)
        ;;       (cl-subst new-tab current-tab current-session-tabs))
        (setf (mesh:get-tabs new-session)
              (thread-first new-tab
                (glof:assoc :conf (current-window-configuration))
                (cl-substitute-if (lambda (tab)
                                    (eq (glof:get current-tab :index)
                                        (glof:get tab :index)))
                                  current-session-tabs)))
        ;; (setq mesh:*session-list*
        ;;       (cl-subst new-session current-session
        ;;                 (mesh:sessions)))
        (mesh:tab--subst-session
         new-session current-session)
        ))
    (cl-letf* ((new-session (mesh:session--new
                             new-session-name
                             (mesh:sessions)))
               (tab (cl-first (mesh:get-tabs new-session)))
               (conf (glof:get tab :conf)))
      (switch-to-buffer
       (thread-first tab
         (glof:get :panes)
         cl-first
         (glof:get :buffer)))
      (delete-other-windows)
      (mesh:set-current-session new-session)
      (setq mesh:*sessions*
            (append (mesh:sessions) (list new-session))))))

(cl-defun mesh:session--command-next ()
  (cl-letf* ((current-session (mesh:current-session))
             (next-session
              (mesh:find-next `[:session ,current-session ]
                              (mesh:sessions))))
    (when next-session
      (cl-letf* ((current-session-tabs (mesh:get-tabs current-session))
                 (current-session-tab (mesh:get-current-tab current-session)))
        (cl-letf ((new-session current-session)
                  (new-tab current-session-tab))
          ;; (setf (mesh:get-conf new-tab) (current-window-configuration))
          ;; (setf (mesh:get-tabs new-session)
          ;;       (cl-subst new-tab current-session-tab current-session-tabs))
          (setf (mesh:get-tabs new-session)
                (thread-first new-tab
                  (glof:assoc :conf (current-window-configuration))
                  (cl-substitute-if (lambda (tab)
                                      (eq (glof:get tab :index)
                                          (glof:get current-session-tab
                                                    :index)))
                                    current-session-tabs)))
          ;; (setq mesh:*session-list*
          ;;       (cl-subst new-session current-session
          ;;                 (mesh:sessions)))
          (mesh:tab--subst-session
           new-session current-session)
          ))
      (cl-letf* ((next-session-conf (thread-first next-session
                                      mesh:get-current-tab
                                      (glof:get :conf))))
        (set-window-configuration next-session-conf)
        (mesh:set-current-session next-session)))))

(cl-defun mesh:session--command-prev ()
  (cl-letf* ((current-session (mesh:current-session))
             (prev-session
              (mesh:find-prev `[:session ,current-session ]
                              (mesh:sessions))))
    (when prev-session
      (cl-letf* ((current-session-tabs (mesh:get-tabs current-session))
                 (current-session-tab (mesh:get-current-tab current-session)))
        (cl-letf ((new-session current-session)
                  (new-tab current-session-tab))
          ;; (setf (mesh:get-conf new-tab) (current-window-configuration))
          ;; (setf (mesh:get-tabs new-session)
          ;;       (cl-subst new-tab current-session-tab current-session-tabs))
          (setf (mesh:get-tabs new-session)
                (thread-first new-tab
                  (glof:assoc :conf (current-window-configuration))
                  (cl-substitute-if
                   (lambda (tab)
                     (eq (glof:get tab :index)
                         (glof:get current-session-tab :index)))
                   current-session-tabs)))
          ;; (setq mesh:*session-list*
          ;;       (cl-subst new-session current-session
          ;;                 (mesh:sessions)))
          (mesh:tab--subst-session new-session current-session)
          ))
      (cl-letf* ((prev-session-conf (thread-first prev-session
                                      mesh:get-current-tab
                                      (glof:get :conf))))
        (set-window-configuration prev-session-conf)
        (mesh:set-current-session prev-session)))))

(cl-defun mesh:session--command-kill ()
  (cl-letf* ((current-session (mesh:current-session))
             (current-session-list (mesh:sessions)))
    (pcase (seq-length current-session-list)
      (1
       (cl-letf ((current-tab (mesh:get-current-tab current-session))
                 (current-tabs (mesh:get-tabs current-session)))
         (mesh:unset-sessions)
         (jump-to-register mesh:*window-configuration-name*)
         (mesh:unset-inside-session)
         (mesh:unset-current-session)
         (seq-each
          #'mesh:tab--kill-panes
          current-tabs)))
      (_
       (cl-letf ((current-tab (mesh:get-current-tab current-session))
                 (current-tabs (mesh:get-tabs current-session))
                 (next-session (mesh:find-next `[:session ,current-session] current-session-list)))
         (setq mesh:*sessions*
               (cl-remove current-session (mesh:sessions)))
         (cl-letf* ((next-session-conf (thread-first next-session
                                         mesh:get-current-tab
                                         (glof:get :conf))))
           (set-window-configuration next-session-conf)
           (mesh:set-current-session next-session))
         (seq-each
          #'mesh:tab--kill-panes
          current-tabs))))))


(provide 'mesh-session)

;;; mesh-session.el ends here
