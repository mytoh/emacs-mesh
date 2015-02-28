;;; mesh-session -*- lexical-binding: t; coding: utf-8; -*-

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'seq)

(require 'mesh-core "lib/mesh-core")
(require 'mesh-class "lib/mesh-class")
(require 'mesh-tab "lib/mesh-tab")


(cl-defun mesh:session--new (session-name sessions)
  (if-let ((found (cl-find-if (lambda (s) (cl-equalp (mesh:get-name s)
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
             (if (cl-find-if
                  (lambda (session) (cl-equalp new-session-name
                                          (mesh:get-name session)))
                  (mesh:session-list))
                 (concat new-session-name "*")
               new-session-name)))
    (cl-letf* ((current-session (mesh:current-session))
               (current-session-tabs (mesh:get-tabs current-session))
               (current-tab (mesh:get-current-tab current-session)))
      (cl-letf* ((new-session current-session)
                 (new-tab current-tab))
        (setf (mesh:get-conf new-tab) (current-window-configuration))
        (setf (mesh:get-tabs new-session)
              (cl-subst new-tab current-tab current-session-tabs))
        (setq mesh:*session-list*
              (cl-subst new-session current-session
                        (mesh:session-list)))))
    (cl-letf* ((new-session (mesh:session--new
                             new-session-name
                             (mesh:session-list)))
               (tab (cl-first (mesh:get-tabs new-session)))
               (conf (mesh:get-conf tab)))
      (switch-to-buffer
       (thread-first tab
         mesh:get-panes
         cl-first
         mesh:get-buffer))
      (delete-other-windows)
      (mesh:set-current-session new-session)
      (setq mesh:*session-list*
            (append (mesh:session-list) (list new-session))))))

(cl-defun mesh:session--command-next ()
  (cl-letf* ((current-session (mesh:current-session))
             (next-session
              (mesh:find-next current-session
                              (mesh:session-list))))
    (when next-session
      (cl-letf* ((current-session-tabs (mesh:get-tabs current-session))
                 (current-session-tab (mesh:get-current-tab current-session)))
        (cl-letf ((new-session current-session)
                  (new-tab current-session-tab))
          (setf (mesh:get-conf new-tab) (current-window-configuration))
          (setf (mesh:get-tabs new-session)
                (cl-subst new-tab current-session-tab current-session-tabs))
          (setq mesh:*session-list*
                (cl-subst new-session current-session
                          (mesh:session-list)))))
      (cl-letf* ((next-session-conf (thread-first next-session
                                      mesh:get-current-tab
                                      mesh:get-conf)))
        (set-window-configuration next-session-conf)
        (mesh:set-current-session next-session)))))

(cl-defun mesh:session--command-prev ()
  (cl-letf* ((current-session (mesh:current-session))
             (prev-session
              (mesh:find-prev current-session
                              (mesh:session-list))))
    (when prev-session
      (cl-letf* ((current-session-tabs (mesh:get-tabs current-session))
                 (current-session-tab (mesh:get-current-tab current-session)))
        (cl-letf ((new-session current-session)
                  (new-tab current-session-tab))
          (setf (mesh:get-conf new-tab) (current-window-configuration))
          (setf (mesh:get-tabs new-session)
                (cl-subst new-tab current-session-tab current-session-tabs))
          (setq mesh:*session-list*
                (cl-subst new-session current-session
                          (mesh:session-list)))))
      (cl-letf* ((prev-session-conf (thread-first prev-session
                                      mesh:get-current-tab
                                      mesh:get-conf)))
        (set-window-configuration prev-session-conf)
        (mesh:set-current-session prev-session)))))

(cl-defun mesh:session--command-kill ()
  (cl-letf* ((current-session (mesh:current-session))
             (current-session-list (mesh:session-list)))
    (pcase (seq-length current-session-list)
      (1
       (cl-letf ((current-tab (mesh:get-current-tab current-session))
                 (current-tabs (mesh:get-tabs current-session)))
         (mesh:unset-session-list)
         (jump-to-register mesh:window-configuration-name)
         (mesh:unset-inside-session)
         (mesh:unset-current-session)
         (seq-each
          #'mesh:tab--kill-panes
          current-tabs)))
      (_
       (cl-letf ((current-tab (mesh:get-current-tab current-session))
                 (current-tabs (mesh:get-tabs current-session))
                 (next-session (mesh:find-next current-session current-session-list)))
         (setq mesh:*session-list*
               (cl-remove current-session (mesh:session-list)))
         (cl-letf* ((next-session-conf (thread-first next-session
                                         mesh:get-current-tab
                                         mesh:get-conf)))
           (set-window-configuration next-session-conf)
           (mesh:set-current-session next-session))
         (seq-each
          #'mesh:tab--kill-panes
          current-tabs))))))


(provide 'mesh-session)

;;; mesh-session.el ends here
