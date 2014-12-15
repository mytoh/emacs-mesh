;;; mesh-session -*- lexical-binding: t; coding: utf-8; -*-

;;; Code:

(require 'cl-lib)
(require 'eieio)

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
      (make-instance 'mesh:session
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
        (oset new-tab :conf (current-window-configuration))
        (oset new-session :tabs
              (cl-subst new-tab current-tab current-session-tabs))
        (setq mesh:*session-list*
              (cl-subst new-session current-session
                        (mesh:session-list)))))
    (cl-letf* ((new-session (mesh:session--new
                             new-session-name
                             (mesh:session-list)))
               (tab (car (mesh:get-tabs new-session)))
               (conf (mesh:get-conf tab)))
      (switch-to-buffer
       (thread-first tab
         mesh:get-panes
         car
         mesh:get-buffer))
      (delete-other-windows)
      (mesh:set-current-session new-session)
      (setq mesh:*session-list*
            (append (mesh:session-list) (list new-session))))))

(cl-defun mesh:session--command-next ()
  (cl-letf* ((current-session (mesh:current-session))
             (next-session
              (mesh:session--find-next-session current-session
                                               (mesh:session-list))))
    (when next-session
      (cl-letf* ((current-session-tabs (mesh:get-tabs current-session))
                 (current-session-tab (mesh:get-current-tab current-session)))
        (cl-letf ((new-session current-session)
                  (new-tab current-session-tab))
          (oset new-tab :conf (current-window-configuration))
          (oset new-session :tabs
                (cl-subst new-tab current-session-tab current-session-tabs))
          (setq mesh:*session-list*
                (cl-subst new-session current-session
                          (mesh:session-list)))))
      (cl-letf* ((next-session-conf (thread-first next-session
                                      mesh:get-current-tab
                                      mesh:get-conf)))
        (set-window-configuration next-session-conf)
        (mesh:set-current-session next-session)))))

(defmethod mesh:session--find-next-session ((current-session mesh:session) sessions)
  (cl-letf* ((current-session-pos (cl-position
                                   current-session
                                   sessions)))
    (cond ((eq (length sessions) 1)
           nil)
          ((eq (- (length sessions) 1) current-session-pos)
           (car sessions))
          ((< current-session-pos (- (length sessions) 1))
           (cl-nth-value (+ current-session-pos 1) sessions))
          (t nil))))

(cl-defun mesh:session--command-prev ()
  (cl-letf* ((current-session (mesh:current-session))
             (prev-session
              (mesh:session--find-prev-session current-session
                                               (mesh:session-list))))
    (when prev-session
      (cl-letf* ((current-session-tabs (mesh:get-tabs current-session))
                 (current-session-tab (mesh:get-current-tab current-session)))
        (cl-letf ((new-session current-session)
                  (new-tab current-session-tab))
          (oset new-tab :conf (current-window-configuration))
          (oset new-session :tabs
                (cl-subst new-tab current-session-tab current-session-tabs))
          (setq mesh:*session-list*
                (cl-subst new-session current-session
                          (mesh:session-list)))))
      (cl-letf* ((prev-session-conf (thread-first prev-session
                                      mesh:get-current-tab
                                      mesh:get-conf)))
        (set-window-configuration prev-session-conf)
        (mesh:set-current-session prev-session)))))

(defmethod mesh:session--find-prev-session ((current-session mesh:session) sessions)
  (cl-letf* ((current-session-pos (cl-position
                                   current-session
                                   sessions)))
    (cond ((eq (length sessions) 1)
           nil)
          ((eq 0 current-session-pos)
           (car (last sessions)))
          (t
           (cl-nth-value (- current-session-pos 1) sessions)))))

(provide 'mesh-session)

;;; mesh-session.el ends here
