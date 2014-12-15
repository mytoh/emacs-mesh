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

(cl-defun mesh:sesssion--command-create (new-session-name)
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
      (mesh:set-current-session new-session)
      (setq mesh:*session-list*
            (append (mesh:session-list) (list new-session))))))

(provide 'mesh-session)

;;; mesh-session.el ends here
