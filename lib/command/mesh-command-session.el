;;; mesh-command-session -*- lexical-binding: t; coding: utf-8; -*-

;;; Code:


;;;###autoload
(cl-defun mesh:new-session (new-session-name)
  (interactive "sSession name: ")
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

(cl-defun mesh:kill-session ())
(cl-defun mesh:next-session ())
(cl-defun mesh:prev-session ())

(provide 'mesh-command-session)

;;; mesh-command-session.el ends here
